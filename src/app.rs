use crate::container::Container;
use crate::provider::{Provider, ServiceId};
use anyhow::anyhow;
use log::{error, info};
use scopeguard::defer;
use std::backtrace::Backtrace;
use std::panic;
use std::sync::mpsc::channel;

pub struct App {
    providers: Vec<Provider>,
}

impl App {
    pub fn new(providers: Vec<Provider>) -> Self {
        App { providers }
    }

    pub fn run(self, service_ids: &[ServiceId]) -> anyhow::Result<()> {
        let old_hook = panic::take_hook();
        defer! {
            panic::set_hook(old_hook);
        }

        panic::set_hook(Box::new(|info| panic_hook(info)));

        let run_action = move || -> anyhow::Result<()> {
            info!("Starting app (services: {service_ids:?})");
            self.do_run(&service_ids)
                .map_err(|e| anyhow!("Error running app: {}", e))?;
            info!("App was stopped");
            Ok(())
        };

        panic::catch_unwind(|| run_action()).map_err(|_| anyhow!("App panicked"))?
    }

    fn do_run(self, service_ids: &[ServiceId]) -> anyhow::Result<()> {
        info!("Building container");
        let mut container = Container::new(&self.providers)?;
        info!("Resolving services");
        for service_id in service_ids {
            container.resolve(*service_id)?;
        }
        info!("Setting termination signals handler");
        let (tx, rx) = channel();
        ctrlc::set_handler(move || tx.send(()).expect("Could not propagate termination signal"))?;
        info!("Running. Send termination signal to stop");
        rx.recv()?;
        Ok(())
    }
}

fn panic_hook(info: &panic::PanicHookInfo) {
    error!("Panic: {}. Backtrace: {}", info, Backtrace::capture());
}
