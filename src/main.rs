use depends_rs::{provide_service, supply_value, App, ServiceId};
use std::sync::Arc;

fn main() -> anyhow::Result<()> {
    env_logger::Builder::from_env(env_logger::Env::default()
        .default_filter_or("info"))
        .format_timestamp_secs()
        .format_target(true)
        .init();

    let mut app = App::new(vec![
        supply_value(ServiceA {}),
        supply_value(ServiceB {}),
        provide_service(|r, lc| {
            let service_c = ServiceC {
                service_a: r.resolve_type::<ServiceA>()?,
                service_b: r.resolve_type::<ServiceB>()?,
            };
            lc.register_start_stop(
                || {
                    println!("Started!");
                    Ok(())
                },
                || println!("Stopped!"),
            );
            Ok(service_c)
        }),
    ]);
    app.run(&[ServiceId::of_type::<ServiceC>()])?;
    Ok(())
}

struct ServiceA {}

struct ServiceB {}

struct ServiceC {
    service_a: Arc<ServiceA>,
    service_b: Arc<ServiceB>,
}
