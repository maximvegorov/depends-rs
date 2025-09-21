use depends_rs::app::App;
use depends_rs::provider::provide_service;
use depends_rs::provider::{supply_trait_object, ServiceId};
use std::rc::Rc;

fn main() -> anyhow::Result<()> {
    env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("info"))
        .format_timestamp_secs()
        .format_target(true)
        .init();

    let app = App::new(vec![
        supply_trait_object(Rc::new(ServiceA {}) as Rc<dyn TraitA>),
        supply_trait_object(Rc::new(ServiceB {}) as Rc<dyn TraitB>),
        provide_service(|r, lc| {
            let service_c = ServiceC {
                service_a: r.resolve_trait_object::<dyn TraitA>()?,
                service_b: r.resolve_trait_object::<dyn TraitB>()?,
            };
            lc.register_start_stop(
                || {
                    println!("Started!");
                    Ok(())
                },
                || {
                    println!("Stopped!");
                    Ok(())
                },
            );
            Ok(Rc::new(service_c))
        }),
    ]);
    app.run(&[ServiceId::of_type::<ServiceC>()])?;
    Ok(())
}

trait TraitA {}

struct ServiceA {}

impl TraitA for ServiceA {}

trait TraitB {}

struct ServiceB {}

impl TraitB for ServiceB {}

struct ServiceC {
    service_a: Rc<dyn TraitA>,
    service_b: Rc<dyn TraitB>,
}
