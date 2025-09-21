use crate::container::{DependencyResolver, Lifecycle};
use std::any::{Any, TypeId};
use std::fmt;
use std::panic::UnwindSafe;
use std::rc::Rc;

#[derive(Copy, Clone, Eq, Debug, Hash, PartialEq)]
pub struct ServiceId {
    pub(crate) type_id: TypeId,
    pub(crate) name: Option<&'static str>,
}

impl ServiceId {
    pub fn of_type<T: 'static>() -> Self {
        ServiceId {
            type_id: TypeId::of::<T>(),
            name: None,
        }
    }

    pub fn of_named_type<T: 'static>(name: &'static str) -> Self {
        ServiceId {
            type_id: TypeId::of::<T>(),
            name: Some(name),
        }
    }
}

impl fmt::Display for ServiceId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.name {
            Some(name) => write!(f, "{:?}-{}", self.type_id, name),
            None => write!(f, "{:?}", self.type_id),
        }
    }
}

pub type Service = dyn Any;
pub type Factory = dyn Fn(&DependencyResolver, &mut Lifecycle) -> anyhow::Result<Rc<Service>>;

pub struct Provider {
    pub(crate) service_id: ServiceId,
    pub(crate) factory: Rc<Factory>,
}

impl UnwindSafe for Provider {}

pub fn supply_value<T: 'static>(v: T) -> Provider {
    let service_id = ServiceId::of_type::<T>();
    let factory: Rc<Factory> = to_value_factory(v);
    Provider {
        service_id,
        factory,
    }
}

pub fn supply_named_value<T: 'static>(name: &'static str, v: T) -> Provider {
    let service_id = ServiceId::of_named_type::<T>(name);
    let factory: Rc<Factory> = to_value_factory(v);
    Provider {
        service_id,
        factory,
    }
}

pub fn supply_trait_object<T: ?Sized + 'static>(v: Rc<T>) -> Provider {
    let service_id = ServiceId::of_type::<Rc<T>>();
    let factory = to_trait_object_factory(v);
    Provider {
        service_id,
        factory,
    }
}

pub fn supply_named_trait_object<V: 'static, T: ?Sized + 'static>(
    name: &'static str,
    v: Rc<V>,
) -> Provider {
    let service_id = ServiceId::of_named_type::<Rc<T>>(name);
    let factory: Rc<Factory> = to_trait_object_factory(v);
    Provider {
        service_id,
        factory,
    }
}

pub type ServiceFactory<T> = fn(&DependencyResolver, &mut Lifecycle) -> anyhow::Result<Rc<T>>;

pub fn provide_service<T: 'static>(f: ServiceFactory<T>) -> Provider {
    let service_id = ServiceId::of_type::<T>();
    let factory = to_service_factory(f);
    Provider {
        service_id,
        factory,
    }
}

pub fn provide_named_service<T: 'static>(name: &'static str, f: ServiceFactory<T>) -> Provider {
    let service_id = ServiceId::of_named_type::<T>(name);
    let factory = to_service_factory(f);
    Provider {
        service_id,
        factory,
    }
}

pub fn provide_trait_object<T: ?Sized + 'static>(f: ServiceFactory<T>) -> Provider {
    let service_id = ServiceId::of_type::<Rc<T>>();
    let factory = to_trait_object_service_factory(f);
    Provider {
        service_id,
        factory,
    }
}

pub fn provide_named_trait_object<T: ?Sized + 'static>(f: ServiceFactory<T>) -> Provider {
    let service_id = ServiceId::of_type::<Rc<T>>();
    let factory = to_trait_object_service_factory(f);
    Provider {
        service_id,
        factory,
    }
}

fn to_value_factory<T: 'static>(v: T) -> Rc<Factory> {
    let service: Rc<Service> = Rc::new(v);

    Rc::new(move |_, _| -> anyhow::Result<Rc<Service>> { Ok(service.clone()) })
}

fn to_trait_object_factory<T: ?Sized + 'static>(v: Rc<T>) -> Rc<Factory> {
    Rc::new(move |_, _| -> anyhow::Result<Rc<Service>> { Ok(Rc::new(v.clone()) as Rc<Service>) })
}

fn to_service_factory<T: 'static>(f: ServiceFactory<T>) -> Rc<Factory> {
    Rc::new(
        move |resolver: &DependencyResolver,
              lifecycle: &mut Lifecycle|
              -> anyhow::Result<Rc<Service>> { Ok(f(resolver, lifecycle)?) },
    )
}

fn to_trait_object_service_factory<T: ?Sized + 'static>(f: ServiceFactory<T>) -> Rc<Factory> {
    Rc::new(
        move |resolver: &DependencyResolver,
              lifecycle: &mut Lifecycle|
              -> anyhow::Result<Rc<Service>> {
            Ok(Rc::new(f(resolver, lifecycle)?) as Rc<Service>)
        },
    )
}
