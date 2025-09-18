use anyhow::{anyhow};
use log::{debug, info};
use std::any::{Any, TypeId};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::rc::Rc;
use std::sync::Arc;
use std::sync::mpsc::channel;
use thiserror::Error;

pub struct App {
    services: Vec<(ServiceId, Rc<Factory>)>,
}

impl App {
    pub fn new(services: Vec<(ServiceId, Rc<Factory>)>) -> Self {
        App { services }
    }

    pub fn run(&mut self, service_ids: &[ServiceId]) -> anyhow::Result<()> {
        info!("Running app (services: {service_ids:?})");
        self.do_run(service_ids)
            .map_err(|e| anyhow!("Error running app: {}", e))?;
        info!("App was stopped");
        Ok(())
    }

    fn do_run(&mut self, service_ids: &[ServiceId]) -> anyhow::Result<()> {
        info!("Building container");
        let mut container = Container::new(&self.services)?;
        for service_id in service_ids {
            container.resolve_service(*service_id)?;
        }
        info!("Setting termination handler");
        let (tx, rx) = channel();
        ctrlc::set_handler(move || tx.send(()).expect("Could not send signal on channel"))?;
        info!("Awaiting termination");
        rx.recv()?;
        Ok(())
    }
}

#[derive(Copy, Clone, Eq, Debug, Hash, PartialEq)]
pub struct ServiceId {
    type_id: TypeId,
    name: Option<&'static str>,
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

pub type Service = dyn Any + Send + Sync + 'static;
pub type Factory =
    dyn Fn(&DependencyResolver, &mut Lifecycle) -> anyhow::Result<Arc<Service>> + 'static;

pub type StartAction = Box<dyn FnOnce() -> anyhow::Result<()> + 'static>;
pub type StopAction = Box<dyn FnOnce() + 'static>;

#[derive(Debug, Error)]
pub enum ResolveError {
    #[error("duplicate registration: {0}")]
    DuplicateRegistration(ServiceId),
    #[error("unknown service: {0}")]
    UnknownService(ServiceId),
    #[error("cyclic dependency for: {0}")]
    CyclicDependency(ServiceId),
    #[error("error while creating {0}: {1}")]
    CreateError(ServiceId, anyhow::Error),
    #[error("error while initializing {0}: {1}")]
    InitError(ServiceId, anyhow::Error),
    #[error("type mismatch for: {0}")]
    TypeMismatch(ServiceId),
}

pub struct Container {
    registered_services: HashMap<ServiceId, Rc<Factory>>,
    resolved_services: HashMap<ServiceId, Arc<Service>>,
    stop_actions: Vec<StopAction>,
}

impl Container {
    pub fn new(services: &[(ServiceId, Rc<Factory>)]) -> Result<Self, ResolveError> {
        let mut registered_services = HashMap::new();
        for (id, f) in services {
            if registered_services.insert(*id, f.clone()).is_some() {
                return Err(ResolveError::DuplicateRegistration(*id));
            }
        }
        Ok(Container {
            registered_services,
            resolved_services: HashMap::new(),
            stop_actions: Vec::new(),
        })
    }

    pub fn resolve_type<T: 'static + Send + Sync>(&mut self) -> Result<Arc<T>, ResolveError> {
        self.resolve(ServiceId::of_type::<T>())
    }

    pub fn resolve_named_type<T: 'static + Send + Sync>(
        &mut self,
        name: &'static str,
    ) -> Result<Arc<T>, ResolveError> {
        self.resolve(ServiceId::of_named_type::<T>(name))
    }

    pub fn resolve<T: 'static + Send + Sync>(
        &mut self,
        service_id: ServiceId,
    ) -> Result<Arc<T>, ResolveError> {
        self.resolve_service(service_id).and_then(|service| {
            service
                .downcast::<T>()
                .map_err(|_| ResolveError::TypeMismatch(service_id))
        })
    }

    pub fn resolve_service(&mut self, service_id: ServiceId) -> Result<Arc<Service>, ResolveError> {
        info!("Resolving service by container {service_id:?}");
        self.resolved_services
            .get(&service_id)
            .map(|service| Ok(service.clone()))
            .unwrap_or_else(|| {
                DependencyResolver::new(RefCell::new(self)).resolve_service(service_id)
            })
    }
}

impl Drop for Container {
    fn drop(&mut self) {
        info!("Stopping container");
        while let Some(stop) = self.stop_actions.pop() {
            stop();
        }
        info!("Container was stopped");
    }
}

pub fn supply_value<T>(v: T) -> (ServiceId, Rc<Factory>)
where
    T: Any + Send + Sync + 'static,
{
    let service_id = ServiceId::of_type::<T>();
    let factory: Rc<Factory> = to_value_factory(v);
    (service_id, factory)
}

pub fn supply_named_value<T>(name: &'static str, v: T) -> (ServiceId, Rc<Factory>)
where
    T: Any + Send + Sync + 'static,
{
    let service_id = ServiceId::of_named_type::<T>(name);
    let factory: Rc<Factory> = to_value_factory(v);
    (service_id, factory)
}

pub fn provide_service<T, F>(f: F) -> (ServiceId, Rc<Factory>)
where
    T: 'static + Send + Sync,
    F: Fn(&DependencyResolver, &mut Lifecycle) -> anyhow::Result<T> + 'static,
{
    let service_id = ServiceId::of_type::<T>();
    let factory = to_service_factory(f);
    (service_id, factory)
}

pub fn provide_named_service<T, F>(name: &'static str, f: F) -> (ServiceId, Rc<Factory>)
where
    T: 'static + Send + Sync,
    F: Fn(&DependencyResolver, &mut Lifecycle) -> anyhow::Result<T> + 'static,
{
    let service_id = ServiceId::of_named_type::<T>(name);
    let factory = to_service_factory(f);
    (service_id, factory)
}

fn to_value_factory<T: 'static + Send + Sync>(v: T) -> Rc<Factory> {
    let service: Arc<Service> = Arc::new(v);

    Rc::new(move |_, _| -> anyhow::Result<Arc<Service>> { Ok(service.clone()) })
}

fn to_service_factory<T, F>(f: F) -> Rc<Factory>
where
    T: 'static + Send + Sync,
    F: Fn(&DependencyResolver, &mut Lifecycle) -> anyhow::Result<T> + 'static,
{
    Rc::new(
        move |resolver: &DependencyResolver,
              lifecycle: &mut Lifecycle|
              -> anyhow::Result<Arc<Service>> {
            let service: T = f(resolver, lifecycle)?;
            Ok(Arc::new(service))
        },
    )
}

pub struct DependencyResolver<'a> {
    container: RefCell<&'a mut Container>,
    resolving_services: RefCell<HashSet<ServiceId>>,
}

impl<'a> DependencyResolver<'a> {
    fn new(container: RefCell<&'a mut Container>) -> Self {
        DependencyResolver {
            container,
            resolving_services: RefCell::new(HashSet::new()),
        }
    }

    pub fn resolve_type<T: 'static + Send + Sync>(&self) -> Result<Arc<T>, ResolveError> {
        let service_id = ServiceId::of_type::<T>();
        self.resolve(service_id)
    }

    pub fn resolve_named_type<T: 'static + Send + Sync>(
        &self,
        name: &'static str,
    ) -> Result<Arc<T>, ResolveError> {
        let service_id = ServiceId::of_named_type::<T>(name);
        self.resolve(service_id)
    }

    pub fn resolve<T: 'static + Send + Sync>(
        &self,
        service_id: ServiceId,
    ) -> Result<Arc<T>, ResolveError> {
        self.resolve_service(service_id).and_then(|service| {
            service
                .downcast::<T>()
                .map_err(|_| ResolveError::TypeMismatch(service_id))
        })
    }

    pub fn resolve_service(&self, service_id: ServiceId) -> Result<Arc<Service>, ResolveError> {
        info!("Resolving service by resolver {service_id:?}");

        if let Some(resolved) = self.container.borrow().resolved_services.get(&service_id) {
            return Ok(resolved.clone());
        }

        let factory = match self.container.borrow().registered_services.get(&service_id) {
            Some(found_factory) => Rc::clone(found_factory),
            None => return Err(ResolveError::UnknownService(service_id)),
        };

        let _resolving_guard = ResolvingGuard::new(&self.resolving_services, service_id)?;

        self.resolve_internal(service_id, factory)
    }

    fn resolve_internal(
        &self,
        service_id: ServiceId,
        factory: Rc<Factory>,
    ) -> Result<Arc<Service>, ResolveError> {
        let mut start_stop_actions = Vec::new();
        let mut lifecycle = Lifecycle::new(&mut start_stop_actions);
        let service = match factory(self, &mut lifecycle) {
            Ok(service) => service,
            Err(e) => return Err(ResolveError::CreateError(service_id, e)),
        };

        let result = service.clone();
        if result.as_ref().type_id() != service_id.type_id {
            return Err(ResolveError::TypeMismatch(service_id));
        }

        if !start_stop_actions.is_empty() {
            let mut performed: Vec<StopAction> = Vec::with_capacity(start_stop_actions.len());
            for action in start_stop_actions {
                if let Some(start) = action.start_action {
                    if let Err(e) = start() {
                        while let Some(stop) = performed.pop() {
                            stop()
                        }
                        return Err(ResolveError::InitError(service_id, e));
                    }
                }

                if let Some(stop) = action.stop_action {
                    performed.push(stop);
                }
            }

            self.container
                .borrow_mut()
                .stop_actions
                .append(&mut performed);
        }

        self.container
            .borrow_mut()
            .resolved_services
            .insert(service_id, service);

        Ok(result)
    }
}

struct ResolvingGuard<'a> {
    set: &'a RefCell<HashSet<ServiceId>>,
    service_id: ServiceId,
}

impl<'a> ResolvingGuard<'a> {
    fn new(
        set: &'a RefCell<HashSet<ServiceId>>,
        service_id: ServiceId,
    ) -> Result<Self, ResolveError> {
        if !set.borrow_mut().insert(service_id) {
            return Err(ResolveError::CyclicDependency(service_id));
        }
        Ok(Self { set, service_id })
    }
}

impl<'a> Drop for ResolvingGuard<'a> {
    fn drop(&mut self) {
        self.set.borrow_mut().remove(&self.service_id);
    }
}

pub struct Lifecycle<'a> {
    actions: &'a mut Vec<StartStopAction>,
}

const LIFECYCLE_LOG: &str = "lifecycle";

impl<'a> Lifecycle<'a> {
    fn new(actions: &'a mut Vec<StartStopAction>) -> Self {
        Lifecycle { actions }
    }

    pub fn register_start<F: FnOnce() -> anyhow::Result<()> + 'static>(&mut self, start_action: F) {
        debug!(target: LIFECYCLE_LOG, "Registering start action");
        self.actions.push(StartStopAction {
            start_action: Some(Box::new(start_action)),
            stop_action: None,
        })
    }

    pub fn register_stop<F: FnOnce() + 'static>(&mut self, stop_action: F) {
        debug!(target: LIFECYCLE_LOG, "Registering stop action");
        self.actions.push(StartStopAction {
            start_action: None,
            stop_action: Some(Box::new(stop_action)),
        })
    }

    pub fn register_start_stop<
        F1: FnOnce() -> anyhow::Result<()> + 'static,
        F2: FnOnce() + 'static,
    >(
        &mut self,
        start_action: F1,
        stop_action: F2,
    ) {
        debug!(target: LIFECYCLE_LOG, "Registering start and stop actions");
        self.actions.push(StartStopAction {
            start_action: Some(Box::new(start_action)),
            stop_action: Some(Box::new(stop_action)),
        })
    }
}

struct StartStopAction {
    start_action: Option<StartAction>,
    stop_action: Option<StopAction>,
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() -> anyhow::Result<()> {
        Ok(())
    }
}
