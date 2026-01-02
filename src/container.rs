use crate::provider::{Factory, Provider, Service, ServiceId};
use log::{debug, error, info};
use scopeguard::defer;
use std::any::TypeId;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use thiserror::Error;

pub type StartAction = Box<dyn FnOnce() -> anyhow::Result<()> + 'static>;
pub type StopAction = Box<dyn FnOnce() -> anyhow::Result<()> + 'static>;

struct StartStopActions {
    start_action: Option<StartAction>,
    stop_action: Option<StopAction>,
}

#[derive(Debug, Error)]
pub enum ResolveError {
    #[error("duplicated registration: {0}")]
    DuplicatedRegistration(ServiceId),
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
    registered_types:  HashMap<TypeId, Rc<Vec<ServiceId>>>,
    resolved_services: HashMap<ServiceId, Rc<Service>>,
    close_actions: Vec<StopAction>,
}

impl Container {
    pub fn new(providers: &[Provider]) -> Result<Self, ResolveError> {
        let mut registered_services = HashMap::new();
        let mut registered_types = HashMap::new();
        for provider in providers {
            let service_id = provider.service_id;
            if registered_services
                .insert(service_id, provider.factory.clone())
                .is_some()
            {
                return Err(ResolveError::DuplicatedRegistration(service_id));
            }
            let entry = registered_types
                .entry(service_id.type_id)
                .or_insert_with(|| Rc::new(Vec::new()));
            Rc::get_mut(entry).unwrap()
                .push(service_id);
        }
        Ok(Container {
            registered_services,
            registered_types,
            resolved_services: HashMap::new(),
            close_actions: Vec::new(),
        })
    }

    pub fn resolve_types<T: 'static>(&mut self) -> Result<Vec<Rc<T>>, ResolveError> {
        let mut result = Vec::new();
        if let Some(service_ids) = self.registered_types.get(&TypeId::of::<T>()) {
            result.reserve(service_ids.len());
            for service_id in service_ids.clone().iter() {
                result.push(self.resolve_service::<T>(*service_id)?);
            }
        }
        Ok(result)
    }

    pub fn resolve_type<T: 'static>(&mut self) -> Result<Rc<T>, ResolveError> {
        self.resolve_service(ServiceId::of_type::<T>())
    }

    pub fn resolve_named_type<T: 'static>(
        &mut self,
        name: &'static str,
    ) -> Result<Rc<T>, ResolveError> {
        self.resolve_service(ServiceId::of_named_type::<T>(name))
    }

    fn resolve_service<T: 'static>(
        &mut self,
        service_id: ServiceId,
    ) -> Result<Rc<T>, ResolveError> {
        downcast_service(service_id, self.resolve(service_id))
    }

    pub fn resolve_trait_objects<T: ?Sized + 'static>(
        &mut self,
    ) -> Result<Vec<Rc<T>>, ResolveError> {
        let mut result = Vec::new();
        if let Some(service_ids) = self.registered_types.get(&TypeId::of::<Rc<T>>()) {
            result.reserve(service_ids.len());
            for service_id in service_ids.clone().iter() {
                result.push(self.resolve_trait_object_service::<T>(*service_id)?);
            }
        }
        Ok(result)
    }

    pub fn resolve_trait_object<T: ?Sized + 'static>(&mut self) -> Result<Rc<T>, ResolveError> {
        self.resolve_trait_object_service(ServiceId::of_type::<Rc<T>>())
    }

    pub fn resolve_named_trait_object<T: ?Sized + 'static>(
        &mut self,
        name: &'static str,
    ) -> Result<Rc<T>, ResolveError> {
        self.resolve_trait_object_service(ServiceId::of_named_type::<Rc<T>>(name))
    }

    fn resolve_trait_object_service<T: ?Sized + 'static>(
        &mut self,
        service_id: ServiceId,
    ) -> Result<Rc<T>, ResolveError> {
        downcast_trait_object_service(service_id, self.resolve(service_id))
    }

    pub fn resolve(&mut self, service_id: ServiceId) -> Result<Rc<Service>, ResolveError> {
        info!("Resolving service {service_id:?} by container");
        self.resolved_services
            .get(&service_id)
            .map(|service| Ok(service.clone()))
            .unwrap_or_else(|| DependencyResolver::new(RefCell::new(self)).resolve(service_id))
    }
}

impl Drop for Container {
    fn drop(&mut self) {
        info!("Stopping container");
        execute_stop_actions(&mut self.close_actions);
        info!("Container was stopped");
    }
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

    pub fn resolve_type<T: 'static>(&self) -> Result<Rc<T>, ResolveError> {
        let service_id = ServiceId::of_type::<T>();
        self.resolve_service(service_id)
    }

    pub fn resolve_named_type<T: 'static>(
        &self,
        name: &'static str,
    ) -> Result<Rc<T>, ResolveError> {
        let service_id = ServiceId::of_named_type::<T>(name);
        self.resolve_service(service_id)
    }

    fn resolve_service<T: 'static>(
        &self,
        service_id: ServiceId,
    ) -> Result<Rc<T>, ResolveError> {
        downcast_service(service_id, self.resolve(service_id))
    }

    pub fn resolve_trait_object<T: ?Sized + 'static>(&self) -> Result<Rc<T>, ResolveError> {
        self.resolve_trait_object_service(ServiceId::of_type::<Rc<T>>())
    }

    pub fn resolve_named_trait_object<T: ?Sized + 'static>(
        &self,
        name: &'static str,
    ) -> Result<Rc<Service>, ResolveError> {
        self.resolve_trait_object_service(ServiceId::of_named_type::<Rc<T>>(name))
    }

    fn resolve_trait_object_service<T: ?Sized + 'static>(
        &self,
        service_id: ServiceId,
    ) -> Result<Rc<T>, ResolveError> {
        downcast_trait_object_service(service_id, self.resolve(service_id))
    }

    pub fn resolve(&self, service_id: ServiceId) -> Result<Rc<Service>, ResolveError> {
        info!("Resolving service {service_id:?} by resolver");

        if let Some(resolved) = self.container.borrow().resolved_services.get(&service_id) {
            return Ok(resolved.clone());
        }

        let factory = match self.container.borrow().registered_services.get(&service_id) {
            Some(found_factory) => Rc::clone(found_factory),
            None => return Err(ResolveError::UnknownService(service_id)),
        };

        if !self.resolving_services.borrow_mut().insert(service_id) {
            return Err(ResolveError::CyclicDependency(service_id));
        }
        defer! {
            self.resolving_services.borrow_mut().remove(&service_id);
        }

        self.resolve_internal(service_id, factory)
    }

    fn resolve_internal(
        &self,
        service_id: ServiceId,
        factory: Rc<Factory>,
    ) -> Result<Rc<Service>, ResolveError> {
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
                        execute_stop_actions(&mut performed);
                        return Err(ResolveError::InitError(service_id, e));
                    }
                }

                if let Some(stop) = action.stop_action {
                    performed.push(stop);
                }
            }

            self.container
                .borrow_mut()
                .close_actions
                .append(&mut performed);
        }

        self.container
            .borrow_mut()
            .resolved_services
            .insert(service_id, service);

        Ok(result)
    }
}

fn execute_stop_actions(stop_actions: &mut Vec<StopAction>) {
    while let Some(stop) = stop_actions.pop() {
        if let Err(e) = stop() {
            error!("Stop action failed: {}", e);
        }
    }
}

pub struct Lifecycle<'a> {
    actions: &'a mut Vec<StartStopActions>,
}

impl<'a> Lifecycle<'a> {
    fn new(actions: &'a mut Vec<StartStopActions>) -> Self {
        Lifecycle { actions }
    }

    pub fn register_start<F: FnOnce() -> anyhow::Result<()> + 'static>(&mut self, start_action: F) {
        debug!("Registering start action");
        self.actions.push(StartStopActions {
            start_action: Some(Box::new(start_action)),
            stop_action: None,
        })
    }

    pub fn register_stop<F: FnOnce() -> anyhow::Result<()> + 'static>(&mut self, stop_action: F) {
        debug!("Registering stop action");
        self.actions.push(StartStopActions {
            start_action: None,
            stop_action: Some(Box::new(stop_action)),
        })
    }

    pub fn register_start_stop<
        F1: FnOnce() -> anyhow::Result<()> + 'static,
        F2: FnOnce() -> anyhow::Result<()> + 'static,
    >(
        &mut self,
        start_action: F1,
        stop_action: F2,
    ) {
        debug!("Registering start and stop actions");
        self.actions.push(StartStopActions {
            start_action: Some(Box::new(start_action)),
            stop_action: Some(Box::new(stop_action)),
        })
    }
}

fn downcast_service<T: 'static>(
    service_id: ServiceId,
    result: Result<Rc<Service>, ResolveError>,
) -> Result<Rc<T>, ResolveError> {
    result.and_then(|service| {
        service
            .downcast::<T>()
            .map_err(|_| ResolveError::TypeMismatch(service_id))
    })
}

fn downcast_trait_object_service<T: ?Sized + 'static>(
    service_id: ServiceId,
    result: Result<Rc<Service>, ResolveError>,
) -> Result<Rc<T>, ResolveError> {
    result
        .and_then(|service| {
            service
                .downcast::<Rc<T>>()
                .map_err(|_| ResolveError::TypeMismatch(service_id))
        })
        .map(|service| service.as_ref().clone())
}
