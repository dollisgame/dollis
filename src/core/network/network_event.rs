pub(crate) mod client_event;
pub(crate) mod server_event;

use std::marker::PhantomData;

use bevy::prelude::*;

/// An event channel counter
///
/// Used to create channels for each event.
#[derive(Clone, Copy, Default, Resource)]
pub(crate) struct NetworkEventCounter {
    /// Increments with each instantiation of [`ServerEventPlugin`].
    pub(super) server: u8,
    /// Increments with each instantiation of [`ClientEventPlugin`].
    pub(super) client: u8,
}

/// Holds a channel ID for `T`.
#[derive(Resource)]
struct EventChannel<T> {
    id: u8,
    marker: PhantomData<T>,
}

impl<T> EventChannel<T> {
    fn new(id: u8) -> Self {
        Self {
            id,
            marker: PhantomData,
        }
    }
}
