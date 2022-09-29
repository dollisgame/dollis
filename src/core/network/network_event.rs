pub(crate) mod client_event;
pub(crate) mod server_event;

use std::marker::PhantomData;

/// An event channel counter
///
/// Used to create channels for each event.
#[derive(Clone, Copy, Default)]
pub(crate) struct NetworkEventCounter {
    /// Increments with each instantiation of [`ServerEventPlugin`].
    pub(crate) server: u8,
    /// Increments with each instantiation of [`ClientEventPlugin`].
    pub(crate) client: u8,
}

/// A resource that holds a channel ID for `T`.
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
