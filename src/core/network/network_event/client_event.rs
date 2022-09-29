use std::marker::PhantomData;

use bevy::{ecs::event::Event, prelude::*};
use bevy_renet::renet::{RenetClient, RenetServer};
use iyes_loopless::prelude::*;
use serde::{de::DeserializeOwned, Serialize};
use tap::TapFallible;

use super::{EventChannel, NetworkEventCounter};
use crate::core::{
    game_world::GameWorld,
    network::{client, SERVER_ID},
};

pub(crate) struct ClientEventPlugin<T> {
    marker: PhantomData<T>,
}

impl<T> Default for ClientEventPlugin<T> {
    fn default() -> Self {
        Self {
            marker: PhantomData,
        }
    }
}

#[derive(SystemLabel)]
enum ClientEventSystems<T> {
    SendingSystem,
    #[allow(dead_code)]
    #[system_label(ignore_fields)]
    Marker(T),
}

impl<T: Serialize + DeserializeOwned + Send + Sync + 'static> Plugin for ClientEventPlugin<T> {
    fn build(&self, app: &mut App) {
        let mut event_counter = app
            .world
            .get_resource_or_insert_with(NetworkEventCounter::default);
        let current_channel_id = event_counter.client;
        event_counter.client += 1;

        app.add_event::<T>()
            .add_event::<EventReceived<T>>()
            .insert_resource(EventChannel::<T>::new(current_channel_id))
            .add_system(
                Self::sending_system
                    .run_if(client::is_connected)
                    .label(ClientEventSystems::<T>::SendingSystem),
            )
            .add_system(
                Self::local_resending_system
                    .run_unless_resource_exists::<RenetClient>()
                    .run_if_resource_exists::<GameWorld>()
                    .after(ClientEventSystems::<T>::SendingSystem),
            )
            .add_system(Self::receiving_system.run_if_resource_exists::<RenetServer>());
    }
}

impl<T: Event + Serialize + DeserializeOwned> ClientEventPlugin<T> {
    fn sending_system(
        mut client_events: EventReader<T>,
        mut client: ResMut<RenetClient>,
        channel: Res<EventChannel<T>>,
    ) {
        for event in client_events.iter() {
            let message = rmp_serde::to_vec(&event).expect("unable to serialize client event");
            client.send_message(channel.id, message);
        }
    }

    /// Transforms [`T`] events into [`EventReceived<T>`] events to "emulate"
    /// message sending for offline mode or when server is also a player
    fn local_resending_system(
        mut client_events: ResMut<Events<T>>,
        mut receive_events: EventWriter<EventReceived<T>>,
    ) {
        for event in client_events.drain() {
            receive_events.send(EventReceived {
                client_id: SERVER_ID,
                event,
            })
        }
    }

    fn receiving_system(
        mut receive_events: EventWriter<EventReceived<T>>,
        mut server: ResMut<RenetServer>,
        channel: Res<EventChannel<T>>,
    ) {
        for client_id in server.clients_id().iter().copied() {
            while let Some(message) = server.receive_message(client_id, channel.id) {
                if let Ok(event) = rmp_serde::from_slice(&message)
                    .tap_err(|e| error!("unable to deserialize event from client {client_id}: {e}"))
                {
                    receive_events.send(EventReceived { client_id, event });
                }
            }
        }
    }
}

/// An event indicating that the message from client was received.
/// Emited only on server.
#[derive(Clone, Copy)]
pub(crate) struct EventReceived<T> {
    pub(crate) client_id: u64,
    pub(crate) event: T,
}

#[cfg(test)]
mod tests {
    use bevy::ecs::event::Events;
    use serde::Deserialize;

    use super::*;
    use crate::core::network::tests::{NetworkPreset, TestNetworkPlugin};

    #[test]
    fn sending_receiving() {
        let mut app = App::new();
        app.add_plugin(ClientEventPlugin::<DummyEvent>::default())
            .add_plugin(TestNetworkPlugin::new(NetworkPreset::ServerAndClient {
                connected: true,
            }));

        let mut dummy_events = app.world.resource_mut::<Events<DummyEvent>>();
        dummy_events.send(DummyEvent);

        app.update();
        app.update();

        let mut receive_events = app
            .world
            .resource_mut::<Events<EventReceived<DummyEvent>>>();
        assert_eq!(receive_events.drain().count(), 1);
    }

    #[test]
    fn local_resending() {
        let mut app = App::new();
        app.init_resource::<GameWorld>()
            .add_plugin(ClientEventPlugin::<DummyEvent>::default());

        let mut dummy_events = app.world.resource_mut::<Events<DummyEvent>>();
        dummy_events.send(DummyEvent);

        app.update();

        let mut receive_events = app
            .world
            .resource_mut::<Events<EventReceived<DummyEvent>>>();
        assert_eq!(receive_events.drain().count(), 1);
    }

    #[derive(Deserialize, Serialize)]
    struct DummyEvent;
}
