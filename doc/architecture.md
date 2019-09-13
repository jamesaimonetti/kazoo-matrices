# Architecture

## High level data structures

* Users
  * Have metadata about them
  * Maintain profiles - public key/value pairs associated with the user
  * Maintain private data - key/value pairs for things like configs and preferences
* Identity
  * Users are identified by their Matrix IDs
  * Can have 3PID (third party IDs) that map to Matrix users
    * Email, phone number, social media accounts, etc
  * Identity then is the user ID + any 3PIDs linked
* Devices
  * Owned by a user
  * Manage keys for encryption
* Events - Data exchanged over Matrix
  * Types are namespaced like Java: `com.example.myapp.event`
  * `m.` is a reserved namespace for Matrix event types like `m.room.message` for IMs
  * Typically sent in the context of a room
* Event Graphs
  * A room maintains a direct acyclic graph (DAG) of events
    * See digraph module in Erlang: `digraph:new([acyclic])`
* Rooms
  * Conceptual place for users to send and receive events
  * Events are sent to a room
  * Users with sufficient access in the room will recieve the event
  * One room ID per room
  * The room does not reside on the domain specified (just for global uniqueness)
  * Event types
    * Message Events - transient one off mesages like IMs, VoIP setups, file transfers
    * State Events - describe updates to the room's state (persistent information) like name, topic, membership, etc.
  * State is modeled by key value pairs where key = `{state_key, event_type}`
  * Current state is calculated by considering all preceding events and including a given event in the graph.
  * Can be versioned
    * No heirarchy or order - opaque version IDs
    * `1`, `1.2` - matrix-reserved versioning
    * `1.2-beta`, `com.example.version` - custom versions by homeserver(s)
* Room Alias
  * Human readable pointer to a room
  * Translated to room ID by the domain specified in the alias
  * Not a fixed mapping between alias and ID
* Timestamps - unix epoch milliseconds
* Matrix Specifications
  * Versioned with `rX.Y.Z`

## Data types

### Events

See [here](https://matrix.org/docs/spec/#event-graphs) for more details

| Type  | Description                                                                       |
| type  | namespaced type describing the event                                              |
| depth | "positive integer that is strictly greater than the depths of any of its parents" |
|       |                                                                                   |

### Rooms

Shared room datastructure between multiple homeservers:

| Type     | Description                      |
| ID       | Room ID !foo:server.com          |
| Servers  | Homeservers involved in the room |
| Members  | Users in the room                |
| Messages | Events sent to the room          |

## Erlang libraries to leverage

* digraph - for DAG event graph
* [base64 encoding/decoding](https://github.com/zuckschwerdt/b64fast) for unpadded base64 (See [here](https://matrix.org/docs/spec/appendices#unpadded-base64))
* For [Canonical JSON](https://matrix.org/docs/spec/appendices#canonical-json), KAZOO uses [jiffy](https://github.com/davisp/jiffy). To generate canonical JSON: `kz_json:encode(JObj, ['force_utf8'])`
