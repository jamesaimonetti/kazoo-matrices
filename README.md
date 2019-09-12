# Matrices - A multi-tenant implementation the Matrix.org protocols

Currently, each homeserver implementation is for a single realm, necessitating a one-tenant-per-server setup.

Erlang seems well-suited to providing a multi-tenant solution where you can host many homeservers on a single server.

Kazoo is a communications platform that provides multi-tenant telecom capabilities on top of a multi-datacenter architecture, where no one server is responsible for a tenant.

Thus, we feel that a implementation of a multi-tenant homeserver that can scale would benefit greatly from Erlang+Kazoo!
