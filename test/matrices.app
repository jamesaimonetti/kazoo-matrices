{application,matrices,
             [{applications,[kazoo,kazoo_amqp,kernel,lager,stdlib]},
              {description,"Matrices - A multi-tenant Matrix.org homeserver"},
              {env,[{is_kazoo_app,true}]},
              {mod,{matrices_app,[]}},
              {modules, [matrices_app,matrices_identifiers,matrices_listener,matrices_sup,matricies_identifiers_test]},
              {registered,[matrices_sup,matrices_listener]},
              {vsn,"0.0.1"}]}.
