{application, erltcps, [
			  		   {description, "Germes Server"},
					   {vsn, "1.0"},
					   {modules,      [erltcps, 
					   				  tcp_server_sup, 
									  tcp_listener, 
									  tcp_client_sup,
									  tcp_fsm,
									  tracker_sup,
									  tracker_parser,
									  mysql]},
					   {registered,   []},
					   {applications, [kernel, stdlib]},
			  		   {mod, {erltcps, []}},
					   {env, []}
			  		   ]}.