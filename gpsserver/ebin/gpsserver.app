{application, gpsserver, [
						  {description, "GPS erlang server"},
						  {vsn, "1.0"},
						  {modules,      [gpsserver, 
										  tcp_server_sup, 
										  tcp_listener, 
										  tcp_client_sup,
										  tcp_fsm,
										  tracker_sup,
										  tracker_parser,
										  mysql]},
						  {registered,   []},
						  {applications, [kernel, stdlib]},
						  {mod, {gpsserver, []}},
						  {env, [{listening_port, 9001}]}
						 ]}.
