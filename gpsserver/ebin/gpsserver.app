{application, gpsserver, [
						  {description, "GPS erlang server"},
						  {vsn, "1.0"},
						  {modules,      [gpsserver, 
										  sup_gps, 
										  sup_clients,
										  srv_client,
										  sup_data,
										  gps_utils,
										  mysql,
										  mysql_auth,
										  mysql_conn,
										  mysql_recv]},
						  {registered,   []},
						  {applications, [kernel, stdlib]},
						  {mod, {gpsserver, []}},
						  {env, []}
						 ]}.
