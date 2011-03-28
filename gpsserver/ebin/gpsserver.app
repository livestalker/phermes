{application, gpsserver, [
						  {description, "GPS erlang server"},
						  {vsn, "1.0"},
						  {modules,      [gpsserver, 
										  sup_gps, 
										  sup_clients,
										  srv_client,
										  sup_data,
										  plg_tk102]},
						  {registered,   []},
						  {applications, [kernel, stdlib]},
						  {mod, {gpsserver, []}},
						  {env, [
								 {listening_port, 9001},
								 {db_host, "localhost"},
								 {db_db, "pgermes"},
								 {db_user, "pgermes"},
								 {db_password, "pgermes"}
								]
						  }
						 ]}.
