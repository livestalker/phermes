%% record for GPRMC data (modified) (see NMEA 0183 protocol)
%% some of the cheap GPS devices do not provide the parameters:
%% bearing, UTC date, checksum
-record(gprmc, 
		{
%%-info about devices----------------------------
		  imei,         %% IMIE
		  cmd,          %% COMMAND tracker|help me|...
		  sn,           %% SERIAL NUMBER
		  tel,          %% TELEPHONE
%%-modified format of GPRMC----------------------
		  sdt,          %% SATELLITE-DERIVED TIME
		  sfs,          %% SATELLITE FIX STATUS
		  lat,          %% LAITUDE
		  long,         %% LONGITUDE
          speed,        %% SPEED
          bearing = na, %% BEARING
		  utcd = na,    %% UTC DATE
		  cs = na       %% CHECKSUM
		}).
