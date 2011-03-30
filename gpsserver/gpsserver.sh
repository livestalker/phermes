#!/bin/sh
### BEGIN INIT INFO
# Provides:          gpsserver
# Required-Start:    $remote_fs $syslog
# Required-Stop:     $remote_fs $syslog
# Default-Start:     2 3 4 5
# Default-Stop:      0 1 6
# Short-Description: GPS server
# Description:       GPS server
### END INIT INFO

PATH=/sbin:/usr/sbin:/bin:/usr/bin:/usr/local/bin 
DESC="GPS erlang daemon"
NODE=germes
HOST=`hostname -f`
DOTTED=`hostname -f | grep '\.'`
if [ "" = "$DOTTED" ] ;then
  TYPE="sname"
else
  TYPE="name"
fi
NAME=erl
DAEMON=/usr/local/bin/$NAME
EMUL_FLAGS="+A 4 +K true"
DAEMON_ARGS="$EMUL_FLAGS -noinput -$TYPE $NODE@$HOST"
PIDFILE=/var/run/$NAME.pid 
SCRIPTNAME=/etc/init.d/$NAME

[ -x "$DAEMON" ] || exit 0 

. /lib/lsb/init-functions 

do_start() 
{ 
	# read config
	if [ -f "/etc/gpsserver/production.config" ] ; then
		CONFIG="-config /etc/gpsserver/production.config"
	elif   [ -f "priv/production.config" ] ; then
		CONFIG="-config priv/production.config"
	else
		CONFIG=""
	fi
	echo $CONFIG
	# test if server running
    if [ -f "$PIDFILE" ]; then 
        return 1 
    else 
		EBIN=`erl -eval 'io:format("~s", [code:lib_dir(erltcps,ebin)])' -s init stop -noshell`
        start-stop-daemon \
			--start --background --pidfile $PIDFILE --make-pidfile \
			--exec $DAEMON -- $DAEMON_ARGS -pa "$EBIN" $CONFIG
    fi 
} 

do_stop() 
{ 
	EVAL="io:format(\"~p\", [rpc:call($NODE@$HOST, init, stop, [])])"
	RES=`erl -eval "$EVAL" -s init stop -$TYPE shutdown@$HOST -noshell -noinput`
	case "$RES" in
		"ok")
			rm -f $PIDFILE 
			return 0
			;;
		*)
			return 2
			;;
	esac
}

do_rmshell()
{
	erl $EMUL_FLAGS -hidden -$TYPE debug@$HOST -remsh $NODE@$HOST -pa ebin
}

case "$1" in 
    start) 
        log_daemon_msg "Starting GPS erlang daemon" 
        do_start 
        log_end_msg $? 
        ;; 
    stop) 
        log_daemon_msg "Stop GPS erlang daemon" 
        do_stop 
        log_end_msg $? 
        ;; 
    status) 
        status_of_proc "$DAEMON" "GPS erlang daemon" && exit 0 || exit $? 
        ;; 
	remsh)
		do_rmshell
		;;
	libupdate)
		rm -rf /usr/local/lib/erlang/lib/erltcps
		cp -r erltcps /usr/local/lib/erlang/lib/
		;;
    restart) 
        log_daemon_msg "Restarting GPS erlang daemon" 
        do_stop 
        case "$?" in 
            0|1) 
                do_start 
                case "$?" in 
                    0) log_end_msg 0 ;; 
                    1) log_end_msg 1 ;; # Old process is still running 
                    *) log_end_msg 1 ;; # Failed to start 
                esac 
                ;; 
            *) 
                log_end_msg 1 # Failed to stop 
                ;; 
        esac 
        ;; 
    *) 
        echo "Usage: $SCRIPTNAME {start|stop|status|restart|remsh|libupdate}" >&2 
        exit 3 
        ;; 
esac