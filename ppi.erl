-module(ppi).

-behaviour(gen_server).

-export([start_link/0,
		start_link/1, 
		start_link/2, 
		terminate/2, 
		handle_call/3,
		handle_cast/2,
		handle_info/2,
		code_change/3,
		init/1]).

-export([read_data/0, 
		write_data/1, 
		read_control/0, 
		write_control/1, 
		start_loop/1,
		read_status/0,
		get_state/0,
		stop/0]). 

-record(pstate, {portpid :: pid(),
				 srvname :: atom()}).

-define(SRVNAME,ppi).

ensure_started() ->
	case global:whereis_name(?SRVNAME) of
		Pid when is_pid(Pid) -> Pid;
		_ ->
			application:start(ppi_app),
			global:whereis_name(?SRVNAME)
	end.

get_state() ->
	gen_server:call({global,?SRVNAME}, getstate).

write_data(X) when X>=0, X=<255 -> 
	ensure_started(),
	gen_server:call({global,?SRVNAME},{write_data,X}),
	read_data().

read_data() -> 
	ensure_started(),
	gen_server:call({global,?SRVNAME},read_data).

write_control(X) when X>=0, X=<255 -> 
	ensure_started(),
	gen_server:call({global,?SRVNAME},{write_control,X}),
	read_control().

read_control() -> 
	ensure_started(),
	gen_server:call({global,?SRVNAME},read_control).

read_status() -> 
	ensure_started(),
	gen_server:call({global,?SRVNAME},read_status).

stop() -> 
	gen_server:call({global,?SRVNAME},stop).

%%initialization functions 
init_port(PortPid,Portname) -> call_port(PortPid,[0,Portname ++ [0]]).

start_link() -> start_link("/dev/ppi0").
start_link(Portname) -> start_link("./ppi",Portname).
start_link(ExtPrg,Portname) ->
	gen_server:start_link({global, ?SRVNAME}, ?MODULE, [ExtPrg,Portname], []).

init([ExtPrg,Portname]) ->
	PortPid = spawn_link(?MODULE, start_loop, [ExtPrg]),
	init_port(PortPid,Portname),
	{ok,#pstate{portpid=PortPid,srvname=?SRVNAME}}.

terminate(shutdown,#pstate{portpid=PortPid}) ->
	PortPid ! stop,
	ok.

%% internal functions
call_port(PortPid, Msg) ->
	PortPid ! {call, self(), Msg},
	receive
		{ppi_port, Data} ->
			Data;
		Res = {error, _} ->
			Res;
		Unk -> 
			Unk
	after 5000 ->
			{error, "timeout waiting on message response"}
	end.

start_loop(ExtPrg) ->
	process_flag(trap_exit, true),
	Port = open_port({spawn_executable, ExtPrg}, [{packet, 2}]),
	loop(Port).   

loop(Port) ->
	receive
		{call, Caller, Msg} ->                   
			Port ! {self(), {command, Msg}},
			receive
				{Port, {data, [Data]}} ->
					Caller ! {ppi_port, Data};
				Unk ->
					Caller ! Unk
			after 1000 ->
					Caller ! {error, "timeout waiting on external process"}
			end,
			loop(Port);
		stop ->
			Port ! {self(), close},
			receive
				{Port, closed} ->
					exit(normal)
			end;
		{'EXIT', Port, _Reason} ->
			exit(port_terminated)
	end.

handle_call(stop, _From, State) -> {stop, shutdown, State};

handle_call({write_data,X},_From, #pstate{portpid=PortPid}=State) ->
	call_port(PortPid, [1,X]),
	{reply, X, State};

handle_call(read_data, _From, #pstate{portpid=PortPid}=State) ->
	{reply, call_port(PortPid, [2]), State};

handle_call({write_control,X}, _From, #pstate{portpid=PortPid}=State) ->
	call_port(PortPid, [3,X]),
	{reply, X, State};

handle_call(read_control, _From, #pstate{portpid=PortPid}=State) ->
	{reply, call_port(PortPid, [4]), State};

handle_call(read_status, _From, #pstate{portpid=PortPid}=State) ->
	{reply, call_port(PortPid, [5]), State};

handle_call(getstate, _From, Pstate) ->
	Pstate;

handle_call(Msg, _From, State) ->
	{reply, {unknown_message, Msg}, State}.

handle_info(_, State) -> {noreply, State}.

handle_cast(_Msg, State) -> {noreply, State}.

code_change(_,_,_) -> {error, "No!"}. 
