-module(world_connect).

-export([connect/0,connect/1]).

connect() ->
	ensure_alive(),
	process(get_names(net_adm:localhost())).
connect(Host) ->
	ensure_alive(),
	process(get_names(Host)).

ensure_alive() ->
	case is_alive() of
		true -> ok;
		false ->
			Node = list_to_atom("web" ++ os:cmd("echo -n $$") ++ "@" ++ net_adm:localhost()),
			net_kernel:start([Node]),
			erlang:set_cookie(Node,webbitch)
	end.

get_names(Host) ->
	{ok, Names} = net_adm:names(Host),
	[list_to_atom(N ++ "@" ++ Host) || {N,_} <- Names].

process([]) -> nodes();

process([Node | Rest]) ->
	net_adm:ping(Node),
	process(Rest).
