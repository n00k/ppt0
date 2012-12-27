-module(ppi_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link(ppi_sup, []).

init(_Args) ->
	{ok, {{one_for_one, 60, 60},
			[{ppi, {ppi, start_link, ["/dev/ppi0"]},
					permanent, brutal_kill, worker, [ppi]}]}}.
