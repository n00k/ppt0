#!/usr/bin/env escript
-module(ppapi).

-export([main/1]).

main(Arg) ->
	world_connect:connect(),
	make_html(io_lib:format("{\"ret\":\"~p\"}~n",[
			try
				process(Arg)
			catch
				Class:Term -> {Class,Term}
			end
		])).

process(["GET"]) -> ppi:read_data();
process(["get"]) -> ppi:read_data();
process([Val]) when is_list(Val) ->
	V = list_to_integer(Val),
	ppi:write_data(V);
process(_) -> fail.

make_html(Msg) ->
	io:format("HTTP/1.0 200 OK~n"),
	{{Year,Mon,Day}=Date,{Hour,Min,Sec}}=calendar:now_to_universal_time(now()),
	WkDay=lists:nth(calendar:day_of_the_week(Date),["Sun","Mon","Tue","Wed","Thu","Fri","Sat"]),
	Month=lists:nth(Mon,["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"]),
	io:format("DATE: ~s, ~p ~s ~p ~2..0p:~2..0p:~2..0p~n",[WkDay,Day,Month,Year,Hour,Min,Sec]),
	io:format("SERVER: homegrown~n"),
	io:format("CONTENT-LENGTH: ~p~n",[length(Msg)]),
	io:format("CONNECTION: close~n"),
	io:format("CONTENT-TYPE: application/json~n"),
	io:format("~n~s",[Msg]).



