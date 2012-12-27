-module(test).

-export([main/1]).

main(_) ->
	world_connect:connect(),
	io:format("Read data register: "),
	Data1 = ppi:read_data(),
	if 
		is_integer(Data1) -> io:format("Success(~p)~n",[Data1]);
		true -> io:format("Fail~n")
	end,
	io:format("Write 0 to data register: "),
	Data2 = ppi:write_data(0),
	if
		Data2 == 0 -> io:format("Success.~n");
		true -> io:format("Fail~n")
	end,
	io:format("Verify data register: "),
	Data3 = ppi:read_data(),
	if
		Data3 == 0 -> io:format("Success.~n");
		true -> io:format("Fail~n")
	end,
	io:format("Read control register: ~p.~n",[ppi:read_control()]),
	io:format("Read status register: ~p~n",[ppi:read_status()]).

	
	
