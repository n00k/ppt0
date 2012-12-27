default: all

all: ppi ppi_beam ppi_app ppi_sup world_connect

ppi: ppi.c erl_comm.c
	gcc -o ppi ppi.c erl_comm.c

ppi_beam:    ppi.erl
	erlc ppi.erl

ppi_app: ppi_app.erl
	erlc ppi_app.erl

ppi_sup: ppi_sup.erl
	erlc ppi_sup.erl

world_connect: world_connect.erl
	erlc world_connect.erl

test: all test.erl
	escript test.erl

clean:
	rm *.beam 
	rm ppi
