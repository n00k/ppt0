{application, ppi_app,
	[{description, "Channel allocator"},
	{vsn, "1"},
	{modules, [ppi_app, ppi_sup, ppi]},
	{registered, [ppi]},
	{applications, [kernel, stdlib]},
	{mod, {ppi_app,[]}}
	]}.
