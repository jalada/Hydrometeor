{application, hydrometeor,
	[{description, "A long-polling webserver based on a pubsub model"},
	 {vsn, "1.0"},
	 {modules, [hm_app, hm_sup, hm_server, hm_web]},
	 {registered, [hm_sup, hm_server, hm_web]},
	 {applications, [kernel, stdlib, inets]},
	 {mod, {hm_app, []}}
	]
}.