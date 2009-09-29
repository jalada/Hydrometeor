// Hydrometeor Javascript Libary.
// Code written while looking at Meteor JS library: http://meteorserver.org

Array.prototype.inArray = function (value) {
	var i;
	for (i=0; i < this.length; i++) {
		// Matches identical (===), not just similar (==).
		if (this[i] === value) {
			return true;
		}
	}
	return false;
};

Array.prototype.remove = function (value) {
	var i;
	for(i=0; i < this.length; i++) {
		if(this[i] === value) {
			this.splice(i, 1);
		}
	}
}

Hydrometeor = {

	callbacks: {
		process: "",
	},
	channels: {},
	channelcount: 0,
	backtrackqueue: new Array(),
	path: null,
	status: 0,
	xhr: null,
	xhrdata: {},

	init: function() {
		if (!Hydrometeor.path) throw "Hydrometeor path not specified";
		if (Hydrometeor.path.charAt(Hydrometeor.path.length) == "/") {
			Hydrometeor.path = Hydrometeor.path.slice(0,Hydrometeor.path.length-1);
		}
		Hydrometeor.updateXhrData();
	},

	joinChannel: function(channelName, backtrack) {
		if (typeof(Hydrometeor.channels[channelName]) != "undefined") throw "Cannot join channel "+channelName+": already subscribed";
		Hydrometeor.addChannel(channelName, backtrack);
		Hydrometeor.updateXhrData();
		if (Hydrometeor.status != 0 && Hydrometeor.channelcount != 0) {
			if (typeof(backtrack) == "number") {	
				Hydrometeor.doBacktrack(channelName, Hydrometeor.connect);
			}
		} else if (Hydrometeor.status == 0 && Hydrometeor.channelcount != 0) {
			if (typeof(backtrack) == "number") {
				Hydrometeor.backtrackqueue.push(channelName);
			}
		}
	},

	leaveChannel: function(channelName) {
		if (typeof(Hydrometeor.channels[channelName]) == "undefined") throw "Cannot leave channel "+channelName+": not subscribed";
		Hydrometeor.deleteChannel(channelName);
		Hydrometeor.updateXhrData();
		if (Hydrometeor.status != 0 && Hydrometeor.channelcount != 0) {
			Hydrometeor.connect();
		} else {
			Hydrometeor.disconnect();
		}
	},

	addChannel: function(channelName, backtrack) {
		Hydrometeor.channels[channelName] = {};
		if (backtrack) {
			Hydrometeor.channels[channelName].backtrack = backtrack;
		}
		// Better safe than sorry
		Hydrometeor.channelcount++;
	},
	
	deleteChannel: function(channelName) {
		delete(Hydrometeor.channels[channelName]);
		Hydrometeor.channelcount--;
	},	

	doBacktrack: function(channelName, callback) {
		$.get(Hydrometeor.path + "/backlog",
			{channel: channelName, count: Hydrometeor.channels[channelName].backtrack, callback: Hydrometeor.callbacks.process},
			function(msg) {
				// Split msg
				m = msg.split("\n");
				for (var i=0; i<m.length; i++) {
					if (m[i] != "") {
						var id = m[i].indexOf(",");
						if (id == -1) throw "Message received from Hydrometeor had no id!";
						Hydrometeor.updateSince(m[i].substr(0,id));
						eval(m[i].substr(id+1));
					}
				}
				delete(Hydrometeor.channels[channelName].backtrack);
				// For when we are performing backtracks in a queue
				Hydrometeor.backtrackqueue.remove(channelName);
				if (Hydrometeor.backtrackqueue.length == 0 && callback) {
					callback();
				}
			});
	},

	connect: function() {
		if (!Hydrometeor.path) throw "Hydrometeor path not specified";
		if (Hydrometeor.channelcount == 0) throw "No channels specified";
		if (!Hydrometeor.callbacks.process) throw "No process callback specified";
		if (!typeof(Hydrometeor.callbacks.process) == "string") throw "Specify process callback as a string = name of function";
		if (Hydrometeor.status != 0) Hydrometeor.disconnect();
		Hydrometeor.setstatus(1);
		if (Hydrometeor.backtrackqueue.length > 0) {
			for (var i=0; i <Hydrometeor.backtrackqueue.length; i++) {
				Hydrometeor.doBacktrack(Hydrometeor.backtrackqueue[i], Hydrometeor.connect);
			}
		} else {
			Hydrometeor.xhr = $.ajax({
				type: "GET",
				url:  Hydrometeor.path + "/subscribe",
				data: Hydrometeor.xhrdata,
				cache: false,
				success: function(msg) {
					m = msg.split("\n");
					for (var i=0; i<m.length; i++) {
						// TODO: Sort why this is returning a blank line at the bottom
						if (m[i] != "") {
							var id = m[i].indexOf(",");
							if (id == -1) throw "Message received from Hydrometeor had no id!";
							Hydrometeor.updateSince(m[i].substr(0,id));
							eval(m[i].substr(id+1));
						}
					}
					// Reconnect by calling this again
					Hydrometeor.connect();
				},
				error: function() {
					throw "Some sort of AJAX error?";
				}
				});
		}
	},

	disconnect: function() {
		if (Hydrometeor.status != 0) {
			Hydrometeor.setstatus(0);
			if (Hydrometeor.xhr) Hydrometeor.xhr.abort();
		}
	},

	updateXhrData: function() {
		if (Hydrometeor.channelcount != 0) {
			Hydrometeor.xhrdata["channel"] = [];
			for (var c in Hydrometeor.channels) {
				Hydrometeor.xhrdata["channel"].push(c);
			}
		} else {
			delete(Hydrometeor.xhrdata["channel"]);
		}
		Hydrometeor.xhrdata["callback"] = Hydrometeor.callbacks.process;	
	},

	updateSince: function(i) {
		Hydrometeor.xhrdata["since"] = ( i > Hydrometeor.xhrdata["since"] || !Hydrometeor.xhrdata["since"] ) ? i : Hydrometeor.xhrdata["since"];
	},

	setstatus: function(newstatus) {
		if (Hydrometeor.status != newstatus) {
			Hydrometeor.status = newstatus;
			// More could be added here, e.g. callbacks
		}
	},

	registerEventCallback: function(evt, functionName) {
		// For now, really simple
		Hydrometeor.callbacks[evt] = functionName;
		Hydrometeor.updateXhrData();
	}


};

if (typeof jQuery == 'undefined') {
	throw "jQuery has not been loaded!";
}

