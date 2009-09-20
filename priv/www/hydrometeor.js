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
	channels: new Array(),
	path: null,
	last: null,
	status: 0,
	xhr: null,
	xhrdata: null,

	init: function() {
		if (!Hydrometeor.path) throw "Hydrometeor path not specified";
		if (Hydrometeor.path.charAt(Hydrometeor.path.length) == "/") {
			Hydrometeor.path = Hydrometeor.path.slice(0,Hydrometeor.path.length-1);
		}
		Hydrometeor.updateXhrData();
	},

	joinChannel: function(channelName, backtrack) {
		if (Hydrometeor.channels.inArray(channelName)) throw "Cannot join channel "+channelName+": already subscribed";
		Hydrometeor.channels.push(channelName);
		Hydrometeor.updateXhrData();
		// For the moment, backtracking occurs even if you're not connected
		if (typeof(backtrack) == "number") {
			$.get(Hydrometeor.path + "/backlog",
				{channel: channelName, count: backtrack, callback: Hydrometeor.callbacks.process},
				function(msg) {
					// Split msg
					m = msg.split("\n");
					for (var i=0; i<m.length; i++) {
						var id = m[i].indexOf(",");
						if (id == -1) throw "Message received from Hydrometeor had no id!";
						Hydrometeor.since = id;
						eval(m[i].substr(id+1));
					}
				});
		}
		if (Hydrometeor.status != 0 && Hydrometeor.channels.length != 0) {
			Hydrometeor.connect();
		}
	},

	leaveChannel: function(channelName) {
		if (!Hydrometeor.channels.inArray(channelName)) throw "Cannot leave channel "+channelName+": not subscribed";
		channels.remove(channelName);
		Hydrometeor.updateXhrData();
		if (Hydrometeor.status != 0 && Hydrometeor.channels.length != 0) {
			Hydrometeor.connect();
		} else {
			Hydrometeor.disconnect();
		}
	},

	connect: function() {
		if (!Hydrometeor.path) throw "Hydrometeor path not specified";
		if (Hydrometeor.channels.length == 0) throw "No channels specified";
		if (!Hydrometeor.callbacks.process) throw "No process callback specified";
		if (!typeof(Hydrometeor.callbacks.process) == "string") throw "Specify process callback as a string = name of function";
		if (Hydrometeor.status != 0) Hydrometeor.disconnect();
		Hydrometeor.setstatus(1);
		Hydrometeor.xhr = $.ajax({
			type: "GET",
			url:  Hydrometeor.path + "/subscribe",
			data: Hydrometeor.xhrdata,
			cache: false,
			success: function(msg) {
				var i = msg.indexOf(",");
				if (i == -1) throw "Message received from Hydrometeor had no id!";
				Hydrometeor.since = i;
				// Reconnect by calling this again
				Hydrometeor.connect();
				// Rest of message is the function to evaluate
				eval(msg.substr(i+1));
			},
			error: function() {
				throw "Some sort of AJAX error?";
			}
			});
	},

	disconnect: function() {
		if (Hydrometeor.status != 0) {
			Hydrometeor.setstatus(0);
			if (Hydrometeor.xhr) Hydrometeor.xhr.abort();
		}
	},

	updateXhrData: function() {
  		Hydrometeor.xhrdata = { "channel": Hydrometeor.channels, "callback": Hydrometeor.callbacks.process };	
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

