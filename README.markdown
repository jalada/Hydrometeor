Hydrometeor
===========

There's not a lot here yet. But if you want instructions:

Installing
----------

 1. Install the latest version of Erlang.

 2. Get the repository (look up how to use Git?)

 3. run make in this directory. Make sure everything looks ok

 4. Adjust hm_rel-1.rel to correspond with your Erlang versions. Whether you
    make changes or not run `erl -pa ./ebin` then on the Erlang command line
    run `systools:make_script("hm_rel-1", [local]).`. Then quit the Erlang
    shell. This updates the boot script to correspond with your versions.
    I'm probably going to remove this step once I understand more about
    Erlang releases etc.

 5. Make sure start.sh is executable: `chmod +x start.sh`.

 6. `./start.sh`

 7. Have fun! You might need to look in the code to know what to do.

Configuration
-----

Hydrometeor doesn't have a configuration file (yet), but most configurable
parameters are easily located. Here's some you might want:

 - **Listening port**: `src/hm_sup.erl`.
 - **Long-polling connection timeout**: `src/hm_web.erl` at the top.
 - **URI for Hydrometeor API**: `src/hm_web.erl` default is '/hm1backend'. 
 - **Maximum channel log size**: `src/hm_server.erl` at the top.
 - **Maximum channel age**: `src/hm_server.erl` at the top.
 - **Channel 'state' file**: `src/hm_server.erl` at the top.
 - **Housekeeping interval**: `src/hm_app.erl` at the top. You can also
   control what housekeeping does here (write in your own Erlang if you want).

Using
-----

At the moment you can use the Erlang shell you are dumped in to send messages,
like:

	hm_server:send("testchannel", <<"Hello there!">>).

Alternatively, you can send HTTP POST data to the server:

	curl -d "channel=testchannel&message=testmessage" http://localhost:9002/hm1backend/admin/send

Before sending that, in another terminal run a curl command ready to receive
it:

	curl "http://localhost:9002/hm1backend/subscribe?channel=testchannel"

Messages are sent in a format of `id,message` for the moment. The id can be
used with a since parameter. So for example, send another message:

	hm_server:send("testchannel", <<"Hello again!">>).

Now run the curl command:

	curl "http://localhost:9002/hm1backend/subscribe?channel=testchannel&since=1"

You should receive `2,Hello again!` straight away.
