Hydrometeor
===========

There's not a lot here yet. But if you want instructions:

Installing
----------

 1. Install the latest version of Erlang. It'll save you having to adjust your
    .rel file below.

 2. Get the repository (look up how to use Git?)

 3. run make in this directory. Make sure everything looks ok

 4. Adjust hm_rel-1.rel to correspond with your Erlang versions. If you have
    to make changes, run `erl -pa ./ebin` then on the Erlang command line run 
    `systools:make_script("hm_rel-1", [local]).`. Then quit the Erlang shell.

 5. Make sure start.sh is executable: `chmod +x start.sh`.

 6. `./start.sh`

 7. Have fun! You might need to look in the code to know what to do.


Using
-----

At the moment you can use the Erlang shell you are dumped in to send messages,
like:
	hm_server:send("testchannel", <<"Hello there!">>).

Before sending that, in another terminal run a curl command ready to receive
it:
	curl "http://localhost:8000/subscribe?channel=testchannel"

Messages are sent in a format of `id,message` for the moment. The id can be
used with a since parameter. So for example, send another message:
	hm_server:send("testchannel", <<"Hello again!">>).

Now run the curl command:
	curl "http://localhost:8000/subscribe?channel=testchannel&since=1"

You should receive `2,Hello again!` straight away.