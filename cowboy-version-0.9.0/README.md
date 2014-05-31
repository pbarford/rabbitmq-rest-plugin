rabbitmq-rest-plugin
====================

erl -pa ebin -pa deps/*/ebin

application:start(crypto).
application:start(cowlib).
application:start(ranch).
application:start(cowboy).
application:start(rabbitmq_rest).
