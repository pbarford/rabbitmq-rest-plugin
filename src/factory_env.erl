-module(factory_env).
-define(APPLICATION, rabbitmq_rest).

-export([get_env/2, set_env/2]).

get_env(Key, Default) ->
    case application:get_env(?APPLICATION, Key) of
        {ok, Value} -> Value;
        undefined -> Default
    end.

set_env(Key, Value) ->
    application:set_env(?APPLICATION, Key, Value).
