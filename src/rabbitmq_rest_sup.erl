-module(rabbitmq_rest_sup).
-behaviour(supervisor).

-define(URI, factory_env:get_env(http_uri, "restmq")).
-define(PORT, factory_env:get_env(http_port, 4040)).
-define(LISTENERS, factory_env:get_env(http_listener_count, 5)).

-export([start_link/0, 
         start_listeners/0,
         init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = []).


start_listeners() ->   
    Dispatch = [
        {'_', [
	  {[ list_to_binary(?URI), '...'], rabbitmq_rest_handler, []}
        ]}
    ],

    cowboy:start_listener(my_http_listener, ?LISTENERS,
        cowboy_tcp_transport, [{port, ?PORT}],
        cowboy_http_protocol, [{dispatch, Dispatch}]
    ),
    rabbitmq_msg:start_link(). 


init([]) ->
    ok = pg2:create(rabbitmq_rest_listeners),
    {ok, { {one_for_one, 5, 10},
    [ {rabbitmq_rest_http,
        {rabbitmq_rest_sup, start_listeners, []},
        permanent, 1000, worker, [rabbitmq_rest_sup]}
    ]}
  }.
