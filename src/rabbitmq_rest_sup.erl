-module(rabbitmq_rest_sup).
-behaviour(supervisor).

-export([start_link/0, 
         start_listeners/0,
         init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = []).

start_listeners() ->   
  Dispatch = [
        {'_', [
	  {[<<"restmq">>, '...'], rabbitmq_rest_handler, []}
        ]}
    ],
    cowboy:start_listener(my_http_listener, 1,
        cowboy_tcp_transport, [{port, 4040}],
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
