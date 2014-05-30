-module(rabbitmq_rest_sup).
-behaviour(supervisor).

-export([start_link/0, 
         start_listeners/0,
         init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = []).

start_listeners() ->
  Dispatch =
    cowboy_router:compile(
      [ {'_',
          [
            {<<"/restmq">>, rabbitmq_rest_handler, []}
          ]
        }
      ]),

  RanchOptions =
    [ {port, 4040}
    ],
  CowboyOptions =
    [ {env,       [{dispatch, Dispatch}]}
    , {compress,  true}
    , {timeout,   12000}
    ],
  rabbitmq_msg:start_link(),  
  cowboy:start_http(rabbitmq_rest_http, 20, RanchOptions, CowboyOptions).

init([]) ->
    ok = pg2:create(rabbitmq_rest_listeners),
    {ok, { {one_for_one, 5, 10},
    [ {rabbitmq_rest_http,
        {rabbitmq_rest_sup, start_listeners, []},
        permanent, 1000, worker, [rabbitmq_rest_sup]}
    ]}
  }.
