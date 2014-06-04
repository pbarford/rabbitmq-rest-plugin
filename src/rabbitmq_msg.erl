-module(rabbitmq_msg).
-author('pbarford@gmail.com').
-behaviour(gen_server).

-define(EXCHANGE_NAME, factory_env:get_env(exchange_name, "restInbound")).
-define(MSG_TTL, factory_env:get_env(msg_ttl, "900000")).

-export([start_link/0]).

-export([init/1,
	 handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([send/3]).

-include("amqp_client.hrl").

-record(state, {channel}).

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, Connection} = amqp_connection:start(#amqp_params_direct{}),
    %{ok, Connection} = amqp_connection:start(#amqp_params_network{}),
    {ok, Channel} = amqp_connection:open_channel(Connection),
    amqp_channel:call(Channel, #'exchange.declare'{exchange = list_to_binary(?EXCHANGE_NAME),
                                                   type = <<"direct">>}),
    {ok, #state{channel = Channel}}.

handle_call({send, ContentType, Body, Headers}, _From, State = #state{channel = Channel}) ->
    %io:format("rabbitmq_msg call~n"),
    Properties = #'P_basic'{content_type = ContentType, expiration= list_to_binary(?MSG_TTL), delivery_mode=1,headers = map_http_headers(Headers)},
    BasicPublish = #'basic.publish'{exchange = list_to_binary(?EXCHANGE_NAME), routing_key = <<"">>},
    Content = #amqp_msg{props = Properties, payload = Body},
    Ret = case amqp_channel:call(Channel, BasicPublish, Content) of
	 'ok' -> 
		{201, <<"message put onto rabbit">>};
	 _ ->
		{500, <<"unable to publish message to rabbit">>}
    end,	
    %io:format("rabbitmq_msg call '~p'~n", [Ret]),
    {reply, Ret, State};

handle_call(_Msg, _From, State) ->
    {reply, unknown_command, State}.

handle_cast(_, State) ->
    {noreply,State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_, #state{channel = Channel}) ->
    %io:format("rabbitmq_msg terminate~n"),
    amqp_channel:call(Channel, #'channel.close'{}),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

map_http_headers(HttpHeaders) ->
    lists:map(fun({K , V}) -> {K, longstr, V} end, HttpHeaders).


send(ContentType, Body, Headers) ->
    %io:format("rabbitmq_msg send~n"),
    gen_server:call({global, ?MODULE}, {send, ContentType, Body, Headers}).

