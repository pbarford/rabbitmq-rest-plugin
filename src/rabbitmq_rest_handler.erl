%% @doc Handler for /rest endpoints
-module(rabbitmq_rest_handler).
-author('pbarford@gmail.com').

-export(
  [ init/3,
    allowed_methods/2,
    content_types_accepted/2,
    terminate/3
  ]).

-export(
  [ handle_post/2 ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% COWBOY CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(_Transport, _Req, []) ->
  io:format("COWBOY init~n"), 
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) -> {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
  {[{<<"application/text">>, handle_post}], Req, State}.

terminate(_Reason, _Req, State) -> 
  io:format("COWBOY terminate~n"),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNAL CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_post(Req, State) ->
  io:format("COWBOY handle_post~n"),
  {ok, Body, Req1} = cowboy_req:body(Req),
  {HttpHeaders, Req2} = cowboy_req:headers(Req1),
  io:format("HttpHeaders-Count: '~p'~n", [length(HttpHeaders)]),
  io:format("HttpHeaders: '~p'~n", [HttpHeaders]),
  MsgHeaders = lists:filter(fun({K , _}) -> validHeader(K) end , HttpHeaders),
  io:format("MsgHeaders: '~p'~n", [MsgHeaders]),

  io:format("adding message: '~p'~n", [Body]),
  rabbitmq_msg:send(Body, MsgHeaders),
  {true, Req2, State}.

t(K) ->
  case re:run(K, "^x-") of 
    {match, Captured} -> true; 
    nomatch -> false 
  end.

validHeader(Header) ->
  HeadersToIgnore = [<<"connection">>, 
		     <<"content-type">>,
		     <<"content-length">>,
		     <<"cache-control">>,
		     <<"user-agent">>,
		     <<"cookie">>,
		     <<"origin">>,
		     <<"accept">>,
		     <<"accept-encoding">>,
		     <<"accept-language">>],
  isHeaderValid(Header, HeadersToIgnore).

isHeaderValid(Element, []) -> true;

isHeaderValid(Element, [Head | Tail]) ->
    case ( Head == Element ) of
        true    ->  false;
        false   ->  isHeaderValid(Element, Tail)
    end.


