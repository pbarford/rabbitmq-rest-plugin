-module(rabbitmq_rest_handler).
-export([init/3, handle/2, terminate/2]).

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(Req, State) ->
    Reply = case cowboy_http_req:method(Req) of
        {'POST', _} ->  
		  io:format("COWBOY handle_post~n"),
		  {ok, Body, Req1} = cowboy_http_req:body(Req),    
		  io:format("COWBOY headers~n"),
		  {HttpHeaders, Req2} = cowboy_http_req:headers(Req1),     
		  io:format("HttpHeaders-Count: '~p'~n", [length(HttpHeaders)]),
		  io:format("HttpHeaders: '~p'~n", [HttpHeaders]),
		  MsgHeaders = lists:filter(fun({K , _}) -> validHeader(K) end , HttpHeaders),
		  io:format("MsgHeaders: '~p'~n", [MsgHeaders]),
		  io:format("adding message: '~p'~n", [Body]),
		  rabbitmq_msg:send(Body, MsgHeaders),
		  {ok, R} = cowboy_http_req:reply(200, [], <<"message created">>, Req2),

            R;
        _ ->
            {ok, R} = cowboy_http_req:reply(405, [], <<"Method not allowed">>, Req),
            R
    end,
    {ok, Reply, State}.

terminate(_Req, _State) ->
    ok.

t(K) ->
  case re:run(K, "^x-") of
    {match, Captured} -> true;
    nomatch -> false
  end.

validHeader(Header) ->
	io:format("validHeader: '~p'~n", [Header]),
  HeadersToIgnore = [<<"connection">>,
				<<"content-type">>,
				<<"content-length">>,
				<<"cache-control">>,
				<<"user-agent">>,
				<<"cookie">>,
				<<"origin">>,
				<<"host">>,
	                <<"accept">>,
				<<"accept-encoding">>,
				<<"accept-language">>,
				'Connection',
				'Content-Type',
				'Content-Length',
				'Cache-Control',
				'User-Agent',
				'Cookie',
				<<"Origin">>,
				'Host',
	                'Accept',
				'Accept-Encoding',
				'Accept-Language'],
  isHeaderValid(Header, HeadersToIgnore).

isHeaderValid(Element, []) -> true;

isHeaderValid(Element, [Head | Tail]) ->
    case ( Head == Element ) of
        true -> false;
        false -> isHeaderValid(Element, Tail)
    end.