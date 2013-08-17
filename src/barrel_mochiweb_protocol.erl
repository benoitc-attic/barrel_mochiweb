%M% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%%% ex: ft=erlang ts=4 sw=4 et
%%%
%%% This file is part of barrel_mochiweb released under the MIT license.
%%% See the NOTICE for more information.

-module(barrel_mochiweb_protocol).

-export([init/4]).

-export([loop/1]).
-export([after_response/2, reentry/1]).
-export([new_request/3, call_body/2]).
-export([remove_connection/0]).
-export([get_listener_ref/0]).

-define(REQUEST_RECV_TIMEOUT, 300000).   %% timeout waiting for request line
-define(HEADERS_RECV_TIMEOUT, 30000).    %% timeout waiting for headers

-define(MAX_HEADERS, 1000).
-define(DEFAULTS, [{name, ?MODULE},
                   {port, 8888}]).


-ifndef(gen_tcp_fix).
-define(R15B_GEN_TCP_FIX, {tcp_error,_,emsgsize} ->
        % R15B02 returns this then closes the socket, so close and exit
        terminate(State);
       ).
-else.
-define(R15B_GEN_TCP_FIX, ).
-endif.

-record(hstate, {
        socket :: inet:socket(),
        transport :: module(),
        loop :: {module(), any(), any()}}).


%% @doc accept a request
-spec init(barrel:ref(), module(), inet:socket(), any()) -> ok | none().
init(ListenerRef, Transport, Socket, Opts) ->
    io:format("opts ~p~n", [Opts]),
    {loop, HttpLoop} = proplists:lookup(loop, Opts),
    put(barrel_ref, ListenerRef),
    loop(#hstate{socket = Socket,
                 transport = Transport,
                 loop = HttpLoop}).

loop(#hstate{transport=Transport, socket=Socket}=State) ->
    ok = Transport:setopts(Socket, [{packet, http}]),
    request(State).

request(#hstate{transport=Transport, socket=Socket}=State) ->
    ok = Transport:setopts(Socket, [{active, once}]),
    receive
        {Protocol, _, {http_request, Method, Path, Version}}
               when Protocol == http orelse Protocol == ssl ->
            ok = Transport:setopts(Socket, [{packet, httph}]),
            headers(State, {Method, Path, Version}, [], 0);
        {Protocol, _, {http_error, "\r\n"}}
                when Protocol == http orelse Protocol == ssl ->
            request(State);
        {Protocol, _, {http_error, "\n"}}
                when Protocol == http orelse Protocol == ssl ->
            request(State);
        {tcp_closed, _} ->
            terminate(State);
        {ssl_closed, _} ->
            terminate(State);
        ?R15B_GEN_TCP_FIX
        _Other ->
            handle_invalid_request(State)
    after ?REQUEST_RECV_TIMEOUT ->
        terminate(State)
    end.

headers(#hstate{transport=Transport, socket=Socket}=State, Request,
        Headers, ?MAX_HEADERS) ->
    %% Too many headers sent, bad request.
    ok = Transport:setopts(Socket, [{packet, raw}]),
    handle_invalid_request(State, Request, Headers);
headers(#hstate{transport=Transport, socket=Socket, loop=Loop}=State, Request,
        Headers, HeaderCount) ->
    ok = Transport:setopts(Socket, [{active, once}]),
    receive
        {Protocol, _, http_eoh}
                when Protocol == http orelse Protocol == ssl ->
            Req = new_request(State, Request, Headers),
            catch(call_body(Loop, Req)),
            ?MODULE:after_response(Loop, Req);
        {Protocol, _, {http_header, _, Name, _, Value}}
                when Protocol == http orelse Protocol == ssl ->
            headers(State, Request, [{Name, Value} | Headers],
                    1 + HeaderCount);
        {tcp_closed, _} ->
            terminate(State);
        ?R15B_GEN_TCP_FIX
        _Other ->
            handle_invalid_request(State, Request, Headers)
    after ?HEADERS_RECV_TIMEOUT ->
        terminate(State)
    end.

call_body({M, F, A}, Req) ->
    erlang:apply(M, F, [Req | A]);
call_body({M, F}, Req) ->
    M:F(Req);
call_body(Body, Req) ->
    Body(Req).

-spec handle_invalid_request(term()) -> no_return().
handle_invalid_request(State) ->
    handle_invalid_request(State, {'GET', {abs_path, "/"}, {0,9}}, []),
    exit(normal).

-spec handle_invalid_request(term(), term(), term()) -> no_return().
handle_invalid_request(#hstate{transport=Transport, socket=Socket}=State,
                       Request, RevHeaders) ->
    Req = new_request(State, Request, RevHeaders),
    Req:respond({400, [], []}),
    Transport:close(Socket),
    exit(normal).

new_request(#hstate{transport=Transport, socket=Socket}=State,
            Request, RevHeaders) ->
    Transport:setopts(Socket, [{packet, raw}]),
    mochiweb:new_request({mochiweb_socket(State), Request,
                          lists:reverse(RevHeaders)}).


reentry(Body) ->
    fun (Req) ->
            ?MODULE:after_response(Body, Req)
    end.

after_response(Body, Req) ->
    {Transport, Socket} = case Req:get(socket) of
        {ssl, S} ->
            {barrel_ssl, S};
        S ->
            {barrel_tcp, S}
    end,

    case Req:should_close() of
        true ->
            mochiweb_socket:close(Socket),
            ok;
        false ->
            Req:cleanup(),
            erlang:garbage_collect(),
            ?MODULE:loop(#hstate{transport=Transport,
                                 socket=Socket,
                                 loop=Body})
    end.

%% @doc get listener ref
get_listener_ref() ->
    get(barel_ref).

%% @doc remove a connection from the connection manager
%% Useful when you want to keep this connection open but don't want to count in the concurrent connections managed by a listener
remove_connection() ->
    Ref = get(barel_ref),
    barrel:remove_connection(Ref).


mochiweb_socket(#hstate{transport=Transport, socket=Socket}) ->
    case Transport of
        barrel_ssl ->
            {ssl, Socket};
        _ ->
            Socket
    end.


terminate(#hstate{transport=Transport, socket=Socket}) ->
    Transport:close(Socket),
    ok.
