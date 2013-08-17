-module(hello).

-export([start/0, stop/0, loop/1]).
-define(LOOP, {?MODULE, loop}).


start() ->
    application:start(barrel),
    barrel:start_listener(http, 100,
                          barrel_tcp, [{port, 8000}],
                          barrel_mochiweb_protocol, [{loop, ?LOOP}]).

stop() ->
    application:stop(barrel).


loop(Req) ->
    Path = Req:get(path),
    Resource = case string:str(Path, "?") of
        0 -> Path;
        N -> string:substr(Path, 1, length(Path) - (N + 1))
    end,
    handle_request(Resource, Req).


handle_request("/hello", Req) ->
    Req:respond({200, [{"Content-Type", "text/html"}], <<"Hello to you as well">>});

handle_request(Path, Req) ->
    Get = Req:parse_qs(),
    Post = Req:parse_post(),
    User_agent = Req:get_header_value("user-agent"),
    erlang:display({get, Get}),
    erlang:display({post, Post}),
    erlang:display({user_agent, User_agent}),
    erlang:display({path, Path}),
    Req:respond({200, [{"Content-Type", "text/html"}], <<"Hello World!">>}).
