# barrel_mochiweb

barrel_mochiweb is a mochiweb adapter for
[barrel](http://github.com/benoitc/barrel) allowing you to use mochiweb with the
barrel TCP acceptor pool.

## Use mochiweb with barrel

To use mochiweb with barrel, you just need to use the `barrel_mochiweb_protocol`
module as the prococol when you start a barrel  listener. You pass the mochiweb
`lopp` in the protocol options via the `loop` property.

Ex:

    -module(hello).
    -export([start/0, stop/0, loop/1]).

    -define(LOOP, {?MODULE, loop}).

    start() ->
        application:start(barrel),
        barrel:start_listener(http, 100,
                              barrel_tcp [{port, 8000}],
                              barrel_mochiweb_protocol, [{loop, ?LOOP}]).

    stop() ->
        application:stop(barrel).


    loop(Req) ->
        Req:respond({200, [{"Content-Type", "text/html"}],
                     <<"Hello world">>});.


See more usage examples in the `examples` for the usage.
