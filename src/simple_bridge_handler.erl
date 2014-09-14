% vim: ts=4 sw=4 et
-module(simple_bridge_handler).
-include("simple_bridge.hrl").

-type data()                ::  iolist() | binary().
-type ws_data()             ::  {text, data()} | {binary, data()}.
-type reply()               ::  ws_data() | [ws_data()].
-type reason()              ::  integer().
-type state()               ::  any().
-type full_reply()          ::  noreply
                                | {noreply, state()}
                                | {reply, reply()}
                                | {reply, reply(), state()}
                                | close
                                | {close, reason()}.


-callback run(bridge())         -> {ok, data()}.

-callback ws_init(bridge())     -> ok 
                                 | {ok, state()}
                                 | close
                                 | {close, reason()}.

-callback ws_message(ws_data(), bridge(), state()) -> full_reply().

-callback ws_info(ws_data(), bridge(), state())    -> full_reply().

-callback ws_terminate(reason(), bridge(), state()) -> ok.
