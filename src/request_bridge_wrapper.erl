% Simple Erlang Web Bridge
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (request_bridge_wrapper, [Mod, Req]).
-compile(export_all).

request_method() -> Mod:request_method(Req).
path() -> Mod:path(Req).
query_string() -> Mod:query_string(Req).

peer_ip() -> Mod:peer_ip(Req).
peer_port() -> Mod:peer_port(Req).

headers() -> Mod:headers(Req).
cookies() -> Mod:cookies(Req).

query_params() -> Mod:query_params(Req).
post_params() -> Mod:post_params(Req).
request_body() -> Mod:request_body(Req).