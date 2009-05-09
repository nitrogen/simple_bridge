% Simple Erlang Web Bridge
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (request_bridge_wrapper, [Mod, Req]).
-compile(export_all).

request_method() -> Mod:request_method(Req).
url() -> Mod:url(Req).
protocol() -> Mod:protocol(Req).
hostname() -> Mod:hostname(Req).
port() -> Mod:port(Req).
path() -> Mod:path(Req).
querystring() -> Mod:querystring(Req).

peer_ip() -> Mod:peer_ip(Req).
peer_port() -> Mod:peer_port(Req).
