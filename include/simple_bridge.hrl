-ifndef(debug_print).
-define(debug_print, true).
-define(PRINT(Var), error_logger:info_msg("DEBUG: ~p:~p~n~p~n  ~p~n", [?MODULE, ?LINE, ??Var, Var])).
-endif.

-ifndef(simple_bridge_hrl).
-define(simple_bridge_hrl, true).

-record(cookie, { name, value, path="/", minutes_to_live=20 }).
-record(header, { name, value }).
-record(response, { status_code=200, headers=[], cookies=[], data=[] }).

%% sbw = (S)imple (B)ridge (W)rapper
-record(sbw, {mod, req, is_multipart=false, post_params=[], query_params=[], post_files=[], error=none, headers=[], cookies=[], response=#response{}}).

-type bridge()		:: #sbw{}.
-type bridge_type()	:: cowboy | inets | mochiweb | webmachine | yaws | atom().
-type req()			:: any().
-type path()		:: string().
-type key()			:: atom() | binary().
-type value()		:: binary() | string().
-type ipv4()		:: {integer(), integer(), integer(), integer()}.
-type ipv8()		:: {integer(), integer(), integer(), integer(), integer(), integer(), integer(), integer()}.

-record(sb_uploaded_file, { original_name, temp_file, size, field_name, data }).
-record(request_cache, {request, docroot="", body=""}).

-define(DEFAULT_IP, 	{0,0,0,0}).
-define(DEFAULT_PORT, 	8000).
-define(DEFAULT_DOCROOT,"./priv/static").
-define(DEFAULT_STATIC_PATHS, []).

-endif.
