-ifndef(debug_print).
-define(debug_print, true).
-define(PRINT(Var), error_logger:info_msg("DEBUG: ~p:~p~n~p~n  ~p~n", [?MODULE, ?LINE, ??Var, Var])).
-endif.

-record(cookie, { name, value, path="/", minutes_to_live=20 }).
-record(header, { name, value }).
-record(response, { status_code=200, headers=[], cookies=[], data=[] }).
-record(simple_bridge_wrapper, {mod, req, is_multipart, post_params, post_files, error, headers=[], response=#response{}}).

-type bridge()		:: #simple_bridge_wrapper{}.
-type bridge_type()	:: cowboy | inets | mochiweb | webmachine | yaws | atom().
-type req()			:: any().
-type path()		:: string().
-type key()			:: atom() | binary().
-type value()		:: binary() | string().
-type ipv4()		:: {integer(), integer(), integer(), integer()}.
-type ipv8()		:: {integer(), integer(), integer(), integer(), integer(), integer(), integer(), integer()}.

-record(sb_uploaded_file, { original_name, temp_file, size, field_name }).
-record(request_cache, {request, docroot="", body=""}).
-compile({parse_transform,pmod_pt}).

-define(B2L(B), simple_bridge_util:b2l(B)).


