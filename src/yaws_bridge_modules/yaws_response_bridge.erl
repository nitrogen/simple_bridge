% vim: sw=4 ts=4 et
% Simple Bridge
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module(yaws_response_bridge).
-behaviour (simple_bridge_response).
-include_lib ("simple_bridge.hrl").
-export ([build_response/2,init/1]).

init(_Arg) ->
    _Arg.

build_response(_Arg, Res) ->
    % Get vars...
    Code = Res#response.statuscode,

    %% Assemble headers...
    Headers = assemble_headers(Res),


    case Res#response.data of
        {data, Body} ->

            %% Get the content type...
            ContentType = get_content_type(Res),

            % Send the yaws response...
            lists:flatten([
                           {status, Code},
                           Headers,
                           {content, ContentType, Body}
                          ]);

        {file, Path} ->
            %% Note: This section should only be entered in the event that a static file is
            %% requested that isn't found in the 'appmod' section of the yaws.conf file.
            %% I've not found a way to "pass the buck" back to yaws and say "even though this
            %% directory isn't found in the appmod, I want you to serve it anyway".  This
            %% means that with the current implementation, you don't want to be serving files
            %% big files that aren't covered in the appmod section, primarily because this little
            %% snippet loads the entire file into memory then passes it off to yaws to be served,
            %% rather than streaming it.  I'll need to look further into to either 1) Pass the buck
            %% completely back to Yaws, or 2) how the streamcontent return types work as define in
            %% yaws_server:handle_out_reply

            %% Static Content should have an expires date.  If not, we're going to make one
            Headers2 = ensure_expires_header(Res, Headers),

            %% Docroot needed to find file in Path
            Docroot = yaws_api:arg_docroot(_Arg),
            FullPath = [Docroot,Path],

            %% Get the content type as defined by yaws
            ContentType = yaws_api:mime_type(Path),
           
            %% Get the file content
            FullResponse = case file:read_file(FullPath) of
                {error,enoent} -> 
                    yaws_outmod:out404(_Arg);
                {ok,Bin} -> 
                    [
                        {status, Code},
                        Headers2,
                        {content, ContentType, Bin}
                    ]
            end,
            lists:flatten(FullResponse)
    end.

assemble_headers(Res) ->
    lists:flatten([
                   [{header, {X#header.name, X#header.value}} || X <- Res#response.headers],
                   [create_cookie(X) || X <- Res#response.cookies]
                  ]).

%% This is slightly different from the one in simple_bridge_util due to the
%% formatting of the yaws headers isn't just a simple proplist.
ensure_expires_header(Res,Headers) ->
    case simple_bridge_util:needs_expires_header(Res#response.headers) of
        true -> 
            Expires = simple_bridge_util:default_static_expires_header(),
            [{header, Expires} | Headers];
        false -> Headers
    end.
    

get_content_type(Res) ->
    coalesce([
              kvl3(content_type, Res#response.headers),
              kvl3("content-type", Res#response.headers),
              kvl3("Content-Type", Res#response.headers),
              kvl3("CONTENT-TYPE", Res#response.headers),
              "text/html"
             ]).

kvl3(Key,L) ->
    case lists:keysearch(Key,2,L) of
        {value, {_,_,Val}} -> Val;
        _                  -> undefined
    end.

coalesce([]) -> undefined;
coalesce([undefined|T]) -> coalesce(T);
coalesce([H|_T]) -> H.

create_cookie(Cookie) ->
    Name = Cookie#cookie.name,
    Value = Cookie#cookie.value,
    Path = Cookie#cookie.path,
    SecondsToLive = Cookie#cookie.minutes_to_live * 60,
    Expire = to_cookie_expire(SecondsToLive),
    yaws_api:setcookie(Name, Value, Path, Expire).

to_cookie_expire(SecondsToLive) ->
    Seconds = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    DateTime = calendar:gregorian_seconds_to_datetime(Seconds + SecondsToLive),
    httpd_util:rfc1123_date(DateTime).
