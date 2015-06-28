# SimpleBridge

[![Build Status](https://travis-ci.org/nitrogen/simple_bridge.svg?branch=master)](https://travis-ci.org/nitrogen/simple_bridge)

SimpleBridge takes the pain out of coding to multiple Erlang HTTP servers by
creating a standardized interface. It currently supports Cowboy, Inets,
Mochiweb, Webmachine, and Yaws.

SimpleBridge is used as the bridge to webservers for two of the most popular
Erlang web frameworks: [Nitrogen Web Framework](http://nitrogenproject.com)
and [ChicagoBoss](http://chicagoboss.org)

In a sense, it is similar to [EWGI](http://github.com/skarab/ewgi), except
SimpleBridge has some key improvements/differences:

  + **Easily extended** - Takes between 200 and 400 lines to add support for a
    new HTTP server, including the Bridge module itself, as well as default
    supervisor, and anchor module.
  + **Websockets** - SimpleBridge provides a single interface for handling
    websockets. Even servers which don't natively support websockets have a
    websocket layer with SimpleBridge.  This means **you can run websockets on
    Inets, Mochiweb, and Webmachine,** none of which natively support
    Websockets.
  + **MultiPart File Uploads** - SimpleBridge has better support for HTTP
    POSTS, including support for multipart file uploads, with size limits and
    handle-able errors.
  + **Static File Support** - Support for sending a static file to the browser,
    using the underlying HTTP server's own methods.
  + **Cookies Support** - SimpleBridge provides interface functions for getting
    and setting cookies.
  + **No Middleware Components** - SimpleBridge does not explicitly support
    EWGI's concept of "middleware components". (Though you could probably fake
    it, haven't tried.)

  + SimpleBridge is split into three parts:
    + A Bridge module to allows you to see information about the incoming
      request and construct a response.
    + A Default supervisor that starts and configures the underlying server
      based on either the server-specific configuration, or the univeral Simple
      Bridge configuration.
    + A Default **anchor** module, which implements the underlying
      communication and connection setup for each request, and that interfaces
      with your appliction through the use of a **handler** module.

## Hello World Example

```erlang
% SimpleBridge Hello World Example

start() ->
  Handler = ?MODULE,
  simple_bridge:start(mochiweb, Handler).

run(Bridge) ->
    HTML = [
        "<h1>Hello, World!</h1>",
        io_lib:format("METHOD: ~p~n<br><br>", [Request:request_method()]),
        io_lib:format("COOKIES: ~p~n<br><br>", [Request:cookies()]),
        io_lib:format("HEADERS: ~p~n<br><br>", [Request:headers()]),
        io_lib:format("QUERY PARAMETERS: ~p~n<br><br>", [Request:query_params()])       
    ],
    Bridge2 = Response:set_status_code(200),
    Bridge3 = Response:set_header("Content-Type", "text/html"),
    Bridge4 = Response2:set_response_data(HTML),
    Bridge4:build_response().
```

## A more complete example:

To see a complete example handler page using SimpleBridge, demonstrating the
results of requests, as well as providing a simple echo websocket server, look at
the code of
[simple_bridge_handler_sample](https://github.com/nitrogen/simple_bridge/blob/ws/src/simple_bridge_handler_sample.erl).

## Quick Start

There is a built-in quickstart for demoing simple bridge with any backend,
using the example mentioned above.

To try it out:

```bash
git clone -b ws git://github.com/nitrogen/simple_bridge.git
cd simple_bridge
make run_inets
# Feel free to replace "inets" with "cowboy", "mochiweb", "webmachine", or "yaws"
```
Then, open your browser and navigate to http://127.0.0.1:8000

## Starting Simple Bridge

As demonstrated above, the simplest approach is to just start simple bridge
with the appropriate backend (or use the simple_bridge.config). Simple Bridge
will then start the server to the specs of your choice.

The following are all valid ways to start Simple Bridge:
```erlang
%% Explicitly start with particular Backend and HandlerMod
simple_bridge:start(Backend, HandlerMod).

%% HandlerMod is determined from the handler config variable
simple_bridge:start(Backend).  

%% Backend and HandlerMod are both determined by the configuration
simple_bridge:start(). 

%% Same as simple_bridge:start().
application:start(simple_bridge). 
```

**Note**: You will still need to ensure that the dependencies are there for the
webserver of your choice.  For example, trying to start Yaws without the yaws
application currently in your dependencies will fail.

## The Anchor Module

When creating your server using any of the above methods, simple_bridge will
automatically use the backend-specific anchor module for the chosen backend
provided by Simple Bridge.

The anchor module serves as an additional layer to eliminate the need to write
*any* platform-specific code in your application.  The anchor module will
transform the platform-specific requests and pass them off to the Handler
Module in a uniform fashion.

## The Handler Module

A Handler module is a standard module that SimpleBridge will call out to when a
request is made, both standard HTTP requests, and websocket frames.

A Handler module is expected to export the following functions:

  + `run(Bridge)` - Bridge will be an initialized instance of a SimpleBridge
    object, and the last step of the run will be the return value of
    `BridgeX:build_response()`
  + `ws_init(Bridge)` - Called when a websocket connection is initialized.
    + Return Values:
      + `ok` - Everything went okay, proceed with the connection.
      + `{ok, State}` - Everything went okay, proceeed with connection and
        initialize with the provided `State` (which will be passed to
        `ws_message`, `ws_info`, and `ws_terminate` functions.
      + `close` - Everything did not go okay, let's shut down the connection.
  + `ws_message(Message, Bridge, State)` - Called when a websocket client has
    sent us something. 
    + `Message` can be:
      + `{Type, Data}` - `Type` will be either `binary` or `text`, depending on
      the nature of the message. `Data` will always be a binary. By the nature of the
      WebSocket protocol, you can be guaranteed that if `Type==text`, that `Data`
      will be verified to be valid UTF8 Unicode.
    + Return Values:
      + `noreply` - No reply will be made.
      + `{noreply, NewState}` - No reply will be made, but change the state to
        `NewState`
      + `{reply, {Type, Data}}` - `Type` can be `text` or `binary` and `Data`
        can be a binary, list, or iolist.
      + `{reply, {Type, Data}, NewState}` - Same as `{reply, {Type, Data}}`,
        except that the internal state will be changed to `NewState`
      + `{reply, [{Type, Data}]}` - Reply with a list of `{Type, Data}` pairs
        as a single message broken into several frames.
      + `{reply, [{Type, Data}], NewState}` - Same as `{reply, [{Type, Data}]}`
        except change the state to `NewState`
      + `close` - Kill the connection (will provide the Websocket Error code 1000)
      + `{close, StatusCode}` - Kill the connection with `StatusCode` (See
        [RFC6455 Sec 7.4.1](https://tools.ietf.org/html/rfc6455#section-7.4.1)
        for the list of valid connection codes).
  + `ws_info(Message, Bridge, State)` - Called when the websocket *process* receives a
    message (that is, a message was sent from an Erlang process).
    + `Message` can be any term
    + Return values are exactly the same as `ws_message`
  + `ws_terminate(ReasonCode, Bridge, State)` - The websocket is shutting down with
    `ReasonCode`.
    + Return Value: `ok`

Notice that each of the call above passes in a `Bridge` object. This object
will be how you interface with the underlying server, both retrieving
information about the request (headers, query strings, etc), as well as
building your response to the server.

### A brief note about `State` and Websockets

In the above handler functions, `State` can be any term.  It's for your own
applications to track the state of your application's.  The `State` is local
only to the specific client's connection.  For example, it could be used for
storing a session identifier (for quick lookup in the session key-value store,
rather than having to read a cookie from the `Bridge` object), or for tracking
some user-specific value that might change from message to message, such as
"Away" or "Do Not Disturb" status.  It's provided as a convenience so that you
won't need to rely on the process dictionary for tracking this state.  As soon
as the connection dies, this `State` will cease to exist. It is not, as one
might be inclined to believe, an application-wide state.

If your function returns, for example, the atom `noreply` or the `{reply,
Message}` two-tuples, then `State` will remain unchanged.  As such, if you
simply don't care about using the `State` variable, then you could easily
ignore the `State` variable by matching it with `_` in your function
definitions, and retuning the "Stateless" versions of each return value
(`noreply`, `{reply, Message}`).

### What can I do with the Bridge?

Once you have created Bridge object, you can interface with it using a series
of standardized function calls universal to all bridges.

You can interface with it using either of two mechanisms:

  + Standard Erlang Calls: `sbw:function_name(Bridge)` - "sbw" is an acronym
    for (S)imple (B)ridge (W)rapper.
  + Parameter Module Style Calls: `Bridge:function_name()`

Additionally, a Bridge provides both the Request and Response interface in a
single object (which means you no longer have to track both a request and a
response bridge in your application. A single bridge will do, pig.)

### Request Bridge Interface

  * **sbw:request_method(Bridge)** - returns atom 'GET', 'POST', 'HEAD', etc.
  * **sbw:path(Bridge)** - returns the requested path and file (string)
  * **sbw:peer_ip(Bridge)** - returns the client's IP address in tuple format
    (74.125.67.100 = `{74, 125, 67, 100}`).
  * **sbw:peer_port(Bridge)** - returns the client's port (integer)
  * **sbw:headers(Bridge)**(+) - returns a proplist of headers, `{<<"header">>,
    <<"Value1">>}, {<<"header2">>, <<"Value2">>}, ...]`.
  * **sbw:header(Header, Bridge)**(++) - returns the value of a header.
  * **sbw:cookies(Bridge)**(+) - returns a proplist of cookies,
    `[{<<"cookie1">>, <<"Value1">>}, {<<"cookie2">>, <<"Value2">>}, ...]`.
  * **sbw:cookie(Cookie, Bridge)**(++) - returns the value of a cookie.
  * **sbw:query_params(Bridge)**(+) - returns a proplist of query params,
    `[{<<"Query1">>, <<"Value1">>}, {<<"Query2">>, <<"Value2">>}, ...]`.
  * **sbw:query_param(Param, Bridge)**(++) - returns value of a query param
    named `Param`, `undefined` if not found.
  * **sbw:query_param_group(Param, Bridge)**(++) - returns values of all query
    params named `Param` as list, `["Value1", "Value2", ...]`,  `[]` if none
    found.
  * **sbw:post_params(Bridge)**(+) - returns a proplist of post params,
    `[{"Post1", "Value1"}, {"Post2", "Value2"}, ...]`.
  * **sbw:post_param(Param, Bridge)**(++) - returns value of a post param named
    `Param`, `undefined` if not found
  * **sbw:post_param_group(Param, Bridge)**(++) - returns values of all post
    params named `Param` as list, `["Value1", "Value2", ...]`,  `[]` if none
    found.
  * **sbw:param(Param, Bridge)**(++) - returns value of a query or post param
    named `Param`, `undefined` if not found
  * **sbw:param_group(Param, Bridge)**(++) - returns values of all query and
    post params named Param as list, `["Value1", "Value2", ...]`,  `[]` if none
    found.
  * **sbw:post_files(Bridge)** - returns a list of `#sb_uploaded_file` records,
    describing the files uploaded in a multipart post. These can be
    conveniently interfaced with the `sb_uploaded_file` module, documented below
    in the "Uploaded File Interface" section.
  * **sbw:request_body(Bridge)** - returns the request body that has been read
    so far, as a list.
  * **sbw:error(Bridge)** - returns an Erlang term describing any errors that
    happened while parsing a multipart post.

**(+)** Return Values will be a proplist of binaries. Keys are normalized to
lower-case binaries.

**(++)** Return type will be dependent on the provided Key (`Cookie`, `Header`,
`Param`, etc).  If the Key is a binary, the return type will be binary. If Key
is an atom or a string (list), the return type will be a string.

**Note:** When using Parameter-module style calls, simple remove the `Bridge`
argument from the call.  For example, when using the `query_param` function as
a P-mod call, use `Bridge:query_param(Param)` instead of
`sbw:query_param(Param, Bridge)`.

### Uploaded File Interface

`sbw:post_files(Bridge)` returns a list of `#sb_uploaded_file` records, but it's
inconvenient to have to include the `simple_bridge.hrl` header in your
application's modules.  The safer and more portable approach is to use the
`sb_uploaded_file` module provided by Simple Bridge.

As with the Bridge module above, all `sb_uploaded_file` objects can be referenced in either of two ways:

  + Standard Erlang Calls: `sb_uploaded_file:function_name(File)`
  + Parameter Module Style Calls: `File:function_name()`

`sb_uploaded_file` exports the following functions:

  * **sb_uploaded_file:original_name(UploadedFile)** - The original name of the file from the
    user's system
  * **sb_uploaded_file:temp_file(UploadedFile)** - The temporary name for the file as it's stored
    on the server. Returns `undefined` if file is kept in memory.
  * **sb_uploaded_file:size(UploadedFile)** - The size of the file in bytes
  * **sb_uploaded_file:field_name(UploadedFile)** - The name of the HTML `<input type=file>`
    element from the page.
  * **sb_uploaded_file:data(UploadedFile)** - The entire data of uploaded file. Returns `undefined`
    if file is stored as temporary file on disk.

By default uploaded files are always stored in temporary file
`sb_uploaded_file:temp_file(UploadedFile)`.  If you want to keep the uploaded
files in memory (`sb_uploaded_file:data(UploadedFile)`) instead of on disk, set
the max memory size for uploaded files by setting the `simple_bridge`
configuration variable `{max_file_in_memory_size, SizeinMB}`.  Uploaded files
larger than `SizeInMB` are still stored in temporary files.


### What can I do with the Response Bridge?

The response portion of the Bridge object provides you with a standard
interface for adding response status codes, headers, cookies, and a body.

Each function below returns a new bridge object, so you will need to 
chain together requests like this:

```erlang
run(Bridge) ->
    Bridge1 = sbw:set_status_code(200, Bridge),
    Bridge2 = sbw:set_header("Header1", "Value1", Bridge1),
    Bridge3 = sbw:set_response_data(HTML, Bridge2),
    sbw:build_response(Bridge3).
```
    
### Response Bridge Interface

As with all SimpleBridge interfaces, you can work with either the standard
Erlang calling syntax or use the P-mod style:

  + `sbw:function_name(Bridge)`
  + `Bridge:function_name()`

The Bridge modules export the following functions:

  * **sbw:set_status_code(Code, Bridge)** - set the HTTP status code. (200, 404, etc.)
  * **sbw:set_header(Name, Value, Bridge)** - set an HTTP header.
  * **sbw:clear_headers(Bridge)** - clear all previously set headers.
  * **sbw:set_cookie(Name, Value, Bridge)** - set a cookie for path "/" with expiration in
    20 minutes.
  * **sbw:set_cookie(Name, Value, Path, Exp, Bridge)** - Set a cookie. Exp is an integer
    in minutes.
  * **sbw:clear_cookies(Bridge)** - clear all previously set cookies.
  * **sbw:set_response_data(Data, Bridge)** - set the data to return in the response. Usually HTML
    goes here.
  * **sbw:set_response_file(File, Bridge)** - Send a static file to the browser.


Finally, you build the response to send to your HTTP server with the
build_response/0 function.

  * **sbw:build_response(Bridge)** - Create a response tuple that you can hand
    off to your HTTP server.

**DEPRECATION NOTICE:** For backwards compatibility with SimpleBridge Version
1, the following functions are also exported. Please refrain from using them in
future code, as they are deprecated.

  * **sbw:status_code/2** - equivilant to `sbw:set_status_code/2`.
  * **sbw:header/3** - equivilant to `sbw:set_header/3`
  * **sbw:cookie/3,5** - equivilant to `sbw:set_cookie/3/5`
  * **sbw:data/2** - equivilant to `sbw:set_response_data/2`
  * **sbw:file/2** - equivilant to `sbw:set_response_file/2`

## Configuration Options

The configuration optiosn found in `etc/simple_bridge.config` are all full
documented within the config file itself.  Feel free to copy it to your project
and use it as a base.

## Migrating from 1.x to 2.x?

Simple Bridge 2.0 should be *mostly* compatible with 1.x versions mostly right
out of the box, but is only compatible through deprecations.

### Let simple bridge start your server and set up the bridge for you

The recommended approach to migrating to 2.x is to remove instantiating your
Bridge altogether (that is, remove your `simple_bridge:make_request` and
`simple_bridge:make_response` functions from your app, and instead rely on the
"Handler" module (above) for handling requests, and the `simple_bridge.config`
file for setting up the backend server.

When doing this, instead of starting the backend servers yourself in the code,
you can rely on Simple Bridge to correctly set up the right configuration based
on `simple_bridge.config` and instantiate the server in the correct way, and
then starting your server with Simple Bridge by using something like
`simple_bridge:start(yaws, my_handler_module)` (or check the "Starting Simple
Bridge" section above).

#### You can still set up the bridge yourself, if you prefer

Following this paradigm, however, is not a requirement.  If you want to
maintain the same basic structure of your app, starting the server yourself,
handling the requests yourself, and then simply using Simple Bridge to
interface with the requests and response, then the recommended approach would
be to convert calls like:

```erlang
ReqBridge = simple_bridge:make_request(cowboy_request_module, Req),
ResBridge = simple_bridge:make_response(cowboy_response_module, Req),
...
ResBridge:build_response().
```

to simply using:
```erlang
Bridge = simple_bridge:make(cowboy, Req),
...
sbw:build_response(Bridge). %% Bridge:build_response() works too!
```

### Return type changes to look out for in 2.0

One of the other things to look out for with the move to 2.0 is the handling of
query parameter, post parameters, and headers.

* The return value of the `sbw:headers/1`, `sbw:query_params/1`, and
  `sbw:post_params/1` functions is now a list of proplists of binaries.
* The return values of the single `sbw:header/query_param/post_param/etc`
  functions however, will be based on the key provided.  For example, calling
  `sbw:header("x-forwarded-for", Bridge)` will return a list, while
  `sbw:header(<<"x-forwarded-for">>, Bridge)` will return a binary.

### Use non-deprecated functions in 2.0

The last thing to deal with when converting from 1.x to 2.0 is making the
changes from the old-style response bridge calls to the new names for the same
functions.  This change was made to disambiguate some of the confusion that
would arise from having `sbw:header/2,3,4` and `sbw:cookie/2,3,5` some of which
being setters and some being getters.

So Make sure that you're using `sbw:set_status_code` instead of
`Bridge:status_code`, `sbw:set_header` instead of `Bridge:header`, etc.  The
thing to note, is that **all of the non-deprecated response functions begin
with a verb** (e.g. `set_response_data`, `clear_cookies`, `build_response`,
etc). See the "DEPRECATION NOTICE" a few sections above.


## TODO

#### Version 2.0 TODO

* Fix the `simple_bridge:ensure_header` stuff to use binaries and require less
  conversion.
* Add Typespecs
* Ensure that internally, `sbw:function` is used everywhere instead of
  `Bridge:function`.
* Test with dialyzer after being converted to `sbw:function`

#### Beyond 2.0

* Support compression in websockets?

## Questions or Comments

We can be found on:

  * [The Nitrogen Mailing List](https://groups.google.com/forum/#!forum/nitrogenweb)
  * irc.freenode.net: #nitrogen
  * Or you can email Jesse Gumm directly at
    [gumm@sigma-star.com](mailto:gumm@sigma-star.com)

## Contributing

If you wish to contribute to SimpleBridge's development, check out our
[contribution
guidelines](https://github.com/nitrogen/nitrogen/blob/master/CONTRIB.markdown).

## License and Copyright

Simple Bridge was created by [Rusty Klophaus](http://rusty.io) in 2008 and has
been maintained by [Jesse Gumm](http://jessegumm.com) since 2011.

Simple Bridge is copyright 2008-2015 Rusty Klophaus and Jesse Gumm.

Licensed under the [MIT
License](https://github.com/nitrogen/simple_bridge/blob/master/MIT-LICENSE)
