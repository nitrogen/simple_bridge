# SimpleBridge

SimpleBridge takes the pain out of coding to multiple Erlang HTTP servers by
creating a standardized interface. It currently supports Cowboy, Inets,
Mochiweb, Webmachine, and Yaws.

SimpleBridge is used as the bridge to webservers for the two most popular
Erlang web frameworks: [Nitrogen Web Framework](http://nitrogenproject.com)
and [ChicagoBoss](http://chicagoboss.org)

In a sense, it is similar to [EWGI](http://github.com/skarab/ewgi), except
SimpleBridge has some key improvements/differences:

  + **Smaller code base** - SimpleBridge is 870 lines, compared to 1,974 lines
    for EWGI. This is because SimpleBridge uses the underlying HTTP server's
    built in parsing functions as much as possible.
  + **Easily extended** - Takes about 150 lines to add support for a new HTTP
    server, vs. ~350 for EWGI.
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
  + SimpleBridge is split into two parts: 
    + A *Request Bridge* is a parameterized module that allows you to see
      information about the incoming request.
    + A *Response Bridge* is a parameterized module that allows you to
      construct a response.


## Hello World Example

```erlang
% SimpleBridge Hello World Example in Mochiweb

start(_, _) ->
    Options = [{ip, "127.0.0.1"}, {port, 8000}],
    Loop = fun loop/1,
    mochiweb_http:start([{name, mochiweb_example_app}, {loop, Loop} | Options]).

loop(Req) ->
    Request = simple_bridge:make_request(mochiweb_request_bridge, {Req, "./wwwroot"}),
    HTML = [
        "<h1>Hello, World!</h1>",
        io_lib:format("METHOD: ~p~n<br><br>", [Request:request_method()]),
        io_lib:format("COOKIES: ~p~n<br><br>", [Request:cookies()]),
        io_lib:format("HEADERS: ~p~n<br><br>", [Request:headers()]),
        io_lib:format("QUERY PARAMETERS: ~p~n<br><br>", [Request:query_params()])       
    ],

    Response = simple_bridge:make_response(mochiweb_response_bridge, {Req, "./wwwroot"}),       
    Response1 = Response:status_code(200),
    Response2 = Response1:header("Content-Type", "text/html"),
    Response3 = Response2:data(HTML),
    Response3:build_response().
```


## Request Bridges

### How do I make a request bridge?

To make a request bridge for an incoming request, call the
simple_bridge:make_request/2 function, specifying the appropriate bridge module
for your HTTP server, and the arguments that it needs. This code would sit in
the loop/1 function of a Mochiweb server, or the do/1 function of an Inets
server.

Inets example:

```erlang
    RequestBridge = simple_bridge:make_request(inets_response_bridge, Info)
```

Mochiweb example:

```erlang
    RequestBridge = simple_bridge:make_request(mochiweb_response_bridge, [{Req, Docroot}]).
```
    
    
### What can I do with the request bridge object?

Once you have created the request bridge object (a parameterized module), it
provides you with a standard interface for accessing the request method, path,
query parameters, post parameters, headers, and cookies of the request:


### Request Bridge Interface

  * *Bridge:request_method()* - returns 'GET', 'POST', 'HEAD', etc.
  * *Bridge:path()* - returns the requested path and file.
  * *Bridge:peer_ip()* - returns the client's IP address in tuple format
    (74.125.67.100 = `{74, 125, 67, 100}`).
  * *Bridge:peer_port()* - returns the client's port.
  * *Bridge:headers()* - returns a proplist of headers, `{header1, "Value1"},
    {header2, "Value2"}, ...]`.
  * *Bridge:header(Header)* - returns the value of a header.
  * *Bridge:cookies()* - returns a proplist of cookies, `[{"Cookie1", "Value1"},
    {"Cookie2", "Value2"}, ...]`.
  * *Bridge:query_params()* - returns a proplist of query params, `[{"Query1",
    "Value1"}, {"Query2", "Value2"}, ...]`.
  * *Bridge:query_param(Param)* - returns value of a query param named `Param`,
    `undefined` if not found.
  * *Bridge:query_param_group(Param)* - returns values of all query params
    named `Param` as list, `["Value1", "Value2", ...]`,  `[]` if none found.
  * *Bridge:post_params()* - returns a proplist of post params, `[{"Post1",
    "Value1"}, {"Post2", "Value2"}, ...]`.
  * *Bridge:post_param(Param)* - returns value of a post param named `Param`,
    `undefined` if not found
  * *Bridge:post_param_group(Param)* - returns values of all post params named
    `Param` as list, `["Value1", "Value2", ...]`,  `[]` if none found
  * *Bridge:param(Param)* - returns value of a query or post param named
    `Param`, `undefined` if not found
  * *Bridge:param_group(Param)* - returns values of all query and post params
    named Param as list, `["Value1", "Value2", ...]`,  `[]` if none fund
  * *Bridge:post_files()* - returns a list of `#sb_uploaded_file` records,
    describing the files uploaded in a multipart post.
  * *Bridge:request_body()* - returns the request body that has been read so
    far, as a list.
  * *Bridge:error()* - returns an Erlang term describing any errors that happened
    while parsing a multipart post.

### Uploaded File Interface

`Bridge:post_files()` returns a list of `#sb_uploaded_file` records, but it's
inconvenient to have to include the `simple_bridge.hrl` header in your
application's modules.  The safer and more portable approach is to use the
`sb_uploaded_file` module provided by Simple Bridge.

`sb_uploaded_file` exports the following functions:

  * *UploadedFile:original_name()* - The original name of the file from the
    user's system
  * *UploadedFile:temp_file()* - The temporary name for the file as it's stored
    on the server. Returns `undefined` if file is kept in memory.
  * *UploadedFile:size()* - The size of the file in bytes
  * *UploadedFile:field_name()* - The name of the HTML `<input type=file>`
    element from the page.
  * *UploadedFile:data()* - The entire data of uploaded file. Returns `undefined`
    if file is stored as temporary file on disk.

By default uploaded files are always stored in temporary file *UploadedFile:temp_file()*.
If you want to keep the uploaded files in memory (*UploadedFile:data()*)
instead of on disk, set the max memory size for uploaded files by adding the
VM runtime argument `-simple_bridge_max_file_in_memory_size SizeInMB`.
Uploaded files larger than `SizeInMB` are still stored in temporary files.

### What modules are involved in a request bridge?

  * *request_bridge.erl* - The behaviour interface that request bridge modules
    must implement.
  * *request_bridge_wrapper.erl* - A parameterized module that wraps a request. 
  * *cowboy_request_bridge.erl* - The request bridge module for Cowboy.
  * *inets_request_bridge.erl* - The request bridge module for Inets.
  * *mochiweb_request_bridge.erl* - The request bridge module for Mochiweb.
  * *webmachine_request_bridge.erl* - The request bridge module for Webmachine.
  * *yaws_request_bridge.erl* - The request bridge module for Yaws.
  * *misultin_request_bridge.erl* - The request bridge module for Misultin.
  * *???_request_bridge.erl* - Support for more servers on the way.
  * *sb_uploaded_file.erl* - API to access information about an uploaded file.

To extend the SimpleBridge to work with other HTTP servers (or other versions
of Cowboy, Inets, Mochiweb, Webmachine, or Yaws), copy and modify
inets_request_bridge.erl or mochiweb_request_bridge.erl.

## Response Bridges

### How do I make a response bridge?

To make a request bridge for an incoming request, call the
simple_bridge:make_response/2 function, specifying the appropriate bridge
module for your HTTP server, and the arguments that it needs. This code would
sit in the loop/1 function of a Mochiweb server, or the do/1 function of an
Inets server.

Inets example:

```erlang
    ResponseBridge = simple_bridge:make_response(inets_response_bridge, Info)
```

Mochiweb example:

```erlang
    ResponseBridge = simple_bridge:make_response(mochiweb_response_bridge, {Req, Docroot})
```

### What can I do with the Response Bridge?

Once you have created the request bridge object (a parameterized module), it
provides you with a standard interface for combining headers, cookies, and a
response body into a response appropriate for your http server. 

Each function below returns a new bridge object, so your will need to 
chain together requests like this:

```erlang
    Bridge = simple_bridge:make_response(inets_response_bridge, Info),
    Bridge1 = Bridge:status_code(200),
    Bridge2 = Bridge1:header("Header1", "Value1"),
    Bridge3 = Bridge2:data(HTML),
    etc.
```
    
### Response Bridge Interface

  * *Bridge:status_code(Code)* - set the HTTP status code. (200, 404, etc.)
  * *Bridge:header(Name, Value)* - set an HTTP header.
  * *Bridge:clear_headers()* - clear all previously set headers.
  * *Bridge:cookie(Name, Value)* - set a cookie for path "/" with expiration in
    20 minutes.
  * *Bridge:cookie(Name, Value, Path, Exp)* - Set a cookie. Exp is an integer
    in minutes.
  * *Bridge:clear_cookies()* - clear all previously set cookies.
  * *Bridge:data(Data)* - set the data to return in the response. Usually HTML
    goes here.
  * *Bridge:file(File)* - Send a static file to the browser.

Finally, you build the response to send to your HTTP server with the
build_response/0 function.

  * *Bridge:build_response()* - Create a response tuple that you can hand off to your HTTP server.


### What modules are involved in a response bridge?

  * *response_bridge.erl* - The behaviour interface that response bridge
    modules must implement.
  * *response_bridge_wrapper.erl* - A parameterized module that wraps a
    response. 
  * *inets_response_bridge.erl* - The response bridge module for Inets.
  * *mochiweb_response_bridge.erl* - The response bridge module for Mochiweb.
  * *webmachine_response_bridge.erl* - The response bridge module for
    Webmachine.
  * *cowboy_response_bridge.erl* - The response bridge module for Cowboy.
  * *yaws_response_bridge.erl* - The response bridge module for Yaws.
  * *misultin_response_bridge.erl* - The response bridge module for Misultin.
  * *???_response_bridge.erl* - Support for more servers on the way.

To extend the SimpleBridge to other HTTP servers (or other versions of Cowboy, Inets,
Mochiweb, Webmachine, or Yaws), copy and modify inets_response_bridge.erl or
mochiweb_response_bridge.erl.

## Contributing

If you wish to contribute to SimpleBridge's development, check out our
[contribution
guidelines](https://github.com/nitrogen/nitrogen/blob/master/CONTRIB.markdown).
