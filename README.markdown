<h1>Simple Bridge</h1>

Simple Bridge takes the pain out of coding to multiple Erlang web servers by providing a 
common interface to Inets, Mochiweb, and Yaws.

Simple Bridge is split into two parts: 

* A *Request Bridge* allows you to see information about the incoming request.
* A *Response Bridge* allows you to construct a response.


<h2>Request Bridges</h2>
<h4>How do I make a request bridge?</h4>

To make a request bridge for an incoming request, call the request_bridge:make/2 function,
specifying the correct bridge module for your http server. 

Inets example:

	Bridge = request_bridge:make(inets_request_bridge, InetsRequestData)

Mochiweb example:

	Bridge = request_bridge:make(mochiweb_request_bridge, MochiwebRequestData)

Yaws example:

	Bridge = request_bridge:make(yaws_request_bridge, YawsRequestData)

<h4>What can I do with the request bridge?</h4>

The request bridge provides you with a standard interface for accessing the request method, 
path, query parameters, post parameters, headers, and cookies of the request:

* *Bridge:request_method()* - returns 'GET', 'POST', 'HEAD', etc.
* *Bridge:path()* - returns the requested path and file.
* *Bridge:query_string()* - returns the query string, anything after the "?" in the URL.
* *Bridge:peer_ip()* - returns the client's IP address in tuple format (74.125.67.100 = {74, 125, 67, 100})
* *Bridge:peer_port()* - returns the client's port.
* *Bridge:headers()* - returns a proplist of headers, [{"Header1", "Value1"}, {"Header2", "Value2"}, ...]
* *Bridge:cookies()* - returns a proplist of cookies, [{"Cookie1", "Value1"}, {"Cookie2", "Value2"}, ...]
* *Bridge:query_params()* - returns a proplist of query params, [{"Query1", "Value1"}, {"Query2", "Value2"}, ...]
* *Bridge:query_params()* - returns a proplist of post params, [{"Post1", "Value1"}, {"Post2", "Value2"}, ...]
* *Bridge:request_body()* - returns a proplist of post params, [{"Post1", "Value1"}, {"Post2", "Value2"}, ...]
* *Bridge:request_body()* - returns the request body as a list.

.

<h4>What modules are involved in a request bridge?</h4>

* *request_bridge.erl* - The behaviour interface that request bridge modules must implement.
* *request_bridge_wrapper.erl* - A parameterized module that wraps a request. 
* *inets_request_bridge.erl* - The request bridge module for Inets.
* *mochiweb_request_bridge.erl* - The request bridge module for Mochiweb.
* *yaws_request_bridge.erl* - The request bridge module for Yaws.

To extend the Simple Bridge to other HTTP servers (or other versions of Inets, Mochiweb, or Yaws), 
copy and modify inets_request_bridge.erl, mochiweb_request_bridge.erl, or yaws_request_bridge.erl.

<h2>Response Bridges</h2>
<h4>How do I make a response bridge?</h4>

To make a response bridge for a response to the client, call the response_bridge:make/1 function,
specifying the correct bridge module for your http server.

Inets example:

	Bridge = response_bridge:make(inets_response_bridge)

Mochiweb example:

	Bridge = response_bridge:make(mochiweb_response_bridge)

Yaws example:

	Bridge = response_bridge:make(yaws_response_bridge)

<h4>What can I do with the response bridge?</h4>

The response bridge provides you with a standard interface for combining headers, cookies,
and a response body into a response appropriate for your http server. 

Each function below returns a new bridge object, so your will need to 
chain together requests like this:

	Bridge = response_bridge:mak(inets_response_bridge),
	Bridge1 = Bridge:status_code(200),
	Bridge2 = Bridge1:header("Header1", "Value1"),
	etc.

* *Bridge:status_code(Code)* - set the HTTP status code. (200, 404, etc.)
* *Bridge:header(Name, Value)* - set an HTTP header.
* *Bridge:clear_headers()* - clear all previously set headers.
* *Bridge:cookie(Name, Value)* - set a cookie for path "/" with expiration in 20 minutes.
* *Bridge:cookie(Name, Value, Path, Exp)* - Set a cookie. Exp is an integer in minutes.
* *Bridge:clear_cookies()* - clear all previously set cookies.
* *Bridge:data(Data)* - set the data to return in the response. Usually HTML goes here.

Finally, you build the response to send to your HTTP server with the build_response/0 function.

* *Bridge:build_response()* - Create a response tuple that you can hand off to your HTTP server.

.

<h4>What modules are involved in a response bridge?</h4>

* *response_bridge.erl* - The behaviour interface that response bridge modules must implement.
* *response_bridge_wrapper.erl* - A parameterized module that wraps a response. 
* *inets_response_bridge.erl* - The response bridge module for Inets.
* *mochiweb_response_bridge.erl* - The response bridge module for Mochiweb.
* *yaws_response_bridge.erl* - The response bridge module for Yaws.

To extend the Simple Bridge to other HTTP servers (or other versions of Inets, Mochiweb, or Yaws), 
copy and modify inets_response_bridge.erl, mochiweb_response_bridge.erl, or yaws_response_bridge.erl.
