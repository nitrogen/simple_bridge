# 2.0.0

* Simple Bridge is no longer dependent on the parameter module system, however,
  all relevant modules have retained the necessary structure to treat them as
  parameter modules. The standard interface is now through a `sbw` module.
* All backends now provide a default supervisor for simplified starting and
  building backend-specific configurations.
* All backends now provide an "anchor" module, which serves as an intermediary
  between the server and the "handler" module.
* Merged Request and Response bridge into a single "sbw" (simple bridge
  wrapper) object.
* Added `simple_bridge:make(Backend)` function to replace having to make both
  request and response bridges.
* Added handler module behaviour (`-behaviour(simple_bridge_handler)`) for each
  platform, customizable with `simple_bridge:start(Backend, HandlerMod)`. This
  relies on the use an "Anchor" module, a default provided for each module.
* Added a sample handler module which can be used as a basis for future
  functionality. (`simple_bridge_handler_sample.erl`)
* Added Websocket Support for Yaws, Cowboy using their native implementations.
* Added a Websocket Hijacker to support websockets on non-websocket backends,
  particularly Mochiweb, Inets, and Webmachine.
* Header values will be returned as binaries if the Header name provided is a
  binary. If header is provided as an atom or a list, it will return a list.
* Provided a quickstart demo to try out all backends with request, response,
  and websockets. `make run_BACKEND`, where BACKEND is `cowboy`, `inets`,
  `mochiweb`, `webmachine`, or `yaws`
* Removed support for Misultin.

# 1.4

* Add ability to store temp files in memory if they are small enough,
  otherwise.

# 1.3 and earlier

* Supports Cowboy, Inets, Mochiweb, Webmachine, Yaws, and Misultin

