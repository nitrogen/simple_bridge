# 2.0

* Added `simple_bridge:make(Backend)` function to replace having to make both
  request and response bridges.
* Added callout module behaviour (`-behaviour(simple_bridge_callout)`) for each
  platform, customizable with `simple_bridge:start(CalloutMod, Backend)`. This
  relies on the use an "Anchor" module, a default provided for each module.
* Added a sample callout module which can be used as a basis for future
  functionality. (`simple_bridge_callout_sample.erl`)
* Added Websocket Support for Yaws, Cowboy using their native implementations.
* All headers will be returned as binaries, header names will be normalized to
  be capitalized and every letter after a dash also capitalized.
* Removed support for Misultin.

# 1.3

* Supports Cowboy, Inets, Mochiweb, Webmachine, Yaws, and Misultin

