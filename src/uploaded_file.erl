%% vim: ts=4 sw=4 et
% Simple Bridge
% Copyright (c) 2013 Jesse Gumm
% See MIT-LICENSE for licensing information.
%
% uploaded_file provides an API to the uploaded_file record without having to
% include simple_bridge.hrl file into user applications.
% 
% This can be called either in standard Erlang:
%    uploaded_file:original_name(File).
%
% Or in parameter module (assuming future version of Erlang still support it):
%    File:original_name().
%
-module(uploaded_file).
-include("simple_bridge.hrl").
-compile({no_auto_import,[size/1]}).

-export([
    original_name/1,
    temp_file/1,
    size/1,
    field_name/1]).

original_name(#uploaded_file{original_name=N}) -> N.

temp_file(#uploaded_file{temp_file=N}) -> N.

size(#uploaded_file{size=S}) -> S.

field_name(#uploaded_file{field_name=N}) -> N.
