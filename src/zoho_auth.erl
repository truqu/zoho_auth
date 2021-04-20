-module(zoho_auth).

%% API
-export([token/0]).

%%==============================================================================================
%% API
%%==============================================================================================

-spec token() -> binary().
token() -> zoho_auth_server:token().

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 96
%% coding: utf-8
%% End:
