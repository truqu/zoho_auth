-module(zoho_auth_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, init/1]).

%%==============================================================================================
%% API
%%==============================================================================================

-spec start_link() -> {ok, pid()}.
start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
  SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
  ChildSpec = [#{ id => zoho_auth_server
                , type => worker
                , start => {zoho_auth_server, start_link, []}
                , restart => permanent
                , shutdown => 5000
                , modules => [zoho_auth_server]
                }],
  {ok, {SupFlags, ChildSpec}}.

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 96
%% coding: utf-8
%% End:
