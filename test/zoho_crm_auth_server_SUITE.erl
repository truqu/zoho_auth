-module(zoho_crm_auth_server_SUITE).

%% CT Callbacks
-export([all/0, init_per_suite/1, init_per_testcase/2, end_per_testcase/2, end_per_suite/1]).

%% Tests
-export([fetches_token_when_requested/1, caches_token_for_ttl/1, refetches_token_after_ttl/1]).

-spec all() -> [atom()].
all() -> [fetches_token_when_requested, caches_token_for_ttl, refetches_token_after_ttl].

%%==============================================================================================
%% Tests
%%==============================================================================================

-define( SAME(MARG1, MARG2)
       , case MARG1 == MARG2 of
           true -> ok;
           false -> erlang:throw({not_same, {expected, MARG1}, {actual, MARG2}})
         end
       ).

-spec fetches_token_when_requested(any()) -> any().
fetches_token_when_requested(_) ->
  meck:new(hackney),
  Response = jsx:encode(#{access_token => <<"123">>, expires_in => 123}),
  meck:expect(hackney, post, 4, {ok, 200, headers, Response}),
  <<"123">> = zoho_crm_auth_server:token(),
  ExpectedUrl = <<"endpoint">>,
  Url = meck:capture(1, hackney, post, 4, 1),
  ?SAME(ExpectedUrl, Url),
  {form, Values} = meck:capture(1, hackney, post, 4, 3),
  ?SAME(refresh_token, proplists:get_value(grant_type, Values)),
  ?SAME(<<"clientid">>, proplists:get_value(client_id, Values)),
  ?SAME(<<"clientsecret">>, proplists:get_value(client_secret, Values)),
  ?SAME(<<"refreshtoken">>, proplists:get_value(refresh_token, Values)),
  meck:unload(hackney).

-spec caches_token_for_ttl(any()) -> any().
caches_token_for_ttl(_) ->
  meck:new(hackney),
  Response = jsx:encode(#{access_token => <<"456">>, expires_in => 5000}),
  meck:expect(hackney, post, 4, {ok, 200, headers, Response}),
  <<"456">> = zoho_crm_auth_server:token(),
  <<"456">> = zoho_crm_auth_server:token(),
  <<"456">> = zoho_crm_auth_server:token(),
  1 = meck:num_calls(hackney, post, 4),
  meck:unload(hackney).

-spec refetches_token_after_ttl(any()) -> any().
refetches_token_after_ttl(_) ->
  meck:new(hackney),
  Response = jsx:encode(#{access_token => <<"456">>, expires_in => 200}),
  meck:expect(hackney, post, 4, {ok, 200, headers, Response}),
  <<"456">> = zoho_crm_auth_server:token(),
  ct:sleep(400),
  <<"456">> = zoho_crm_auth_server:token(),
  2 = meck:num_calls(hackney, post, 4),
  meck:unload(hackney).

%%==============================================================================================
%% CT Callbacks
%%==============================================================================================

-spec init_per_suite(any()) -> any().
init_per_suite(Config) ->
  application:ensure_all_started(zoho_crm_auth),
  application:set_env(zoho_crm_auth, endpoint, <<"endpoint">>),
  application:set_env(zoho_crm_auth, client_id, <<"clientid">>),
  application:set_env(zoho_crm_auth, client_secret, <<"clientsecret">>),
  application:set_env(zoho_crm_auth, refresh_token, <<"refreshtoken">>),
  Config.

-spec init_per_testcase(atom(), any()) -> any().
init_per_testcase(_, Config) ->
  supervisor:restart_child(zoho_crm_auth_sup, zoho_crm_auth_server),
  Config.

-spec end_per_testcase(atom(), any()) -> any().
end_per_testcase(_, Config) ->
  supervisor:terminate_child(zoho_crm_auth_sup, zoho_crm_auth_server),
  Config.

-spec end_per_suite(any()) -> ok.
end_per_suite(_) -> ok.

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 96
%% coding: utf-8
%% End:
