-module(zoho_auth_server).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([token/0]).

%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {access_token = undefined :: binary() | undefined}).

-type state() :: #state{}.
-type token_resp() :: #{access_token := binary(), expires_in := non_neg_integer()}.
-type fetch_error() :: dj:error() | {http_error, pos_integer()}.

%%==============================================================================================
%% API
%%==============================================================================================

-spec token() -> binary().
token() -> gen_server:call(?MODULE, get_token).

%%==============================================================================================
%% gen_server callbacks
%%==============================================================================================

-spec start_link() -> {ok, pid()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init(Args :: term()) -> {ok, state()}.
init(_) -> {ok, #state{}}.

-spec handle_call(Call, From, State) -> Resp when
    Call :: term(),
    From :: {pid(), Tag :: term()},
    Resp :: {reply, term(), State} | {noreply, State},
    State :: state().
handle_call(get_token, _, State = #state{access_token = undefined}) ->
  ?LOG_DEBUG(#{msg => <<"Refreshing access token">>}),
  {ok, #{access_token := AccessToken, expires_in := Expiry}} = do_refresh(),
  erlang:send_after(Expiry, self(), expire),
  {reply, AccessToken, State#state{access_token = AccessToken}};
handle_call(get_token, _, State = #state{access_token = AccessToken}) ->
  {reply, AccessToken, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_, State) -> {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(expire, State) ->
  ?LOG_DEBUG(#{msg => <<"Expiring access token">>}),
  {noreply, State#state{access_token = undefined}}.

%%==============================================================================================
%% Internal functions
%%==============================================================================================

-spec do_refresh() -> {ok, token_resp()} | {error, fetch_error()}.
do_refresh() ->
  Values = [ {grant_type, refresh_token}
           , {client_id, config_value(client_id)}
           , {client_secret, config_value(client_secret)}
           , {refresh_token, config_value(refresh_token)}
           ],
  case hackney:post(config_value(endpoint), [], {form, Values}, [with_body]) of
    {ok, 200, _, Body} -> dj:decode(Body, token_decoder());
    {ok, StatusCode, _, _} -> {error, {http_error, StatusCode}};
    {error, _} = Res -> Res
  end.

-spec token_decoder() -> dj:decoder(token_resp()).
token_decoder() ->
  dj:to_map(#{ access_token => dj:field(access_token, dj:binary())
             , expires_in => dj:field(expires_in, dj:non_neg_integer())
             }).

-spec config_value(atom()) -> term().
config_value(Key) ->
  {ok, Value} = application:get_env(zoho_auth, Key),
  Value.

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 96
%% coding: utf-8
%% End:
