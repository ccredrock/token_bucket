%%%-------------------------------------------------------------------
%%% @author ccredrock@gmail.com
%%% @copyright 2018 redrock
%%% @doc token bucket
%%% @end
%%%-------------------------------------------------------------------
-module(token_bucket).

-export([start/0,
         add_buckets/2,
         consume_token/2,
         list_buckets/1]).

%% callbacks
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%------------------------------------------------------------------------------
%% define
%%------------------------------------------------------------------------------
-include("token_bucket.hrl").

-define(MS2US(X), ((X) * 1000)).
-define(US2MS(X), ((X) div 1000)).

-define(INVALID_INT_MIN, 10000 * 10000).
-define(INVALID_INT_MAX, 20000 * 10000).

-define(FORM_POOL(X), list_to_atom("tb_" ++ atom_to_list(X))).

-record(state, {depot::atom(),
                bucket = <<>>,
                props = #{},
                acc_tokens = 0,
                init_time  = 0,
                prev_time  = 0,
                now_offset = 0}).

%%------------------------------------------------------------------------------
%% interface
%%------------------------------------------------------------------------------
-spec start() -> {ok, [atom()]} | {error, any()}.
start() ->
    application:ensure_all_started(?MODULE).

-spec add_buckets(DepotName::atom(),
                  [{BucketName::term(), Props::[tuple()]}]) -> ok | {error, any()}.
add_buckets(DepotName, Buckets) ->
    case token_depot:update_buckets(DepotName, Buckets) of
        ok ->
            case worker_pool:add_pool(?FORM_POOL(DepotName),
                                      [{DepotName, X} || {X, _} <- Buckets],
                                      {?MODULE, start_link}) of
                ok -> ok;
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

consume_token(DepotName, Bucket) ->
    case catch ets:update_counter(DepotName, Bucket, {#bucket.tokens, -1}) of
        Val when Val >= 0 andalso Val < ?INVALID_INT_MIN -> true;
        _ -> false
    end.

list_buckets(DepotName) ->
    [{Bucket, Tokens} || #bucket{name = Bucket, tokens = Tokens} <- ets:tab2list(DepotName)].

%%------------------------------------------------------------------------------
%% gen_server
%%------------------------------------------------------------------------------
-spec start_link({DepotName::atom(), BucketName::term()}) -> {ok, pid()} | {error, any()}.
start_link({DepotName, BucketName}) ->
    gen_server:start_link(?MODULE, [DepotName, BucketName], []).

%% @hidden
init([DepotName, BucketName]) ->
    process_flag(trap_exit, true),
    error_logger:info_msg("start token bucket ~p", [{BucketName}]),
    {ok, reset_bucket(erlang:system_time(micro_seconds),
                      #state{depot = DepotName, bucket = BucketName})}.

%% @hidden
handle_call(_Call, _From, State) ->
    {reply, ok, State}.

%% @hidden
handle_cast(_Request, State) ->
    {noreply, State}.

%% @hidden
handle_info(timeout, State) ->
    {noreply, handle_timeout(State)};
handle_info(_Info, State) ->
    {noreply, State}.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @hidden
terminate(_Reason, _State) -> ok.

%%------------------------------------------------------------------------------
%% @private
handle_timeout(#state{props = #{time := Time}, init_time = InitTime} = State) ->
    Now = erlang:system_time(micro_seconds),
    State1 = add_tokens(Now, State),
    case Now - InitTime - Time >= -1 of
        true -> reset_bucket(Now, State1);
        false -> cal_offset(Now, State1)
    end.

add_tokens(Now, #state{depot  = Depot,
                       bucket = Bucket,
                       props  = #{time := Time, count := Count},
                       prev_time = PrevTime,
                       acc_tokens = AccTokens} = State) ->
    Tokens = min((Now - PrevTime) * Count / Time + AccTokens, Count),
    IntTokens = trunc(Tokens),
    FloatTokens = Tokens - IntTokens,
    Val = ets:update_counter(Depot, Bucket, {#bucket.tokens, ?INVALID_INT_MAX}),
    Left = max(Val - ?INVALID_INT_MAX, 0),
    ets:update_element(Depot, Bucket, {#bucket.tokens, min(Left + IntTokens, Count)}),
    State#state{acc_tokens = FloatTokens}.

cal_offset(Now, #state{props = #{gap := Gap},
                       prev_time = PrevTime,
                       now_offset = NowOffset} = State) ->
    NowOffset1 = NowOffset + PrevTime + Gap - Now,
    erlang:send_after(?US2MS(max(Gap + NowOffset1, 0)), self(), timeout),
    State#state{prev_time  = Now, now_offset = NowOffset1}.

reset_bucket(Now, #state{depot  = Depot, bucket = Bucket} = State) ->
    [#bucket{props = Props}] = ets:lookup(Depot, Bucket),
    erlang:send_after(proplists:get_value(gap, Props), self(), timeout),
    State#state{props = #{time  => ?MS2US(proplists:get_value(time, Props)),
                          count => proplists:get_value(count, Props),
                          gap   => ?MS2US(proplists:get_value(gap, Props))},
                acc_tokens = 0,
                init_time  = Now,
                prev_time  = Now,
                now_offset = 0}.

