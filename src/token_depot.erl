%%%-------------------------------------------------------------------
%%% @author ccredrock@gmail.com
%%% @copyright 2018 redrock
%%% @doc token bucket
%%% @end
%%%-------------------------------------------------------------------
-module(token_depot).

-export([update_buckets/2]).

%% callbacks
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%------------------------------------------------------------------------------
%% define
%%------------------------------------------------------------------------------
-include("token_bucket.hrl").

-record(state, {name::atom(), buckets = []}).

%%------------------------------------------------------------------------------
%% interface
%%------------------------------------------------------------------------------
-spec update_buckets(PoolName::atom(), Buckets::list()) -> ok | {error, any()}.
update_buckets(PoolName, Buckets) ->
    case token_bucket_sup:ensure_depot(PoolName) of
        {ok, PID} ->
            case catch gen_server:call(PID, {update_buckets, Buckets}) of
                ok -> ok;
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%------------------------------------------------------------------------------
-spec start_link(Depot::atom()) -> {ok, pid()} | {error, any()}.
start_link(Depot) ->
    gen_server:start_link(?MODULE, [Depot], []).

%% @hidden
init([Depot]) ->
    process_flag(trap_exit, true),
    ets:new(Depot, [named_table, public,
                    {keypos, #bucket.name},
                    {read_concurrency, true},
                    {write_concurrency, true}]),
    error_logger:info_msg("start token depot ~p", [{Depot}]),
    {ok, #state{name = Depot}, 0}.

%% @hidden
handle_call({update_buckets, Buckets}, _From, State) ->
    {reply, ok, handle_update_buckets(Buckets, State)};
handle_call(_Call, _From, State) ->
    {reply, ok, State}.

%% @hidden
handle_cast(_Request, State) ->
    {noreply, State}.

%% @hidden
handle_info(_Info, State) ->
    {noreply, State}.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @hidden
terminate(Reason, State) ->
    Reason =/= normal andalso
    Reason =/= shutdown andalso
    error_logger:error_msg("token_depot dead ~p~n", [{Reason, State}]).

%%------------------------------------------------------------------------------
%% @private
handle_update_buckets(Buckets, #state{name = Name, buckets = OldBuckets} = State) ->
    [ets:delete(Name, N)
     || {N, _P} <- OldBuckets, lists:keyfind(N, 1, Buckets) =:= false],
    [case lists:keyfind(N, 1, OldBuckets) of
         false ->
             ets:insert(Name, #bucket{name = N, props = P, tokens = 0});
         _ ->
             ets:update_element(Name, N, [{#bucket.props, P}])
     end || {N, P} <- Buckets],
    State#state{buckets = Buckets}.

