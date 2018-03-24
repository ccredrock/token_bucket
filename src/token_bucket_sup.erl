%%%-------------------------------------------------------------------
%%% @author ccredrock@gmail.com
%%% @copyright 2018 redrock
%%% @doc token bucket
%%% @end
%%%-------------------------------------------------------------------
-module(token_bucket_sup).

-export([start_link/0,
         ensure_depot/1,
         init/1]).

%%------------------------------------------------------------------------------
-behaviour(supervisor).

%%------------------------------------------------------------------------------
-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec ensure_depot(DepotName::atom()) -> {ok, pid()} | {error, any()}.
ensure_depot(DepotName) ->
    case which_depot(DepotName) of
        undefined -> create_depot(DepotName);
        PID -> {ok, PID}
    end.

create_depot(DepotName) ->
    supervisor:start_child(?MODULE, {DepotName,
                                     {token_depot, start_link, [DepotName]},
                                     transient, infinity, worker,
                                     []}).

which_depot(DepotName) ->
    List = [{ID, PID} || {ID, PID, _, _} <- supervisor:which_children(?MODULE)],
    case maps:find(DepotName, maps:from_list(List)) of
        error -> undefined;
        {ok, PID} -> PID
    end.

-spec init([]) -> supervisor:init().
init([]) ->
    {ok, {{one_for_one, 1, 60}, []}}.

