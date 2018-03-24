%%%-------------------------------------------------------------------
%%% @author ccredrock@gmail.com
%%% @copyright 2018 redrock
%%% @doc token bucket
%%% @end
%%%-------------------------------------------------------------------
-module(token_bucket_app).

-export([start/2, stop/1]).

%%------------------------------------------------------------------------------
-behaviour(application).

%%------------------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    token_bucket_sup:start_link().

stop(_State) ->
    ok.

