%%%-------------------------------------------------------------------
%%% @author ccredrock@gmail.com
%%% @copyright 2018 redrock
%%% @doc token bucket
%%% @end
%%%-------------------------------------------------------------------
-ifndef(TOKEN_BUCKET_HRL).
-define(TOKEN_BUCKET_HRL, true).

-record(bucket,
        {name::atom(),
         props = #{},
         tokens = 0}).

-endif.
