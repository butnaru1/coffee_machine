%%%-------------------------------------------------------------------
%%% @author olegb
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. mar. 2021 09:26
%%%-------------------------------------------------------------------
-module(user_api).
-author("olegb").

%% API
-export([espresso/0, cappuccino/0, water/0, add_sugar/1, supply/0, boom/0]).


espresso()->coffee_machine_server:espresso().
cappuccino()->coffee_machine_server:cappuccino().
water()->coffee_machine_server:water().
add_sugar(Add)->coffee_machine_server:add_sugar(Add).
supply()->coffee_machine_server:supply().

boom()->coffee_machine_server:boom().