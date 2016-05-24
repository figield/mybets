-module(mybets_schema).

-export([
	create_bet/2,
	read_bet/1,
	delete_bet/1,
	bet_exists/1,
	init_tables/0,
	clear_tables/0,
	fill_with_dummies/0,
	delete_dummies/0
]).

-compile([export_all]).

-include("include/mybets_datatypes.hrl").


%%%%%%%%%%%%%%%%%%%%%
%% Storage API
%%%%%%%%%%%%%%%%%%%%

create_bet(ID, Title) ->
	io:format("~p bet created: ~p, ~p~n", [?LINE, ID, Title]),
	Write = fun() -> mnesia:write({bet, ID, Title}) end,
	case mnesia:transaction(Write) of
		{atomic, ok} -> ok
	end.


read_bet(Id) when is_list(Id)->
	read_bet(list_to_integer(Id));

read_bet(ID) ->
	Read = fun() -> mnesia:read(bet, ID) end,
	case mnesia:transaction(Read) of
		{atomic, [Bet]} -> Bet;
		{atomic, []} -> {error, not_exists}
	end.


update_bet(Id, Title) ->
	Read = fun() -> mnesia:write({bet, Id, Title}) end,
	case mnesia:transaction(Read) of
		{atomic, ok} -> ok
	end.


delete_bet(Id) when is_list(Id)->
	delete_bet(list_to_integer(Id));

delete_bet(ID) ->
	Delete = fun() -> mnesia:delete({bet, ID}) end,
	case mnesia:transaction(Delete) of
		{atomic, ok} ->
			ok
	end.


bet_exists(Id) when is_list(Id)->
	bet_exists(list_to_integer(Id));

bet_exists(Id) ->
	Read = fun() -> mnesia:read(bet, Id) end,
	case mnesia:transaction(Read) of
		{atomic, []} ->
			io:format("~p, Id: ~p Exists: ~p ~n", [?LINE, Id, false]),
			false;
		{atomic,[{bet, _, _}]} ->
			io:format("~p, Id: ~p Exists: ~p ~n", [?LINE, Id, true]),
			true
	end.


%%%%%%%%%%%%%%%%%%%%%
%% Schema
%%%%%%%%%%%%%%%%%%%%

init_tables() ->
	mnesia:create_table(bet, [{attributes, record_info(fields, bet)}]).

clear_tables() ->
	mnesia:clear_table(bet).

delete_tables() ->
	mnesia:delete_table(bet).


%%%%%%%%%%%%%%%%%%%%%
%% Fixtures
%%%%%%%%%%%%%%%%%%%%


fill_with_dummies() ->
	Fill = mnesia:transaction(fun() ->
		mnesia:write({bet, 1, "1"}),
		mnesia:write({bet, 2, "2"}),
		mnesia:write({bet, 3, "3"}) end),

	case Fill of
		{atomic, ok} -> ok
	end.


delete_dummies() ->
	mnesia:transaction(fun() ->
				[ mnesia:delete({bet, ID}) || ID <- lists:seq(1, 3) ] end).
