-module(mybets_schema_tests).

-include_lib("eunit/include/eunit.hrl").


create_bet_test() ->
	?assertEqual(ok, mybets_schema:create_bet(999, "ABC")),
	?assertEqual([{bet, 999, "ABC"}], mnesia:dirty_read(bet, 999)),
	mnesia:dirty_delete(bet, 999),
	?assertEqual([], mnesia:dirty_read(bet, 999)).


read_bet_test() ->
	mnesia:dirty_write({bet, 123, "ABC"}),
	?assertEqual({bet, 123, "ABC"}, mybets_schema:read_bet(123)),
	mnesia:dirty_delete(bet, 123),
	?assertEqual({error, not_exists}, mybets_schema:read_bet(123)).


update_bet_test() ->
	?assertEqual({bet, 1, "1"}, mybets_schema:read_bet(1)),
	?assertEqual(ok, mybets_schema:update_bet(1, "DEF")),
	?assertEqual({bet, 1, "DEF"}, mybets_schema:read_bet(1)),
	mnesia:dirty_write({bet, 1, 1}).


delete_bet_test() ->
	?assertEqual(ok, mybets_schema:create_bet(99, "ABC")),
	?assertEqual(ok, mybets_schema:delete_bet(99)).


exists_bet_test() ->
	?assertEqual(true, mybets_schema:bet_exists(1)),
	?assertEqual(true, mybets_schema:bet_exists("1")),
	?assertEqual(false, mybets_schema:bet_exists(999)).


permanent_save_test() ->
	?assertEqual(ok, mnesia:dirty_write({bet, 123, "abc"})),
	?assertEqual([{bet,123,"abc"}], mnesia:dirty_read(bet, 123)),
	mnesia:dirty_delete(bet, 999).


fill_with_dummies_test() ->
	?assertEqual(ok, mybets_schema:fill_with_dummies()),
	?assertEqual([{bet, 2, "2"}], mnesia:dirty_read(bet, 2)).


delete_dummies_test() ->
	mybets_schema:fill_with_dummies(),
	?assertEqual([{bet, 1, "1"}] , mnesia:dirty_read(bet, 1)),
	mybets_schema:delete_dummies(),
	?assertEqual([], mnesia:dirty_read(bet, 1)),
	mybets_schema:fill_with_dummies().
