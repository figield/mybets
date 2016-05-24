-module(mybets_bet).

-export([init/1, content_types_provided/2, content_types_accepted/2,
		allowed_methods/2, resource_exists/2, to_html/2, to_json/2,
		from_json/2, delete_resource/2, post_is_create/2, create_path/2,
		allow_missing_post/2]).

-include_lib("webmachine/include/webmachine.hrl").


-spec init(list()) -> {ok, term()}.
init(Config) ->
	mybets_schema:init_tables(),
	mybets_schema:fill_with_dummies(),
  	{{trace, "/tmp/traces"}, Config}.    %% debugging
    %{ok, undefined}.

content_types_provided(RD, Ctx) ->
	{[ {"text/html", to_html}, {"application/json", to_json} ], RD, Ctx}.

content_types_accepted(RD, Ctx) ->
	{[ {"application/json", from_json} ], RD, Ctx }.

allowed_methods(RD, Ctx) ->
	{['GET', 'POST', 'PUT', 'DELETE', 'HEAD'], RD, Ctx}.

resource_exists(RD, Ctx) ->
    %io:format("DF:resource_exists:~p",[RD]),
	Id = wrq:path_info(id, RD),
	{mybets_schema:bet_exists(Id), RD, Ctx}.

-spec to_html(wrq:reqdata(), term()) -> {iodata(), wrq:reqdata(), term()}.
to_html(RD, Ctx) ->
    %io:format("to_html:RD:~p~n",[RD]),
	Id = wrq:path_info(id, RD),
	Resp = "<html><body>" ++ Id ++ "</body></html>",
	{Resp, RD, Ctx}.

-spec to_json(wrq:reqdata(), term()) -> {iodata(), wrq:reqdata(), term()}.
to_json(RD, Ctx) ->
	Id = wrq:path_info(id, RD),
	{bet, Id1, Title} = mybets_schema:read_bet(Id),
	Resp = bet2json(Id1, Title),
	{Resp, RD, Ctx}.

-spec from_json(wrq:reqdata(), term()) -> {iodata(), wrq:reqdata(), term()}.
from_json(RD, Ctx) ->
    %io:format("from_json:RD:~p",[RD]),
    PQ = mochiweb_util:parse_qs(wrq:req_body(RD)),
    from_json(RD, Ctx, get_value(PQ, "title")).

-spec from_json(wrq:reqdata(), any(), {error, no_data})
			-> {{halt, 400}, wrq:reqdata(), any()};
		(wrq:reqdata(), any(), string())
			-> {boolean(), wrq:reqdata(), any()}.
from_json(RD, Ctx, {error, no_data}) ->
	signal_malformed_request(RD, Ctx);

from_json(RD, Ctx, Title) ->
	Id = id_from_path(RD),
    %% Enable Response Code 201 at decision node P11 in diagram
	Resp = set_location_header_if_not_exists(RD, Ctx, Id),
	mybets_schema:create_bet(list_to_integer(Id), Title),
	Resp1 = wrq:set_resp_body(bet2json(Id, Title), Resp),
	{true, Resp1, Ctx}.

delete_resource(RD, Ctx) ->
	Id = wrq:path_info(id, RD),
	mybets_schema:delete_bet(Id),
	{true, RD, Ctx}.


%%%%%%%%%
% POST
%%%%%%%%%

post_is_create(RD, Ctx) ->
	{true, RD, Ctx}.

allow_missing_post(RD, Ctx) ->
	{true, RD, Ctx}.

create_path(RD, Ctx) ->
	Path = "/bet/" ++ integer_to_list(generate_id()),
	{Path, RD, Ctx}.

%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private Helpers
%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec get_value(list(), list()) -> string() | {atom(), atom()}.
get_value(PQ, Key) ->
    %io:format("get_value:PQ:~p~n",[PQ]),
	case proplists:get_value(Key, PQ) of
	  undefined ->
      {error, no_data};
	  Val ->
		  Val
	end.

-spec id_from_path(wrq:reqdata()) -> string().
id_from_path(RD) ->
	case wrq:path_info(id, RD) of
		undefined->
			["bet", Id] = string:tokens(wrq:disp_path(RD), "/"),
			Id;
		Id -> Id
	end.


-spec set_location_header_if_not_exists(wrq:reqdata(), any(), string()) -> wrq:reqdata().
set_location_header_if_not_exists(RD, Ctx, Id) ->
	case resource_exists(RD, Ctx) of
		{false, _, _} ->
			wrq:set_resp_header("Location", Id, RD);
		{true, _, _}  -> RD
	end.

-spec signal_malformed_request(wrq:reqdata(), any())-> {{halt, 400}, wrq:reqdata(), any()}.
signal_malformed_request(RD, Ctx) ->
	{{halt, 400}, RD, Ctx}.

-spec bet2json(integer(), string()) -> string();
				(string(), string()) -> string().
bet2json(Id, Title) when is_integer(Id) ->
	bet2json(integer_to_list(Id), Title);
bet2json(Id, Title) ->
	mochijson:encode({struct, [
					{id, Id},
					{title, Title} ]}).

-spec generate_id()->integer().
generate_id() ->
	mnesia:table_info(bet, size) + 1.
