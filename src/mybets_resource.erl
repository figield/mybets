-module(mybets_resource).
-export([
    init/1,
    to_html/2
]).

-include_lib("webmachine/include/webmachine.hrl").

-spec init(list()) -> {ok, term()}.
init([]) ->
     %{ok, undefined}.
     {{trace, "/tmp/traces"}, []}.    %% debugging


-spec to_html(wrq:reqdata(), term()) -> {iodata(), wrq:reqdata(), term()}.
to_html(ReqData, State) ->
      {"<html><body>Hello new world</body></html>", ReqData, State}.
