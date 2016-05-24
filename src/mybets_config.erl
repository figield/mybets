-module(mybets_config).

-export([
    dispatch/0,
    web_config/0
]).

-spec dispatch() -> [webmachine_dispatcher:route()].
dispatch() ->
  {ok, Dispatch} = file:consult(filename:join(
                         [filename:dirname(code:which(?MODULE)),
                          "..", "priv", "dispatch.conf"])),
   io:format("Dispatch:~n~p~n",[Dispatch]),
   Dispatch.

web_config() ->
    {ok, App} = application:get_application(?MODULE),
    {ok, Ip} = application:get_env(App, web_ip),
    {ok, Port} = application:get_env(App, web_port),
    [
        {ip, Ip},
        {port, Port},
        {log_dir, "priv/log"},
        {dispatch, dispatch()}
    ].
