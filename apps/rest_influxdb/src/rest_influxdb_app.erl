-module(rest_influxdb_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, setup_api/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    application:ensure_started(ranch),
    application:ensure_started(crypto),
    application:ensure_started(cowlib),
    application:ensure_started(cowboy),
    setup_api(),
    start_erflux(),
    rest_influxdb_sup:start_link().

start_erflux() ->
    application:ensure_started(crypto),
    application:ensure_started(certifi),
    application:ensure_started(idna),
    application:ensure_started(mimerl),
    application:ensure_started(ssl),
    application:ensure_started(asn1),
    application:ensure_started(public_key),
    application:ensure_started(ssl),
    application:ensure_started(idna),
    application:ensure_started(hackney),
    application:ensure_started(jsx),
    application:ensure_started(erflux),
    erflux_sup:add_erflux(erflux_http),
    erflux_http:get_databases().

setup_api() ->
    Routes = [
              {"/api/influxdb", rest_handler, []}
             ],
    Dispatch = cowboy_router:compile([{'_', Routes}]),
    ApiPort = application:get_env(qks, api_port, 8080),
    {ok, _} = cowboy:start_http(http, 100, [{port, ApiPort}], [{env, [{dispatch, Dispatch}]}]).

stop(_State) ->
    ok.
