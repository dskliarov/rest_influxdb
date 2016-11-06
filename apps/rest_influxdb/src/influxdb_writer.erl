%%%-------------------------------------------------------------------
%%% @author dskliarov
%%% @copyright (C) 2016, dskliarov
%%% @doc
%%%
%%% @end
%%% Created : 2016-03-13 10:46:50.034037
%%%-------------------------------------------------------------------
-module(influxdb_writer).

-behaviour(gen_server).

%% API
-export([start_link/0,
         write_to_db/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {uri::string()}).

%%%===================================================================
%%% API
%%%===================================================================

write_to_db(Dps) ->
    gen_server:cast(?SERVER,{write_to_db,Dps}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
        gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    Db = application:get_env(rest_influxdb,erfluxdb,<<"measurments">>),
    User = application:get_env(rest_influxdb,username, <<"root">>),
    Password = application:get_env(rest_influxdb,password, <<"root">>),
    Host = application:get_env(rest_influxdb,host, <<"localhost">>),
    Port = application:get_env(rest_influxdb,port, <<"8086">>),
    Uri = <<<<"http://">>/binary,Host/binary,<<":">>/binary, Port/binary, <<"/write?db=">>/binary, Db/binary, <<"&u=">>/binary, User/binary, <<"&p=">>/binary, Password/binary>>,
    {ok, #state{uri = Uri}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
        Reply = ok,
        {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({write_to_db,Dps},#state{ uri = Uri } = State) ->
    spawn(fun() -> lists:foreach(fun(M) -> post_value(M,Uri) end, Dps) end),
    {noreply, State};
handle_cast(_Msg, State) ->
        {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
        {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
        ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

post_value(Metric,Uri) ->
    Name = proplists:get_value(<<"name">>, Metric),
    Value = term_to_binary(proplists:get_value(<<"value">>, Metric)),
    Body = <<Name/binary, <<" value=">>/binary, Value/binary>>,
    Rslt = hackney:post(Uri,[],Body,[{pool, default}]),
    lager:info("DB write result is ~p", [Rslt]).
