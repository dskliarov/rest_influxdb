%%%-------------------------------------------------------------------
%%% @author dskliarov
%%% @copyright (C) 2016, dskliarov
%%% @doc %%%
%%% @end
%%% Created : 2016-03-03 17:53:04.497933
%%%-------------------------------------------------------------------
-module(rest_handler).
-author("dskliarov").

-define(ERROR_TYPES, [{ok, 200},
                      {created, 201},
                      {accepted, 202},
                      {bad_request, 400},
                      {unauthorized, 401},
                      {forbidden, 403},
                      {not_found, 404},
                      {method_not_allowed, 405},
                      {request_timeout, 408},
                      {conflict, 409},
                      {unprocessable_entity,422},
                      {internal_server_error, 500},
                      {service_unavailable, 503},
                      {gateway_timeout, 504}]).

%API
-export([init/3,
        rest_init/2,
        allowed_methods/2,
        content_types_accepted/2,
        post_json/2]).

-define(KEY_SIZE, 6).

-record(state, {routing_options}).

init({_TransportName, _ProtocolName}, _Request, _Options) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Request, Options) ->
    {ok, Request, #state{routing_options = Options}}.

allowed_methods(Request, State) ->
    {[<<"POST">>], Request, State}.

content_types_accepted(Request, State) ->
    {[{<<"application/json">>, post_json}], Request, State}.

post_json(Req, State) ->
    RespMessage = case encoded_body(Req) of
                      {ok, EncodedBody} ->
                          MessageToProcess = get_values(EncodedBody),
                          process_if_valid(MessageToProcess);
                      {invalid, _Message} = M ->
                          M
                  end,
    {ok,Resp} = update_response(RespMessage,Req),
    {halt, Resp, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

process_if_valid([H|_T]=Message) when is_list(H) ->
    case valid([<<"name">>, <<"value">>], H) of
        valid ->
            process_message(Message);
        _ ->
            {bad_request, "Invalid request"}
    end.

process_message(Message) ->
    influxdb_writer:write_to_db(Message),
            {accepted, "Message accepted"}.

update_response({error,not_found}, Request) ->
    update_response({not_found,jsx:encode(<<"Record not found">>)},Request);
update_response({error,Resp}, Request) ->
    update_response(Resp,Request);
update_response({invalid,Message}, Request) ->
    MessageBinary = list_to_binary(Message),
    MessageEncoded = jsx:encode(MessageBinary),
    update_response({bad_request, MessageEncoded}, Request);
update_response({Resp, Message}, Request) ->
    R = proplists:get_value(Resp, ?ERROR_TYPES, undefined),
    case R of
        undefined ->
            cowboy_req:reply(500, Request);
        _ ->
            BinaryMessage = to_binary(Message),
            EncodedMessage = jsx:encode(BinaryMessage),
            Request1 = cowboy_req:set_resp_body(EncodedMessage,Request),
            cowboy_req:reply(R, Request1)
    end.

to_binary(M) when is_binary(M) ->
    M;
to_binary(M) when is_list(M) ->
    list_to_binary(M);
to_binary(M) ->
    term_to_binary(M).

normalize_body({value, Body}) ->
    Body;
normalize_body(Body) ->
    Body.

encoded_body(Req) ->
    {ok,Body,_Req1} = cowboy_req:body(Req),
    Body1 = normalize_body(Body),
    case jsx:is_json(Body1) of
        true ->
            {ok,jsx:decode(Body1)};
        _ ->
            {invalid, "invalid json"}
        end.

valid([], _PropList) ->
    valid;
valid([Key|Keys], PropList) ->
    case proplists:get_value(Key, PropList) of
        undefined ->
            invalid;
        _ ->
            valid(Keys, PropList)
        end;
valid(Key, PropList) -> 
    valid([Key], PropList).

get_values(EncodedMessage) ->
    case proplists:get_value(<<"values">>, EncodedMessage) of
        undefined ->
            EncodedMessage;
        ValuesMessage ->
            ValuesMessage
    end.
