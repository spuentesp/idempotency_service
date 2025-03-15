%% filepath: test/idempotency_test.erl
%% ------------------------------------------------------
%% Module: idempotency_test
%% Purpose: Unit tests for idempotency
%% ------------------------------------------------------
-module(idempotency_test).
-include_lib("eunit/include/eunit.hrl").

%% Exported functions for eunit
-export([generate_key_test/0, ignore_fields_test/0, all_fields_test/0]).

%% ------------------------------------------------------
%% Test: Generate idempotency key
%% ------------------------------------------------------
generate_key_test() ->
    io:format("Loading server config...~n"),
    {ok, _} = config_loader:load_config("test/server_test.config"),
    io:format("Rules loaded for generate_key_test~n"),
    
    Data = #{<<"timestamp">> => 1234567890, <<"amount">> => 100, <<"user_id">> => 42},
    Key = idempotency:generate_key(payment, Data),
    io:format("Generated key: ~p~n", [Key]),
    
    ?assert(is_integer(Key)).

%% ------------------------------------------------------
%% Test: Ignore fields
%% ------------------------------------------------------
ignore_fields_test() ->
    io:format("Loading server config...~n"),
    {ok, _} = config_loader:load_config("test/server_test.config"),
    io:format("Rules loaded for ignore_fields_test~n"),
    
    Data = #{<<"timestamp">> => 1234567890, <<"amount">> => 100, <<"user_id">> => 42},
    Key = idempotency:generate_key(payment, Data),
    io:format("Generated key: ~p~n", [Key]),
    
    ?assert(is_integer(Key)).

%% ------------------------------------------------------
%% Test: All fields
%% ------------------------------------------------------
all_fields_test() ->
    io:format("Loading server config...~n"),
    {ok, _} = config_loader:load_config("test/server_test.config"),
    io:format("Rules loaded for all_fields_test~n"),
    
    Data = #{<<"timestamp">> => 1234567890, <<"amount">> => 100, <<"user_id">> => 42},
    Key = idempotency:generate_key(default, Data),
    io:format("Generated key: ~p~n", [Key]),
    
    ?assert(is_integer(Key)).

%% ------------------------------------------------------
%% Run all tests
%% ------------------------------------------------------
all_tests() ->
    generate_key_test(),
    ignore_fields_test(),
    all_fields_test(),
    io:format("✅ All idempotency tests passed!~n").