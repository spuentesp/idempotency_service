%% ------------------------------------------------------
%% Module: idempotency_test
%% Purpose: Unit tests for the idempotency module.
%% ------------------------------------------------------
-module(idempotency_test).
-include_lib("eunit/include/eunit.hrl").
-export([run/0]).

%% ------------------------------------------------------
%% Create a temporary test file for config loading
%% ------------------------------------------------------
create_test_file() ->
    FilePath = "test/idempotency_rules_test.json",
    Content = <<"
    {
        \"rules\": [
          {
            \"name\": \"default\",
            \"ignore_fields\": [\"timestamp\"],
            \"key_fields\": \"all\"
          },
          {
            \"name\": \"payment\",
            \"ignore_fields\": [\"timestamp\", \"request_id\"],
            \"key_fields\": [\"user_id\", \"amount\"]
          },
          {
            \"name\": \"user_event\",
            \"ignore_fields\": [\"timestamp\", \"event_id\"],
            \"key_fields\": [\"user_id\", \"event_type\"]
          }
        ]
    }">>,

    ok = file:write_file(FilePath, Content),
    FilePath.

%% ------------------------------------------------------
%% Test: Generate idempotency key for a rule
%% ------------------------------------------------------
generate_key_test() ->
    FilePath = create_test_file(),
    config_loader:load_rules(FilePath),

    %% Input data
    Data = #{
        <<"user_id">> => 42,
        <<"amount">> => 100,
        <<"timestamp">> => 1700,
        <<"request_id">> => <<"ABC123">>
    },

    %% Expected: "timestamp" and "request_id" should be ignored
    ExpectedHash = idempotency:generate_key(<<"payment">>, Data),

    %% Ensure hash is a valid integer
    ?assert(is_integer(ExpectedHash)).

%% ------------------------------------------------------
%% Test: Ensure ignored fields are not included
%% ------------------------------------------------------
ignore_fields_test() ->
    FilePath = create_test_file(),
    config_loader:load_rules(FilePath),

    Data = #{
        <<"user_id">> => 42,
        <<"amount">> => 100,
        <<"timestamp">> => 1700  %% Should be ignored
    },

    Key = idempotency:generate_key(<<"payment">>, Data),
    KeyWithoutIgnoredField = idempotency:generate_key(<<"payment">>, maps:remove(<<"timestamp">>, Data)),

    %% Hash should be the same whether "timestamp" is present or not
    ?assertEqual(Key, KeyWithoutIgnoredField).

%% ------------------------------------------------------
%% Test: Using "all" should hash everything except ignored fields
%% ------------------------------------------------------
all_fields_test() ->
    FilePath = create_test_file(),
    config_loader:load_rules(FilePath),

    Data = #{
        <<"user_id">> => 42,
        <<"amount">> => 100,
        <<"event_id">> => 200,
        <<"timestamp">> => 1700  %% Should be ignored
    },

    Key = idempotency:generate_key(<<"default">>, Data),
    KeyExpected = idempotency:generate_key(<<"default">>, maps:remove(<<"timestamp">>, Data)),

    ?assertEqual(Key, KeyExpected).

%% ------------------------------------------------------
%% Run all tests
%% ------------------------------------------------------
run() ->
    generate_key_test(),
    ignore_fields_test(),
    all_fields_test(),
    io:format("✅ All idempotency tests passed!~n").
