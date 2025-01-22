%% ------------------------------------------------------
%% Module: config_loader_test
%% Purpose: Unit tests for config_loader
%% ------------------------------------------------------
-module(config_loader_test). 
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
%% Test: Successfully loading rules
%% ------------------------------------------------------
load_rules_test() ->
    FilePath = create_test_file(),
    
    {ok, Rules} = config_loader:load_rules(FilePath),
    
    ?assert(maps:is_key(<<"default">>, Rules)),
    ?assert(maps:is_key(<<"payment">>, Rules)),
    ?assert(maps:is_key(<<"user_event">>, Rules)).

%% ------------------------------------------------------
%% Test: Handling missing file error
%% ------------------------------------------------------
load_rules_missing_file_test() ->
    {error, {file_read_error, enoent}} = config_loader:load_rules("non_existing_file.json").

%% ------------------------------------------------------
%% Test: Retrieving a specific rule
%% ------------------------------------------------------
get_rule_test() ->
    FilePath = create_test_file(),
    config_loader:load_rules(FilePath),

    PaymentRule = config_loader:get_rule(<<"payment">>),
    ?assertEqual([<<"timestamp">>, <<"request_id">>], maps:get(ignore_fields, PaymentRule)),
    ?assertEqual([<<"user_id">>, <<"amount">>], maps:get(key_fields, PaymentRule)).

%% ------------------------------------------------------
%% Test: Retrieving default rule when rule is missing
%% ------------------------------------------------------
get_default_rule_test() ->
    FilePath = create_test_file(),
    config_loader:load_rules(FilePath),

    DefaultRule = config_loader:get_rule(<<"unknown_rule">>),
    ?assertEqual([<<"timestamp">>], maps:get(ignore_fields, DefaultRule)),
    ?assertEqual(<<"all">>, maps:get(key_fields, DefaultRule)).


%% ------------------------------------------------------
%% Run all tests
%% ------------------------------------------------------
run() ->
    load_rules_test(),
    load_rules_missing_file_test(),
    get_rule_test(),
    get_default_rule_test(),
    io:format("✅ All config_loader tests passed!~n").
