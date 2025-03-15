%%% filepath: test/config_loader_test.erl
%% ------------------------------------------------------
%% Module: config_loader_test
%% Purpose: Unit tests for config_loader
%% ------------------------------------------------------
-module(config_loader_test).
-include_lib("eunit/include/eunit.hrl").

%% Exported functions for eunit
-export([load_rules_test/0, load_rules_missing_file_test/0, get_rule_test/0, get_default_rule_test/0]).

%% ------------------------------------------------------
%% Test: Successfully loading rules
%% ------------------------------------------------------
load_rules_test() ->
    io:format("Starting load_rules_test~n"),
    io:format("Loading server config...~n"),
    {ok, Config} = file:consult("test/server_test.config"),
    io:format("Server config loaded: ~p~n", [Config]),
    
    {ok, Rules} = config_loader:load_config("test/idempotency_rules_test.config"),
    io:format("Rules loaded: ~p~n", [Rules]),
    
    ?assert(maps:is_key(default, Rules)),
    ?assert(maps:is_key(payment, Rules)),
    ?assert(maps:is_key(user_event, Rules)),
    io:format("load_rules_test completed successfully~n").

%% ------------------------------------------------------
%% Test: Handling missing file error
%% ------------------------------------------------------
load_rules_missing_file_test() ->
    io:format("Starting load_rules_missing_file_test~n"),
    io:format("Testing missing file error...~n"),
    {error, {file_read_error, enoent}} = config_loader:load_config("non_existing_file.config"),
    io:format("load_rules_missing_file_test completed successfully~n").

%% ------------------------------------------------------
%% Test: Retrieving a specific rule
%% ------------------------------------------------------
get_rule_test() ->
    io:format("Starting get_rule_test~n"),
    io:format("Loading server config...~n"),
    {ok, Config} = file:consult("test/server_test.config"),
    io:format("Server config loaded: ~p~n", [Config]),
    
    {ok, _} = config_loader:load_config("test/idempotency_rules_test.config"),
    io:format("Rules loaded for get_rule_test~n"),
    
    PaymentRule = config_loader:get_rule(payment),
    io:format("Payment rule: ~p~n", [PaymentRule]),
    
    ?assertEqual([<<"timestamp">>, <<"request_id">>], maps:get(ignore_fields, PaymentRule)),
    ?assertEqual([<<"user_id">>, <<"amount">>], maps:get(key_fields, PaymentRule)),
    io:format("get_rule_test completed successfully~n").

%% ------------------------------------------------------
%% Test: Retrieving default rule when rule is missing
%% ------------------------------------------------------
get_default_rule_test() ->
    io:format("Starting get_default_rule_test~n"),
    io:format("Loading server config...~n"),
    {ok, Config} = file:consult("test/server_test.config"),
    io:format("Server config loaded: ~p~n", [Config]),
    
    {ok, _} = config_loader:load_config("test/idempotency_rules_test.config"),
    io:format("Rules loaded for get_default_rule_test~n"),
    
    DefaultRule = config_loader:get_rule(default),
    io:format("Default rule: ~p~n", [DefaultRule]),
    
    ?assertEqual([<<"timestamp">>], maps:get(ignore_fields, DefaultRule)),
    ?assertEqual(all, maps:get(key_fields, DefaultRule)),
    io:format("get_default_rule_test completed successfully~n").