%% filepath: src/config_loader.erl
-module(config_loader).
-export([load_config/1, get_rule/1]).

%% Load configuration from a file
load_config(ServerConfigFile) ->
    io:format("Loading server config from file: ~s~n", [ServerConfigFile]),
    case file:consult(ServerConfigFile) of
        {ok, Config} ->
            io:format("Server config loaded: ~p~n", [Config]),
            {ok, ServiceConfig} = lists:keyfind(idempotency_service, 1, Config),
            RulesPath = proplists:get_value(rules_path, ServiceConfig),
            io:format("Rules path: ~s~n", [RulesPath]),
            load_rules(RulesPath);
        {error, Reason} ->
            io:format("Error loading server config: ~p~n", [Reason]),
            {error, {file_read_error, Reason}}
    end.

%% Load idempotency rules from a file
load_rules(RulesFile) ->
    io:format("Loading rules from file: ~s~n", [RulesFile]),
    case file:consult(RulesFile) of
        {ok, RulesConfig} ->
            io:format("Rules config loaded: ~p~n", [RulesConfig]),
            {ok, Rules} = lists:keyfind(idempotency_service, 1, RulesConfig),
            application:set_env(idempotency_service, rules, Rules),
            io:format("Rules set in environment: ~p~n", [Rules]),
            {ok, Rules};
        {error, Reason} ->
            io:format("Error loading rules: ~p~n", [Reason]),
            {error, {file_read_error, Reason}}
    end.

%% Retrieve a specific rule by name
get_rule(Name) ->
    io:format("Retrieving rule for name: ~p~n", [Name]),
    {ok, Rules} = application:get_env(idempotency_service, rules),
    case maps:get(Name, Rules, undefined) of
        undefined ->
            io:format("Rule not found, returning default rule~n"),
            maps:get(default, Rules);
        Rule ->
            io:format("Rule found: ~p~n", [Rule]),
            Rule
    end.