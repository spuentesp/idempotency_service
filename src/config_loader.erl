%% ------------------------------------------------------
%% Module: config_loader
%% Purpose: Loads and provides access to idempotency rules
%% from the JSON configuration file.
%% ------------------------------------------------------
-module(config_loader).
-export([load_rules/1, get_rules/0, get_rule/1]).

%% ------------------------------------------------------
%% Load rules from JSON and store globally.
%% ------------------------------------------------------
%% @param FilePath The path to the `idempotency_rules.json` file.
%% @return {ok, Rules} if successfully loaded, {error, Reason} otherwise.
load_rules(FilePath) ->
    case file:read_file(FilePath) of
        {ok, Binary} ->  %% Successfully read the file
            Json = jsx:decode(Binary, [return_maps]), 
            RulesList = maps:get(<<"rules">>, Json, undefined),
            case RulesList of
                undefined ->
                    {error, no_rules_found};
                _ ->
                    RulesMap = lists:foldl(fun parse_rule/2, #{}, RulesList),
                    application:set_env(idempotency_service, rules, RulesMap),
                    {ok, RulesMap}
            end;
        {error, Reason} ->  %% File read error (file missing, permissions issue, etc.)
            {error, {file_read_error, Reason}}
    end.

%% ------------------------------------------------------
%% Convert list of rules into a map {RuleName => RuleData}
%% ------------------------------------------------------
parse_rule(#{<<"name">> := Name, <<"ignore_fields">> := Ignore, <<"key_fields">> := Keys}, Acc) ->
    Acc#{Name => #{ignore_fields => Ignore, key_fields => Keys}}.

%% ------------------------------------------------------
%% Retrieve all stored rules from memory.
%% ------------------------------------------------------
get_rules() ->
    case application:get_env(idempotency_service, rules) of
        {ok, Rules} -> Rules;
        undefined -> #{}  %% No rules loaded yet
    end.

%% ------------------------------------------------------
%% Retrieve a specific rule by name.
%% ------------------------------------------------------
get_rule(Name) ->
    Rules = get_rules(),
    case maps:get(Name, Rules, undefined) of
        undefined -> maps:get(<<"default">>, Rules, #{ignore_fields => [], key_fields => []});
        Rule -> Rule
    end.
