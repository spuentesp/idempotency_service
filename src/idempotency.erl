%% filepath: src/idempotency.erl
-module(idempotency).
-export([generate_key/2]).

%% Generate an idempotency key from the event data
generate_key(RuleName, Data) ->
    Rule = config_loader:get_rule(RuleName),  %% Get rule for the event type
    IgnoreFields = maps:get(ignore_fields, Rule, []),  %% Fields to ignore
    KeyFields = maps:get(key_fields, Rule, []),  %% Fields to include in hash
    FilteredData = filter_fields(Data, IgnoreFields, KeyFields),
    Hash = hash_data(FilteredData),
    Hash.

%% Remove ignored fields and keep only the key fields
filter_fields(Data, IgnoreFields, <<"all">>) ->  %% If "all" is specified, use all fields
    maps:without(IgnoreFields, Data);
filter_fields(Data, IgnoreFields, KeyFields) ->  
    CleanedData = maps:without(IgnoreFields, Data),  %% Remove ignored fields
    maps:with(KeyFields, CleanedData).  %% Keep only key fields

%% Generate a hash from a given data map
hash_data(Data) ->
    KeyList = maps:to_list(Data),  %% Convert map to list for hashing
    erlang:phash2(KeyList).  %% Generate a consistent, fast hash