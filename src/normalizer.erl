%% filepath: src/normalizer.erl
-module(normalizer).
-export([normalize/2]).

%% Normalize data based on the provided rules
normalize(Data, Rule) ->
    %% Extract normalization rules, key fields, and ignore fields from the rule
    NormalizationRules = maps:get(normalization_rules, Rule, []),
    KeyFields = maps:get(key_fields, Rule, []),
    IgnoreFields = maps:get(ignore_fields, Rule, []),
    %% Remove ignored fields
    FilteredData = maps:without(IgnoreFields, Data),
    %% Check for required fields
    case check_required_fields(FilteredData, KeyFields) of
        ok ->
            %% Normalize data based on the normalization rules
            NormalizedData = lists:foldl(fun normalize_field/2, FilteredData, NormalizationRules),
            {ok, NormalizedData};
        {error, Reason} ->
            %% Log the rejection reason
            io:format("Data rejected: ~p~n", [Reason]),
            {error, Reason}
    end.

%% Check if the data contains all required key fields
check_required_fields(Data, <<"all">>) ->
    ok;
check_required_fields(Data, KeyFields) ->
    case lists:all(fun(Key) -> maps:is_key(Key, Data) end, KeyFields) of
        true -> ok;
        false -> {error, missing_key_fields}
    end.

%% Normalize a single field based on the normalization rule
normalize_field(NormalizationRule, Data) ->
    Field = maps:get(field, NormalizationRule),
    Type = maps:get(type, NormalizationRule),
    case maps:get(Field, Data, undefined) of
        undefined -> Data;
        Value -> maps:put(Field, normalize_value(Value, Type), Data)
    end.

%% Normalize a value based on the specified type
normalize_value(Value, string) when is_integer(Value) ->
    integer_to_binary(Value);
normalize_value(Value, string) when is_float(Value) ->
    float_to_binary(Value);
normalize_value(Value, float) when is_integer(Value) ->
    float(Value);
normalize_value(Value, float) when is_binary(Value) ->
    binary_to_float(Value);
normalize_value(Value, _Type) ->
    Value.