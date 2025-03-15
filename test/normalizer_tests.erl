%% filepath: test/normalizer_tests.erl
-module(normalizer_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test Normalization with All Key Fields
normalize_all_key_fields_test() ->
    Data = #{<<"timestamp">> => 1234567890, <<"amount">> => 100, <<"user_id">> => 42},
    Rule = #{ignore_fields => [<<"timestamp">>],
             key_fields => <<"all">>,
             normalization_rules => [
                 #{field => <<"amount">>, type => float},
                 #{field => <<"user_id">>, type => string}
             ]},
    {ok, NormalizedData} = normalizer:normalize(Data, Rule),
    ExpectedData = #{<<"amount">> => 100.0, <<"user_id">> => <<"42">>},
    ?assertEqual(ExpectedData, NormalizedData).

%% Test Normalization with Specific Key Fields
normalize_specific_key_fields_test() ->
    Data = #{<<"timestamp">> => 1234567890, <<"amount">> => 100, <<"user_id">> => 42},
    Rule = #{ignore_fields => [<<"timestamp">>],
             key_fields => [<<"user_id">>, <<"amount">>],
             normalization_rules => [
                 #{field => <<"amount">>, type => float},
                 #{field => <<"user_id">>, type => string}
             ]},
    {ok, NormalizedData} = normalizer:normalize(Data, Rule),
    ExpectedData = #{<<"amount">> => 100.0, <<"user_id">> => <<"42">>},
    ?assertEqual(ExpectedData, NormalizedData).

%% Test Missing Key Fields
missing_key_fields_test() ->
    Data = #{<<"timestamp">> => 1234567890, <<"amount">> => 100},
    Rule = #{ignore_fields => [<<"timestamp">>],
             key_fields => [<<"user_id">>, <<"amount">>],
             normalization_rules => [
                 #{field => <<"amount">>, type => float},
                 #{field => <<"user_id">>, type => string}
             ]},
    {error, missing_key_fields} = normalizer:normalize(Data, Rule),
    ?assertEqual({error, missing_key_fields}, normalizer:normalize(Data, Rule)).

%% Test Normalization of Different Types
normalize_different_types_test() ->
    Data = #{<<"amount">> => 100, <<"user_id">> => 42},
    Rule = #{ignore_fields => [],
             key_fields => [<<"user_id">>, <<"amount">>],
             normalization_rules => [
                 #{field => <<"amount">>, type => float},
                 #{field => <<"user_id">>, type => string}
             ]},
    {ok, NormalizedData} = normalizer:normalize(Data, Rule),
    ExpectedData = #{<<"amount">> => 100.0, <<"user_id">> => <<"42">>},
    ?assertEqual(ExpectedData, NormalizedData).

%% Run all tests
all_tests() ->
    normalize_all_key_fields_test(),
    normalize_specific_key_fields_test(),
    missing_key_fields_test(),
    normalize_different_types_test().