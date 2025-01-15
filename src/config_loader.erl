-module(config_loader).
-export([load_rules/1]). 

%% Load rules from a JSON file
load_rules(FilePath) ->
    case file:read_file(FilePath) of
        {ok, Binary} ->
            case jsx:decode(Binary, [return_maps]) of
                {ok, Json} ->
                    case maps:get(rules, Json, undefined) of
                        undefined ->
                            {error, no_rules_found};
                        Rules ->
                            {ok, Rules}
                    end;
                Error ->
                    {error, {invalid_json, Error}}
            end;
        {error, Reason} ->
            {error, {file_read_error, Reason}}
    end.
