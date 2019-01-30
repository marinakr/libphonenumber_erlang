-module(libphonenumbers_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_suite/1]).

all() ->
    [mod_exists].

init_per_suite(Config) ->
  application:load(libphonenumber_erlang),
  DataDir = proplists:get_value(data_dir, Config),
  TestDataFile = filename:join([DataDir, "PhoneNumberMetadataForTesting.xml"]),
  io:format("~nTest File: ~s~n", [TestDataFile]),
  Examples = libphonenumber_parser:xml_file2memory(TestDataFile),
  PhoneExamples = maps:fold(fun(Code, V, Acc) ->
    N = [binary_to_list(Code) ++ E || #{options := [#{example_number := E}]} <- V],
    lists:foldl(fun(Ex, LocAcc) -> [Ex | LocAcc] end, N, Acc)
  end, [], Examples),
  io:format("~p", [PhoneExamples]),
  [{phone_examples, PhoneExamples} |Config].
