-module(libphonenumbers_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_suite/1, mod_exists/1, is_mobile_valid_phone_test/1, end_per_suite/1]).

all() ->
  [
  mod_exists,
  is_mobile_valid_phone_test
  ].

init_per_suite(Config) ->
  ok = application:load(libphonenumber_erlang),
  application:ensure_all_started(libphonenumber_erlang),
  DataDir = proplists:get_value(data_dir, Config),
  TestDataFile = filename:join([DataDir, "PhoneNumberMetadataForTesting.xml"]),
  Examples = libphonenumber_parser:xml_file2memory(TestDataFile),
  PhoneExamples = maps:fold(fun(Code, V, Acc) ->
    N = [iolist_to_binary(["+",Code,E]) || #{options := [#{example_number := E}]} <- V],
    lists:foldl(fun(Ex, LocAcc) -> [Ex | LocAcc] end, N, Acc)
  end, [], Examples),
  [{valid_phone_examples, PhoneExamples}].

mod_exists(_) ->
    {module, libphonenumbers} = code:load_file(libphonenumbers).

is_mobile_valid_phone_test(Config) ->
  PhoneExamples = proplists:get_value(valid_phone_examples, Config),
  Valid = lists:all(fun libphonenumbers:is_mobile_valid_phone/1, PhoneExamples),
  ?assert(Valid).

end_per_suite(_Config) -> [].
