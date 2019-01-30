-module(libphonenumbers_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_suite/1]).

all() ->
    [mod_exists].

init_per_suite(Config) ->
  application:load(libphonenumber_erlang),
  TestDataFile = filename:join([?config(data_dir, Config), "PhoneNumberMetadataForTesting.xml"]),
  Examples = libphonenumber_parser:xml_file2memory(TestDataFile),
  io:format("~p", [Examples]).
