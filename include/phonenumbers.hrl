%%%-------------------------------------------------------------------
%%% @author marinakr
%%% Created : 17. Apr 2018 13:03
%%%-------------------------------------------------------------------
-author("marinakr").

-define(FILE_PHONE_PHONE_FORMATS, "PhoneNumberMetadata.xml").
-define(FILE_SHORT_PHONE_FORMATS, "ShortNumberMetadata.xml").
-define(REGEXP_PHONE, <<"^\\+[0-9]{3,15}$">>).
-define(PLUS_CHAR_PATTERN, <<"[+\uFF0B]+">>).
-define(VALID_START_CHAR_PATTERN, <<"[+\uFF0B\\p{Nd}]">>).
-define(SECOND_NUMBER_START_PATTERN, <<"[\\\\/] *x">>).
-define(UNWANTED_END_CHARS_PATTERN, <<"[[\\P{N}&&\\P{L}]&&[^#]]+$">>).

-define(ETS_TABLE, libphonenumber_erlang_mobile_registry).
-define(ETS_TABLE_SHORT, libphonenumber_erlang_short_registry).

-record(phone_pattern, {
  code = undefined,
  id = undefined,
  possible_length_regexp = [],
  pattern,
  options = []}).

-record(length, {
  min = "",
  max = "",
  is_range = false,
  part}).

-record(code_set, {id, lengths = [], name, pattern, metadata}).

-record(countryphones, {
  code :: binary(),
  code_rules = [] :: list(#code_set{})
}).

