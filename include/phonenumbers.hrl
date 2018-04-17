%%%-------------------------------------------------------------------
%%% @author marinakr
%%% Created : 17. Apr 2018 13:03
%%%-------------------------------------------------------------------
-author("marinakr").

-define(FILE_PHONE_PHONE_FORMATS, "/PhoneNumberMetadata.xml").
-define(REGEXP_PHONE, <<"^\\+[0-9]{8,15}$">>).

-define(PHONENUMBERS, countryphones).
-record(?PHONENUMBERS, {code, id, lengths = [], name, pattern, metadata}).

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