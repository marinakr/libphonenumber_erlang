%%%-------------------------------------------------------------------
%%% @author marinakr
%%% Created : 04. Apr 2018 19:04
%%%-------------------------------------------------------------------
-module(phone_utils).
-author("marinakr").

-include("include/phonenumbers.hrl").

%% API
-export([
  is_mobile_valid_phone/1,
  mobile_phone_number_info/1
  ]).

%%%------------------------------------------------------------------------------------------------
%%% Validation phone number
%%% for country
%%% according to data from https://github.com/googlei18n/libphonenumber/
%%%------------------------------------------------------------------------------------------------
%% -------------------------------------------------------------------
%% @private
%% Check msisdn is match rules in include file
%% -------------------------------------------------------------------
-spec is_mobile_valid_phone(Msisdn) -> Result when
  Msisdn :: binary(),
  Result :: boolean().

is_mobile_valid_phone(Msisdn) ->
  maps:get(valid, mobile_phone_number_info(Msisdn)).

%% -------------------------------------------------------------------
%% @private
%% check does phone numbers match rules
%% -------------------------------------------------------------------
-spec mobile_phone_number_info(Msisdn) -> Result when
  Msisdn :: binary(),
  Result :: maps:map().

mobile_phone_number_info(<<"+", C1:1/binary, C2:1/binary, C3:1/binary, Phone3N/binary>>) ->
  Code3N = <<C1:1/binary, C2:1/binary, C3:1/binary>>,
  Code2N = <<C1:1/binary, C2:1/binary>>,
  Phone2N = <<C3/binary, Phone3N/binary>>,
  Pairs = [
    {Code3N, Phone3N},
    {Code2N, Phone2N}],
  get_rules_for_code(Pairs);

mobile_phone_number_info(_) ->
  #{valid => false}.

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec get_rules_for_code(CodePhonePairs) -> Result when
  CodePhonePairs :: proplists:proplist(),
  Result :: maps:map().

get_rules_for_code([]) ->
  #{valid => false};

get_rules_for_code([{Code, Phone} | Rest]) ->
  case mnesia:dirty_read(?PHONENUMBERS, Code) of
    [#?PHONENUMBERS{
      lengths = LenghtRange,
      pattern = Pattern,
      name = Name,
      id = Id}] ->
      IsValid = valid_phone_with_rules(Phone, LenghtRange, Pattern),
      InternationalPhone = <<"+", Code/binary, Phone/binary>>,
      if IsValid ->
        #{valid => true,
          phone => InternationalPhone,
          country_metadata => #{
            name => Name,
            id => Id,
            code => Code}};
        true ->
          get_rules_for_code(Rest)
      end;
    _ ->
      get_rules_for_code(Rest)
  end.

%% -------------------------------------------------------------------
%% @private
%% check does phone match length rules
%% -------------------------------------------------------------------
-spec valid_phone_with_rules(Phone, Rules, Pattern) -> Result when
  Phone :: binary(),
  Rules :: list({integer(), integer()}),
  Pattern :: binary(),
  Result :: boolean().

valid_phone_with_rules(Phone, no_rules, _) ->
  %validate does it looks like phone number
  case re:run(Phone, ?REGEXP_PHONE, [{capture, none}]) of
    match ->
      true;
    _ ->
      false
  end;

valid_phone_with_rules(_Phone, [], _) ->
  false;

valid_phone_with_rules(Phone, [{Min, Max} | LengthRange], Pattern) ->
  Size = erlang:size(Phone),
  if (Size >= Min) and (Size =< Max) ->
    case re:run(Phone, Pattern, [{capture, none}]) of
      match ->
        true;
      _ ->
        false
    end;
    true ->
      valid_phone_with_rules(Phone, LengthRange, Pattern)
  end.

