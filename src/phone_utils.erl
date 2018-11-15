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
  get_rules_for_code(#{errors => []}, Pairs);

mobile_phone_number_info(_) ->
  #{valid => false, errors => [<<"Phone number should starts with '+'">>]}.

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec get_rules_for_code(ValidationLog, CodePhonePairs) -> Result when
  ValidationLog :: maps:map(),
  CodePhonePairs :: proplists:proplist(),
  Result :: maps:map().

get_rules_for_code(ValidationLog, []) -> ValidationLog#{valid => false};

get_rules_for_code(ValidationLog = #{errors := Errors}, [{Code, Phone} | Rest]) ->
  case mnesia:dirty_read(?PHONENUMBERS, Code) of
    [
    #?PHONENUMBERS{
      lengths = LenghtRange,
      pattern = Pattern,
      name = Name,
      id = Id}
    ] ->
      #{valid := IsValid, error := Error} =  valid_phone_with_rules(Phone, LenghtRange, Pattern),
      InternationalPhone = <<"+", Code/binary, Phone/binary>>,
      if IsValid ->
        #{valid => true,
          phone => InternationalPhone,
          country_metadata => #{
            name => Name,
            id => Id,
            code => Code}};
        true ->
          get_rules_for_code(ValidationLog#{errors => [Error | Errors]}, Rest)
      end;
    _ ->
    Error = <<"No country code info found for code ", Code/binary>>,
    get_rules_for_code(ValidationLog#{errors => [Error | Errors]}, Rest)
  end.

%% -------------------------------------------------------------------
%% @private
%% check does phone match length rules
%% -------------------------------------------------------------------
-spec valid_phone_with_rules(Phone, Rules, Pattern) -> Result when
  Phone :: binary(),
  Rules :: list({integer(), integer()}),
  Pattern :: binary(),
  Result :: maps:map().

valid_phone_with_rules(Phone, no_rules, _) ->
  %validate does it looks like phone number
  case re:run(Phone, ?REGEXP_PHONE, [{capture, none}]) of
    match ->
      #{valid => true, error => null};
    _ ->
      #{valid => false, error => <<"Not match with phonenumber regular expression">>}
  end;

valid_phone_with_rules(_Phone, [], _) ->
  #{valid => false, error => <<"No matched rules for current phone min/max length find">>};

valid_phone_with_rules(Phone, [{Min, Max} | LengthRange], Pattern) ->
  Size = erlang:size(Phone),
  if (Size >= Min) and (Size =< Max) ->
    case re:run(Phone, Pattern, [{capture, none}]) of
      match ->
        #{valid => true, error => null};
      _ ->
        #{valid => false, error => <<"Pattern '", Pattern/binary, "' compilation failed">>}
    end;
    true ->
      valid_phone_with_rules(Phone, LengthRange, Pattern)
  end.
