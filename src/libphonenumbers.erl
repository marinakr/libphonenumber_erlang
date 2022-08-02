%%%-------------------------------------------------------------------
%%% @author marinakr
%%% Created : 04. Apr 2018 19:04
%%%-------------------------------------------------------------------
-module(libphonenumbers).
-author("marinakr").

-include_lib("libphonenumber_erlang/include/phonenumbers.hrl").

%% API
-export([
  is_mobile_phone_valid/1,
  is_emergency_number/2,
  short_phone_number_info/2,
  mobile_phone_number_info/1,
  extract_possible_number/1
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
-spec is_mobile_phone_valid(Msisdn) -> Result when
  Msisdn :: binary(),
  Result :: boolean().

is_mobile_phone_valid(Msisdn) ->
  maps:get(valid, mobile_phone_number_info(Msisdn)).

%%%------------------------------------------------------------------------------------------------
%%% Validation emergency number
%%% for country
%%% according to data from https://github.com/googlei18n/libphonenumber/
%%%------------------------------------------------------------------------------------------------
%% -------------------------------------------------------------------
%% @private
%% Check number match rules in include file
%% -------------------------------------------------------------------
-spec is_emergency_number(Code, Number) -> Result when
  Code :: binary(),
  Number :: binary(),
  Result :: boolean().

is_emergency_number(Code, Number) ->
  maps:get(valid, short_phone_number_info(Code, Number)).

%% -------------------------------------------------------------------
%% @private
%% check does short numbers match rules
%% -------------------------------------------------------------------
-spec short_phone_number_info(Code, Phone) -> Result when
  Code :: binary(),
  Phone :: binary(),
  Result :: maps:map().

short_phone_number_info(Code, Phone) ->
  Pairs = [{Code, Phone}],
  rules_for_codepairs(?ETS_TABLE_SHORT, Pairs, #{valid => false, errors => []}).

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
  Code1N = C1,
  Phone1N = <<C2/binary, C3/binary, Phone3N/binary>>,
  Phone2N = <<C3/binary, Phone3N/binary>>,
  Pairs = [
    {Code3N, Phone3N},
    {Code2N, Phone2N},
    {Code1N, Phone1N}],
  rules_for_codepairs(?ETS_TABLE, Pairs, #{valid => false, errors => []});

mobile_phone_number_info(_) ->
  #{valid => false, errors => [#{"NO CODE" => <<"Phone number should starts with '+'">>}]}.

%% -------------------------------------------------------------------
%% @private
%% check does phone pairs match any rule
%% -------------------------------------------------------------------
-spec rules_for_codepairs(Table, Pairs, ValidationLog) ->  Result when
  Table :: string(),
  Pairs :: list(),
  ValidationLog :: maps:map(),
  Result :: maps:map().

rules_for_codepairs(_, [], #{errors := Errors} = ValidationLog) ->
  ValidationLog#{valid => false, errors => [#{"NO PAIRS" => "Finished"} | Errors]};

rules_for_codepairs(Table, [{Code, Phone} | Pairs], ValidationLog) ->
  Rules = ets:lookup(Table, Code),
  #{valid := IsValid, errors := ResErrors} = ValidationResult = rules_for_code(Rules, ValidationLog, {Code, Phone}),
  if IsValid ->
    ValidationResult;
    true ->
    rules_for_codepairs(Table, Pairs, ValidationLog#{errors => ResErrors})
  end.

%% -------------------------------------------------------------------
%% @private
%% check does phone match rule for region
%% -------------------------------------------------------------------
-spec rules_for_code(Rules, ValidationLog, {Code, Phone}) -> Result when
  Rules :: list(),
  ValidationLog :: maps:map(),
  Code :: binary(),
  Phone :: binary(),
  Result :: maps:map().

rules_for_code([], #{errors := Errors} = ValidationLog, {Code, _}) ->
  Error = #{Code => <<"No country code info found">>},
  ValidationLog#{valid => false, errors => [Error | Errors]};

rules_for_code([#countryphones{code_rules = CR}],  ValidationLog, {Code, Phone}) ->
  match_code_rule(CR, ValidationLog, {Code, Phone});

rules_for_code(_E, #{errors := Errors} = ValidationLog, {Code, _}) ->
  Error = #{Code => <<"Mnesia cointains corrupted record">>},
  ValidationLog#{valid => false, errors => [Error | Errors]}.


%% -------------------------------------------------------------------
%% @private
%% match code on rule
%% -------------------------------------------------------------------
match_code_rule([], ValidationLog, _) ->
  ValidationLog;

match_code_rule([#code_set{lengths = LenghtRange, pattern = Pattern, name = Name, id = Id} |
                 Rules], #{errors := Errors} = ValidationLog, {Code, Phone}) ->
#{valid := IsValid, error := Error} =  valid_phone_with_length_rules(Code, Phone, LenghtRange, Pattern),
InternationalPhone = <<"+", Code/binary, Phone/binary>>,
  if IsValid ->
    #{valid => true,
    phone => InternationalPhone,
    country_metadata => #{
    name => Name,
    id => Id,
    code => Code},
    errors => []};
  true ->
  match_code_rule(Rules, ValidationLog#{errors => [Error | Errors]}, {Code, Phone})
end.

%% -------------------------------------------------------------------
%% @private
%% check does phone match length rules
%% -------------------------------------------------------------------
-spec valid_phone_with_length_rules(Code, Phone, Rules, Pattern) -> Result when
  Code :: binary(),
  Phone :: binary(),
  Rules :: list({integer(), integer()}),
  Pattern :: binary(),
  Result :: maps:map().

valid_phone_with_length_rules(Code, Phone, no_rules, _) ->
  %validate does it looks like phone number
  case re:run(Phone, ?REGEXP_PHONE, [{capture, none}]) of
    match ->
      #{valid => true, error => null};
    _ ->
      #{valid => false, error => #{Code => <<"No match with phonenumber regular expression">>}}
  end;

valid_phone_with_length_rules(Code, _Phone, [], _) ->
  #{valid => false, error => #{Code => <<"No match for current phone min/max length find">>}};

valid_phone_with_length_rules(Code, Phone, [{Min, Max} | LengthRange], Pattern) ->
  Size = erlang:size(Phone),
  if (Size >= Min) and (Size =< Max) ->
    case re:run(Phone, Pattern, [{capture, none}]) of
      match ->
        #{valid => true, error => null};
      _ ->
        #{valid => false, error => #{Code => <<"Pattern '", Pattern/binary, "' compilation failed">>}}
    end;
    true ->
      valid_phone_with_length_rules(Code, Phone, LengthRange, Pattern)
  end.

%% -------------------------------------------------------------------
%% @private
%% Attempts to extract a possible number from the string passed in.
%% Ported from: https://github.com/google/libphonenumber/blob/ded8ce8ce366e545ac44dbfd27b8fa085fc6fc93/java/libphonenumber/src/com/google/i18n/phonenumbers/PhoneNumberUtil.java#L720
%% -------------------------------------------------------------------
-spec extract_possible_number(Msisdn) -> Result when
  Msisdn :: binary(),
  Result :: binary().

extract_possible_number(Msisdn) -> 
  Len = string:length(Msisdn),
  case re:run(Msisdn, ?VALID_START_CHAR_PATTERN) of
    {match,[{Start,_}]} ->
      Part1 = binary:part(Msisdn, Start, Len - 1 - Start),
      Last = case re:run(Part1, ?UNWANTED_END_CHARS_PATTERN) of
        {match,[{Last1,_}]} ->
          Last1 - 1;
        _ -> 
          Len - 1 - Start
      end,
      Part2 = binary:part(Part1, 0, Last),
      case re:run(Part2, ?SECOND_NUMBER_START_PATTERN) of
        {match,[{Last2,_}]} ->
          binary:part(Part2, 0, Last2 - 1);
        _ -> 
          Part2
      end;            
    _ -> 
      <<"">>
  end.
  
