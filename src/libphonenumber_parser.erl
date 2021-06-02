%%%-------------------------------------------------------------------
%%% @author marinakr
%%% Created : 17. Apr 2018 15:20
%%%-------------------------------------------------------------------
-module(libphonenumber_parser).
-author("marinakr").

-include_lib("libphonenumber_erlang/include/phonenumbers.hrl").
-include_lib("xmerl/include/xmerl.hrl").


%% API
-export([
  xml_file2memory/1
]).

%%%------------------------------------------------------------------------------------------------
%%% Parsing xml-file
%%% to generate map COUNTRY_PHONE_RULES in country_phones_rules.hrl
%%%------------------------------------------------------------------------------------------------
%% -------------------------------------------------------------------
%% @doc
%% Parse xml file
%% https://github.com/googlei18n/libphonenumber/blob/master/resources/PhoneNumberMetadata.xml
%% And return map stored in include/country_phone_rules.hrl
%% @end
%% -------------------------------------------------------------------
-spec xml_file2memory(list()) -> maps:map() | error.

xml_file2memory(FileName) ->
  case xmerl_scan:file(FileName) of
    {Xml, _} ->
      #xmlElement{content = [_, TerritoriesEl, _]} = Xml,
      TerritoriesInfo = TerritoriesEl#xmlElement.content,
      parse_country_name(TerritoriesInfo);
    _ ->
      error
  end.

%% --------------------------------------------------------------------------
%% @private
%% This block of code match name of country and block of rules for territory
%% --------------------------------------------------------------------------
-spec parse_country_name(list()) -> maps:map().

parse_country_name(Elements) ->
  parse_country_name(undefined, Elements, #{}).

-spec parse_country_name(Name, XmlDoc, Acc) -> Res when
  Name :: undefined | binary,
  XmlDoc :: list(),
  Acc :: maps:map(),
  Res :: maps:map().

parse_country_name(_, [], Acc) ->
  Acc;

parse_country_name(undefined, [C = #xmlComment{} | Rest], Acc) ->
  Name = C#xmlComment.value,
  NormalName = trim_first_last_whitespaces(Name),
  parse_country_name(unicode:characters_to_binary(NormalName, utf8), Rest, Acc);

parse_country_name(Name, [E = #xmlElement{name = territory} | Rest], Acc) when is_binary(Name) ->
  #xmlElement{name = territory, attributes = Attrs, content = Content} = E,
  PhonePattern = parse_attributes(Attrs, #phone_pattern{}),
  CountryPhoneInfo = parse_mobile_content(Content, PhonePattern),
  #phone_pattern{
    id = Id,
    code = Code,
    possible_length_regexp = LengthInfo,
    pattern = Pattern,
    options = Options} = CountryPhoneInfo,
  CountryInfoMap = #{
    id => Id, name => Name, pattern => Pattern, lengths => format_rules(LengthInfo), options => Options},
  PrevCodes = maps:get(Code, Acc, []),
  NewAcc =  maps:put(Code, [CountryInfoMap | PrevCodes], Acc),
  parse_country_name(undefined, Rest, NewAcc);

parse_country_name(State, [_H|Rest], Acc) ->
  parse_country_name(State, Rest, Acc).

%% -------------------------------------------------------------------
%% @private
%% Parse country attributes
%% -------------------------------------------------------------------
-spec parse_attributes(Attributes, State) -> State when
  Attributes :: list(#xmlAttribute{}),
  State :: #phone_pattern{}.

parse_attributes([], State) ->
  State;

parse_attributes([#xmlAttribute{name = id, value = Id} | Rest], State) ->
  parse_attributes(Rest, State#phone_pattern{id = list_to_binary(Id)});

parse_attributes([#xmlAttribute{name = countryCode, value = Code} | Rest], State) ->
  parse_attributes(Rest, State#phone_pattern{code = list_to_binary(Code)});

parse_attributes([_ | Rest], State) ->
  parse_attributes(Rest, State).

%% -------------------------------------------------------------------
%% @private
%% Parse country mobile possible length
%% -------------------------------------------------------------------
-spec parse_mobile_content(Elements, State) -> State when
  Elements :: list(#xmlElement{}),
  State :: #phone_pattern{}.

parse_mobile_content([], State) ->
  State;

parse_mobile_content([#xmlElement{name = mobile, content = Content} | Rest], State) ->
  #phone_pattern{possible_length_regexp = Ls} = State,
  #{pattern := Pattern,
    length := LengthAttributes} = get_pattern_and_length(Content),
    ExampleNumber = get_example_number(Content),
  [NewLength] = parse_possible_length(LengthAttributes, Ls),
  NewState = State#phone_pattern{
      possible_length_regexp = NewLength,
      pattern = Pattern,
      options = if ExampleNumber == null -> []; true -> [#{example_number => ExampleNumber}] end
      },
  parse_mobile_content(Rest, NewState);

parse_mobile_content([_ | Rest], State) ->
  parse_mobile_content(Rest, State).

%% -------------------------------------------------------------------
%% @private
%% Get possible length and pattern by one run
%% -------------------------------------------------------------------
-spec get_pattern_and_length(list()) -> map().

get_pattern_and_length(Content) ->
  get_pattern_and_length(Content, #{}).

-spec get_pattern_and_length(Elements, Acc) -> Res when
  Acc :: maps:map(),
  Elements :: list(),
  Res :: map().

get_pattern_and_length(_, #{pattern := _Pattern, length := _LengthAttrs} = Acc) ->
  Acc;

get_pattern_and_length([#xmlElement{name = possibleLengths, attributes = Attrs} | Rest], Acc) ->
  CurrAttrs = maps:get(length, Acc, []),
  get_pattern_and_length(Rest, maps:put(length, [Attrs | CurrAttrs], Acc));

get_pattern_and_length([#xmlElement{name = nationalNumberPattern, content = C} | Rest], Acc) ->
  [PatternVal] = [V || #xmlText{value = V} <- C],
  Pattern = re:replace(PatternVal, "\s+|\n|\t", "", [global, {return, binary}]),
  get_pattern_and_length(Rest, maps:put(pattern, Pattern, Acc));

get_pattern_and_length([_|Rest], Acc) ->
  get_pattern_and_length(Rest, Acc).

%% -------------------------------------------------------------------
%% @private
%% Get example of valid phone, only for test
%% -------------------------------------------------------------------
get_example_number([]) -> null;

get_example_number([#xmlElement{name = exampleNumber, content = [#xmlText{value = ExampleNumber}]} | _]) ->
  ExampleNumber;

get_example_number([_E|Rest]) ->
  get_example_number(Rest).

%% -------------------------------------------------------------------
%% @private
%% Parse country mobile possible length from attributes
%% -------------------------------------------------------------------
-spec parse_possible_length(Attrs, Acc) -> ResAcc when
  Attrs :: list(#xmlAttribute{}),
  Acc :: list(binary()),
  ResAcc :: list(binary()).

parse_possible_length([], Acc) ->
  Acc;

parse_possible_length([Attrs | Rest], Acc) ->
  Ls =[V || #xmlAttribute{name = national, value = V} <- Attrs],
  parse_possible_length(Rest, Acc ++ Ls).

%% -------------------------------------------------------------------
%% @private
%% Formats length to valid value
%% Block of ugly code parse length range from xml for validator
%% -------------------------------------------------------------------
-spec format_rules(Length) -> Result when
  Length :: list(),
  Result :: {Min, Max},
  Min :: integer(),
  Max :: integer().

format_rules([]) ->
  no_rules;

format_rules(Length) ->
  format_length(Length, #length{}).

-spec format_length(LengthText, Length) -> Result when
  LengthText :: list(),
  Length :: #length{},
  Result :: {Min, Max},
  Min :: integer(),
  Max :: integer().

%% when length in [1-9] format
format_length([$[ | Rest], CLength) ->
  format_length(Rest, CLength#length{is_range = true, part = 1});

format_length([$- | Rest], CLength = #length{is_range = true}) ->
  format_length(Rest, CLength#length{is_range = true, part = 2});

format_length([$]], #length{is_range = true, min = Min, max = Max}) ->
  [{list_to_integer(Min), list_to_integer(Max)}];

format_length([$],$, | Rest], #length{is_range = true, min = Min, max = Max}) ->
    [{list_to_integer(Min), list_to_integer(Max)}] ++ format_length(Rest, #length{});

format_length([Symb | Rest], CLength = #length{is_range = true}) ->
  #length{part = Part, min = Min, max = Max} = CLength,
  case Part of
    1 ->
      format_length(Rest, CLength#length{min = Min ++ [Symb]});
    2 ->
      format_length(Rest, CLength#length{max = Max ++ [Symb]})
  end;

%% when length like 9 or 8,11
format_length(Length, #length{is_range = false}) ->
  BLen = list_to_binary(Length),
  AvailableLength = binary:split(BLen, <<",">>, [global]),
  [{binary_to_integer(M), binary_to_integer(M)} || M <- AvailableLength].

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec trim_first_last_whitespaces(Name) -> NoWhitespaceName when
  Name :: list(),
  NoWhitespaceName :: list().

trim_first_last_whitespaces(Name) ->
  LName = trim_leading_whitespaces(Name),
  RName = trim_leading_whitespaces(lists:reverse(LName)),
  lists:reverse(RName).

-spec trim_leading_whitespaces(list()) -> list().

% Be accurate here with tabulation,
% Whitespace after $ is matter, it is whitespace symbol, 32
trim_leading_whitespaces([$ | Name]) ->
  trim_leading_whitespaces(Name);

trim_leading_whitespaces([$\t | Name]) ->
  trim_leading_whitespaces(Name);

trim_leading_whitespaces([$\n | Name]) ->
  trim_leading_whitespaces(Name);

trim_leading_whitespaces(Name) ->
  Name.
