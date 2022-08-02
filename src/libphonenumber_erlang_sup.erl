-module(libphonenumber_erlang_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-include_lib("libphonenumber_erlang/include/phonenumbers.hrl").

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	init_ets(),
	load_countryphonenumbers_rules(),

	Procs = [],
	{ok, {{one_for_one, 1, 5}, Procs}}.

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec load_countryphonenumbers_rules() -> ok.

load_countryphonenumbers_rules() ->
	PhoneNumberMetadata = code:priv_dir(libphonenumber_erlang) ++ "/" ++ ?FILE_PHONE_PHONE_FORMATS,
	Rules = libphonenumber_parser:xml_file2memory(PhoneNumberMetadata),
	load_table_rules(?ETS_TABLE, Rules),
	ShortNumberMetadata = code:priv_dir(libphonenumber_erlang) ++ "/" ++ ?FILE_SHORT_PHONE_FORMATS,
	RulesShort = libphonenumber_parser:xml_file2memory(ShortNumberMetadata),
	load_short_table_rules(?ETS_TABLE_SHORT, RulesShort),
	ok.

load_table_rules(Table, Rules) -> 
	maps:fold(
		fun(Code, CodeRulesInfo, _) ->
			CodeRules = [#code_set{id = Id, lengths = Length, name = Name, pattern = Pattern} ||
				#{id := Id, lengths := Length, pattern := Pattern, name := Name} <- CodeRulesInfo],
			Record = #countryphones{
				code = Code,
				code_rules = CodeRules},
			ets:insert(Table, Record)
		end, #{}, Rules),
		ok.

load_short_table_rules(Table, Rules) ->
	#{undefined := NoCodeRules} = Rules,
	lists:foldl(
		fun(Element, _) ->
			#{id := Code} = Element,
			CodeRules = [#code_set{id = Id, lengths = Length, name = Name, pattern = Pattern} ||
				#{id := Id, lengths := Length, pattern := Pattern, name := Name} <- [Element]],
			Record = #countryphones{
				code = Code,
				code_rules = CodeRules},
			ets:insert(Table, Record)
		end, #{}, NoCodeRules),
		ok.

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
init_ets() ->
	ets:new(?ETS_TABLE,
		[named_table, public, ordered_set, {keypos, 2},
			{write_concurrency, true}, {read_concurrency, true}]),
	ets:new(?ETS_TABLE_SHORT,
		[named_table, public, ordered_set, {keypos, 2},
			{write_concurrency, true}, {read_concurrency, true}]).
