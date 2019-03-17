-module(libphonenumber_erlang_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-include_lib("libphonenumber_erlang/include/phonenumbers.hrl").

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	init_mnesia(),
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
	maps:fold(
		fun(Code, CodeRulesInfo, _) ->
			CodeRules = [#code_set{id = Id, lengths = Length, name = Name, pattern = Pattern} ||
			  #{id := Id, lengths := Length, pattern := Pattern, name := Name} <- CodeRulesInfo],
			Record = #countryphones{
				code = Code,
				code_rules = CodeRules},
			mnesia:dirty_write(Record)
		end, #{}, Rules),
	ok.

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec init_mnesia() -> ok.

init_mnesia() ->
	mnesia:create_schema([node()]),
	mnesia:start(),
	mnesia:create_table(countryphones,
		[{attributes, record_info(fields, countryphones)}]),
	ok.
