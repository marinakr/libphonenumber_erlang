-module(libphonenumber_erlang_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-include("../include/phonenumbers.hrl").

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
	Rules = libphonenumber_parser:xml_file2memory(?FILE_PHONE_PHONE_FORMATS),
	maps:fold(
		fun(Code, #{id := Id, lengths := Length, pattern := Pattern, name := Name}, _) ->
			Record = #?PHONENUMBERS{
				code = Code,
				id = Id,
				lengths = Length,
				name = Name,
				pattern = Pattern},
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
	mnesia:create_table(?PHONENUMBERS,
		[{attributes, record_info(fields, ?PHONENUMBERS)},
			{index, [#?PHONENUMBERS.id]}]),
	ok.

