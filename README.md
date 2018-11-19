# libphonenumber_erlang
Erlang version of [libphonenumber](https://github.com/googlei18n/libphonenumber)
by Google.
Usage:

```
cd libphonenumber_erlang/
 make
 ./start.sh
 ```
 Functions examples:
 ```
libphonenumbers:is_mobile_valid_phone(<<"+380988562241">>).
true

libphonenumbers:is_mobile_valid_phone(<<"+380338560241">>).
false

libphonenumbers:mobile_phone_number_info(<<"+380967112244">>).
#{country_metadata =>
      #{code => <<"380">>,id => <<"UA">>,name => <<"Ukraine">>},
  phone => <<"+380967112244">>,valid => true}

libphonenumbers:mobile_phone_number_info(<<"+14088881406">>).    
  #{country_metadata =>
        #{code => <<"1">>,id => <<"US">>,name => <<"United States">>},
    errors => [],phone => <<"+14088881406">>,valid => true}
  ```

  Use as dependency:
  Example of rebar.config for application myapp
  ```
  {erl_opts, [debug_info]}.
  {deps, [
     {libphonenumber_erlang, ".*", {git, "https://github.com/marinakr/libphonenumber_erlang.git", {branch, "master"}}}
     ]}.

  {shell, [
    % {config, [{config, "config/sys.config"}]},
      {apps, [libphonenumber_erlang, myapp]}
    ]}.
  ```
