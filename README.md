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
phone_utils:is_mobile_valid_phone(<<"+380988562241">>).
true

phone_utils:is_mobile_valid_phone(<<"+380338560241">>).
false

phone_utils:mobile_phone_number_info(<<"+380967112244">>).
#{country_metadata =>
      #{code => <<"380">>,id => <<"UA">>,name => <<"Ukraine">>},
  phone => <<"+380967112244">>,valid => true}

phone_utils:mobile_phone_number_info(<<"+798063262045">>).
#{errors =>
      [<<"No country code info found for code 79">>,
       <<"No country code info found for code 798">>],
  valid => false}

phone_utils:mobile_phone_number_info(<<"+38063262045">>).  
#{errors =>
      [<<"No country code info found for code 38">>,
      <<"No matched rules for current phone min/max length find">>],
    valid => false}  
  ```
