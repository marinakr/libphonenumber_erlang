# libphonenumber_erlang
Usage:

 cd libphonenumber_erlang/
 make
 ./start.sh
 (libphonenumber_erlang@localhost)1> phone_utils:is_mobile_valid_phone(<<"+380988562241">>).
true
(libphonenumber_erlang@localhost)2> phone_utils:is_mobile_valid_phone(<<"+380338560241">>).
false
(libphonenumber_erlang@localhost)3> phone_utils:mobile_phone_number_info(<<"+380967112244">>).
#{ciuntry_metadata =>
      #{code => <<"380">>,id => <<"UA">>,name => <<"Ukraine">>},
  phone => <<"+380967112244">>,valid => true}
