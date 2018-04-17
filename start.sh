MNESIADIR='"MnesiaLibPhoneNumber"'
echo libphonenumber_erlang run with $MNESIADIR 
exec erl -pa ebin/ deps/*/ebin -s libphonenumber_erlang_app -name libphonenumber_erlang@localhost \
	-mnesia dir $MNESIADIR
