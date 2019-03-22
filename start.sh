#!/usr/bin/env bash
make
echo libphonenumber_erlang starting
exec erl -pa ./ebin ./src -name libphonenumber_erlang@localhost -eval "application:start(tools), application:start(libphonenumber_erlang)"
