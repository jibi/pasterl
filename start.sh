#!/bin/sh
erl -mnesia dir '"./db"' -pa ebin deps/*/ebin -s pasterl
