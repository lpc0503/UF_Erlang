-module(test).
-compile(export_all).


a(A, B, C) ->
	A+B-C.

a(A, B) ->
	A+B.


start() ->

	a(5, 6, 7),
	a(5, 6).
