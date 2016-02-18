-module(oserl_tcp).

-export([listen/2]).
-export([accept/1]).
-export([accept/2]).
-export([connect/4]).
-export([peername/1]).
-export([setopts/2]).
-export([close/1]).
-export([controlling_process/2]).

listen(Port, Opts) ->
	gen_tcp:listen(Port, Opts).

accept(LSocket) ->
    accept(LSocket, infinity).

accept(LSocket, Timeout) ->
	gen_tcp:accept(LSocket, Timeout).

connect(Host, Port, Opts, Timeout) ->
	gen_tcp:connect(Host, Port, Opts, Timeout).

peername(Socket) ->
	inet:peername(Socket).

setopts(Socket, Opts) ->
	inet:setopts(Socket, Opts).

close(Socket) ->
	gen_tcp:close(Socket).

controlling_process(Socket, NewOwner) ->
    gen_tcp:controlling_process(Socket, NewOwner).
