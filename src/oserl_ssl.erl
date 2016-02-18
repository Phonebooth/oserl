-module(oserl_ssl).

-export([listen/2]).
-export([accept/1]).
-export([accept/2]).
-export([connect/4]).
-export([peername/1]).
-export([setopts/2]).
-export([close/1]).
-export([controlling_process/2]).

listen(Port, Opts) ->
	ssl:listen(Port, Opts).

accept(LSocket) ->
    accept(LSocket, infinity).

accept(LSocket, Timeout) ->
	case ssl:transport_accept(LSocket, Timeout) of
		{ok, CSocket} ->
			ssl_accept(CSocket, Timeout);
		{error, Reason} ->
			{error, Reason}
	end.

connect(Host, Port, Opts, Timeout) ->
	gen_tcp:connect(Host, Port, Opts, Timeout).

peername(Socket) ->
	ssl:peername(Socket).

setopts(Socket, Opts) ->
	ssl:setopts(Socket, Opts).

close(Socket) ->
	ssl:close(Socket).

controlling_process(Socket, NewOwner) ->
    gen_tcp:controlling_process(Socket, NewOwner).

%% Internal.

ssl_accept(Socket, Timeout) ->
	case ssl:ssl_accept(Socket, Timeout) of
		ok ->
			{ok, Socket};
		{error, Reason} ->
			{error, {ssl_accept, Reason}}
	end.
