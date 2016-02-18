%%% Copyright (C) 2009 Enrique Marcote, Miguel Rodriguez
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%
%%% o Redistributions of source code must retain the above copyright notice,
%%%   this list of conditions and the following disclaimer.
%%%
%%% o Redistributions in binary form must reproduce the above copyright notice,
%%%   this list of conditions and the following disclaimer in the documentation
%%%   and/or other materials provided with the distribution.
%%%
%%% o Neither the name of ERLANG TRAINING AND CONSULTING nor the names of its
%%%   contributors may be used to endorse or promote products derived from this
%%%   software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
%%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE.
-module(smpp_session).

%%% INCLUDE FILES
-include_lib("oserl/include/oserl.hrl").

%%% EXTERNAL EXPORTS
-export([congestion/3, connect/1, listen/1, tcp_send/2, send_pdu/3]).

%%% SOCKET LISTENER FUNCTIONS EXPORTS
-export([transport/1, set_transport/2, spawn_link/3, wait_accept/3, wait_recv/3, recv_loop/4]).

%% TIMER EXPORTS
-export([cancel_timer/1, start_timer/2]).

%% SOCKET OPERATIONS
-export([accept/1]).
-export([peername/1]).
-export([setopts/2]).
-export([close/1]).
-export([controlling_process/2]).

%%% MACROS
-define(CONNECT_OPTS(Ip),
        case Ip of
            undefined -> [binary, {packet, 0}, {active, false}];
            _         -> [binary, {packet, 0}, {active, false}, {ip, Ip}]
        end).
-define(CONNECT_TIME, 30000).
-define(LISTEN_OPTS(Ip),
        case Ip of
            undefined ->
              [binary, {packet, 0}, {active, false}, {reuseaddr, true}];
            _ ->
              [binary, {packet, 0}, {active, false}, {reuseaddr, true}, {ip, Ip}]
        end).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
%% Computes the congestion state.
%%
%% - CongestionSt: Current ``congestion_state`` value.
%% - WaitTime: Are the microseconds waiting for the PDU.
%% - Timestamp: Represents the moment when the PDU was received.
%%
%% The time since ``Timestamp`` is the PDU dispatching time.  If
%% this value equals the ``WaitTime`` (i.e. ``DispatchTime/WaitTime = 1``),
%% then we shall assume optimum load (value 85).  Having this in mind the
%% instant congestion state value is calculated.  Notice this value cannot be
%% greater than 99.
congestion(CongestionSt, WaitTime, Timestamp) ->
    case (timer:now_diff(now(), Timestamp) div (WaitTime + 1)) * 85 of
        Val when Val < 1 ->
            0;
        Val when Val > 99 ->  % Out of bounds
            ((19 * CongestionSt) + 99) div 20;
        Val ->
            ((19 * CongestionSt) + Val) div 20
    end.


connect(Opts) ->
    Ip = proplists:get_value(ip, Opts),
    case proplists:get_value(sock, Opts, undefined) of
        undefined ->
            do_connect(Opts);
        Sock ->
            case setopts(Sock, ?CONNECT_OPTS(Ip)) of
                ok    -> {ok, Sock};
                Error -> Error
            end
    end.


listen(Opts) ->
    case proplists:get_value(lsock, Opts, undefined) of
        undefined ->
            do_listen(Opts);
        LSock ->
            Addr = proplists:get_value(addr, Opts, default_addr()),
            case proplists:get_value(setopts, Opts, true) of
                true ->
                    case setopts(LSock, ?LISTEN_OPTS(Addr)) of
                        ok ->
                            {ok, LSock};
                        Error ->
                            Error
                    end;
                false ->
                    {ok, LSock}
            end
    end.

transport(_Tag) ->
    T = get(oserl_transport),
    %io:format("~p ~p got transport ~p~n", [self(), _Tag, T]),
    T.

set_transport(Transport, _Tag) -> 
    %io:format("~p ~p putting transport ~p~n", [self(), _Tag, Transport]),
    put(oserl_transport, Transport).

spawn_link(M, F, A) ->
    Transport = transport(spawn_link),
    spawn_link(fun() ->
                       set_transport(Transport, spawn_link),
                       erlang:apply(M, F, A)
               end).

do_listen(Opts) ->
    MoreOpts = case proplists:get_value(ssl, Opts) of
        undefined -> [];
        SslOpts -> 
                       set_transport(oserl_ssl, listen),
                       SslOpts
    end,
    Addr = proplists:get_value(addr, Opts, default_addr()),
    Port = proplists:get_value(port, Opts, ?DEFAULT_SMPP_PORT),
    case transport(listen) of
        undefined ->
            gen_tcp:listen(Port, ?LISTEN_OPTS(Addr));
        Module ->
            Module:listen(Port, MoreOpts ++ ?LISTEN_OPTS(Addr))
    end.

do_connect(Opts) ->
    MoreOpts = case proplists:get_value(ssl, Opts) of
        undefined -> [];
        SslOpts -> 
                       set_transport(oserl_ssl, connect),
                       SslOpts
    end,
    Ip = proplists:get_value(ip, Opts),
    Addr = proplists:get_value(addr, Opts),
    Port = proplists:get_value(port, Opts, ?DEFAULT_SMPP_PORT),
    case transport(connect) of
        undefined ->
            gen_tcp:connect(Addr, Port, ?CONNECT_OPTS(Ip), ?CONNECT_TIME);
        Module ->
            Module:connect(Addr, Port, MoreOpts ++ ?CONNECT_OPTS(Ip), ?CONNECT_TIME)
    end.

accept(Socket) ->
    case transport(accept) of
        undefined ->
            gen_tcp:accept(Socket);
        Module ->
            Module:accept(Socket)
    end.

peername(Socket) ->
    case transport(peername) of
        undefined ->
            inet:peername(Socket);
        Module ->
            Module:peername(Socket)
    end.

setopts(Socket, Opts) ->
    case transport(setopts) of
        undefined ->
            inet:setopts(Socket, Opts);
        Module ->
            Module:setopts(Socket, Opts)
    end.

close(Socket) ->
    case transport(close) of
        undefined ->
            gen_tcp:close(Socket);
        Module ->
            Module:close(Socket)
    end.

controlling_process(Socket, NewOwner) ->
    case transport(controlling_process) of
        undefined ->
            gen_tcp:controlling_process(Socket, NewOwner);
        Module ->
            Module:controlling_process(Socket, NewOwner)
    end.

tcp_send(Sock, Data) when is_port(Sock) ->
    try erlang:port_command(Sock, Data) of
        true -> ok
    catch
        error:_Error -> {error, einval}
    end.


send_pdu(Sock, BinPdu, Log) when is_list(BinPdu) ->
    case tcp_send(Sock, BinPdu) of
        ok ->
            ok = smpp_log_mgr:pdu(Log, BinPdu);
        {error, Reason} ->
            gen_fsm:send_all_state_event(self(), {sock_error, Reason})
    end;


send_pdu(Sock, Pdu, Log) ->
    case smpp_operation:pack(Pdu) of
        {ok, BinPdu} ->
            send_pdu(Sock, BinPdu, Log);
        {error, _CmdId, Status, _SeqNum} ->
            close(Sock),
            exit({command_status, Status})
    end.

%%%-----------------------------------------------------------------------------
%%% SOCKET LISTENER FUNCTIONS
%%%-----------------------------------------------------------------------------
wait_accept(Pid, LSock, Log) ->
    case accept(LSock) of
        {ok, Sock} ->
            case handle_accept(Pid, Sock) of
                true ->
                    ?MODULE:recv_loop(Pid, Sock, <<>>, Log);
                false ->
                    close(Sock),
                    ?MODULE:wait_accept(Pid, LSock, Log)
            end;
        {error, Reason} ->
            gen_fsm:send_all_state_event(Pid, {listen_error, Reason})
    end.


wait_recv(Pid, Sock, Log) ->
    receive activate -> ?MODULE:recv_loop(Pid, Sock, <<>>, Log) end.


recv_loop(Pid, Sock, Buffer, Log) ->
    Timestamp = now(),
    setopts(Sock, [{active, once}]),
    receive
        {tcp, Sock, Input} ->
            L = timer:now_diff(now(), Timestamp),
            B = handle_input(Pid, list_to_binary([Buffer, Input]), L, 1, Log),
            ?MODULE:recv_loop(Pid, Sock, B, Log);
        {tcp_closed, Sock} ->
            gen_fsm:send_all_state_event(Pid, {sock_error, closed});
        {tcp_error, Sock, Reason} ->
            gen_fsm:send_all_state_event(Pid, {sock_error, Reason})
    end.

%%%-----------------------------------------------------------------------------
%%% TIMER FUNCTIONS
%%%-----------------------------------------------------------------------------
cancel_timer(undefined) ->
    false;
cancel_timer(Ref) ->
    gen_fsm:cancel_timer(Ref).


start_timer(#timers_smpp{response_time = infinity}, {response_timer, _}) ->
    undefined;
start_timer(#timers_smpp{response_time = infinity}, enquire_link_failure) ->
    undefined;
start_timer(#timers_smpp{enquire_link_time = infinity}, enquire_link_timer) ->
    undefined;
start_timer(#timers_smpp{session_init_time = infinity}, session_init_timer) ->
    undefined;
start_timer(#timers_smpp{inactivity_time = infinity}, inactivity_timer) ->
    undefined;
start_timer(#timers_smpp{response_time = Time}, {response_timer, _} = Msg) ->
    gen_fsm:start_timer(Time, Msg);
start_timer(#timers_smpp{response_time = Time}, enquire_link_failure) ->
    gen_fsm:start_timer(Time, enquire_link_failure);
start_timer(#timers_smpp{enquire_link_time = Time}, enquire_link_timer) ->
    gen_fsm:start_timer(Time, enquire_link_timer);
start_timer(#timers_smpp{session_init_time = Time}, session_init_timer) ->
    gen_fsm:start_timer(Time, session_init_timer);
start_timer(#timers_smpp{inactivity_time = Time}, inactivity_timer) ->
    gen_fsm:start_timer(Time, inactivity_timer).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
default_addr() ->
    {ok, Host} = inet:gethostname(),
    {ok, Addr} = inet:getaddr(Host, inet),
    Addr.


handle_accept(Pid, Sock) ->
    case peername(Sock) of
        {ok, {Addr, _Port}} ->
            gen_fsm:sync_send_event(Pid, {accept, Sock, Addr});
        {error, _Reason} ->  % Most probably the socket is closed
            false
    end.


handle_input(Pid, <<CmdLen:32, Rest/binary>> = Buffer, Lapse, N, Log) ->
    Now = now(), % PDU received.  PDU handling starts now!
    Len = CmdLen - 4,
    case Rest of
        <<PduRest:Len/binary-unit:8, NextPdus/binary>> ->
            BinPdu = <<CmdLen:32, PduRest/binary>>,
            case catch smpp_operation:unpack(BinPdu) of
                {ok, Pdu} ->
                    smpp_log_mgr:pdu(Log, BinPdu),
                    CmdId = smpp_operation:get_value(command_id, Pdu),
                    Event = {input, CmdId, Pdu, (Lapse div N), Now},
                    gen_fsm:send_all_state_event(Pid, Event);
                {error, _CmdId, _Status, _SeqNum} = Event ->
                    gen_fsm:send_all_state_event(Pid, Event);
                {'EXIT', _What} ->
                    Event = {error, 0, ?ESME_RUNKNOWNERR, 0},
                    gen_fsm:send_all_state_event(Pid, Event)
            end,
            % The buffer may carry more than one SMPP PDU.
            handle_input(Pid, NextPdus, Lapse, N + 1, Log);
        _IncompletePdu ->
            Buffer
    end;
handle_input(_Pid, Buffer, _Lapse, _N, _Log) ->
    Buffer.
