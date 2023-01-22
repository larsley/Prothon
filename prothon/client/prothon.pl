% Copyright (c) 2022-2023 by Lars Leyendecker
% 
% This program and the accompanying materials are made available under
% the terms of the Eclipse Public License v2.0 which accompanies this
% distribution and is available at:
% 
%     http://www.eclipse.org/legal/epl-2.0/
% 
% This program may also be made available under the following secondary
% licenses when the conditions for such availability set forth in the
% Eclipse Public License v2.0 are satisfied:
% 
%    GNU General Public License, Version 2.0, or any later versions of
%    that license
% 
% SPDX-License-Identifier: EPL-2.0 OR GPL-2.0-or-later
 
:- module(prothon, [pyconnect/0, pyconnect/1,
                       pydisconnect/0,
                       pyquery/1,
                       pycall/2, pycall/3, pycall/4,
                       pymethod/3, pymethod/4, pymethod/5,
                       pygarbage/1,
                       pykeep/1,
                       pyanswer/1,
                       pyderef/2,
                       pyderefall/2,
                       pyimport/1, pyimport/2
                       ]).

:- use_module(library(sockets)).
:- use_module(library(lists)).

:- use_module(grammar).

:- dynamic pyconnection/1.


% pycall(+Function, +Args)
pycall(Function, Args) :-
    pycall(Function, Args, dict([])).

% pycall(+Function, +Args, +KeywordArgs)
pycall(Function, Args, KeywordArgs) :-
    phrase(q_call(Function, Args, KeywordArgs), Query),
    pyquery(Query).

% pycall(+Function, +Args, +KeywordArgs, -Answer)
pycall(Function, Args, KeywordArgs, Answer) :-
    pycall(Function, Args, KeywordArgs),
    pyanswer(Answer).

% pymethod(+Object, +Method, +Args)
pymethod(Object, Method, Args) :-
    pymethod(Object, Method, Args, dict([])).

% pymethod(+Object, +Method, +Args, +KeywordArgs)
pymethod(Object, Method, Args, KeywordArgs) :-
    phrase(q_method(Object, Method, Args, KeywordArgs), Query),
    pyquery(Query).

% pymethod(+Object, +Method, +Args, +KeywordArgs, -Answer)
pymethod(Object, Method, Args, KeywordArgs, Answer) :-
    pymethod(Object, Method, Args, KeywordArgs),
    pyanswer(Answer).


% pyanswer(-Object)
pyanswer(Object) :-
    pyfetch(0, Object).

% pyderef(+Object, -Value)
pyderef(Object, Value) :-
    pypush(Object),
    pyfetch(1, Value).

% pyderefall(+Object, -Value)
pyderefall(Object, Value) :-
    pypush(Object),
    pyfetch(-1, Value).

% pygarbage(+Ids)
pygarbage(Ids) :-
    phrase(q_garbage(Ids), Query),
    pyquery(Query),
    pyfetch(1, _).

% pykeep(+Ids)
pykeep(Ids) :-
    phrase(q_keep(Ids), Query),
    pyquery(Query),
    pyfetch(1, _).

% pyimport(+Module)
pyimport(Module) :-
    pyimport(Module, Module).

% pyimport(+Module, +Alias)
pyimport(Module, Alias) :-
    phrase(q_import(Module, Alias), Query),
    pyquery(Query),
    pyfetch(1, _).

% pyconnect
% connects to a prothon server on the local host
pyconnect :-
    pyconnect('').

% pyconnect(+Address)
% connects to a prothon server on the specified address
% the address can be either an ip address or ip:port
pyconnect(Ip:Port) :-
    !,
    % if already connected, throw an exception
    (pyconnection(_) ->
        throw(prothon_exception('ConnectionAlreadyEstablished',
            'To close the old connection use pydisconnect/0'))
    ;
        true),
    % open the socket in latin-1 text mode.
    % eol(lf) prevents cr-lf to be read as one character 
    socket_client_open(Ip:Port, Stream, 
        [type(text), encoding('ISO-8859-1'), eol(lf)]),
    assert(pyconnection(Stream)).

pyconnect(Ip) :-
    pyconnect(Ip:44644).

% pydisconnect
pydisconnect :-
    % if a connection exists close it, otherwise throw an exception
    (prothon:pyconnection(Stream) ->
        close(Stream),
        retractall(prothon:pyconnection(_))
    ;
        throw(prothon_exception('NotConnected',
            'No connection to a prothon server is established'))).

% Not exported:


% pyconnection_protected(-Stream)
% throws an exception if no connection is established
pyconnection_protected(Stream) :-
    (prothon:pyconnection(Stream) ->
        true
    ;
        throw(prothon_exception('NotConnected',
            'No connection to a prothon server is established'))).


% pyquery(+Query)
% prepends the query with its length and writes it to the socket
pyquery(Query) :-
    pyconnection_protected(Stream),
    length(Query, Len),
    format(Stream, '~|~`0t~d~10+~s', [Len, Query]),
    flush_output(Stream).

% pyfetch(+Depth, -Object)
pyfetch(Depth, Object) :-
    % query the server for the object
    phrase(q_fetch(Depth), Query),
    pyquery(Query),
    % read the answer
    number_of_chars(N),
    get_n_chars(N, Answer), !,
    % parse it
    phrase(answer(Object, Err), Answer),
    % if an exception occured throw it
    (Err = tuple([string(ErrType), string(ErrMsg)]) ->
        atom_codes(ErrTypeAtom, ErrType),
        atom_codes(ErrMsgAtom, ErrMsg),
        throw(prothon_exception(ErrTypeAtom, ErrMsgAtom))
        ;
        true).

% pypush(+Object)
pypush(Object) :-
    phrase(q_push(Object), Query),
    pyquery(Query).

% number_of_chars(-N)
% reads the first 10 chars from the socket to deduce the message length
number_of_chars(N) :-
    get_n_chars(10, Chars),
    number_codes(N, Chars).

% get_n_chars(+N, -Chars)
% reads N chars from the socket
get_n_chars(N, Chars) :-
    pyconnection_protected(Stream), !,
    phrase(get_n_chars(N, Stream), Chars).

get_n_chars(0, _) --> !, [].
get_n_chars(N, Stream) --> 
    {get_code(Stream, C), N1 is N - 1},
    [C],
    get_n_chars(N1, Stream).
