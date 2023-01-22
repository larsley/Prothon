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
 
:- module(grammar, [answer/4,
                    q_call/5,
                    q_garbage/3,
                    q_keep/3,
                    q_import/4,
                    q_fetch/3,
                    q_push/3,
                    q_method/6]).

:- use_module(library(avl)).

% Exported rules

% answer(-Object, -Error)
% Parses an answer from the server
% If an exception was thrown, Object remains uninstantiated.
% Error is unified either with 'success' or the exception-tuple.
answer(Object, success) --> "0", !, object(Object).
answer(_, Error) --> "1", !, object(Error).

% q_call(+Function, +Args, +KeywordArgs)
% Generates a call query.
% Function must be an atom.
% Args must be a list.
% KeywordArgs must be a prothon dictionary.
q_call(Function, Args, KeywordArgs) -->
    "call,",
    atomstring(Function), ",",
    tuple(tuple([Args, KeywordArgs])).

% q_method(+Object, +Method, +Args, +KeywordArgs)
% Generates a query to call Method on the Object.
% Object must be a prothon reference.
% Method must be an atom.
% Args must be a list.
% KeywordArgs must be a prothon dictionary.
q_method(Object, Method, Args, KeywordArgs) -->
    "method,",
    id(Object), ",",
    atomstring(Method), ",",
    tuple(tuple([Args, KeywordArgs])).

% q_garbage(+Ids)
% Generates a garbage-collector query, that deletes all Ids.
% Ids must be a list of prothon references
q_garbage(Ids) --> "del,", id_list(Ids).

% q_keep(+Ids)
% Generates a garbage-collector query, that deletes everything except the Ids.
% Ids must be a list of prothon references
q_keep(Ids) --> "keep,", id_list(Ids).

% q_import(+Module, +Alias)
% Generates a query to import the Module with the Alias.
% Module and Alias have to be atoms.
q_import(Module, Alias) -->
    "import,",
    atomstring(Module), ",",
    atomstring(Alias).

% q_fetch(+Depth)
% Generates a fetch query, dereferencing to Depth
q_fetch(Depth) --> "fetch,", {number_codes(Depth, Codes)}, Codes.

% q_push(+Object)
% Generates a query to push Object on the answer stack.
% Object must be a prothon reference.
q_push(py(ID)) --> "push,", num(ID).

% Auxillary rules

object(X) --> list(X), !.
object(X) --> tuple(X), !.
object(X) --> dict(X), !.
object(X) --> num(X), !.
object(X) --> string(X), !.
object(X) --> id(X), !.
object(X) --> bool(X), !.
object(X) --> none(X).

id(py(Id)) --> "py[", num(Id), "]".

none(none) --> "None".

bool(true) --> "True", !.
bool(false) --> "False".

string(string(S)) --> "\"", !, string_content(S).

% \ and " are escaped
string_content([]) --> "\"", !.
string_content([92|T]) --> "\\\\", !, string_content(T).
string_content([34|T]) --> "\\\"", !, string_content(T).
string_content([H|T]) --> [H], string_content(T).

% turns a list of prothon references into a list of their ids
id_list(X) --> "[", ids(X), "]".

ids([]) --> "".
ids([py(H)]) --> num(H), !.
ids([py(H)|T]) --> num(H), ",", ids(T).

% lists and tuples are the same except their brackets
list(L) --> "[", !, elements(L), "]".
tuple(tuple(L)) --> "(", !, elements(L), ")".

elements([H|T]) --> object(H), !, elements_remainder(T).
elements([]) --> []. 

elements_remainder([H|T]) --> ",", !, object(H), elements_remainder(T).
elements_remainder([]) --> [].

% A dictionaries can be either an AVL tree or a list of key(value) pairs
% AVL tree
dict(AVL) --> 
    {var(AVL)}, "{", !, avl_pairs(List), "}", {list_to_avl(List, AVL)}. 
dict(AVL) --> {avl_to_list(AVL, List)}, !, "{", avl_pairs(List), "}".
% List of key(value) pairs
dict(dict(X)) --> "{", dict_elements(X), "}".

dict_elements([H|T]) --> dict_element(H), !, dict_elements_remainder(T).
dict_elements([]) --> [].

dict_elements_remainder([H|T]) --> 
    ",", !, dict_element(H), dict_elements_remainder(T).
dict_elements_remainder([]) --> [].

% Key(Value) --> "Key:Value"
dict_element(X) -->
    {ground(X), X =.. [Key, Value], atom_codes(Key, KeyCodes)}, !,
    string(string(KeyCodes)), ":", object(Value).

avl_pairs([H|T]) --> key_value_pair(H), !, avl_pairs_remainder(T).
avl_pairs([]) --> [].

avl_pairs_remainder([H|T]) --> 
    ",", !, key_value_pair(H), avl_pairs_remainder(T).
avl_pairs_remainder([]) --> [].

% K-V --> "K:V"
key_value_pair(K-V) --> object(K), ":", object(V).

% if X is instantiated, it gets translated by number_codes
num(X) --> {number(X), !, number_codes(X, C)}, C.
% if X is uninstantiated and negative, a "-" is prepended
num(X) --> "-", !, num([H|T], 0), !, {number_codes(X, [0x2D, H|T])}.
% if X is uninstantiated and positive
num(X) --> num([H|T], 0), !, {number_codes(X, [H|T])}.

% reads a number. The second argument signifies if a dot was already read.
num([H|T], X) --> digit(H), !, num(T, X).
num([0x2E, H2|T], 0) --> ".", !, num([H2|T], 1).
num([], _) --> [].

digit(X) --> [X], {ground(X), X =< 0x39, X >= 0x30}.

% auxillary rule to turn an atom into its string representation
atomstring(X) --> {atom_codes(X, C)}, C.

