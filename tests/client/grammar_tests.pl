:- use_module(library(plunit)).
:- use_module(library(lists)).
:- use_module(library(avl)).

:- use_module(grammar).

benchmark(Pred) :-
    profile_reset,
    prolog_flag(profiling,_,on),
    call(Pred),
    prolog_flag(profiling,_,off).

translate_call(Function, Args, QueryTest) :-
     phrase(q_call(Function, Args, dict([])), Query),
     Query == QueryTest.

translate_answer(String, TranslatedTest, ErrorTest) :-
     atom_codes(String, Codes),
     phrase(answer(Translated, Error), Codes),
     Translated == TranslatedTest,
     Error == ErrorTest.

:- begin_tests(grammar_answer).

test('float123.456') :-
    translate_answer('0123.456', 123.456, success).

test('float1.0') :-
    translate_answer('01.0', 1.0, success).

test(int1234567890) :-
    translate_answer('01234567890', 1234567890, success).

test(int1) :-
    translate_answer('01', 1, success).

test(int_list) :-
    translate_answer('0[1]', [1], success).

test(empty_list) :-
    translate_answer('0[]', [], success).

test(list_recursive_deep) :-
    translate_answer('0[[[[[[[[[[[[[[[[[]]]]]]]]]]]]]]]]]',
              [[[[[[[[[[[[[[[[[]]]]]]]]]]]]]]]]], success).

test(list_recursive_empty_2) :-
    translate_answer('0[[],[]]',
              [[], []], success).

test(list_recursive_empty) :-
    translate_answer('0[[],[[[],[]],[],[],[[],[]]]]',
              [[], [[[], []], [], [], [[], []]]], success).

test(string_simple) :-
    translate_answer('0"this is a simple string"', string("this is a simple string"), success).

test(string_escapes) :-
    translate_answer('0"Escaped \\\\ and \\""', string("Escaped \\ and \""), success).

test(string_empty) :-
    translate_answer('0""', string(""), success).

test(id) :-
    translate_answer('0py[123089]', py(123089), success).

test(everything) :-
    translate_answer('0["string",42,13.37,([],"str",[py[1337],py[42]])]', 
              [string("string"), 42, 13.37, tuple([[], string("str"), [py(1337), py(42)]])], success).

:- end_tests(grammar_answer).



:- begin_tests(grammar_query).

test(print_empty) :-
    translate_call(print, [], "call,print,([],{})"). 

test(print_1) :-
    translate_call(print, [1], "call,print,([1],{})"). 

test('print_13.37') :-
    translate_call(print, [13.37], "call,print,([13.37],{})"). 

test(print_tuple) :-
    translate_call(print, [tuple([1,2])], "call,print,([(1,2)],{})"). 

test(print_nested_list) :-
    translate_call(print, [[1, [2,3]]], "call,print,([[1,[2,3]]],{})"). 

test(print_hello_world) :-
    translate_call(print, [string("Hello World!")], "call,print,([\"Hello World!\"],{})"). 

test(print_escape) :-
    translate_call(print, [string("escape \" and \\")], "call,print,([\"escape \\\" and \\\\\"],{})"). 

test(print_object) :-
    translate_call(print, [py(101)], "call,print,([py[101]],{})").
    
test(print_dict_simple) :-
    translate_call(print, [dict([first(string("test")), second(2)])], "call,print,([{\"first\":\"test\",\"second\":2}],{})").
    
test(print_dict_simple_list) :-
    translate_call(print, [dict([first([1, 2, 3]), second([[], []])])], "call,print,([{\"first\":[1,2,3],\"second\":[[],[]]}],{})").

test(print_dict_avl_empty) :-
    empty_avl(Tree),
    translate_call(print, [Tree], "call,print,([{}],{})").

test(print_dict_avl_single) :-
    empty_avl(Tree),
    avl_store(1, Tree, 10, Tree2),
    translate_call(print, [Tree2], "call,print,([{1:10}],{})").

test(print_dict_avl_double) :-
    empty_avl(Tree),
    avl_store(1, Tree, 10, Tree2),
    avl_store(2, Tree2, 18, Tree3),
    translate_call(print, [Tree3], "call,print,([{1:10,2:18}],{})").

test(print_dict_avl_tuple) :-
    empty_avl(Tree),
    avl_store(tuple([8, 4]), Tree, tuple([1, 2]), Tree2),
    translate_call(print, [Tree2], "call,print,([{(8,4):(1,2)}],{})").

test(print_dict_avl_string) :-
    empty_avl(Tree),
    avl_store(string("key"), Tree, string("value"), Tree2),
    translate_call(print, [Tree2], "call,print,([{\"key\":\"value\"}],{})").

test(print_dict_avl_bool) :-
    empty_avl(Tree),
    avl_store(true, Tree, false, Tree2),
    translate_call(print, [Tree2], "call,print,([{True:False}],{})").

test(print_dict_avl_obj) :-
    empty_avl(Tree),
    avl_store(py(1337), Tree, py(42), Tree2),
    translate_call(print, [Tree2], "call,print,([{py[1337]:py[42]}],{})").

test(print_dict_avl_recursive) :-
    empty_avl(Tree),
    avl_store(true, Tree, false, Tree2),
    empty_avl(Tree10),
    avl_store(10, Tree10, Tree2, Tree11),
    translate_call(print, [Tree11], "call,print,([{10:{True:False}}],{})").

:- end_tests(grammar_query).

:- begin_tests(messages).

test(call) :-
    phrase(q_call(test, [1], dict(['2'(3)])), Msg),
    Msg == "call,test,([1],{\"2\":3})".

test(method) :-
    phrase(q_method(py(12349876), test, [1], dict(['2'(3)])), Msg),
    Msg == "method,py[12349876],test,([1],{\"2\":3})".

test(garbage0) :-
    phrase(q_garbage([]), Msg),
    Msg == "del,[]".

test(garbage1) :-
    phrase(q_garbage([py(12349876)]), Msg),
    Msg == "del,[12349876]".

test(garbage2) :-
    phrase(q_garbage([py(12349876), py(10101010)]), Msg),
    Msg == "del,[12349876,10101010]".

test(keep0) :-
    phrase(q_keep([]), Msg),
    Msg == "keep,[]".

test(keep1) :-
    phrase(q_keep([py(12349876)]), Msg),
    Msg == "keep,[12349876]".

test(keep2) :-
    phrase(q_keep([py(12349876), py(10101010)]), Msg),
    Msg == "keep,[12349876,10101010]".

test(import) :-
    phrase(q_import(numpy, np), Msg),
    Msg == "import,numpy,np".

test(import_dot) :-
    phrase(q_import('matplotlib.pyplot', plt), Msg),
    Msg == "import,matplotlib.pyplot,plt".

test(fetch_0) :-
    phrase(q_fetch(0), Msg),
    Msg == "fetch,0".

test(fetch_1) :-
    phrase(q_fetch(1), Msg),
    Msg == "fetch,1".

test('fetch_-1') :-
    phrase(q_fetch(-1), Msg),
    Msg == "fetch,-1".

test(push) :-
    phrase(q_push(py(1357924680)), Msg),
    Msg == "push,1357924680".

:- end_tests(messages).

:- begin_tests(individual).

test(number_10) :-
    phrase(grammar:num(10), Str),
    Str == "10",
    phrase(grammar:num(X), Str),
    X == 10.

test(number_0) :-
    phrase(grammar:num(0), Str),
    Str == "0",
    phrase(grammar:num(X), Str),
    X == 0.

test('number_-12') :-
    phrase(grammar:num(-12), Str),
    Str == "-12",
    phrase(grammar:num(X), Str),
    X == -12.

test('number_57.93') :-
    phrase(grammar:num(57.93), Str),
    Str == "57.93",
    phrase(grammar:num(X), Str),
    X == 57.93.

test('number_-57.93') :-
    phrase(grammar:num(-57.93), Str),
    Str == "-57.93",
    phrase(grammar:num(X), Str),
    X == -57.93.

test(list_empty) :-
    phrase(grammar:list([]), Str),
    Str == "[]",
    phrase(grammar:list(X), Str),
    X == [].

test(list_simple) :-
    phrase(grammar:list([1, 5, 6]), Str),
    Str == "[1,5,6]",
    phrase(grammar:list(X), Str),
    X == [1, 5, 6].

test(list_recursive) :-
    phrase(grammar:list([[1], [[2], [3]]]), Str),
    Str == "[[1],[[2],[3]]]",
    phrase(grammar:list(X), Str),
    X == [[1], [[2], [3]]].

test(list_recursive2) :-
    phrase(grammar:list([[1], [[2, []], [3]]]), Str),
    Str == "[[1],[[2,[]],[3]]]",
    phrase(grammar:list(X), Str),
    X == [[1], [[2, []], [3]]].

test(tuple_empty) :-
    phrase(grammar:tuple(tuple([])), Str),
    Str == "()",
    phrase(grammar:tuple(X), Str),
    X == tuple([]).

test(tuple_simple) :-
    phrase(grammar:tuple(tuple([1, 5, 6])), Str),
    Str == "(1,5,6)",
    phrase(grammar:tuple(X), Str),
    X == tuple([1, 5, 6]).

test(tuple_recursive) :-
    phrase(grammar:tuple(tuple([tuple([1]), tuple([tuple([2]), tuple([3])])])), Str),
    Str == "((1),((2),(3)))",
    phrase(grammar:tuple(X), Str),
    X == tuple([tuple([1]), tuple([tuple([2]), tuple([3])])]).

test(tuple_recursive2) :-
    phrase(grammar:tuple(tuple([tuple([1]), tuple([tuple([2, tuple([])]), tuple([3])])])), Str),
    Str == "((1),((2,()),(3)))",
    phrase(grammar:tuple(X), Str),
    X == tuple([tuple([1]), tuple([tuple([2, tuple([])]), tuple([3])])]). 

test(referece) :-
    phrase(grammar:id(py(14253748)), Str),
    Str == "py[14253748]",
    phrase(grammar:id(X), Str),
    X == py(14253748).

test(bool_true) :-
    phrase(grammar:bool(true), Str),
    Str == "True",
    phrase(grammar:bool(X), Str),
    X == true.

test(bool_false) :-
    phrase(grammar:bool(false), Str),
    Str == "False",
    phrase(grammar:bool(X), Str),
    X == false.

test(none) :-
    phrase(grammar:none(none), Str),
    Str == "None",
    phrase(grammar:none(X), Str),
    X == none.

:- end_tests(individual).

:- begin_tests(object).

test(object_number_10) :-
    phrase(grammar:object(10), Str),
    Str == "10",
    phrase(grammar:object(X), Str),
    X == 10.

test(object_number_0) :-
    phrase(grammar:object(0), Str),
    Str == "0",
    phrase(grammar:object(X), Str),
    X == 0.

test('object_number_-12') :-
    phrase(grammar:object(-12), Str),
    Str == "-12",
    phrase(grammar:object(X), Str),
    X == -12.

test('object_number_57.93') :-
    phrase(grammar:object(57.93), Str),
    Str == "57.93",
    phrase(grammar:object(X), Str),
    X == 57.93.

test('object_number_-57.93') :-
    phrase(grammar:object(-57.93), Str),
    Str == "-57.93",
    phrase(grammar:object(X), Str),
    X == -57.93.

test(object_list_empty) :-
    phrase(grammar:object([]), Str),
    Str == "[]",
    phrase(grammar:object(X), Str),
    X == [].

test(object_list_simple) :-
    phrase(grammar:object([1, 5, 6]), Str),
    Str == "[1,5,6]",
    phrase(grammar:object(X), Str),
    X == [1, 5, 6].

test(object_list_recursive) :-
    phrase(grammar:object([[1], [[2], [3]]]), Str),
    Str == "[[1],[[2],[3]]]",
    phrase(grammar:object(X), Str),
    X == [[1], [[2], [3]]].

test(object_list_recursive2) :-
    phrase(grammar:object([[1], [[2, []], [3]]]), Str),
    Str == "[[1],[[2,[]],[3]]]",
    phrase(grammar:object(X), Str),
    X == [[1], [[2, []], [3]]].

test(object_tuple_empty) :-
    phrase(grammar:object(tuple([])), Str),
    Str == "()",
    phrase(grammar:object(X), Str),
    X == tuple([]).

test(object_tuple_simple) :-
    phrase(grammar:object(tuple([1, 5, 6])), Str),
    Str == "(1,5,6)",
    phrase(grammar:object(X), Str),
    X == tuple([1, 5, 6]).

test(object_tuple_recursive) :-
    phrase(grammar:object(tuple([tuple([1]), tuple([tuple([2]), tuple([3])])])), Str),
    Str == "((1),((2),(3)))",
    phrase(grammar:object(X), Str),
    X == tuple([tuple([1]), tuple([tuple([2]), tuple([3])])]).

test(object_tuple_recursive2) :-
    phrase(grammar:object(tuple([tuple([1]), tuple([tuple([2, tuple([])]), tuple([3])])])), Str),
    Str == "((1),((2,()),(3)))",
    phrase(grammar:object(X), Str),
    X == tuple([tuple([1]), tuple([tuple([2, tuple([])]), tuple([3])])]). 

test(object_referece) :-
    phrase(grammar:object(py(14253748)), Str),
    Str == "py[14253748]",
    phrase(grammar:object(X), Str),
    X == py(14253748).

test(object_bool_true) :-
    phrase(grammar:object(true), Str),
    Str == "True",
    phrase(grammar:object(X), Str),
    X == true.

test(object_bool_false) :-
    phrase(grammar:object(false), Str),
    Str == "False",
    phrase(grammar:object(X), Str),
    X == false.

test(object_none) :-
    phrase(grammar:object(none), Str),
    Str == "None",
    phrase(grammar:object(X), Str),
    X == none.

:- end_tests(object).

