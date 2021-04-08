% learned about plunit from https://stackoverflow.com/questions/57263196/unit-tests-in-swi-prolog-visibility-of-user-predicates-from-within-a-module
:- begin_tests(integerInvariants, [setup(loadTestGraph(_))]).
:- include(integerInvariants).

loadTestGraph(Graph) :-
    write('% PL-Unit: loading test graph...\n'),
    working_directory(Cwd, ''),
    atom_concat(Cwd, '/res/well-formed/1.json', FilePath),
    buildGraphFromFile(FilePath, Graph),
    write('% PL-Unit: test graph loaded.\n').

test(order1, [true(Order =:= 4)]) :-
    order(toplevel, Order).

test(order2, [true(Order =:= 3)]) :-
    order(subgraph1, Order).

test(order3) :-
    not(order(subgraph2, _)).

test(size1, [true(Size =:= 2)]) :-
    size(toplevel, Size).

test(size2, [true(Size =:= 2)]) :-
    size(subgraph1, Size).

test(size3) :-
    not(size(subgraph2, _)).

:- end_tests(integerInvariants).
    