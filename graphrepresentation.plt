% learned about plunit from https://stackoverflow.com/questions/57263196/unit-tests-in-swi-prolog-visibility-of-user-predicates-from-within-a-module
:- begin_tests(graphrepresentation, [setup(loadTestGraph(_))]).
:- include(graphrepresentation).

loadTestGraph(Graph) :-
    write('% PL-Unit: loading test graph...\n'),
    working_directory(Cwd, ''),
    atom_concat(Cwd, '/res/well-formed/3.json', FilePath),
    buildGraphFromFile(FilePath, Graph),
    write('% PL-Unit: test graph loaded.\n').

test(distance1, [true(Distance =:= 5)]) :-
    distance(toplevel,"Y","B",Distance).

test(distance2, [true(Distance =:= 1)]) :-
    distance(toplevel,"A","B",Distance).

test(path1, [true(Path =:= ["Y","Z","A","B"])]) :-
    path(toplevel1, "Y","B",Path).

% test(order3) :-
%     not(order(subgraph2, _)).

% test(size1, [true(Size =:= 2)]) :-
%     size(toplevel, Size).

% test(size2, [true(Size =:= 2)]) :-
%     size(subgraph1, Size).

% test(size3) :-
%     not(size(subgraph2, _)).

:- end_tests(graphrepresentation).