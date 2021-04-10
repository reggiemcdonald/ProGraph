% learned about plunit from https://stackoverflow.com/questions/57263196/unit-tests-in-swi-prolog-visibility-of-user-predicates-from-within-a-module
:- begin_tests(graphrepresentation, [setup(loadTestGraph(_))]).
:- include(graphrepresentation).

loadTestGraph(Graph) :-
    write('% PL-Unit: loading test graph...\n'),
    working_directory(Cwd, ''),
    atom_concat(Cwd, '/res/well-formed/3.json', FilePath),
    buildGraphFromFile(FilePath, Graph),
    write('% PL-Unit: test graph loaded.\n').

test(distance1, [true(Distance =:= 4)]) :-
    distance(toplevel,'Y','B',Distance).

test(distance2, [true(Distance =:= 2)]) :-
    distance(toplevel,'A','B',Distance).

test(path1, [true(Path == ['Y','Z','A','B'])]) :-
    path(toplevel, 'Y','B',Path).

test(ecc1, [true(Ecc =:= 4)]) :-
    eccentricity(toplevel, Ecc).

% test(order3) :-
%     not(order(subgraph2, _)).

% test(size1, [true(Size =:= 2)]) :-
%     size(toplevel, Size).

% test(size2, [true(Size =:= 2)]) :-
%     size(subgraph1, Size).

% test(size3) :-
%     not(size(subgraph2, _)).

same([H1|R1], [H2|R2]):-
    H1 = H2,
    same(R1, R2).

:- end_tests(graphrepresentation).