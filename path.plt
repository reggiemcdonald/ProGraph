% learned about plunit from https://stackoverflow.com/questions/57263196/unit-tests-in-swi-prolog-visibility-of-user-predicates-from-within-a-module
:- begin_tests(path, [setup(loadTestGraph(_))]).
:- use_module(path).
:- use_module(graphload).

loadTestGraph(Graph) :-
    write('% PL-Unit: loading test graph...\n'),
    working_directory(Cwd, ''),
    atom_concat(Cwd, '/res/well-formed/3.json', FilePath),
    buildGraphFromFile(FilePath, Graph),
    write('% PL-Unit: test graph loaded.\n').

test(distance1, [true(Distance =:= 4)]) :-
    distance(toplevel,'Y','B',Distance),!.

test(distance2, [true(Distance =:= 2)]) :-
    distance(toplevel,'A','B',Distance),!.

test(path1, [true(Path == ['Y','Z','A','B'])]) :-
    path(toplevel, 'Y','B',Path),!.

test(path2, [true(Path == ['A','B'])]) :-
    path(toplevel, 'A','B',Path),!.

test(ecc1, [true(Ecc =:= 4)]) :-
    eccentricity(toplevel, Ecc),!.

:- end_tests(path).