

% learned about plunit from https://stackoverflow.com/questions/57263196/unit-tests-in-swi-prolog-visibility-of-user-predicates-from-within-a-module
:- begin_tests(graphrepresentation, [setup(loadTestGraph(_))]).
:- include(graphrepresentation).

loadTestGraph(Graph) :-
    write('% PL-Unit: loading test graph...\n'),
    working_directory(Cwd, ''),
    atom_concat(Cwd, '/res/well-formed/1.json', FilePath),
    buildGraphFromFile(FilePath, Graph),
    write('% PL-Unit: test graph loaded.\n').


test(cycle1) :-
             cycle(toplevel).
