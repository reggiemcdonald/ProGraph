

% learned about plunit from https://stackoverflow.com/questions/57263196/unit-tests-in-swi-prolog-visibility-of-user-predicates-from-within-a-module
:- begin_tests(cycle, [setup(loadTestGraph(_))]).
:- include(graphload).
:- include(cycle).

loadTestGraph(Graph) :-
    write('% PL-Unit: loading test graph...\n'),
    working_directory(Cwd, ''),
    atom_concat(Cwd, '/res/well-formed/4.json', FilePath),
    buildGraphFromFile(FilePath, Graph),
    write('% PL-Unit: test graph loaded.\n').


test(subgraphWithNoCycle) :-
    not(cycle(noCycle)).

test(subgraphWithCycle) :- 
    cycle(cycle1).

test(toplevelWithCycle) :-
    cycle(toplevel).
