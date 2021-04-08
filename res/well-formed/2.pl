same(Parsed) :-
    isSame(Parsed, graph(toplevel, [node('A', [dEdge('A', 'B')]), node('B', []), node('C', []), node('D', [])], [dEdge('A', 'B')], [])).

isSame(A,A).