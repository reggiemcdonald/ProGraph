same(Parsed) :-
    isSame(Parsed, 
        graph(toplevel, 
            [
                node('A', [dEdge('A', 'B')]), 
                node('B', [dEdge('B', 'A')]), 
                node('C', []), 
                node('D', [])], [dEdge('B', 'A'), dEdge('A', 'B')
            ], 
        [
            graph(subgraph1, 
                [
                    node('A', [dEdge('A', 'B')]), 
                    node('B', [dEdge('B', 'A')]), 
                    node('D', [])], [dEdge('A', 'B'), dEdge('B', 'A')
                ], 
            [])])
    ).

    isSame(A,A).