same(Parsed) :-
    isSame(Parsed, 
        graph(toplevel, 
            [
                node('A', [dEdge('A', 'B'),dEdge('A','C')]), 
                node('B', [dEdge('B', 'A')]), 
                node('C', [dEdge('C','A')]), 
                node('D', [])], [dEdge('B', 'A'), dEdge('A', 'B'),dEdge('A','C'),dEdge('C','A')
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