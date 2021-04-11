same(Parsed) :-
    isSame(Parsed, graph(toplevel,
                         [
                             node('A', [dEdge('A', 'D'), dEdge('A', 'B')]),
                             node('B', [dEdge('B', 'C')]),
                             node('C', [dEdge('C', 'A')]),
                             node('D', [dEdge('D', 'E')]),
                             node('E', [])
                         ],
                         [
                             dEdge('D', 'E'),
                             dEdge('A', 'D'),
                             dEdge('A', 'B'),
                             dEdge('B', 'C'),
                             dEdge('C', 'A')
                         ],
                         [
                             graph(noCycle,
                                   [
                                       node('D', [dEdge('D', 'E')]),
                                       node('E', [])
                                   ],
                                   [
                                       dEdge('D', 'E')
                                   ],
                                   []),
                             graph(cycle1,
                                   [
                                       node('A', [dEdge('A', 'B')]),
                                       node('B', [dEdge('B', 'C')]),
                                       node('C', [dEdge('C', 'A')])
                                   ],
                                   [
                                       dEdge('A', 'B'),
                                       dEdge('B', 'C'),
                                       dEdge('C', 'A')
                                   ],
                                  [])
                         ])).

isSame(A,A).
