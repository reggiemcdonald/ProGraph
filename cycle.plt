% This should return true

cycle(graph(ex1,
        [
            node(a,[dEdge(a,b), dEdge(a,c)]),
            node(b,[]),
            node(c,[dEdge(c,a)])
        ],
        [
            dEdge(a,b),
            dEdge(b,a)
        ], [])).
