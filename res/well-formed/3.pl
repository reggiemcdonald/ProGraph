same(Parsed) :-
    isSame(Parsed, graph(toplevel,
    [
        node("Y",[dEdge("Y","Z")]),
        node("Z",[dEdge("Z","A")]),
        node("A",[dEdge("A","B")]),
        node("B",[dEdge("B","A")]),
        node("C",[])
    ],
    [
        dEdge("Y","Z"),
        dEdge("Z","A"),
        dEdge("A","B"),
        dEdge("B","A")
    ],
    [
        graph(ex1,
            [
                node("A",[dEdge("A","B")]),
                node("B",[dEdge("B","A")]),
                node("C",[])
            ],
            [
                dEdge("A","B"),
                dEdge("B","A")
            ],[])
    ])).


isSame(A,A).