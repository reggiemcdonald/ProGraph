%
% Graph is graph(NAME, [NODE], [EDGE], [GRAPH])
% Node is node(NAME, [EDGE])
%
%Checking if a graph has a cycle

module(cycle).

cycle(graph(_, [F|R], _, _)) :-
    hasCycle(F, [F|R], []).


hasCycle(node(Name, _), _, Visited) :-
    member(Name, Visited),
    !.
hasCycle(node(Name, [dEdge(From, To)|Rest]), Nodes, Visited) :-
    findNode(To, Nodes, node(To, Edges)),
    hasCycle(node(To, Edges), Nodes, [Name|Visited]);
    hasCycle(node(Name, Rest), Nodes, Visited).

findNode(Name, [node(Name,Edges)|Rest], node(Name,Edges)).
findNode(Name, [node(Other,_)|Rest], N) :-
    not(Name == Other),
    findNode(Name, Rest, N).
