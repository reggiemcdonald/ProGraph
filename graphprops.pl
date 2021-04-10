
module(cycle).

% Cycle is true when GRAPH contains a cycle.
cycle(graph(_, [F|R], _, _)) :-
    hasCycle(F, [F|R], []).

% hasCycle is true when Name is in Visited
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
