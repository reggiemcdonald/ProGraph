% This module provides measurements on the graph

:- module(integerInvariants, [order/2, size/2, kosarajus/2]).
:- use_module(graphrepresentation).
:- use_module(graphutil).

% order is true when graph Name has Order vertices.
% See 
order(Name, Order) :-
    graph(Name, Nodes, _, _),
    length(Nodes, Order).

% size is true when graph Name has Size edges.
size(Name, Size) :-
    graph(Name, _, Edges, _),
    length(Edges, Size).

% Kosaraju's algorithm adapted from https://en.wikipedia.org/wiki/Kosaraju%27s_algorithm
kosarajus(Name, FilteredAssignments) :-
    graph(Name, Nodes, Edges, _),
    % mark each vertex unvisited
    unvisit(Nodes, [H|T]),
    names(Nodes, Names),
    kNext(Names, [H|T], _, L),
    reverseEdges(Edges,ReverseEdges),
    generateNodes(Names, ReverseEdges, ReverseNodes),
    unvisit(ReverseNodes, UnvisitedReverseNodes),
    assign(L, UnvisitedReverseNodes, _, Assignments),
    filterEmpties(Assignments, FilteredAssignments).

% kNext is true when all nodes have been visited.
kNext([],_,_,[]).
kNext([H|T], AllNodes, UpdatedNodes2, L) :-
    kRun(H, AllNodes, UpdatedNodes, L1),
    kNext(T, UpdatedNodes, UpdatedNodes2, L2),
    append(L2, L1, L).

% kRun is true when a strongly connected component has been traversed.
kRun(Name, AllNodes, UpdatedNodes2, [Name|L]) :-
    findKNode(Name, AllNodes, kNode(node(Name, Edges), unvisited)),
    markAsVisited(Name, AllNodes, _, UpdatedNodes1),
    visitEachNeighbour(Edges, UpdatedNodes1, UpdatedNodes2, L).
kRun(Name, AllNodes, AllNodes, []) :-
    findKNode(Name, AllNodes, kNode(node(Name, _), visited)).

% markAsVisited is true when node Name has been set to visited.
markAsVisited(Name, [kNode(node(N, E), V)|T], VisitedNode, [kNode(node(N, E),V)|UpdatedNodes]) :-
    not(Name == N),
    markAsVisited(Name, T, VisitedNode, UpdatedNodes).
markAsVisited(Name, [kNode(node(Name, Edges), unvisited)|T], kNode(node(Name, Edges), visited), [kNode(node(Name, Edges), visited)|T]).

% visitEachNeighbour is ture when all nodes have been visited
visitEachNeighbour([], AllNodes, AllNodes, []).
visitEachNeighbour([dEdge(_, To)|RestEdges], AllNodes, UpdatedNodes, L) :-
    findKNode(To, AllNodes, kNode(node(Name, _), _)),
    kRun(Name, AllNodes, UpdatedNodes1, L1),
    visitEachNeighbour(RestEdges, UpdatedNodes1, UpdatedNodes, L2),
    append(L2, L1, L).

% findKNode is true when a kNode of name Name has been found.
findKNode(Name, [kNode(node(Name, Edges),Visited)|_], kNode(node(Name, Edges),Visited)).
findKNode(Name, [kNode(node(N, _),_)|Rest], Node) :-
    not(Name == N),
    findKNode(Name, Rest, Node).

% unvisit is true when all nodes have been copied to unvisited kNodes
unvisit([],[]).
unvisit([node(Name, Edges)|T], [kNode(node(Name, Edges), unvisited)|Progress]) :-
    unvisit(T, Progress).

% names is true when the names of all nodes have been collected
names([],[]).
names([node(Name,_)|Rest], [Name|RestNames]) :-
    names(Rest, RestNames).

% reverseEdges is true when the graph has been transposed
reverseEdges([],[]).
reverseEdges([dEdge(From, To)|T], [dEdge(To,From)|Rest]) :- reverseEdges(T, Rest).

% assign is true when all nodes have been assigned to a strongly connected component
assign([],_,_,[]).
assign([H|T], AllNodes, UpdatedNodes2, [L1|L2]) :-
    runAssign(H, AllNodes, UpdatedNodes, L1),
    assign(T, UpdatedNodes, UpdatedNodes2, L2).

% runAssign is true when a node and its neighbours have been assigned to a connected component.
runAssign(Name, AllNodes, UpdatedNodes2, [Name|L]) :-
    findKNode(Name, AllNodes, kNode(node(Name, Edges), unvisited)),
    markAsVisited(Name, AllNodes, _, UpdatedNodes1),
    runVisitEachNeighbour(Edges, UpdatedNodes1, UpdatedNodes2, L).
runAssign(Name, AllNodes, AllNodes, []) :-
    findKNode(Name, AllNodes, kNode(node(Name,_), visited)).

% runVisitEachNeighbour is true when all neighbouring nodes have been assigned to a connected component.
runVisitEachNeighbour([], AllNodes, AllNodes, []).
runVisitEachNeighbour([dEdge(_,To)|RestEdges], AllNodes, UpdatedNodes, L) :-
    findKNode(To, AllNodes, kNode(node(Name, _), _)),
    runAssign(Name, AllNodes, UpdatedNodes1, L1),
    runVisitEachNeighbour(RestEdges, UpdatedNodes1, UpdatedNodes, L2),
    append(L2, L1, L).

% filterEmpties is true when all empty lists have been removed.
filterEmpties([],[]).
filterEmpties([H|T], Rest) :- length(H,0), filterEmpties(T, Rest).
filterEmpties([H|T], [H|Rest]) :- length(H,L), not(L==0), filterEmpties(T, Rest).

