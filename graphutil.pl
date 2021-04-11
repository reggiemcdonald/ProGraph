
:- module(graphutil, [findNode/3, generateNodes/3, getAllEdgesFrom/3]).
:- use_module(graphrepresentation).

% findNode is true when a node of name Name is present in the list of nodes.
findNode(Name, [node(Name,Edges)|_], node(Name,Edges)).
findNode(Name, [node(N,_)|T], R) :-
    not(Name == N),
    findNode(Name, T, R).

% generateNodes is true when all the nodes have been created with all their edges.
generateNodes([],_,[]).
generateNodes([H|T], Edges, [node(H, EdgesOfNode)|Rest]) :-
    getAllEdgesFrom(H, Edges, EdgesOfNode),
    generateNodes(T, Edges, Rest).

% getAllEdgesFrom is true when list 2 contains all the edges leading from node From.
getAllEdgesFrom(_,[],[]).
getAllEdgesFrom(From, [dEdge(F,_)|T], R) :-
    not(From == F),
    getAllEdgesFrom(From, T, R).
getAllEdgesFrom(From, [dEdge(From,To)|T], [dEdge(From, To)|R]) :-
    getAllEdgesFrom(From, T, R).