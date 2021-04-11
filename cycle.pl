%
% Graph is graph(NAME, [NODE], [EDGE], [GRAPH])
% Node is node(NAME, [EDGE])
%
%Checking if a graph has a cycle

:- module(cycle, [cycle/1]).
:- use_module(graphrepresentation).
:- use_module(graphutil).

cycle(Name) :-
    graph(Name, _, Edges, _),
    hasCycle(Edges, []).

hasCycle([dEdge(_,T)|_], Visited) :-
    member(T, Visited).
hasCycle([dEdge(F,_)|R], Visited) :-
    hasCycle(R, [F|Visited]).
