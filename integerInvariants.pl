% This module provides measurements on the graph
% including the size and order of the graph.

:- module(integerInvariants).
:- include(graphload).
:- dynamic graph/4.

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
