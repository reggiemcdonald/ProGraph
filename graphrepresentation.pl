% This file contains out internal representation for graphs.
% our approach uses a graph-term from.

:- module(graphrepresentation).
:- include(graphload).
:- dynamic graph/4.

% A GRAPH is defined by graph(NAME,[NODE],[EDGE],[GRAPH]) where:
% - NAME is a globally unique string.
% - NODE is defined by node(NAME, [EDGE]) where:
%   - NAME is a unique string to the graph.
% - EDGE is is defined by dEdge(FROM,TO) where:
%   - FROM/TO is the NAME of a NODE.

% Graph validity constraints
% - There must no be no edges to a node not found in [NODE].
% - There must not be any duplicate edges in [EDGES].
% - NAME in graph must be globally unique.

% The example graph is [a]<->[b] [c]
% graph(ex1,
%     [
%         node(a,[dEdge(a,b)]),
%         node(b,[dEdge(b,a)]),
%         node(c,[])
%     ],
%     [
%         dEdge(a,b),
%         dEdge(b,a)
%     ], []).

% A graph can have a subgraph. Suppose ex1 is a subgraph of ex2
% This graph contains is [y]->[z]->[a]<->[b] [c]
% graph(ex2,
%     [
%         node(y,[dEdge(y,z)]),
%         node(z,[dEdge(z,a)]),
%         node(a,[dEdge(a,b)]),
%         node(b,[dEdge(b,a)]),
%         node(c,[])
%     ],
%     [
%         dEdge(y,z),
%         dEdge(z,a),
%         dEdge(a,b),
%         dEdge(b,a)
%     ],
%     [
%         graph(ex1,
%             [
%                 node(a,[dEdge(a,b)]),
%                 node(b,[dEdge(b,a)]),
%                 node(c,[])
%             ],
%             [
%                 dEdge(a,b),
%                 dEdge(b,a)
%             ],[])
%     ]).

% Graphs can have properties that are calculated by the following predicates:

% Len(Id,N): the number of edges in a graph.
% - Id is the graphID
% - N is the number of edges
len(Id, N) :- graph(Id, _, L2,_), length(L2, N).

% dis(Id, N1, N2, N): the distance between From -> To.
% - Id is the graphID
% - From/To Node name
% - N is the distance
distance(Id, From, To, N) :-
    path(Id, From, To, Path),
    length(Path, N).

% path(ID, From, To, Path): the path between From -> To.
% - Id is the graphID
% - From/To Node name
% - Path is a list of Node names
path(_, From, From, [From]).
path(Id, From, To, Path1) :-
    graph(Id, _,Edges, _),
    findPath(From, To, Edges, [], Path),
    reverse([To|Path], Path1).

findPath(From,From,_,[From],[From]).
findPath(From,To,Edges,Visited,Path) :-
    \+ member(From,Visited),
    relevantEdges(From, Edges,[], Relevant),
    findPath_(To,Relevant,Edges,[From|Visited],Path).

findPath_(To,[To|_],_,Visited,Visited).
findPath_(To,[Next|Rest],Edges,Visited,Path) :-
    not(findPath(Next,To,Edges,Visited,Path)),
    findPath_(To,Rest,Edges,Visited,Path).
findPath_(To,[Next],Edges,Visited,Path) :-
    findPath(Next,To,Edges,Visited,Path).

relevantEdges(_,[],Relevant,Relevant).
relevantEdges(From,[dEdge(From,To)|Rest],Relevant,Path) :-
    relevantEdges(From,Rest,[To|Relevant],Path).
relevantEdges(From,[_|Rest],Relevant,Path) :-
    relevantEdges(From,Rest,Relevant,Path).

% eccentricity(Id, Dis): the max Distance from a node to all other node.
eccentricity(Id, Dis) :-
    graph(Id, Nodes, _, _),
    findLongestDistanceInGraph(Id, Nodes, Nodes, 0, Dis).

findLongestDistanceInGraph(_, [], _, Max, Max).
findLongestDistanceInGraph(Id, [node(From,_)|Res], Original, Max, Ref) :-
    ( findLongestDistance(Id, From, Original, Max, N)
    ->  findLongestDistanceInGraph_(Id, Res, Original, N, Max, Ref)
    ;   findLongestDistanceInGraph_(Id, Res, Original, 0, Max, Ref)
    ).
findLongestDistanceInGraph_(Id, Res, Original, N, Max, Ref) :-
    ( N > Max
        ->  findLongestDistanceInGraph(Id, Res, Original, N, Ref)
        ;   findLongestDistanceInGraph(Id, Res, Original, Max, Ref)
    ).

findLongestDistance(_,_,[],Ref, Ref).

findLongestDistance(Id,From,[node(To,_)|Res], Max, Ref) :-
    ( distance(Id, From, To, N)
    ->  findLongestDistance_(Id, From, Res, N, Max, Ref)
    ;   findLongestDistance_(Id, From, Res, 0, Max, Ref)
    ).

findLongestDistance_(Id, From, Res, N, Max, Ref) :-
    ( N > Max 
        -> findLongestDistance(Id, From, Res, N, Ref)
        ;  findLongestDistance(Id, From, Res, Max, Ref)
    ).