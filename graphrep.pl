% This file contains out internal representation for graphs.
% our approach uses a graph-term from.
% Design initially borrowed from prologsite.

% :- dynamc graph/3.

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
graph(ex1,
    [
        node(a,[dEdge(a,b)]),
        node(b,[dEdge(b,a)]),
        node(c,[])
    ],
    [
        eEdge(a,b),
        dEdge(b,a)
    ], []).

% A graph can have a subgraph. Suppose ex1 is a subgraph of ex2
% This graph contains is [y]->[z]->[a]<->[b] [c]
graph(ex2,
    [
        node(y,[dEdge(y,z)]),
        node(z,[dEdge(z,a)]),
        node(a,[dEdge(a,b)]),
        node(b,[dEdge(b,a)]),
        node(c,[])
    ],
    [
        dEdge(y,z),
        dEdge(z,a),
        dEdge(a,b),
        dEdge(b,a)
    ],
    [
        graph(ex1,
            [
                node(a,[dEdge(a,b)]),
                node(b,[dEdge(b,a)]),
                node(c,[])
            ],
            [
                eEdge(a,b),
                dEdge(b,a)
            ],[])
    ]).

% Graphs can have properties that are calculated by the following predicates:

% Len(Id,N): the number of edges in a graph.
% - Id is the graphID
% - N is the number of edges
len(Id, N) :- graph(Id, _, L2,_), len(L2, N).

len([], N) :- N is 0.
len([_|T], N) :-
    len(T, N1),
    N is N1+1.

% dis(Id, N1, N2, N): the distance between From -> To.
% - Id is the graphID
% - From/To Node name
% - N is the distance
dis(Id, From, To, N) :-
    graph(Id, Nodes,_, _),
    findPath(From, To, Nodes, [], Path),
    len(Path, N).

% path(ID, From, To, Path): the path between From -> To.
% - Id is the graphID
% - From/To Node name
% - Path is a list of Node names
path(Id, From, To, Path) :-
    graph(Id, Nodes, _, _),
    findPath(From, To, Nodes, [], Path).

findNode(Name, [node(Name,Edges)|_], node(Name,Edges)).
findNode(Name, [node(Other,_)|Rest], N) :-
    not(Name == Other),
    findNode(Name, Rest, N).

findPath(From, From, _, _, [From]).
findPath(From, To, Nodes, Visited, [From|Path]) :-
    \+ member(From, Visited),
    findNode(From, Nodes, node(From, Edges)),
    findPathForNode(Edges, To, Nodes, [From|Visited], Path).

findPathForNode([dEdge(_,Next)|_], To, Nodes, Visited, Path) :-
    findPath(Next, To, Nodes, Visited, Path).
findPathForNode([dEdge(_,_)|Rest], To, Nodes, Visited, Path) :-
    findPathForNode(Rest, To, Nodes, Visited, Path).

% Eccentricity: the max distance from a node to all other node.
% e(I) :- 

% Radius: The min eccentricity from all the edges of the Graph.
% rad(I) :-

% Diameter: the max eccentricity from all the edges of a Graph.
% diam(I) :-

% Central Point: If the radius is equal to the ecentricity.
% cp(I) :- e(I),rad(I).