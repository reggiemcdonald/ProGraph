% This file contains out internal representation for graphs.
% our approach uses a graph-term from.
% Design initially borrowed from prologsite.

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
        eEdge(a,b),
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

% Eccentricity: the max distance from a node to all other node.
% e(I) :-

% Radius: The min eccentricity from all the edges of the Graph.
% rad(I) :-

% Diameter: the max eccentricity from all the edges of a Graph.
% diam(I) :-

% Central Point: If the radius is equal to the ecentricity.
% cp(I) :- e(I),rad(I).

% TODO:
%https://en.wikipedia.org/wiki/Graph_property