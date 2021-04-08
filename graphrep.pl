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

% Len(I,N): the number of edges in a graph.
% - I is the graphID
% - N is the number of edges
len(I, N) :- graph(I, _, L2,_), len(L2, N).
len([], N) :- N is 0.
len([_|T], N) :-
    len(T, N1),
    N is N1+1.

% dis(I, N1, N2, N): the distance from N1 to N2 in graph I.
% - I is the graphID
% - N1/N2 is From/To Node name
% - N is the distance
dis(I, N1, N2, N) :-
    graph(I, _, EDGES, _),
    path(N1, N2, EDGES, [], N).

% path(I, N1, N2, PATH): the path from N1 to N2 in graph I.
% This is broken idk why I can find the length but not the path.....
% path(I, N1, N2, PATH) :-
%     graph(I, _, EDGES, _),
%     path1(N1, N2, EDGES, PATH).

% path1(FROM,FROM,_,[FROM]).
% path1(FROM, TO, [dEdge(FROM,NEXT)|EDGES], VISITED) :-
%     \+ member(FROM, VISITED),
%     path1(NEXT,TO,EDGES,VISITED1),
%     append(VISITED1, [FROM], VISITED).

path(FROM,FROM,_,_,N) :- N is 0.
path(FROM, TO, [dEdge(FROM,NEXT)|EDGES], VISITED, N) :-
    \+ member(FROM, VISITED),
    path(NEXT,TO,EDGES,[FROM|VISITED],N1),
    N is N1 + 1.



% Path

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