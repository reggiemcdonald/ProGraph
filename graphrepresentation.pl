% This file contains out internal representation for graphs.
% our approach uses a graph-term from.

:- module(graphrepresentation, [graph/4]).
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


