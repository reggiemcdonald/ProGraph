% This file contains out internal representation for graphs.
% our approach uses a graph-term from.
% Design initially borrowed from prologsite.
% @see: https://sites.google.com/site/prologsite/prolog-problems/6
% @note: Node > Vertex

% A graph is defined by graph(I,L1,L2):
% - I is the ID
% - L1 is the list of nodes in the graph: ie [a,b,c]
% - l2 is a list of edges in the graph: ie [e(a,b), e(b,a)]
% The example graph is [a]<->[b] [c]
graph(ex1, [a,b,c], [e(a,b), e(b,a)]).

% Graph validity constraints
% - There must no be no edges to a node not found in L1

% A graph can have a subgraph. Suppose ex1 is a subgraph of ex1
% You add a subgraph by adding it as a node and
% you connect the edges of the graph and subgraph.
% This graph contains is [y]->[z]->[a]<->[b] [c]
graph(ex2, [y,z,ex1], [e(y,z), e(z,a)]).

% Graphs can have properties that are calculated by the following predicates:

% Eccentricity: the max distance from a node to all other node.
% e(I) :-

% Radius: The min eccentricity from all the edges of the Graph.
% rad(I) :-

% Diameter: the max eccentricity from all the edges of a Graph.
% diam(I) :-

% Central Point: If the radius is equal to the ecentricity.
% cp(I) :- e(I),rad(I).