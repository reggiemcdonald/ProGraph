:- module(path, [len/2, distance/4, path/4, eccentricity/2]).
:- use_module(graphrepresentation).

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