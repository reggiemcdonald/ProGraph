% graphload.pl loads the graph into memory from file
:- module(graphload, [buildGraphFromFile/2]).
:- use_module(library(http/json)).
:- use_module(graphrepresentation).
:- use_module(graphutil).

% buildGraphFromFile is true when the graph dsl at FilePath is successfully parsed.
buildGraphFromFile(FilePath, Graph) :- 
    retractall(graph(_,_,_,_)),
    loadDefinition(FilePath, JsonStructure),
    generateGraph(JsonStructure, Graph),
    staticCheck(Graph),
    assertAllGraphs(Graph).

% loadDefinition is true if the JSON file specified at the file path can be read and parsed into a json structure.
loadDefinition(FilePath, JsonStructure) :- 
    open(FilePath, read, Stream),
    json_read(Stream, JsonStructure),
    close(Stream).

% generateGraph is true when the top level graph is successfully parsed.
generateGraph(json([name=Name, nodes=NodesJson, edges=EdgesJson, subgraphs=SubgraphsJson]), graph(Name, AllNodes, AllEdges, Subgraphs)) :-
    generateAllEdges(EdgesJson, SubgraphsJson, AllEdges),
    generateNodes(NodesJson, AllEdges, AllNodes),
    generateSubgraphs(SubgraphsJson, AllNodes, Subgraphs).

% generateSubgraph is true when all the subgraphs are parsed successfully.
generateSubgraphs([],_,[]).
generateSubgraphs(
    [json([name=Name, nodes=NodesJson, edges=EdgesJson, subgraphs=SubgraphsJson]) | T], 
    AllNodes, 
    [Graph | RestOfSubgraphs]
) :-
    generateGraph(json([name=Name, nodes=NodesJson, edges=EdgesJson, subgraphs=SubgraphsJson]), Graph),
    generateSubgraphs(T, AllNodes, RestOfSubgraphs).

% generateAllEdges is true when all edges in toplevel and the subgraphs have been parsed.
generateAllEdges(ParentGraphEdges, [], Edges) :-
    generateEdges(ParentGraphEdges, Edges).
generateAllEdges(ParentGraphEdges,
    [json([name=_, nodes=_, edges=Edges, subgraphs=S]) | RestOfSubgraphs],
    AllEdges) :- 
    generateEdges(Edges, ParsedEdges),
    generateEdges(ParentGraphEdges, ParsedParentGraphEdges),
    generateAllEdges([], S, SubSubgraphEdges),
    generateAllEdges([], RestOfSubgraphs, RestOfSubgraphsEdges),
    appendNoDup(SubSubgraphEdges, RestOfSubgraphsEdges, SubSubAndRest),
    appendNoDup(SubSubAndRest, ParsedParentGraphEdges, ParentAndRest),
    appendNoDup(ParentAndRest, ParsedEdges, AllEdges).

% generateEdges is true when the edges are parsed.
generateEdges([], []).
generateEdges([json([from=A, to=B])|T], [dEdge(A,B)|R]) :-
    generateEdges(T,R).

% appendNoDup is true when list 3 is the concatenation of lists 1 and 2 with all duplicates removed.
appendNoDup([],[],[]).
appendNoDup([H|T], [], C) :-
    appendNoDup(T, [], C),
    member(H,C).
appendNoDup([H|T], [], [H|C]) :-
    appendNoDup(T, [], C),
    not(member(H,C)).
appendNoDup(A, [H|T], C) :-
    appendNoDup(A, T, C),
    member(H,C).
appendNoDup(A, [H|T], [H|C]) :- 
    appendNoDup(A, T, C),
    not(member(H,C)).



/*
    Static Checks
*/

% staticCheck is true when the graph is properly formed.
staticCheck(graph(_, AllNodes, _, Subgraphs)) :-
    staticCheckSubgraphs(AllNodes, Subgraphs).

% staticCheckSubgraphs is true when all subgraphs are properly formed.
staticCheckSubgraphs(_,[]).
staticCheckSubgraphs(AllNodes, [graph(_, Nodes, Edges, Subgraphs)|T]) :-
    checkNodeScoping(AllNodes, Nodes),
    checkEdgesForUndeclaredNode(AllNodes, Edges),
    staticCheckSubgraphs(AllNodes, T),
    staticCheckSubgraphs(Nodes, Subgraphs).


% checkNodeScoping is true when all nodes are properly scoped.
% That is to say, no new nodes have been defined in a subgraph.
% Any node in a subgraph must be present in a parent graph.
checkNodeScoping(_, []).
checkNodeScoping(AllowedNodes, [node(Name,_)|T]) :-
    findNode(Name, AllowedNodes, _),
    checkNodeScoping(AllowedNodes, T).

% checkEdgesForUndeclaredNode is true when all edges of the graph
% refer to nodes that are present in the graph.
checkEdgesForUndeclaredNode(_, []).
checkEdgesForUndeclaredNode(AllowedNodes, [dEdge(From,To)|T]) :-
    findNode(From, AllowedNodes,_),
    findNode(To, AllowedNodes,_),
    checkEdgesForUndeclaredNode(AllowedNodes, T).

/* Miscellaneous Utilities */

% assertAllGraphs is true when the toplevel and subgraphs have been asserted in the knowledge base.
assertAllGraphs(graph(Name, Nodes, Edges, Subgraphs)) :-
    assertz(graph(Name, Nodes, Edges, Subgraphs)),
    assertSubgraphs(Subgraphs).

% assertSubgraphs is true when the subgraphs produced have been added to the knowledge base
assertSubgraphs([]).
assertSubgraphs([graph(Name, Nodes, Edges, Subgraphs)|Rest]) :-
    assertz(graph(Name,Nodes,Edges,Subgraphs)),
    assertSubgraphs(Subgraphs),
    assertSubgraphs(Rest).






