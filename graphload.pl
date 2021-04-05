% graphload.pl loads the graph into memory from file

:- use_module(library(http/json)).

% buildGraphFromFile is true when the graph dsl at FilePath is successfully parsed.
buildGraphFromFile(FilePath, Graph) :- 
    loadDefinition(FilePath, JsonStructure),
    generateGraph(JsonStructure, Graph).

% loadDefinition is true if the JSON file specified at the file path can be read and parsed into a json structure.
loadDefinition(FilePath, JsonStructure) :- 
    open(FilePath, read, Stream),
    json_read(Stream, JsonStructure),
    close(Stream).

% generateEdges is true when the edges are parsed.
generateEdges([], []).
generateEdges([json([from=A, to=B]) | T], [e(A,B) | R]) :-
    generateEdges(T,R).

% generateSubgraph is true when all the subgraphs are parsed successfully.
generateSubgraph([],[]).
generateSubgraph([Subgraph | T], [ParsedSubgraph | R]) :-
    generateGraph(Subgraph, ParsedSubgraph),
    generateSubgraph(T, R).

% generateGraph is true when the graph is parsed successfully.
generateGraph(json([name=Name, nodes=Nodes, edges=Edges, subgraphs=S]), graph(Name, Nodes, ParsedEdges, ParsedSubgraphs)) :- 
    generateEdges(Edges, ParsedEdges),
    generateSubgraph(S,ParsedSubgraphs).




