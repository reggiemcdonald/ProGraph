
:- module(viz, [view_graph/1, export_graph/1]).
:- use_module(library(gv)).
:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(graphrepresentation).

view_graph(Name) :-
    graph(Name, Nodes, Edges, SubGraphs),
    view(graph(Name, Nodes, Edges, SubGraphs)).

export_graph(Name) :-
    graph(Name, Nodes, Edges, SubGraphs),
    export(graph(Name, Nodes, Edges, SubGraphs)).

export(Graph) :-
    gv_export(
        'graph.png',
        {Graph}/[Out]>>export_graph_(Out, Graph),
        options{directed: true}
    ).

view(Graph) :-
    gv_view(
        {Graph}/[Out]>>export_graph_(Out, Graph),
        options{directed: true}
    ).

export_graph_(Out, Graph) :-
    Graph = graph(_, Nodes, Edges, _),
    maplist(export_node(Out), Nodes),
    maplist(export_arc(Out), Edges).

export_node(Out, node(N, _)) :-
    dot_node(Out, N).

export_arc(Out, dEdge(F,T)) :-
    dot_arc(Out, F, T).
