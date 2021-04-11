% to view example: `swipl -s viz.pl -g view -t halt` in your shell
% to export example: `swipl -s viz.pl -g export -t halt` in your shell



:- use_module(library(gv)).
:- use_module(library(apply)).
:- use_module(library(yall)).
:- include(graphload).
:- dynamic graph/4.

example_(graph(example1, [node('A', [dEdge('A', 'B')]), node('B', [dEdge('B', 'A')])], [dEdge('A', 'B'), dEdge('B', 'A')], [])).


view_graph(Name) :-
    graph(Name, Nodes, Edges, SubGraphs),
    view(graph(Name, Nodes, Edges, SubGraphs)).

export_graph(Name) :-
    graph(Name, Nodes, Edges, SubGraphs),
    export(graph(Name, Nodes, Edges, SubGraphs)).

export :-
    example_(Graph),
    export(Graph).

export(Graph) :-
    gv_export(
        'graph.svg',
        {Graph}/[Out]>>export_graph_(Out, Graph),
        options{directed: true}
    ).


view :-
    example_(Graph),
    view(Graph).

view(Graph) :-
    gv_view(
        {Graph}/[Out]>>export_graph_(Out, Graph),
        options{directed: true}
    ).



export_graph_(Out, Graph) :-
    Graph = graph(Name, Nodes, Edges, SGs),
    maplist(export_node(Out), Nodes),
    maplist(export_arc(Out), Edges).

export_node(Out, node(N, _)) :-
    dot_node(Out, N).

export_arc(Out, dEdge(F,T)) :-
    dot_arc(Out, F, T).
