## ProGraph

### Syntax
```
DEFINITION ::= GRAPH

GRAPH ::= { NAME, NODES, EDGES, DIRECTED, SUBGRAPHS }

NAME ::= "name": STRING

NODES ::= "nodes": [STRING]

EDGES ::= "edges": [EDGE]

EDGE ::= { "from": STRING, "to": STRING }

SUBGRAPHS ::= "subgraphs": [GRAPH]

STRING ::= "[a-zA-Z0-9]+"
```
EBNF syntax style adopted from https://cswr.github.io/JsonSchema/spec/grammar/

### Running Locally
```bash
# 1.) Ensure that graphviz and SWI-Prolog are installed

# 2.) Install gv dependency
swipl -g 'pack_install(prolog_graphviz)' -t halt

# 3.) Run the program
make run
```

### Running using docker
```bash
make docker-prepare
make docker-run
```

### Example
```json
{
    "name": "toplevel",
    "nodes": ["A","B","C","D"],
    "edges": [],
    "subgraphs": [
        {
            "name": "subgraph1",
            "nodes": ["A","B"],
            "edges": [
                {"from": "A", "to": "B"},
                {"from": "B", "to": "A"}
            ],
            "subgraphs": []
        }
    ]
}
```

### Notes
- `NAME` must be globally unique
- Nodes must be globally unique
- You can define new edges in the subgraphs - ProGraph automatically adds them to the parent graphs

### Running Modules
- To run using local swipl: `make run MOD=$MODULE` where `$MODULE` is the name of the module you want to run.
- To run using dockerized swipl: `make docker-run MOD=$MODULE` where `$MODULE` is the name of the module you want to test.

### Testing Modules
- To test using local swipl: `make test MOD=$MODULE` where `$MODULE` is the name of the module you want to test.
- To test using dockerized swipl: `make docker-test MOD=$MODULE` where `$MODULE` is the name of the module you want to test.