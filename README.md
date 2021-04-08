## ProGraph

### Syntax
```
DEFINITION ::= GRAPH

GRAPH ::= { ( EDGES, )? ( DIRECTED, )? ( SUBGRAPHS, )? NAME, NODES }

NAME ::= "name": STRING

NODES ::= "nodes": [STRING]

EDGES ::= "edges": [EDGE]

EDGE ::= { "from": STRING, "to": STRING }

DIRECTED ::= "directed": true | "directed": false

SUBGRAPHS ::= "subgraphs": [GRAPH]

STRING ::= "[a-zA-Z0-9]+"
```
EBNF syntax style adopted from https://cswr.github.io/JsonSchema/spec/grammar/


### Example
```json
{
    "name": "toplevel",
    "nodes": ["A","B","C","D"],
    "edges": [],
    "directed": true,
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

### Running
- To run using local swipl: `make run MOD=$MODULE` where `$MODULE` is the name of the module you want to run.
- To run using dockerized swipl: `make docker-run MOD=$MODULE` where `$MODULE` is the name of the module you want to test.

### Testing
- To test using local swipl: `make test MOD=$MODULE` where `$MODULE` is the name of the module you want to test.
- To test using dockerized swipl: `make docker-test MOD=$MODULE` where `$MODULE` is the name of the module you want to test.