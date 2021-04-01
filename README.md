## ProGraph

### Syntax
```
DEFINITION ::= {definitions: [GRAPH]}

GRAPH ::= GRAPH_DECLARATIVE | GRAPH_IMPERATIVE

GRAPH_DECLARATIVE ::= {name: NAME, directed: BOOL, edges: [EDGE]}

GRAPH_IMPERATIVE ::= {name: NAME, directed: BOOL, properties: [PROPERTY]}

NODE ::= {name: NAME}

NAME ::= STRING

EDGE ::= {to: NODE, from: NODE}

PROPERTY ::= ?

STRING ::= ^[a-zA-Z]+[0-9]*

BOOL ::= true | false
```
