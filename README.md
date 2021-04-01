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
### Syntax 2
```
DEFINITION ::= {definitions: { VAR: GRAPH, ... }}

GRAPH ::= GRAPH_DECLARITIVE | GRAPH_IMPERATIVE

GRAPH_IMPERATIVE ::= {head: NODE, directed: BOOL, edges: [EDGE]}

GRAPH_DECLARATIVE ::= {properties: [PROPERTY]}

NODE ::= {name: NAME}

NAME ::= STRING

VAR ::= STRING

EDGE ::= {to: NODE, from: NODE}

PROPERTY ::= ?

STRING ::= ^[a-zA-Z]+[0-9]*

BOOL ::= true | false
```

### Syntax 3
```
DEFINITION ::= {definitions: [GRAPH]}

GRAPH ::= GRAPH_DECLARATIVE | GRAPH_IMPERATIVE

GRAPH_IMPERATIVE ::= {name: NAME, head : NODE}

GRAPH_DECLARATIVE ::= {name: NAME, properties: [PROPERTIES]}

NODE ::= {name: NAME, edges: [EDGE]}

NAME ::= STRING

EDGE ::= {to: NODE, from: NODE}

PROPERTY ::= ?

STRING ::= ^[a-zA-Z]+[0-9]*

BOOL ::= true | false
```
