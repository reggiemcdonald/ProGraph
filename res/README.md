### Adding a DSL test case
- Decide whether you're making a malformed or a well-formed test case
- If its a **well-formed** test case, then add a new `.json` file to `res/well-formed` with  the name `N.json` where `N` is the next number in the series. *Additionally, you must add a `N.pl` file to the same directory with the answer. Use the existing files as a template.
- If its a **malformed** test case, then add a new `.json` file to `res/malformed` with the name `N.json` where `N` is the next number if the series. Add an entry in desc.txt with a description of why the test case is malformed.
