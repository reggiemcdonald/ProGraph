:- initialization main.

main :- 
    % learned how to run and parse args from https://stackoverflow.com/questions/25467090/how-to-run-swi-prolog-from-the-command-line
    current_prolog_flag(argv, Argv),
    [Argv],
    % learned how to run tests from https://stackoverflow.com/questions/57263196/unit-tests-in-swi-prolog-visibility-of-user-predicates-from-within-a-module
    make,
    load_test_files([]),
    run_tests,
    halt(0).