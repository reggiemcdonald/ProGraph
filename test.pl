:- initialization main.

main :- 
    % learned how to parse args from https://stackoverflow.com/questions/25467090/how-to-run-swi-prolog-from-the-command-line
    current_prolog_flag(argv, Argv),
    [Argv],
    make,
    load_test_files([]),
    run_tests,
    halt(0).