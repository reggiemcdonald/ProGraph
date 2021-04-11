% learned about plunit from https://stackoverflow.com/questions/57263196/unit-tests-in-swi-prolog-visibility-of-user-predicates-from-within-a-module
:- begin_tests(graphload).
:- use_module(graphload).
:- dynamic same/1.

malformed_generator(FullFilePath) :-
    path_generator('res/malformed/', FullFilePath).

well_formed_generator(FullFilePath) :-
    path_generator('res/well-formed/', FullFilePath).

path_generator(SubDir,FullFilePath) :-
    working_directory(Cwd,''),
    atom_concat(Cwd, SubDir, Dir),
    directory_files(Dir, Files),
    gen(Files,File),
    atom_concat(Dir,File,FullFilePath).

gen([File|_],File) :-
    not(File == '.'),
    not(File == '..'),
    atom_concat(_,'.json',File).
gen([_|T], R) :-
    gen(T, R).

validateSolution(FilePath, Graph) :-
    % Get the name of the file without the extension
    % as the solution will have the same name
    atom_concat(Name, '.json', FilePath),
    atom_concat(Name, '.pl', SolFile),
    load_files(SolFile),
    same(Graph),
    unload_file(SolFile).

% learned how to use forall here https://stackoverflow.com/questions/54334567/swi-prolog-unit-testing-library-plunit-how-is-forall-option-used
test(malformed, [forall(malformed_generator(FilePath)), error(_)]) :-
    buildGraphFromFile(FilePath,_).

test(wellformed, [forall(well_formed_generator(FilePath))]) :-
    buildGraphFromFile(FilePath, Graph),
    validateSolution(FilePath, Graph),!.

:- end_tests(graphload).