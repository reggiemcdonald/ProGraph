:- begin_tests(graphload).
:- include(graphload).
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

test(malformed, [forall(malformed_generator(FilePath))]) :-
    not(buildGraphFromFile(FilePath,_)).

test(wellformed, [forall(well_formed_generator(FilePath))]) :-
    buildGraphFromFile(FilePath, Graph),
    validateSolution(FilePath, Graph).

:- end_tests(graphload).