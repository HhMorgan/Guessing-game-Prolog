%I finished this code alone for practicig purposes.
:- dynamic word/1.
:- dynamic check/0.
:- initialization(menu).
%====================================================%
%     Menu structure and processing user commands    %
%====================================================%
menu:-
    assert(check),
    writeln('G u e s s  a  W o r d'),
    writeln('---------------------'),
    writeln(''),
    writeln('r - read database file'),
    writeln('w - write database file'),
    writeln('l - list knowledge base'),
    writeln('c - clear knowledge base'),
    writeln('a - add a new word'),
    writeln('d - delete a word'),
    writeln('g - guess a word'),
    writeln('e - end the game'),
    repeat,
    read_single_char(X), 
    ((run_operation(X)),
    (X = e,!)).

run_operation(e).

run_operation(r):- 
    writeln('Please write the word that will you wish to add to the database\nThe command should be in the form file(FileName)'),
    read(file(FileNameRead)), 
    readfile(FileNameRead).

run_operation(l):- 
    listkb(KB),
    writeln(KB).

run_operation(c):- 
    clearkb.

run_operation(a):- 
    writeln('Please write the word that will you wish to add to the database\nThe command should be in the form word(Word)'),
    read(word(Word_to_be_added)), 
    addword(Word_to_be_added).

run_operation(d):- 
    writeln('Please write the word that will you wish to delete to the database\nThe command should be in the form word(Word)'),
    read(word(Word_to_be_deleted)),
    delword(Word_to_be_deleted).

run_operation(w):-
    writeln('Please write the name of the file you wish to be saved\nThe command should be in the form file(FileName)'),
    read(file(FileNameWrite)), 
    writefile(FileNameWrite).

run_operation(g):-
    guess.

read_single_char(Char) :-
    get_single_char(Code), 
    put_code(Code), 
    nl, 
    Code\= 10,
    char_code(Char, Code).

%====================================================%
%              Maintaining the database              %
%====================================================%
addword(Word):- 
    atom_string(Word_atom,Word),
    (
        (word(Word_atom), writeln('The word is already in the database'));
        (\+ word(Word_atom), assert(word(Word_atom)), changes_check)
    ).

delword(Word):- 
    atom_string(Word_atom,Word),
    retract(word(Word_atom)), 
    changes_check.

listkb(List):- 
    findall(Word,word(Word),List).

clearkb:- 
    retractall(word(_)).

writefile(FileName):-
    atom_concat(FileName,'.txt',FileNameExtended),
    findall(Word,word(Word),Word_list),
    state_Overwriting_warning(FileNameExtended),
    open(FileNameExtended,write,Out),
    writewords(Word_list,Out),
    close(Out),
    assert(check).

writewords([],_).
writewords([H|T],Out):-
    writeln(Out,H),
    writewords(T,Out).

state_Overwriting_warning(FileNameExtended):-
    \+ exists_file(FileNameExtended),!.
state_Overwriting_warning(FileNameExtended):-
    exists_file(FileNameExtended),
    writeln('Are you sure that you want to overwrite the file?\nThe command is in the form command(Response), Response = y/n'),
    read(command(Char)), 
    (
        ( 
            Char = y
        )
        ;
        (
            Char = n,
            fail
        )
    ).

changes_check:-
    \+ check, 
    !.
changes_check:-
    retract(check).

 readfile(FileName):-
    state_unsaved_changes_warning,
    retractall(word(_)),
    atom_concat(FileName,'.txt',FileNameExtended),
    check_if_file_exists(FileNameExtended),
    open(FileNameExtended,read,In),
    repeat,
    read_line_to_codes(In,X),
    atom_string(X_atom,X),
    assert(word(X_atom)),
    X=end_of_file,!,
    close(In),
    retract(word(end_of_file)),
    assert(check).

check_if_file_exists(FileName):- 
    exists_file(FileName).

state_unsaved_changes_warning:-
    check,
    !.
state_unsaved_changes_warning:-
    \+ check,
    writeln('Are you sure that you want to not save the changes?\nThe command is in the form command(Response), Response = y/n'),
    read(command(Char)), 
    (
        ( 
            Char = y
        )
        ;
        (
            Char = n,
            fail
        )
    ).
%====================================================%
%                     Game engine                    %
%====================================================%

guess:-
    listkb(List),
    % writeln(List),
    length(List,Len),
    state_empty_kb_warning(Len),
    Random_Upper_Bound is Len + 1,
    random(1,Random_Upper_Bound,Random),
    nth1(Random,List,Word),
    write('The word that should be guessed is '),
    writeln(Word),
    atom_chars(Word,Word_chars),
    length(Word_chars, Len_Word_chars),
    length(GuessedWord, Len_Word_chars),
    maplist(=(*), GuessedWord),
    % writeln(GuessedWord),
    user_guesses(Word_chars,GuessedWord,0).

user_guesses(Word,Guessed,N):- 
    Guessed = Word, 
    write('Congratulations! It took you only '),
    write(N),
    writeln(' guesses'),
    !.

user_guesses(Word,Guessed,N):-
    write('Please guess the letter:'),
    read_single_char(Char), 
    write('Your solution:'),
    replace_letter(Word,Guessed,Char,Res),
    atomics_to_string(Res,String_GuessedWord),
    writeln(String_GuessedWord),
    N1 is N + 1, 
    user_guesses(Word,Res,N1).

replace_letter([],[],_,[]).
replace_letter([HW|TW],[_|TG],HW,[HW|TR]):-
    replace_letter(TW,TG,HW,TR),
    !.
replace_letter([_|TW],[HG|TG],X,[HG|TR]):-
    replace_letter(TW,TG,X,TR).   

state_empty_kb_warning(Len):-
    Len = 0, 
    writeln('The database is currently empty'),
    fail,
    !.
state_empty_kb_warning(Len):-
    Len > 0.
