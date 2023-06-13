getproperty(In):-
    string_to_list(In,L),tokenize(L,L1),write(L1),prop_check(L1).
%---------------------------------------------------------------------------------------
getobject(File,ObjList) :-
    open(File, read,S),
    lex(S,X),
    close(S),parse(P,X,[]),
    predsort(comparelist,P,PL),extractvars(PL,Res),sort(Res,ObjList).
%----------------------------------------------------------------------------------
extractvars([],[]).
extractvars([data(_,X,Y,_)|T],[Val|VT]):- 
   string_concat(X,'=>',X1),string_concat(X1,Y,Val),extractvars(T,VT).
%-----------------------------------------------------------------------
lex(Stream,Tokens) :-
   get_chars(Stream,L), !,
   tokenize(L,Tokens),!.

get_chars(Str,L) :-
   get_code(Str,C),
   get_chars(Str,C,L).
get_chars(Str,_, []) :- at_end_of_stream(Str). %termination
get_chars(Str,C,  [C|L1]) :-
   get_chars(Str,L1).

tokenize([], []) :- !.
tokenize([C|L], L3) :-
   white(C), !, skip_whites(L,L2),
   tokenize(L2,L3).

tokenize([C|L], [X|L3]) :-
   alpha(C), identifier(X,[C|L],L2), !,
   tokenize(L2,L3).


tokenize(L, [X|L3]) :-
   special(X,L,L2), !,
   tokenize(L2,L3).
   


tokenize([C|_L], _) :-
   print('Error: Cannot tokenize the character1: '),
   name(BadChar, [C]),
   print(BadChar),
   fail.
   
skip_whites([], []).
skip_whites([C|L], L2) :-
   (white(C) -> skip_whites(L,L2); L2 = [C|L]).

white(9).  % tab
white(32). % blank
white(10). % newline
white(13). %carriage return
special(':',[58|L],L).
special('=>',[61,62|L],L).
special(=,[61|L],L).
special('->',[45,62|L],L).
special(>,[62|L],L).
special(<,[60|L],L).


identifier(id(N)) --> 
   ident(L), {name(N,L)}.

identifier(id(X)) --> 
   ident(L), {name(N,L),(keyword(N) -> X=N; X=id(N))}.

ident([X|L]) --> 
   letter(X), ident(L).
ident([X])   --> 
   letter(X).
   
letter(X) --> 
   [X],  {alpha(X)}.



alpha(X) :-  X > 64,  X < 91.
alpha(X) :-  X > 96,  X < 123.
alpha(X) :- X > 47,  X < 58.
alpha(95). % ascii value of _
alpha(46).
alpha(44).
alpha(45).
alpha(43).
alpha(91).
alpha(93).
alpha(60).
alpha(62).

%keyword
keyword('G').
keyword('F').

%tagsforproperty
begintag('<').
endtag('>').
%-------------------------------------------------------------------------
parse_input([],[],[]).
parse_input([X|T]) --> 
  parse_input1(X),[':'],parse_input(T).
parse_input([X]) --> 
   parse_input1(X).
parse_input1([Obj,Var]) --> 
   [id(Obj)],['=>'],[id(Var)].
%--------------------------------------------------------------------------
parse([],[],[]).
parse([X|T]) --> 
   parse1(X),parse(T).
parse1(data(X,Y,Z,V)) --> 
   [id('Time')],['='],[id(X)],[':'],[id('Obj')],['='],[id(Y)],
   [':'],[id('Var')],['='],[id(Z)],[':'],[id('Val')],['='],[id(V)].
%---------------------------------------------------------------------------
comparelist(<,data(X,_,_,_),data(X,_,_,_)).
comparelist(>,data(X,_,_,_),data(Y,_,_,_)) :- X > Y.
comparelist(<,data(X,_,_,_),data(Y,_,_,_)) :- X < Y.
%----------------------------------------------------------------------
process(P,FL,SL) :-
   get_time_list(P,L),sort(L,SL),time_basedlist(P,SL,TL),
remove_nav(TL,FL).

get_time_list([],[]).
get_time_list([data(X,_,_,_)|T],[X|XT]) :- 
   get_time_list(T,XT).


time_basedlist(_,[],[]).
time_basedlist(P,[X|TT],[L1|L2]) :- 
  find_entry(P,X,L1,Re),time_basedlist(Re,TT,L2).

find_entry([],_,[],[]).
find_entry([data(X,Y,Z,V)|PT],X,[[X,Y,Z,V]|L],Re) :- 
   find_entry(PT,X,L,Re).
find_entry([data(X,Y,Z,V)|PT],T,[],[data(X,Y,Z,V)|PT]) :- 
   X > T,!.  
find_entry(PT,[X],[L],Re) :- 
   find_entry(PT,X,L,Re).
find_entry([_|PT],X,L,Re) :- 
   find_entry(PT,X,L,Re).
remove_nav([],[]).
remove_nav([X|T],[L1|L2]) :- 
   remove_nav1(X,X,L1),remove_nav(T,L2).
   
remove_nav1(L,[],L).
remove_nav1(IL, [[X,Y,Z,V]|T],OL) :- 
   V='NaV',check(IL,[X,Y,Z,V]),
   delete(IL,[X,Y,Z,V],L),remove_nav1(L,T,OL).
remove_nav1(IL,[_|T],L) :- 
   remove_nav1(IL,T,L).
%---------------------------PROPERTY CHECK CODE STARTS HERE----------------------------------------
%___----------------------------------------------------------------------------------------------
getStateList(File,StList,TimeList) :-
    open(File, read,S),
    lex(S,X),
    close(S),parse(P,X,[]),
    predsort(comparelist,P,PL),extractstate(PL,StList),
    get_time_list(PL,TimeList).
%--------------------------------------------------------------------------------------------------
extractstate([],[]).
extractstate([data(_,X,Y,Z)|T],[Val|VT]):- string_concat(X,'=>',X1),string_concat(X1,Y,X2),string_concat(X2,'=',X3),string_concat(X3,Z,Val),
                               extractstate(T,VT).
%----------------------------------------------------------------------------------------------------
prop_check([]).
prop_check([H|T]):- H=id(X),check_var(X),prop_check1([X|T]).
prop_check1([H|T]) :- (H='G' -> prop_g(T) ; prop_f(T)).

prop_g([H|T]) :- H=id(X),begintag(X),check_var(T,Var),nl,write(Var),getobject('logProp.txt',ObjList),
                 (memberchk(Var,ObjList) -> nl ; nl,print('Wrong Variable')),
                 check_var2(T,Var1),string_concat(Var,Var1,State),nl,write(State),
                  getStateList('logProp.txt',StList,TimeList),
                 (memberchk(State,StList) -> nl ; nl,print('Wrong Variable')),
                  occurrences_of(StList, State, Count),nl,write(Count),
                 last(TimeList,Last),nl,write(Last),
                 (Count=Last -> print('Property VERIFIED AND ITS TRUE '); print('Property VERIFIED AND ITS FALSE ')).
                 
prop_f([H|T]) :- H=id(X),begintag(X),check_var(T,Var),nl,write(Var),getobject('logProp.txt',ObjList),
                (memberchk(Var,ObjList) -> nl ; nl,print('Wrong Variable')),
                check_var2(T,Var1),string_concat(Var,Var1,State),nl,write(State),
                 getStateList('logProp.txt',StList,TimeList),
                (memberchk(State,StList) -> nl ; nl,print('Wrong Variable')),
                (member(State,StList) -> print('Property VERIFIED AND ITS TRUE '); print('Property VERIFIED AND ITS FALSE ')).
%-------------------------------------------------------------------------------------------------
check_var(X) :- keyword(X),nl,write(X).
check_var([X,Y,Z|T],Var):- X=id(A),Z=id(C),string_concat(A,Y,X1),string_concat(X1,C,Var).
check_var2([_,_,_,X,Y|T],Var1) :- Y=id(W),string_concat(X,W,Var1).

occurrences_of(List, X, Count) :- aggregate_all(count, member(X, List), Count).

last([X],X).
last([H|T],R):- last(T,R).



%--to run
%---[propertyparser].
%---getproperty('G < Cont=>T=S0 >').
%---getproperty('G < Train=>S=1 >').
%---getproperty('G < Cont=>S=S0 >').
%---getproperty('F < Cont=>S=S2 >').
%---getproperty('F < Cont=>s=S0 >').
%---getproperty('F < Cont=>S=s0 >').
%---getproperty('F < Train=>S=1 >').


