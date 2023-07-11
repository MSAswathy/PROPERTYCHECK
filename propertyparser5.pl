getproperty(In):-
    string_to_list(In,L),tokenize(L,L1),write(L1),
    getobject('log3sample.txt',ObjList),getStateList('log3sample.txt',StList,TimeList),
    (prop_check(L1,ObjList,StList,TimeList) -> nl,print('Property VERIFIED AND ITS TRUE '); nl,print('Property VERIFIED AND ITS FALSE:- Invalid syntax or Property is negative')).
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
special('~',[126|L],L).
special('&',[38|L],L).
special('|',[124|L],L).

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
begintag(<).
endtag(>).
logop('&').
logop('|').
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

check([[X,Y,Z,V]|_],[X,Y,Z,IV]) :- V \= IV.
check([_|T],X) :- check(T,X).
%---------------------------PROPERTY CHECK CODE STARTS HERE----------------------------------------
%___----------------------------------------------------------------------------------------------
getStateList(File,StList,TimeList) :-
    open(File, read,S),
    lex(S,X),close(S),
    parse(P,X,[]),
    predsort(comparelist,P,PL),process(PL,FL,SL),extractstate(FL,StList),
    get_time_list(PL,TimeList).
%--------------------------------------------------------------------------------------------------
%--------------------------------------------------------------------------------------------------
%----------------------------------------------------------------------------------------------------
extractstate([],[]).
extractstate([H|T],[St|StList]):- extractstate1(H,St),extractstate(T,StList).
extractstate1([],[]).
extractstate1([[_,X,Y,Z]|T],[Val|VT]):- string_concat(X,'=>',X1),string_concat(X1,Y,X2),string_concat(X2,'=',X3),string_concat(X3,Z,Val),
                                        extractstate1(T,VT).  
                                                                               
%------------------------------------------------------------------------------------------------------------------------------------------
prop_check([],[],[],[]):- !.
prop_check([H|T],OL,SL,TL):-   H=id(X),keyword(X),T=[X1|T1],prop_check([X,X1|T1],OL,SL,TL).
prop_check([X,Y|T],OL,SL,TL):- keyword(X),Y=id(X1),begintag(X1),last(T,ET),ET=id(E),endtag(E),
                                (X='G' -> prop_g(T,OL,SL,TL); prop_f(T,OL,SL,TL)).
prop_check([H|T],OL,SL,TL):- H=id(X),keyword(X),T=[X1|T1],X1='~',nl,write(X1),
                             prop_checkfornegation([X|T1],OL,SL,TL).
%-------------------negation with & symbol-----------------------------
prop_checkfornegation([],[],[],[]):-  !.                          
prop_checkfornegation([X|T],OL,SL,TL):- keyword(X),T=[Y|T1],Y=id(X1),begintag(X1),
                                        prop_checkfornegation([X|T1],OL,SL,TL),
                                        last(T1,ET),ET=id(E),endtag(E).
prop_checkfornegation([X|T],OL,SL,TL):- keyword(X),T=[A,B,C,D,E,id(ET),Z|T1],check_var([A,B,C],OVar),memberchk(OVar,OL),
                                        check_var2([D,E],SVar),string_concat(OVar,SVar,State),
                                        endtag(ET),logop(Z),
                                        prop_checkfornegation1([X,Z|T1],OL,SL,TL,[State]).
prop_checkfornegation1([],[],[],[],[]):- !.                                       
prop_checkfornegation1([X,Y|T],OL,SL,TL,ChkSt):- keyword(X),T=[id(BT),A,B,C,D,E,id(ET),Z|T1],
                                        begintag(BT),check_var([A,B,C],OVar),memberchk(OVar,OL),
                                        check_var2([D,E],SVar),string_concat(OVar,SVar,State),append(ChkSt,[State],Final),
                                        endtag(ET),(logop(Z) ->
                                        prop_checkfornegation1([X,Y|T1],OL,SL,TL,Final); prop_checkfornegation1([X,Y],SL,Final)).   
prop_checkfornegation1([],[],[]):- !.
prop_checkfornegation1(Key,SL,ChkSt)  :-memberchk('G',Key),memberchk('&',Key),\+ subseq(ChkSt,SL).

subseq([], []):-!.
subseq([H|T],[Head|Tail]) :-memberchk(H,Head),subseq(T,[Head]),!;subseq([H|T],Tail).
subseq([],[Tail]):-!.
  
%------------------------------------------------------------------------------
prop_g([],[],[],[]):- !.
prop_g([X,Y,Z|T],OL,SL,TL) :- check_var([X,Y,Z],Var),memberchk(Var,OL), !,prop_g([Var|T],SL,TL).
prop_g([H|T],SL,TL) :- check_var2(T,Var1),string_concat(H,Var1,State),
                                traverseallstatechk(State,SL), 
                                nl,write(State),print('         State holds at every timeline.'), !.

%-----------------------------------------------------------------------------------------------------
traverseallstatechk(_, []).
traverseallstatechk(X, [H | T]) :- member(X,H),traverseallstatechk(X,T). %Recursive call looking in the tail
traverseallstatechk(X,[T]) :-  traverseallstatechk(X,T). %You look for all the subsequent cases
%-----------------------------------------------------------------------------------------------------
%-----------------------------------------------------------------
             
prop_f([],[],[],[]):- !. 
prop_f([X,Y,Z|T],OL,SL,TL) :- check_var([X,Y,Z],Var),memberchk(Var,OL),prop_f1([Var|T],SL,TL).                
prop_f1([H|T],SL,TL) :- check_var2(T,Var1),string_concat(H,Var1,State),
                                traversestatechk(State,SL),nl,write(State),print('         State holds at some point in timeline.').
%-------------------------------------------------------------------------------------------------
traversestatechk([], []).
traversestatechk(X, [H | T]) :- member(X,H) -> !;traversestatechk(X,T). %Recursive call looking in the tail
traversestatechk(X,[T]) :-  traversestatechk(X,T). %You look for all the subsequent cases
%-------------------------------------------------------
check_var([X,Y,Z|T],Var):- X=id(A),Z=id(C),string_concat(A,Y,X1),string_concat(X1,C,Var).
check_var2([X,Y|T],Var1) :- Y=id(W),string_concat(X,W,Var1).

last([X],X).
last([H|T],R):- last(T,R).

removelast([X|R],Rest) :- removelast(R,Rest,X).
removelast([],[],_).
removelast([PossiblyLast|R],[NotLast|S],NotLast) :-
     removelast(R,S,PossiblyLast).



%--to run
%---[propertyparser5].
%---getproperty('G  < NS=>C=red >'). True for log1sample.txt
%---getproperty('g  < NS=>C=red >'). 'Property VERIFIED AND ITS FALSE:- Invalid syntax or Property is negative'
%---getproperty('F  < NS=>C=red >'). True for log1sample.txt
%---getproperty('F  < NS=>C=green >').  True for log1sample.txt
%---getproperty('f  < NS=>C=green >').   Bad syntax
%---getproperty('G ~ < < NS=>C=green  > & < EW=>C=green  > >'). False for log2sample.txt
%---getproperty('G ~ < < NS=>C=green  > & < EW=>C=green  > >'). True for log3sample.txt
%---getproperty('g ~ < < NS=>C=green  > & < EW=>C=green  > >'). syntax Error
%---getproperty('G ~ < < NS=>C=green  > & < EW=>C=green  > '). Syntax error

