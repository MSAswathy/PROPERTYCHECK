diagram(File,In,Res1,Res2,Res3):- 
   getvalues(File,In,Res1,Res2,Res3).

diagram(File,In):- 
   getvalues(File,In,Res),getstates(Res,'(*)',Scr),sort(Scr,States),write('@startuml\n'),
   writestates(States),
   write('@enduml\n \n Paste it in: http://www.plantuml.com/plantuml').

getvalues(File,In,TStates,AStates,LineData):-
    open(File, read,S),
    lex(S,X),
    string_to_list(In,L),tokenize(L,L1),parse_input(VarList,L1,[]),
    close(S),parse(P,X,[]),
    predsort(comparelist,P,PL),process(PL,FL,SL),getstatedata(FL,VarList,Line),append(['\'x\''],SL,XD),append([XD],Line,LineData),
    get_plantml_states(FL,VarList,Res),write_time_diagram(Res,'\'[*]\'',TStates),
    getstates(Res,['(*)'],Scr),remove_duplicates(Scr,St),writestates(St,AStates).
    
compareval(=,X,X).
compareval(<,_,_).

remove_duplicates([],[]).

remove_duplicates([H | T], List) :-    
     member(H, T),
     remove_duplicates( T, List).

remove_duplicates([H | T], [H|T1]) :- 
      \+member(H, T),
      remove_duplicates( T, T1).
    
getobject(File,ObjList) :-
    open(File, read,S),
    lex(S,X),
    close(S),parse(P,X,[]),
    predsort(comparelist,P,PL),extractvars(PL,Res),sort(Res,ObjList).

extractvars([],[]).
extractvars([data(_,X,Y,_)|T],[Val|VT]):- 
   string_concat(X,'=>',X1),string_concat(X1,Y,Val),extractvars(T,VT).
    
write_time_diagram([],_,[]).
write_time_diagram([X|T],Prev,[Re1|Re2]):- 
   write_state(X,Prev,Curr,Re1),write_time_diagram(T,Curr,Re2).
   
write_state([],_,_,[]).
write_state([[X,Y]|T],Prev,X,[Prev,Arrow,X,'\':\'',Res]):-
    (Prev='\'[*]\''->Arrow='\'->\'';Mod is mod(Prev,10), (Mod = 0 ->Arrow='\' -down->\'';
    Mod1 is mod(Prev,20),(Mod1>10->Arrow='\' -left->\'';Arrow='\' -right->\''))),
    write_state1(T,Re) ,append([Y],Re,Ret),add_quotes_list(Ret,Res1),add_single_quotes(Res1,Res).
add_single_quotes([],[]).    
add_single_quotes(A,AB):- 
   atom_concat('\'',A,I),atom_concat(I,'\'',AB).
add_quotes_list([],[]).
add_quotes_list([X],X).
add_quotes_list([X|T],Res):- 
   add_quotes_list(T,RT),atom_concat(X,',',RX),atom_concat(RX,RT,Res).
    
write_state1([],[]).
write_state1([[_,Y]|T],[Y|RT]):- write_state1(T,RT).

parse([],[],[]).
parse([X|T]) --> 
   parse1(X),parse(T).
parse1(data(X,Y,Z,V)) --> 
   [id('Time')],['='],[id(X)],[':'],[id('Obj')],['='],[id(Y)],
   [':'],[id('Var')],['='],[id(Z)],[':'],[id('Val')],['='],[id(V)].
   
   
parse_input([],[],[]).
parse_input([X|T]) --> 
  parse_input1(X),[':'],parse_input(T).
parse_input([X]) --> 
   parse_input1(X).
parse_input1([Obj,Var]) --> 
   [id(Obj)],['=>'],[id(Var)].

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

comparelist(<,data(X,_,_,_),data(X,_,_,_)).
comparelist(>,data(X,_,_,_),data(Y,_,_,_)) :- X > Y.
comparelist(<,data(X,_,_,_),data(Y,_,_,_)) :- X < Y.


getstatedata(_,[],[]).
getstatedata(X,[SV|SVT],[H|T]):-
    [O,V]=SV,atom_concat(O,'.',He1),atom_concat(He1,V,He),add_single_quotes(He,VN), getstatedata1(X,SV,TS),append([VN],TS,H),getstatedata(X,SVT,T).
    
getstatedata1([],_,[]).
getstatedata1([XT|DT],SV,R):-
    iteratedata(XT,SV,RX),last(RX,RXX),getstatedata1(DT,SV,RT),append([RXX],RT,R).
iteratedata([],_,[]).
iteratedata([[_,X,Y,Z]|DT],[X,Y],[Z|RD]):-
    iteratedata(DT,[X,Y],RD).
iteratedata([_|DT],SV,RD):-
    iteratedata(DT,SV,RD).

get_plantml_states([],_,[]).
get_plantml_states([X|T],SV,[L1|L2]) :- 
   get_plantml_states1(X,SV,L1),get_plantml_states(T,SV,L2).
get_plantml_states1(_,[],[]).
get_plantml_states1(TL,[[Y,Z]|SVT],L) :- 
  iterate_over_timelist(TL,[Y,Z],L1),last(L1,L11),
  get_plantml_states1(TL,SVT,L2),append([L11],L2,L).
get_plantml_states1(TL,[[Y,Z]],L1) :- 
   iterate_over_timelist(TL,[Y,Z],L1).

iterate_over_timelist([],_,[]).
iterate_over_timelist([[X,Y,Z,V]|T],[Y,Z],[[X,V]|VL]) :- 
   iterate_over_timelist(T,[Y,Z],VL).
iterate_over_timelist([_|T],YZ,L):- 
  iterate_over_timelist(T,YZ,L).
%--------------------------------
getstates([],_,[]).
getstates([X|T],Prev,[[Prev,V]|ResT]) :- 
   extractvalues(X,V),getstates(T,V,ResT).

extractvalues([],[]).
extractvalues([[_,Y]|T],[Y|TT]):- 
   extractvalues(T,TT).
   
writestates([],[]).
writestates([[A,B]|T],[[RA,'\'-->\'',RB]|RT]) :- 
   writeval(A,IA),add_quotes(IA,RA),writeval(B,IB),
   add_quotes(IB,RB),writestates(T,RT).
writeval([],[]).
writeval([X],X).
writeval([X|T],R):- writeval(T,RT),atom_concat(X,',',IX),atom_concat(IX,RT,R).
add_quotes([],[]).
add_quotes('(*)',AB):-atom_concat('\"','(*)',I),atom_concat(I,'\"',AB).
add_quotes(A,AB):- atom_concat('\'\"',A,I),atom_concat(I,'\"\'',AB).
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




identifier(id(N)) --> 
   ident(L), {name(N,L)}.

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


