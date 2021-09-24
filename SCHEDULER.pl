append1([],L,L).
append1([H|T],L,[H|T1]):- append1(T,L,T1).
 
length1([],0).
length1([_|Tail],N) :- length1(Tail,Prev),N is Prev+1.
 
indexOf([Element|_], Element, 0). % We found the element
indexOf([_|Tail], Element, Index):-
  indexOf(Tail, Element, Index1), % Check in the tail of the list
  Index is Index1+1.  % and increment the resulting index
 
courses([(csen403,2),(csen905,2),(csen709,1),(csen601,2),(csen301,3),(csen701,2)
,(csen503,3),(csen501,2)]).
 
slots([slot(sunday,1),slot(sunday,2),slot(sunday,3),slot(monday,1),slot(monday,2),
slot(monday,3),slot(tuesday,1),slot(tuesday,2),slot(tuesday,3),slot(wednesday,1),
slot(wednesday,2),slot(wednesday,3)]).
 
%putSlots([]).
putSlots([(slot(X,Y),_)|T]):-
  putSlotsHelper([(slot(X,Y),_)|T],0)  .
 
putSlotsHelper(_,I):- slots(A) ,length(A,Lengthh),I=Lengthh.
 
putSlotsHelper([(slot(X,Y),_)|T],I):-
    slots(List),
    indexOf(List,slot(X,Y),Index),
    I=Index,
    I1 is I+1,
    putSlotsHelper(T,I1).
 
courseNotDone([],_).
courseNotDone([H|T],Subject):- \+H=Subject, courseNotDone(T,Subject).
 
pickAnotDoneCourse([(H,HH)|_],List,(H,HH)):-courseNotDone(List,H).
pickAnotDoneCourse([_|T],List,C):-pickAnotDoneCourse(T,List,C).
 
%scheduleCourse([ (slot(sunday, 1), X), (slot(sunday, 2), Y), (slot(sunday, 3), Z)]
%,csen502,2).
 
%scheduleCourse(_,_,0)
scheduleCourse([ (slot(_, _),X)|Tail],Name,N):-
    \+ Name=X,
    scheduleCourse(Tail,Name,N)
    ;
    Name=X,
    N1 is N-1,
    scheduleCourse2(Tail,Name,N1).
 
scheduleCourse2(_,_,0).
scheduleCourse2([(slot(Day, Slot),X)|Tail],Name,N):-
    X=Name,
    N1 is N-1,
    scheduleCourse2([(slot(Day, Slot),X)|Tail],Name,N1).
 
removeFromNotDone(_,[],_).
removeFromNotDone(C,[C|T],T).
removeFromNotDone(C,[X|T],[X|T1]):-
    \+ C=X,
    removeFromNotDone(C,T,T1).
 
schedule(_,L,_,A,DoneSubjN):-
       A=<1,
    length(L,DoneSubjN).
 
schedule(L,DoneSubjects,NotDoneSubjects,AvailableSlots,DoneSubjN):-
    pickAnotDoneCourse(NotDoneSubjects,DoneSubjects,(Cname,Ccount)),
    AvailableSlots >= Ccount,
    AvailableSlots2 is AvailableSlots - Ccount ,
    removeFromNotDone((Cname,Ccount),NotDoneSubjects,NewNotDoneSubjects),
    scheduleCourse(L,Cname,Ccount),
    append(DoneSubjects,[(Cname,Ccount)],NewDoneSubjects),
    schedule(L,NewDoneSubjects,NewNotDoneSubjects,AvailableSlots2,DoneSubjN).
 
 
 
solve(L,DoneSubjN):-
putSlots(L),
length(L,N),
courses(NotDone),
sort(2,@=<,NotDone,SortedCourses), %A greedy approach to the maximization problem at hand
schedule(L,[],SortedCourses,N,DoneSubjN).
