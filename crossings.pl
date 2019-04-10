%% File: crossings.pl
%% Name: Noah-Vincenz Noeh
%% Date: 01/11/2018
%%
%% This program is a solution to Prolog 531 Assessed Exercise 2 'Crossings'
%% The exercise is a version of the classic Farmer-Wolf-Goat-Cabbage Puzzle

%% Step 1 safe(+Bank)
% a bank is safe if 1) wolf and goat are together and there is no farmer or 2) goat and cabbage are together and there is no farmer
safe(Bank)  :-
  \+ (member(w, Bank), member(g, Bank), \+ member(f, Bank)),
  \+ (member(g, Bank), member(c, Bank), \+ member(f, Bank)).


%% Step 2 goal(+State)
goal([]-SouthBank)  :-
  length(SouthBank, 5),
  member(g, SouthBank),
  member(c, SouthBank),
  member(w, SouthBank),
  member(f, SouthBank),
  member(b, SouthBank).


%% Step 3 equiv(+State1, +State2)
% this will match if all items are the same f.ex. all are empty
equiv(A-A, A-A).

% A and C must be the same length and B and D must be the same length
equiv(A-B, C-D) :-
  length(A, X),
  length(C, X),
  length(B, Y),
  length(D, Y),
  equiv_states(A-B, C-D).

% if both A and B are non-empty
equiv_states([HeadA|TailA]-[HeadB|TailB], C-D)  :-
  member(HeadA, C),
  member(HeadB, D),
  equiv_states(TailA-TailB, C-D).

% if A is empty
equiv_states([]-[HeadB|TailB], C-D) :-
  member(HeadB, D),
  equiv_states([]-TailB, C-D).

% if B is empty
equiv_states([HeadA|TailA]-[], C-D) :-
  member(HeadA, C),
  equiv_states(TailA-[], C-D).

% if A and B are both empty - note: this is only after items have been removed from A and B
equiv_states([]-[], _-_).


%% Step 4 visited(+State, +Sequence)
% if the state is equivalent to the head of the sequence X then succeed
visited(State, [X|_]) :-
  equiv(State, X).

% else check if state is equivalent to next element in the sequence
visited(State, [_|Tail])  :-
  visited(State, Tail).

% if the sequence is empty then we cannot have visited the state - fail
visited(_, [])  :-
  fail.


%% Step 5 choose(-Items, +Bank)
% can only choose items if 'f' is a member of the bank
choose(Items, Bank):-
  member(f, Bank),
  remove_list(Bank, [f], NewList),
  choose_items(Items, [f], NewList, NewList),
  remove_list(Bank, Items, NewerList),          % otherwise [f] gets added for every single case
  safe(NewerList).

% we want to find any combination of another item together with 'f' that keeps the leftover items in the bank safe (possibly just 'f')
choose_items(Items, Acc, [X|Tail], Bank) :-
  length(Acc, N),
  N < 2,
  remove_list(Bank, [X|Acc], NewList),
  safe(NewList),
  choose_items(Items, [X|Acc], Tail, Bank).

% This case is used when we already have two elements in Acc then we do not want to keep going through leftovers in bank but we want to output result
choose_items(Items, Acc, [_|_], Bank) :-
  length(Acc, N),
  N = 2,!,
  choose_items(Items, Acc, [], Bank).

% if any of the parts of the if clauses above fail then this is called - skips head and checks next element
choose_items(Items, Acc, [_|Tail], Bank) :-
  choose_items(Items, Acc, Tail, Bank).

choose_items(Acc, Acc, [], _).

% remove_list(ListA, ListB, ListC) removes all items in ListB from ListA and the output is ListC
remove_list([], _, []).

remove_list([X|Tail], ListB, ListOutput)  :-
  member(X, ListB), !,
  remove_list(Tail, ListB, ListOutput).

remove_list([X|Tail], ListB, [X|ListOutput])  :-
  remove_list(Tail, ListB, ListOutput).


%% Step 6 journey(+State1, -State2)
% if f is a member of A then find items from choose append them to B in order to get D and remove them from A in order to get C - if any of these fail then fail
journey(A-B, C-D) :-
  member(f,A),!,
  choose(Items, A),
  append(B, Items, D),
  remove_list(A, Items, C).

journey(A-B, C-D) :-
  member(f,B),
  choose(Items, B),
  append(A, Items, C),
  remove_list(B, Items, D).


%% Step 7 succeeds(-Sequence)
succeeds(Sequence) :-
  extend([ [f,w,g,c,b]-[] ], ReversedSequence),
  reverse(ReversedSequence, Sequence).  % the sequence was reversed by appending to the front of the sequence, so we need to reverse it in order get the sequence from start to end order

% if the last state in the sequence (head) is a goal state then return
extend([X|StatesVisitedSequence], [X|StatesVisitedSequence]) :-
  goal(X).

extend(StatesVisitedSequence, FinalSequence) :-
  StatesVisitedSequence = [X|_],
  journey(X, SomeNextState),
  \+ visited(SomeNextState, StatesVisitedSequence),
  extend([SomeNextState|StatesVisitedSequence], FinalSequence).

% reverses the sequence as adding elements to the head of the sequence keeps the last element at the front
reverse(Sequence, NewSequence) :-
  reverse_sequence(Sequence, [], NewSequence).

reverse_sequence([], NewSequence, NewSequence).

reverse_sequence([X|Tail], Acc, NewSequence) :-
  reverse_sequence(Tail, [X|Acc], NewSequence).


%% Step 8 fee(+State1, +State2, -Fee)
% checks the difference of elements in the banks of the states to check whether the farmer travelled alone (cost 1) or with another item (cost 2)
fee(A-B, C-D, Fee) :-
  length(A, LengthA),
  length(B, LengthB),
  length(C, LengthC),
  length(D, LengthD),
  calc_fee(LengthA, LengthB, LengthC, LengthD, Fee).

calc_fee(LengthA, _, LengthC, _, Fee) :-
  LengthA is LengthC+1,!,
  fees(Fee, _).

calc_fee(_, LengthB, _, LengthD, Fee) :-
  LengthB is LengthD+1,!,
  fees(Fee, _).

calc_fee(LengthA, _, LengthC, _, Fee) :-
  LengthC is LengthA+1,!,
  fees(Fee, _).

calc_fee(_, LengthB, _, LengthD, Fee) :-
  LengthD is LengthB+1,!,
  fees(Fee, _).

calc_fee(_, _, _, _, Fee) :-
  fees(_, Fee).

fees(1, 2).


%% Step 9 cost(-Sequence, -Cost)
cost(Sequence, Cost) :-
  succeeds(Sequence),
  calculate_cost(Sequence, 0, Cost).

% uses an accumulator to add a fee for each trip between states
calculate_cost([X|Tail], Acc, Cost) :-
  Tail = [Y|_],!,
  fee(X,Y,Fee),
  NewAcc is Acc + Fee,
  calculate_cost(Tail, NewAcc, Cost).

calculate_cost(_, Cost, Cost).
