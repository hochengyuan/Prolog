%HW4 Question2 Prolog Rules

% predicate if item is the member in the given list, which is useful for the following questions.
member(X,[X|_]):- !.
member(X,[_|T]) :- member(X,T).

% 1. remove_item(I , L , O). I is item
remove_item(_ , [] , []) :- !.
% adopt unification on atom in I and L.
remove_item(I , [L | Ls] , O) :- I = L , ! , remove_item(I , Ls, O).
remove_item(I , [L | Ls] , [L | O]) :- remove_item(I , Ls , O). 

% 2. remove_items(I , L , O).  I is list.
remove_items(_ , [] , []) :- !.
remove_items(I , [L | Ls] , O) :- member(L , I) , ! , remove_items(I , Ls , O).
remove_items(I , [L | Ls] , [L | O]) :- remove_items(I , Ls , O).

% 3. intersection
intersection([] , _ , []).
% after find L1 own membership of L2, remove all L1 in L1s to get L1x.
intersection([L1 | L1s] , L2, [L1 | F]) :- member(L1 , L2) , ! , remove_item(L1 , L1s , L1x ) , intersection(L1x , L2 , F).
intersection([L1 | L1s] , L2 , F) :- intersection(L1s , L2 , F).

% 4. is_set
is_set([]) :- !.
is_set([_ , []]) :- !.
is_set([Head | Tail]) :- \+ member(Head , Tail) , is_set(Tail).


% 5. disjunct_union
% disjunct_union is combine Left_Outer_Join and Right_Outer_Join together. Thus, we define left_right_outer_join first.

left_right_outer_join([], _, []).
left_right_outer_join([Head | Tail], L2, [Head|Differences]) :-
    \+ member(Head, L2), ! , remove_item(Head , Tail , Tailx) , left_right_outer_join(Tailx, L2, Differences).
left_right_outer_join([_|Tail], L2, Differences) :- left_right_outer_join(Tail, L2, Differences).

% Then, do two times left_right_outer_join to find the disjunct_union.
disjunct_union(L1, L2, U) :-
    left_right_outer_join(L1, L2, Left_Outer_Join) , left_right_outer_join(L2, L1, Right_Outer_Join) , append(Left_Outer_Join , Right_Outer_Join, U).


% 6. remove_dups(L1,L2).
% if no duplicate, totally copy the original to the result.
remove_dups([] , []).
remove_dups([L1] , [L1]).
% recusion: check whether head item is member of tail or not.
% if head is not the member of the tail, then do remove_dups the next head of tail.
remove_dups([L1 | L1s] , [L1 | L2]) :- \+ member(L1 , L1s), ! , remove_dups(L1s , L2).
remove_dups([L1 | L1s] , L2) :- remove_dups(L1s , L2).

% 7. union(L1 , L2 , U).
union([] , L2 , L2s) :- remove_dups(L2 , L2s).
union([L1 | L1s] , L2 , [L1 | U]) :- remove_item(L1 , L1s , L1x) , remove_item(L1 , L2 , L2x) , union(L1x , L2x , U).