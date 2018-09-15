% 4. Define aunt and uncle.
% aunt is female sibling of parent.
% uncle is male sibling of parent.
male(tom).
male(brian).
male(kevin).
male(zhane).
male(fred).
male(jake).
male(bob).
male(stephen).
male(paul).

female(melissa).
female(mary).
female(sarah).
female(jane).
female(jennifer).
female(emily).

parent(tom , stephen).
parent(stephen , jennifer).
parent(zhane , melissa).
parent(zhane , mary).
parent(melissa , brian).
parent(mary , sarah).
parent(bob , jane).
parent(tom, mary).
parent(paul , kevin).
parent(jake , bob).
parent(emily , bob).
parent(stephen , paul).

% relation spouse: two differnt parents has same child(ren).
spouse(A ,B) :- parent(A , X) , parent(B , X) , X \== A , X \== B , A \== B.

% relation sibling is for testing brother and sister sets.
sibling(X , Y) :- spouse(A , B) , parent(A , X) , parent(B , Y) , X \== Y.
sibling(X , Y) :- parent(Z , X) , parent(Z , Y) , X \== Y.

sister(X , Y) :- sibling(X , Y) , female(X) , X \== Y.
brother(X , Y) :- sibling(X , Y) , male(X) , X \== Y.

aunt(X , Y) :- sister(X , Z) , parent(Z , Y).
uncle(X , Y) :- brother(X , Z) , parent(Z , Y).