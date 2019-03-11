% Restrictions
%
% You can use all standard Prolog functions and operators in this assignment,
% except: you cannot use operators that modify the backtracking search, such
% as the cut operator !, the negation operators not, \+, or the if-then-else
% operator with -> and ;.

:- module(assignment3, [alternate/3, counti/2, umem/2, required/2, can_take/2, in_cycle/2]).

% 1 (1 mark)
%
% Define the predicate
%
% alternate(+L1, +L2, ?L)
%
% as follows: L is the result of taking elements from L1 and L2 alternately,
% and putting them into L. If one of L1 or L2 is longer than the other, then
% the extra elements go to the end.
%
% In our tests, L1 and L2 will be fully instantiated (no variables). L may be
% any term and may include some variables. See the test cases for examples.
%
% For your education, you should also experiment with what happens if either
% of L1 and L2 are variables, or are lists that contain some variables.
alternate(L1, [], L1).
alternate([], L2, L2).
alternate([L1E1 | L1T], [L2E1 | L2T], [L1E1, L2E1|L]) :- alternate(L1T, L2T, L).

% #2 (1 mark)
%
% Define the predicate
%
% counti(+L, ?N)
%
% Here, L is a fully instantiated, possibly nested list of integers and atoms.
% N is the count of how many integers appear anywhere within L. N can be given,
% or a variable. See the examples. Note that atom(X) is false for integers X.
counti([], 0).
counti([LE | LT], N + 1) :- counti(LT, N), integer(LE).
counti([LE | LT], N + B) :- counti(LE, N), counti(LT, B), is_list(LE).
counti([LE | LT], N) :- counti(LT, N), atom(LE).

% Test:
% ?- counti([1, ['foo', 2], [3, 4], ['foo', 'foo', [5]], 'foo', 6], A), X is A.
% A =  0+1+(0+1+1+(0+1+0+(0+1)))+1,
% X = 6

% #3 (1 mark)
%
% Define the predicate
%
% umem(-X,+L)
%
% as follows:
%
% Consider the behavior of the built-in predicate member on backtracking, in
% case an argument appears more than once in a list:
%
% ?- member(X, [a,a,b]).
% X = a ;
% X = a ;
% X = b.
%
% Your predicate umem should generate repeated arguments only once: the first
% time they appear in the list.
%
% Note: this question is simpler to solve with the cut operator, so the
% challenge here is how to do it without cut or negation or if-then-else.
%
% ?- umem(X, [a,a,b]).
% X = a ;
% X = b.

% research: looked at how member is implemented
% member(X, [Y|T]) :- X = Y; member(X, T).

% implementation:
umem(X, [Y|_]) :-  X \== Y, X = Y.
umem(X, [Y|T]) :-  umem(X, T), X \== Y.

% test:
% ?- umem(X, [1,1,B,A,B,C,D,E]).
% X = 1 ;
% X = B ;
% X = A ;
% X = C ;
% X = D ;
% X = E ;
% false.

% #4 Course Prerequisites (2 marks)
%
% You are given a database with facts about courses and their prerequisites,
% for example this is a3_miniDB.pl from our prolog code page:
%
% TODO: enable for development of 4
% course(cmput325).
% coursce(mput175).
% course(cmput201).
% course(cmput204).
% prerequisite(cmput204, cmput325).
% prerequisite(cmput175, cmput201).
% prerequisite(cmput175, cmput204).
%
% To keep things simple, the following (slightly unrealistic) constraints
% apply: All listed prerequisites are required for a course, and there are no
% options such as "take courseA or courseB".
%
% Now implement the following predicates:

% #4.1 The Predicate required
%
% required(+C,?L)
%
% Here, C is a given course, and L is the list of all courses which are direct
% prerequisites, or are indirectly required because they are prerequisite for
% other required courses. L should be sorted in the same order as in the facts
% database.
%
% Example: with the database above, a query
%
% ?- required(cmput325, L).
%
% should return
%
% L = [cmput175,cmput204]
%
% as its only solution.
required(C, []) :- course(C), \+ prerequisite(_, C).
required(C, RE) :- course(C), course(PR), prerequisite(PR, C), required(PR, R), append(R, [PR], RE).

% #4.2 The Predicate can_take
%
% can_take(+L,?C)
%
% Here, L is a given list of courses that a student has already taken. If C is
% also given,then the predicate should check whether the student has all the
% required courses for C. If C is a variable, then with backtracking, the
% predicate should produce one course at a time that the student can take now.
% Courses can be in any order, but each course should be generated only once,
% and you should not return any courses that the student has already taken.
%
% Example: with the database above, a query
%
% ?- can_take([cmput175], C).
%
% should return
%
% C = cmput201 ;
% C = cmput204
%
% as its only two solutions. Your order may be different.
%
% Equivalently, a call
%
% ?- findall(C, can_take([cmput175], C), L).
%
% should return
%
% L = [cmput201, cmput204].
%
% Note that if a course has no prerequisites at all, then that is also a
% course that you can take.

% helper function to check if the first list arguement is a subset of
% the second list arguement.
subset([ ],_).
subset([H|T],List) :-
    member(H,List),
    subset(T,List).

can_take(CL, C) :- required(C, R), subset(R, CL), \+ subset(R, [C]).

% #4.3 The Predicate in_cycle
%
% in_cycle(+C,-Cycle)
%
% The input is a course C. If there is a cyclic dependency between course
% prerequisites involving course C, then compute a cycle starting and ending
% with C, as follows: Return Cycle = [C1, C2, .., Cn-1, Cn], Such
% that C1 = Cn = C, and for all successive courses Ci, Ci+1 in the list,
% prerequisite(Ci, Ci+1) is a fact.
%
% In case there is no cycle involving C, the query should return
%
% false.
%
% Example: in the database above, there are no cycles at all, so all queries
% with that database should return false.
%
% Now, consider adding another fact to the database, as in sample file
% a3_mini_cycleDB.pl:
%
% prerequisite(cmput325, cmput175).
%
% Now there is a cycle involving cmput175, cmput204 and cmput325
% (but not cmput201). So that gives the following results:
%
% ?- in_cycle(cmput325, Cycle).
% Cycle = [cmput325, cmput175, cmput204, cmput325]
%
% ?- in_cycle(cmput175, Cycle).
% Cycle = [cmput175, cmput204, cmput325, cmput175]
%
% ?- in_cycle(cmput204, Cycle).
% Cycle = [cmput204, cmput325, cmput175, cmput204]
%
% ?- in_cycle(cmput201, Cycle).
% false.
%
% Which cycles to compute? It is possible that a course C is part of multiple
% cycles. For example, consider adding a second fact:
%
% prerequisite(cmput201, cmput175).
% prerequisite(cmput175, cmput175).
%
% Now cmput175 would be part of two different cycles. Your program can return
% either one of them (first), both are correct. We will only test the first
% cycle output by your program.
%
% Any cycle that you compute has to be simple: only the input course should
% occur twice in the output, at the beginning and at the end. No other course
% should appear more than once in a cycle.
%
% We recommend that you write your program in a way that generates each simple
% cycle involving a given course exactly once. However, we will only test the
% first solution as explained above.
%
% TODO: enable for development of 4.3
% prerequisite(cmput325, cmput175).
%
rcycle(C, [MR, C]) :- course(C), course(MR), prerequisite(MR, C).
rcycle(C, [PR, MR|R]) :- course(C), course(PR), course(MR), prerequisite(PR, MR), \+ prerequisite(MR, PR), C \== PR, C \== MR, rcycle(C, [MR|R]).
in_cycle(C, [PR, C]) :- course(C), course(PR), prerequisite(PR, C), PR == C.
in_cycle(C, [PR, MR|R]) :- course(C), course(PR), course(MR), prerequisite(PR, MR), C == PR, rcycle(C, [MR|R]).
