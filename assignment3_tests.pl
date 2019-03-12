:- use_module(assignment3).

% TODO: enable for development of 4.1, 4.2
course(cmput325).
course(cmput175).
course(cmput201).
course(cmput204).
prerequisite(cmput204, cmput325).
prerequisite(cmput175, cmput201).
prerequisite(cmput175, cmput204).

:- begin_tests(assignment3).

test(alternate, all(L=[[], []])) :- alternate([], [], L).
test(alternate, all(L=[[1]])) :- alternate([1], [], L).
test(alternate, all(L=[[1]])) :- alternate([], [1], L).
test(alternate, all(L=[[1,2,3]])) :- alternate([1,3], [2], L).
test(alternate, all(L=[[1,2,3,4], [1,2,3,4]])) :- alternate([1,3], [2,4], L).
test(alternate, all(L=[[1,2,3,4,6]])) :- alternate([1,3], [2,4,6], L).
test(alternate, all(L=[[1,2,3,4,5,6,7]])) :- alternate([1,3,5,7], [2,4,6], L).

test(counti, all(Count=[5])) :- counti([1, 2, 3, 4, 5], N), Count is N.
test(counti, all(Count=[5])) :- counti([1, [2, 3, 4, 5]], N), Count is N.
test(counti, all(Count=[5])) :- counti([1, [2], [3, 4], [[5]]], N), Count is N.
test(counti, all(Count=[5])) :- counti([1, ['foo', 2], [3, 4], ['foo', 'foo', [5]], 'foo'], N), Count is N.

test(umem, all(X=[a, b])) :- umem(X, [a,a,b]).
test(umem, all(X=[1,a,2,3,b,4])) :- umem(X, [1,a,2,a,3,b,4]).
test(umem, all(X=[1,a,A,2,3,b,B,4])) :- umem(X, [1,a,A,2,a,A,3,b,B,4]).
test(umem, all(X=[1,a,A,2,3,b,B,4])) :- umem(X, [1,a,A,2,2,a,A,3,3,3,b,B,4,4,4,4]).

test(required, all(L=[[cmput175, cmput204]])) :- required(cmput325, L).

test(can_take, all(L=[[cmput201, cmput204]])) :- findall(C, can_take([cmput175], C), L).

:- end_tests(assignment3).
