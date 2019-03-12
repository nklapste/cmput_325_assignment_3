:- use_module(assignment3).

% TODO: enable for development of 4.3
course(cmput325).
course(cmput175).
course(cmput201).
course(cmput204).
prerequisite(cmput204, cmput325).
prerequisite(cmput175, cmput201).
prerequisite(cmput175, cmput204).
prerequisite(cmput325, cmput175).
prerequisite(cmput201, cmput175).

:- begin_tests(assignment3_cycle_double).

test(cycle_cmput325, all(Cycle = [[cmput325, cmput175, cmput204, cmput325]])) :- in_cycle(cmput325, Cycle).
test(cycle_cmput175, all(Cycle = [[cmput175, cmput201, cmput175], [cmput175, cmput204, cmput325, cmput175]])) :- in_cycle(cmput175, Cycle).
test(cycle_cmput204, all(Cycle = [[cmput204, cmput325, cmput175, cmput204]])) :- in_cycle(cmput204, Cycle).

% note: this can infintely generate cycles
test(cycle_cmput201, Cycle=[cmput201, cmput175, cmput201]) :- in_cycle(cmput201, Cycle).

:- end_tests(assignment3_cycle_double).
