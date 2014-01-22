:- module intutil.
:- interface.
:- import_module int.
:- import_module list.
:- func range(int, int) = list(int).
:- implementation.
:- import_module solutions.

range(Lo, Hi) = Range :-
    solutions(
            (pred(I::out) is nondet :- nondet_int_in_range(Lo, Hi, I)),
            Range).
