:- module sleep_test.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module sleep.

main(!IO) :-
    sleep(2.5, !IO).
