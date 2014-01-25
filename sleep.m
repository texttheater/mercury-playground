:- module sleep.

/*
   Provides a predicate sleep/1 where sleep(Time) suspends execution for Time
   seconds, like SWI-Prolog's sleep/1.
   Based on a solution for sleep given at
   http://lists.mercurylang.org/pipermail/developers/2009-March/015216.html
   Further down that thread there is some information how we could make this
   portable to Windows (currently only works in recent Unix-like environments).
   FIXME irritating warning about supposed implicit function declaration
*/

:- interface.
:- import_module io.
:- impure pred sleep(float::in) is det.
:- pred sleep(float::in, io::di, io::uo) is det.

:- implementation.
:- pragma foreign_decl("C", "#include <math.h>").
:- pragma foreign_decl("C", "#include <time.h>").

:- pragma promise_pure(sleep/3).
sleep(Seconds, !IO) :-
    impure sleep(Seconds).

:- pragma foreign_proc("C", sleep(Seconds::in),
    [will_not_call_mercury, thread_safe], "
    struct timespec req;
    req.tv_sec = (time_t) Seconds;
    req.tv_nsec = (long) ((Seconds - floor(Seconds)) * 1000000000);
    nanosleep(&req, &req);
").
