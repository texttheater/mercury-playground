:- module cls.
:- interface.
:- import_module io.
:- pred cls(io::di, io::uo) is det.
:- implementation.
:- import_module list.

cls(!IO) :-
  format("\033\[H\033\[2J",[],!IO).
