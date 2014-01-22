:- module life.

/*
  Conway's Game of Life
  Usage: life BOARDFILE
  Example board files (block.txt, blinker.txt...) are provided.
  Go to the next generation by hitting Return.
  End the program by hitting Ctrl+C.
*/

:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module exception.
:- import_module list.
:- import_module char.
:- import_module cls.
:- import_module int.
:- import_module intutil.
:- import_module string.
:- import_module univ.

:- type cell_state ---> dead ; live.
:- type row == list(cell_state).
:- type board == list(row).

main(!IO) :-
    try_io(life, Result, !IO),
    (  Result = succeeded(_)
    ;  Result = exception(Exception),
       output_exception(Exception, !IO),
       set_exit_status(1, !IO)
    ).

:- pred output_exception(univ::in, io::di, io::uo) is det.
output_exception(Exception, !IO) :-
    (  if   univ_to_type(Exception, Message)
       then format(stderr_stream, "%s\n", [s(Message)], !IO)
       else print("ERROR: ", !IO),
            print(Exception, !IO),
            print("\n", !IO)
    ).

/* The first argument is just to keep try_io happy, could we avoid it? */
:- pred life(int::out, io::di, io::uo) is cc_multi.
life(1, !IO) :-
    command_line_arguments(Args, !IO),
    (  if   Args = [Filename]
       then life2(Filename, !IO)
       else throw("Usage: life BOARDFILE")
    ).

:- pred life2(string::in, io::di, io::uo) is cc_multi.
life2(Filename, !IO) :-
    see(Filename, SeeResult, !IO),
    (  SeeResult = ok,
       life3(!IO)
    ;  SeeResult = error(Error),
       throw(error_message(Error) `with_type` string)
    ).

:- pred life3(io::di, io::uo) is cc_multi.
life3(!IO) :-
    read_board(Board, !IO),
    seen(!IO),
    evolve(Board, !IO).

/* TODO usage of read_line/3 leads to not very elegant code in read_row/2,
        we should process the file char by char. */
:- pred read_board(board::out, io::di, io::uo) is cc_multi.
read_board(Board, !IO) :-
    read_line(Result, !IO),
    (  Result = ok(Line),
       read_board2(Line, Board, !IO)
    ;  Result = eof,
       Board = []
    ;  Result = error(Error),
       throw(error_message(Error) `with_type` string)
    ).

:- pred read_board2(list(char)::in, board::out, io::di, io::uo) is cc_multi.
read_board2(Line, [Row|Rest], !IO) :-
    read_row(Line, Row),
    read_board(Rest, !IO).

:- pred read_row(list(char)::in, row::out) is cc_multi.
read_row([], []).
read_row([Char|Chars], Row) :-
    (  if      Char = ('.')
       then    read_row(Chars, Rest),
               Row = [dead|Rest]
       else if Char = 'O'
       then    read_row(Chars, Rest),
               Row = [live|Rest]
       else if Char = '\n'
       then    Row = []
       else    throw(string.format("invalid char: %c", [c(Char)]) `with_type`
                       string)
    ).

:- pred evolve(board::in, io::di, io::uo) is det.
evolve(Board, !IO) :-
    cls(!IO),
    write_board(Board, !IO),
    read_char(_, !IO),
    evolve(next_board(Board), !IO).

:- func get_state(int, int, board) = cell_state.
get_state(RowIndex, ColIndex, Board) = State :-
    (  if   index0(Board, RowIndex, Row),
            index0(Row, ColIndex, State0)
       then State = State0
       else State = dead
    ).

:- func next_state(int, int, board) = cell_state.
next_state(RowIndex, ColIndex, Board) = NextState :-
    CurrentState = get_state(RowIndex, ColIndex, Board),
    NeighborStates = [
            get_state(RowIndex - 1, ColIndex - 1, Board),
            get_state(RowIndex - 1, ColIndex, Board),
            get_state(RowIndex - 1, ColIndex + 1, Board),
            get_state(RowIndex, ColIndex - 1, Board),
            get_state(RowIndex, ColIndex + 1, Board),
            get_state(RowIndex + 1, ColIndex - 1, Board),
            get_state(RowIndex + 1, ColIndex, Board),
            get_state(RowIndex + 1, ColIndex + 1, Board)],
    filter((pred(live::in) is semidet), NeighborStates, LiveList),
    LiveNeighborsCount = length(LiveList),
    NextState = next_state_aux(CurrentState, LiveNeighborsCount).

:- func next_state_aux(cell_state, int) = cell_state.
next_state_aux(dead, Count) =
    (  if   Count = 3
       then live
       else dead
    ).
next_state_aux(live, Count) =
    (  if   Count < 2 ; Count > 3
       then dead
       else live
    ).

:- func next_board(board) = board.
next_board(Board) = NextBoard :-
    length(Board, NumRows),
    NumCols = (  if   NumRows = 0
                 then 0
                 else length(det_index0(Board, 0))
              ),
    NextBoard = map(
            (func(RowIndex) = map(
                    (func(ColIndex) = next_state(RowIndex, ColIndex, Board)),
                    range(0, NumCols - 1))),
            range(0, NumRows - 1)).

:- pred write_board(board::in, io::di, io::uo) is det.
write_board([], !IO).
write_board([Row|Rows], !IO) :-
    write_row(Row, !IO),
    write_board(Rows, !IO).

:- pred write_row(row::in, io::di, io::uo) is det.
write_row([], !IO) :-
    nl(!IO).
write_row([Cell|Cells], !IO) :-
    write_cell(Cell, !IO),
    write_row(Cells, !IO).

:- pred write_cell(cell_state::in, io::di, io::uo) is det.
write_cell(dead, !IO) :-
    print(".", !IO).
write_cell(live, !IO) :-
    print("O", !IO).
