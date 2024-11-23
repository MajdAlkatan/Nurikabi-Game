:- use_module(library(pce)).
:- use_module(library(pce_style_item)).
:- dynamic solve_cell/3.
:- dynamic user_cell/3.
:- dynamic fxd_cell/3.

level(4, [
    fxd_cell(2, 4, 3),
    fxd_cell(3, 5, 1),
    fxd_cell(4, 4, 5),
    fxd_cell(6, 4, 1),
    fxd_cell(5, 5, 9)
]).

level(3, [
    fxd_cell(2, 1, 2),
    fxd_cell(1, 6, 4),
    fxd_cell(2, 5, 2),
    fxd_cell(3, 2, 3),
    fxd_cell(5, 6, 1),
    fxd_cell(6, 4, 5),
    fxd_cell(6, 7, 3),
    fxd_cell(7, 2, 2)
]).
level(2, [
           fxd_cell(1, 5, 7),
fxd_cell(1, 7, 1),
fxd_cell(3, 7, 5),
fxd_cell(5, 7, 7),
fxd_cell(7, 5, 2),
fxd_cell(7, 7, 1)

   ]).

level(1, [
           fxd_cell(1, 2, 3),
fxd_cell(1, 6, 1),
fxd_cell(3, 1, 2),
fxd_cell(3, 4, 1),
fxd_cell(5, 2, 1),
fxd_cell(5, 5, 2),
fxd_cell(6, 3, 2),
fxd_cell(7, 1, 1),
fxd_cell(7, 5, 1),
fxd_cell(7, 7, 6)
   ]).


init_user_grid(Level) :-
    retractall(fxd_cell(_, _, _)),
    level(Level, FixedCells),
    maplist(assertz, FixedCells),
    forall(between(1, 7, Row),
           forall(between(1, 7, Col),
                  ( fxd_cell(Row, Col, _) ->
                    assertz(user_cell(Row, Col, land))
                  ; assertz(user_cell(Row, Col, gray))))).

adjacent(X, Y, X1, Y) :- X1 is X + 1.
adjacent(X, Y, X1, Y) :- X1 is X - 1.
adjacent(X, Y, X, Y1) :- Y1 is Y + 1.
adjacent(X, Y, X, Y1) :- Y1 is Y - 1.

same_color_neighbors(Row, Col, Color, Neighbors) :-
    findall((R, C),
            (adjacent(Row, Col, R, C), user_cell(R, C, Color)),
            Neighbors).

connected_cells(Row, Col, Color, Connected) :-
    connected_cells(Row, Col, Color, [(Row, Col)], [], Connected).

connected_cells(_, _, _, [], Visited, Visited).
connected_cells(Row, Col, Color, [(R, C) | Rest], Visited, Connected) :-
    same_color_neighbors(R, C, Color, Neighbors),
    subtract(Neighbors, Visited, NewNeighbors),
    append(Rest, NewNeighbors, NewToVisit),
    connected_cells(Row, Col, Color, NewToVisit, [(R, C) | Visited], Connected).

sea_one :-
    user_cell(Row, Col, water),
    connected_cells(Row, Col, water, Sea),
    forall(user_cell(R, C, water), member((R, C), Sea)).

sea_2_by_2_no :-
    \+ (user_cell(Row, Col, water),
        Row1 is Row + 1, Col1 is Col + 1,
        user_cell(Row1, Col, water),
        user_cell(Row, Col1, water),
        user_cell(Row1, Col1, water)).

island_in_cell_fixed_one :-
    findall((Row, Col), fxd_cell(Row, Col, _), FixedCells),
    maplist(validate_island_has_one_fixed, FixedCells).

validate_island_has_one_fixed((Row, Col)) :-
    connected_cells(Row, Col, land, Island),
    include_fixed_cells(Island, FixedCells),
    length(FixedCells, 1).

include_fixed_cells([], []).
include_fixed_cells([(R, C) | Rest], [(R, C) | FixedCells]) :-
    fxd_cell(R, C, _),
    include_fixed_cells(Rest, FixedCells).
include_fixed_cells([(R, C) | Rest], FixedCells) :-
    \+ fxd_cell(R, C, _),
    include_fixed_cells(Rest, FixedCells).

size_equals_number_island :-
    forall(fxd_cell(Row, Col, Num),
           (connected_cells(Row, Col, land, Island),
            length(Island, Size),
            Num == Size)).

no_gray_cells :-
    \+ user_cell(_, _, gray).

solved :-
    no_gray_cells,
    sea_one,
    sea_2_by_2_no,
    island_in_cell_fixed_one,
    size_equals_number_island.

:- pce_begin_class(grid_view, frame, "Grid View").

initialise(F) :->
    send(F, send_super, initialise, 'Grid Solver'),
    send(F, append, new(D, dialog)),
    send(D, append, button('Draw Grid', message(@prolog, draw_grid))),
    send(D, append, button('Exit', message(F, destroy))),
    send(D, append, button('Select Level', message(@prolog, select_level))).

draw_grid :-
    new(P, picture('Grid')),
    send(P, size, size(700, 700)),
    send(P, open),
    draw_cells(P),
    send(P, display, new(_SolveButton, button('Solve', message(@prolog, solve_puzzle))), point(50, 710)),
    send(P, display, new(_RestartButton, button('Restart', message(@prolog, restart_grid))), point(150, 710)).

draw_cells(P) :-
    forall(between(1, 7, Row),
           forall(between(1, 7, Col),
                  draw_cell(P, Row, Col))).

restart_grid :-
    retractall(user_cell(_, _, _)),
    retractall(fxd_cell(_, _, _)),
    init_user_grid(1),
    draw_grid.

draw_cell(P, Row, Col) :-
    X is (Col - 1) * 100,
    Y is (Row - 1) * 100,
    new(Cell, box(100, 100)),
    ( fxd_cell(Row, Col, _) ->
        send(Cell, fill_pattern, colour(lightgreen)),
        assertz(user_cell(Row, Col, land))
    ; send(Cell, fill_pattern, colour(grey))),
    send(P, display, Cell, point(X, Y)),
    send(Cell, recogniser, click_gesture(left, '', single, message(@prolog, box_click, Cell, Row, Col))),
    ( fxd_cell(Row, Col, Num) ->
        new(Text, text(Num, center)),
        send(Text, font, font(screen, bold, 20)),
        send(P, display, Text, point(X + 50, Y + 50))
    ; true).

box_click(Cell, Row, Col) :-
    ( fxd_cell(Row, Col, _) ->
        true
    ; get(Cell, fill_pattern, colour(grey)) ->
        send(Cell, fill_pattern, colour(lightgreen)),
        retractall(user_cell(Row, Col, _)),
        assertz(user_cell(Row, Col, land))
    ; get(Cell, fill_pattern, colour(lightgreen)) ->
        send(Cell, fill_pattern, colour(darkblue)),
        retractall(user_cell(Row, Col, _)),
        assertz(user_cell(Row, Col, water))
    ; get(Cell, fill_pattern, colour(darkblue)) ->
        send(Cell, fill_pattern, colour(grey)),
        retractall(user_cell(Row, Col, _)),
        assertz(user_cell(Row, Col, gray))
    ).

solve_puzzle :-
    (solved ->
        send(@display, inform, 'The puzzle is solved correctly!')
    ; send(@display, inform, 'The puzzle is not solved correctly!')).

select_level :-
    new(Dialog, dialog('Select Level')),
    send(Dialog, append, new(LevelItem, menu(level, cycle))),
    send(LevelItem, layout, horizontal),
    send(LevelItem, append, 1),
    send(LevelItem, append(2)),
    send(LevelItem, append, 3),
    send(LevelItem, append, 4),
    send(Dialog, append, button(ok, message(Dialog, return, ok))),
    send(Dialog, append, button(cancel, message(Dialog, return, cancel))),
    get(@display, size, size(W, H)),
    DialogX is W // 2,
    DialogY is H // 2,
    send(Dialog, open_centered, point(DialogX, DialogY)),
    get(Dialog, confirm, Answer),
    (Answer == ok ->
        get(LevelItem, selection, SelectedLevel),
        retractall(user_cell(_, _, _)),
        retractall(fxd_cell(_, _, _)),
        init_user_grid(SelectedLevel),
        draw_grid
    ; true),
    send(Dialog, destroy).

:- pce_end_class.

start :-
    retractall(user_cell(_, _, _)),
    init_user_grid(1),
    new(G, grid_view),
    send(G, open).

:- start.
