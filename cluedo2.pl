:- use_module(library(clpz)).
:- use_module(library(lists), [same_length/2]).
:- use_module(library(pairs), [pairs_keys_values/3]).

:- use_module(library(dcgs), [phrase/2]).
:- use_module(library(reif), [tmember/2]).
:- use_module(library(between), [numlist/3]).

:- use_module(library(debug)).
:- use_module(library(format), [portray_clause/1]).

suspects(["Mustard", "Scarlett", "Peacock", "Plum", "Orchid", "Green"]).

weapons(["Candlestick", "Dagger", "Lead Pipe", "Revolver", "Rope", "Wrench"]).

rooms(["Ballroom", "Billiard Room", "Conservatory", "Dining Room", "Hall", "Kitchen", "Library", "Lounge", "Study"]).

cards(Pairs) :-
  suspects(S), weapons(W), rooms(R),
  append([S,W,R], Cards),
  length(Cards, Count),
  N #= Count - 1,
  numlist(0, N, Indices),
  pairs_keys_values(Pairs, Indices, Cards).

card_count(Count) :-
  cards(Ps), length(Ps, Count).

cardinality(Car, Vars) :-
  Vars ins 0 .. 1,
  sum(Vars, #=, Car).

note_sheet(PlayerCount, Rows) :-
  suspects(Suspects),
  weapons(Weapons),
  rooms(Rooms),

  same_length(Suspects, SuspectRows),
  same_length(Weapons, WeaponRows),
  same_length(Rooms, RoomRows),

  append([ SuspectRows, WeaponRows, RoomRows ], Rows),

  RowLength #= PlayerCount + 1,
  length(Row, RowLength),
  maplist(same_length(Row), Rows),
  maplist(cardinality(1), Rows),

  transpose(Rows, Cols),
  Cols = [ EnvCol | PlayerCols ],
  envelope_column(EnvCol),
  player_columns(PlayerCols).

note_sheet_column(Col) :-
  card_count(CardCount),
  length(Col, CardCount).

envelope_column(Col) :-
  note_sheet_column(Col),
  suspects(Suspects),
  weapons(Weapons),
  rooms(Rooms),
  same_length(Suspects, EnvSuspects),
  same_length(Weapons, EnvWeapons),
  same_length(Rooms, EnvRooms),
  append([ EnvSuspects, EnvWeapons, EnvRooms ], Col),
  cardinality(1, EnvSuspects),
  cardinality(1, EnvWeapons),
  cardinality(1, EnvRooms).

player_columns(Cols) :-
  maplist(note_sheet_column, Cols),
  length(Cols, PlayerCount),

  card_count(CardCount),
  LastPlayerCardCount #= (CardCount-3) div PlayerCount,
  FirstPlayerCardCount #= LastPlayerCardCount + 1,
  ExcessCards #= (CardCount-3) mod PlayerCount,

  length(FirstPlayerCols, ExcessCards),
  append(FirstPlayerCols, LastPlayerCols, Cols),
  maplist(cardinality(FirstPlayerCardCount), FirstPlayerCols),
  maplist(cardinality(LastPlayerCardCount), LastPlayerCols).


matrix_cell(Rows, X, Y, Val) :-
  nth0(Y, Rows, Row),
  nth0(X, Row, Val).

matrix_cells(_, [], _) --> [].
matrix_cells(_, _, []) --> [].
matrix_cells(Rows, [X|Xs], Ys) -->
  { maplist(matrix_cell(Rows, X), Ys, Vals1) },
  Vals1,
  matrix_cells(Rows, Xs, Ys).

game_player_count((_, _, N, _), N).
game_player_me((_, P, _, _), P).
game_player_first((_, _, _, P), P).
game_note_sheet((S,_,_,_),S).

player(Game, Player) :-
  game_player_count(Game, PlayerCount),
  Player in 0 .. PlayerCount.

players_hold_cards(Game, Players, Vars, Cards) :-
  game_note_sheet(Game, Sheet),

  % enforce iterative deepening
  VarsLen #= CardsLen * PlayersLen,
  length(Vars, VarsLen),
  length(Players, PlayersLen),
  length(Cards, CardsLen),

  maplist(player(Game), Players),
  maplist(card_index, Cards, CardIndices),

  % break symmetry / prune duplicates (because [1] ~ [1,1], [2,1] ~ [1,2], ...)
  chain(#<, Players),
  chain(#<, CardIndices),

  phrase(matrix_cells(Sheet, Players, CardIndices), Vars).

sit_between(PlayerCount, P0, P1, Ps) :-
  numlist(1, PlayerCount, Ps0),
  append(PreP0, [P0|PostP0], Ps0),
  append(PostP0, PreP0, Ps1),
  append(Ps, [P1|_], Ps1).

current_player(Game, TurnIndex, Player) :-
  game_player_count(Game, PlayerCount),
  game_player_first(Game, FirstPlayer),
  TurnIndex #>= 0,
  Player in 1 .. PlayerCount,
  (TurnIndex - Player + FirstPlayer) mod PlayerCount #= 0.

turn_constraint(Game, Index-turn(S,W,R,P1,C)) :-
  current_player(Game, Index, P0),

  % A player posesses a card, if it's used to disproof a suggestion.
  tmember(=(C), [S,W,R]),
  % card_index(C,CI),
  % maplist(card_index, [S,W,R], [SI,WI,RI]),
  % (CI #= SI) #\/ (CI #= WI) #\/ (CI #= RI),
  players_hold_cards(Game, [P1], [1], [C]),

  % All players who sit between the players P0 and P1, could not disprove the
  % suggestion and therefore have none of the named cards.
  game_player_count(Game, PlayerCount),
  sit_between(PlayerCount, P0, P1, Ps),
  maplist(player(Game), Ps),
  players_hold_cards(Game, Ps, Vars2, [S,W,R]),
  maplist(#=(0), Vars2),

  % Players want to learn something about cards they don't have by making
  % suggestions. Thus, we assume that at least one of the named cards
  % is not in the players hand.
  players_hold_cards(Game, [P0], Vars3, [S,W,R]),
  sum(Vars3, #<, 3).
% turn_constraint(Sheet, _, _-turn(S,W,R)) :-
  % suggestion that nobody could disproof

card_index(Card, Index) :-
  cards(Pairs),
  length(Pairs, CardCount),
  Index in 0 .. CardCount,
  tmember(=(Index-Card), Pairs).

% turn_encoded(Game, turn(S0,W0,R0,P,C0), turn(S,W,R,P,C)) :-
%   player(Game, P),
%   maplist(card_index, [S0,W0,R0,C0],[S,W,R,C]),
%   element(_,[S,W,R],C).

game1(Game) :-
  IAmPlayer = 1,
  PlayerCount = 4,
  PlayerFirst = 4,
  Game = (Sheet, IAmPlayer, PlayerCount, PlayerFirst),

  note_sheet(PlayerCount, Sheet),

  OwnCards = ["Orchid", "Dagger", "Wrench", "Dining Room", "Library"],
  same_length(OwnCards, OwnCardsVars),
  maplist(#=(1), OwnCardsVars),
  players_hold_cards(Game, [IAmPlayer], OwnCardsVars, OwnCards),

  Turns =
    [ turn("Green", "Dagger", "Library", 1, "Dagger")
    , turn("Mustard", "Candlestick", "Ballroom", 2, "Mustard")
    , turn("Orchid", "Revolver", "Kitchen", 1, "Orchid")
    , turn("Orchid", "Wrench", "Study", 4, _)

    , turn("Orchid", "Wrench", "Hall", 1, "Wrench")
    , turn("Scarlett", "Revolver", "Kitchen", 2, "Revolver")
    , turn("Green", "Lead Pipe", "Ballroom", 3, _)
    , turn("Orchid", "Revolver", "Kitchen", 1, "Orchid")

    , turn("Orchid", "Revolver", "Dining Room", 1, "Dining Room")
    , turn("Scarlett", "Candlestick", "Ballroom", 2, "Scarlett")
    , turn("Orchid", "Rope", "Library", 3, _)
    , turn("Orchid", "Wrench", "Kitchen", 1, "Wrench")

    , turn("Orchid", "Rope", "Billiard Room", 1, "Orchid")
    , turn("Scarlett", "Candlestick", "Kitchen", 2, "Candlestick")
    , turn("Plum", "Wrench", "Lounge", 4, _)
    , turn("Orchid", "Revolver", "Lounge", 4, _)

    , turn("Orchid", "Rope", "Ballroom", 1, "Orchid")
    , turn("Plum", "Lead Pipe", "Kitchen", 4, "Plum")
    , turn("Peacock", "Wrench", "Conservatory", 3, _)
    , turn("Plum", "Revolver", "Dining Room", 4, _)

    , turn("Orchid", "Revolver", "Billiard Room", 1, "Orchid")
    % , turn("Peacock", "Lead Pipe", "Kitchen")

    % player 1 wins
    ],

  length(Turns, TurnCount),
  LastTurn #= TurnCount - 1,
  numlist(0, LastTurn, TurnIndices),
  pairs_keys_values(TurnsWithIndex, TurnIndices, Turns),

  maplist(turn_constraint(Game), TurnsWithIndex).

show(Rows) :-
  append(Rows, Vars),
  labeling([], Vars),
  maplist(portray_clause, Rows).

players_hold_cards_(Ps, Vs, Cs) :-
  game1(Game), !,
  players_hold_cards(Game, Ps, Vs, Cs).

%%% TESTs %%%

test(sit_between) :-
  sit_between(5, 1, 2, []),
  sit_between(5, 1, 5, [2,3,4]),
  sit_between(5, 3, 2, [4,5,1]).
