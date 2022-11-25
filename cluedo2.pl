:- use_module(library(clpz)).
:- use_module(library(lists)).
:- use_module(library(pairs), [pairs_keys/2, pairs_keys_values/3]).

:- use_module(library(dcgs)).
:- use_module(library(reif)).
:- use_module(library(between), [numlist/3]).

:- use_module(library(debug)).
:- use_module(library(tabling)).
:- use_module(library(format), [portray_clause/1]).

:- dynamic(suspect/1).
:- dynamic(weapon/1).
:- dynamic(room/1).

suspect("Mustard").
suspect("Scarlett").
suspect("Peacock").
suspect("Plum").
suspect("Orchid").
suspect("Green").

weapon("Candlestick").
weapon("Dagger").
weapon("Lead Pipe").
weapon("Revolver").
weapon("Rope").
weapon("Wrench").

room("Ballroom").
room("Billiard Room").
room("Conservatory").
room("Dining Room").
room("Hall").
room("Kitchen").
room("Library").
room("Lounge").
room("Study").

% -----------------------------------------

suspects(Ss) :-
  setof(S, suspect(S), Ss).

weapons(Ws) :-
  setof(W, weapon(W), Ws).

rooms(Rs) :-
  setof(R, room(R), Rs).

cards(Cs) :-
  suspects(Ss),
  weapons(Ws),
  rooms(Rs),
  append([Ss, Ws, Rs], Cs).

card_count(Count) :-
  cards(Cs),
  length(Cs, Count).

card_type(Card, suspect) :- suspect(Card).
card_type(Card, weapon) :- weapon(Card).
card_type(Card, room) :- room(Card).

% card_type_count(Type, Count) :-
%   Template =.. [Type, Card],
%   findall(Card, Template, Cards),
%   length

card_id(Card, ID) :-
  var(Card),
  suspect_id(Card, SID),
  weapon_id(Card, WID),
  room_id(Card, RID),
  domains_union([SID,RID,WID], Dom),
  ID in Dom.
card_id(Card, ID) :-
  ground(Card),
  suspect_id(Card, ID).
card_id(Card, ID) :-
  ground(Card),
  weapon_id(Card, ID).
card_id(Card, ID) :-
  ground(Card),
  room_id(Card, ID).

suspect_id(Card, ID) :-
  suspects(Cards),
  length(Cards, Count),
  Index in 1 .. Count,
  ID #= Index * 3 - 2,
  ( ground(Card) -> nth1(Index, Cards, Card) ; true ).

weapon_id(Card, ID) :-
  weapons(Cards),
  length(Cards, Count),
  Index in 1 .. Count,
  ID #= Index * 3 - 1,
  ( ground(Card) -> nth1(Index, Cards, Card) ; true ).

room_id(Card, ID) :-
  rooms(Cards),
  length(Cards, Count),
  Index in 1 .. Count,
  ID #= Index * 3,
  ( ground(Card) -> nth1(Index, Cards, Card) ; true ).

player_card_count(PlayerCount, Player, CardCount) :-
  card_count(TotalCards),
  % total cards -3 cards in the envelope
  HandCards #= TotalCards - 3,
  % If cards can't be distributed evenly among players,
  % the excess cards are given to the first players in order.
  ExtraCard #<==> Player #=< (HandCards mod PlayerCount),
  CardCount #= (HandCards div PlayerCount) + ExtraCard.

deck_card(Deck, Card) :-
  card_id(Card, ID),
  tmember(#=(ID), Deck).

% ---------------------------------------------------------------

initial_game_state(PlayerCount, FirstPlayer, IAmPlayer, OwnDeck0, State0) :-
  Turns = [],

  numlist(1, PlayerCount, Players),

  % for each player, describe a deck of hand cards
  same_length(Players, PlayerDecks),
  maplist(player_card_count(PlayerCount), Players, PlayerCardCounts),
  maplist(length, PlayerDecks, PlayerCardCounts),

  % my own cards are defined from the beginning
  maplist(card_id, OwnDeck0, OwnDeck1),
  sort(OwnDeck1, OwnDeck),
  nth1(IAmPlayer, PlayerDecks, OwnDeck),

  % break symmetry / prune duplicates (because [1] ~ [1,1], [2,1] ~ [1,2], ...)
  maplist(chain(#<), PlayerDecks),

  Envelope = [ EnvSuspect, EnvWeapon, EnvRoom ],
  suspect_id(_, EnvSuspect),
  weapon_id(_, EnvWeapon),
  room_id(_, EnvRoom),

  card_count(CardCount),
  append(PlayerDecks, AllPlayerCards),
  append(Envelope, AllPlayerCards, AllCards),
  AllCards ins 1 .. CardCount,
  all_distinct(AllCards),

  State0 = s(PlayerCount, IAmPlayer, FirstPlayer, PlayerDecks, Envelope, Turns).

game_state(State), [State] --> [State].

player_count(PlayerCount) -->
  game_state(s(PlayerCount, _, _, _, _, _)).

i_am_player(IAmPlayer) -->
  game_state(s(_, IAmPlayer, _, _, _, _)).

first_player(FirstPlayer) -->
  game_state(s(_, _, FirstPlayer, _, _, _)).

player_decks(Decks) -->
  game_state(s(_, _, _, Decks, _, _)).

player_deck(Player, Deck) -->
  player_decks(Decks),
  { nth1(Player, Decks, Deck) }.

envelope(Env) -->
  game_state(s(_, _, _, _, Env, _)).

new_turn(NewTurn0), [s(PlayerCount, IAmPlayer, FirstPlayer, PlayerDecks, Envelope, Turns)] -->
  [ s(PlayerCount, IAmPlayer, FirstPlayer, PlayerDecks, Envelope, Turns0) ],
  {
    NewTurn0 = turn(Suspect, Weapon, Room, DisproofPlayer, Disproof),
    suspect_id(Suspect, SuspectID),
    weapon_id(Weapon, WeaponID),
    room_id(Room, RoomID),
    card_id(Disproof, DisproofID),

    NewTurn = turn(SuspectID, WeaponID, RoomID, DisproofPlayer, DisproofID),
    phrase((seq(Turns0),seq([NewTurn])), Turns),
    domains_union([SuspectID, WeaponID, RoomID], SuggestionDomain),
    DisproofID in SuggestionDomain,

    % The player who used posesses a card, if it's used to disproof a suggestion.
    nth1(DisproofPlayer, PlayerDecks, Deck),
    % write(SuggestionDomain), nl,
    write(DisproofID), nl,
    tmember(#=(DisproofID), Deck)

    % domains_union(Deck, DeckDomain),
    % DisproofID in SuggestionDomain /\ DeckDomain,
    % DisproofID in DeckDomain,
  }.

new_turns([]), [State] --> [State].
new_turns([T|Ts])      --> new_turn(T), new_turns(Ts).

player(P) -->
  player_count(PlayerCount),
  { P in 1 .. PlayerCount }.

% player_play_turn(Player, Turn) -->
%   player(Player),
%   player_count(PlayerCount),
%   first_player(FirstPlayer),
%   turns(Turns),
%   {
%     length(Turns, TurnCount),
%     TurnNumber in 1 .. TurnCount,
%     (TurnNumber - Player + FirstPlayer) mod PlayerCount #= 0,
%     nth1(TurnNumber, Turns, Turn)
%   }.

% ---------------------------------------

wrap(Functor, Arg1, Arg2, Term) :-
  Term =.. [Functor, Arg1, Arg2].

domains_union([X|Xs], Domain) :-
  maplist(fd_dom, [X|Xs], [D|Ds]),
  foldl(wrap(\/), Ds, D, Domain).

% current_player(Player) -->
%   player(Player),
%   player_count(PlayerCount),
%   first_player(FirstPlayer),
%   turns(Turns),

%   { TurnIndex #>= 0,
%     0 #= (TurnIndex - Player + FirstPlayer) mod PlayerCount.
%   }.

% player_disproof(Player, suggestion(S,W,R)) -->
%   turns(Turns),
%   { tmember(=(turn(S,W,R,Player,Disproof)), Turns) }.

% ----------------------------------------

sit_between(PlayerCount, P0, P1, Ps) :-
  numlist(1, PlayerCount, Ps0),
  append(PreP0, [P0|PostP0], Ps0),
  append(PostP0, PreP0, Ps1),
  append(Ps, [P1|_], Ps1).

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


% turn_encoded(Game, turn(S0,W0,R0,P,C0), turn(S,W,R,P,C)) :-
%   player(Game, P),
%   maplist(card_index, [S0,W0,R0,C0],[S,W,R,C]),
%   element(_,[S,W,R],C).

% show_note_sheet :-
%   note_sheet(Sheet), clues,
%   % pairs_keys_values(Sheet, _, Rows),
%   % * length(Rows1, 19),
%   % * append(Rows1, _, Rows),
%   % * append(Rows1, Vars),
%   % * labeling([], Vars),
%   maplist(portray_clause, Sheet).

% players_hold_cards_(Ps, Vs, Cs) :-
%   game1(Game), !,
%   players_hold_cards(Game, Ps, Vs, Cs).

%%% TESTs %%%

% test(sit_between) :-
%   sit_between(5, 1, 2, []),
%   sit_between(5, 1, 5, [2,3,4]),
%   sit_between(5, 3, 2, [4,5,1]).

% --------------------------------------------------------

example_game(State) :-
  PlayerCount = 4,
  IAmPlayer = 1,
  FirstPlayer = 4,
  OwnCards =["Orchid", "Dagger", "Wrench", "Dining Room", "Library"],
  Turns =
    [ turn("Green", "Dagger", "Library", 1, "Dagger")
    , turn("Mustard", "Candlestick", "Ballroom", 2, "Mustard")
    , turn("Orchid", "Revolver", "Kitchen", 1, "Orchid")
    , turn("Orchid", "Wrench", "Study", 4, _)

    % , turn("Orchid", "Wrench", "Hall", 1, "Wrench")
    % , turn("Scarlett", "Revolver", "Kitchen", 2, "Revolver")
    % , turn("Green", "Lead Pipe", "Ballroom", 3, _)
    % , turn("Orchid", "Revolver", "Kitchen", 1, "Orchid")

    % , turn("Orchid", "Revolver", "Dining Room", 1, "Dining Room")
    % , turn("Scarlett", "Candlestick", "Ballroom", 2, "Scarlett")
    % , turn("Orchid", "Rope", "Library", 3, _)
    % , turn("Orchid", "Wrench", "Kitchen", 1, "Wrench")

    % , turn("Orchid", "Rope", "Billiard Room", 1, "Orchid")
    % , turn("Scarlett", "Candlestick", "Kitchen", 2, "Candlestick")
    % , turn("Plum", "Wrench", "Lounge", 4, _)
    % , turn("Orchid", "Revolver", "Lounge", 4, _)

    % , turn("Orchid", "Rope", "Ballroom", 1, "Orchid")
    % , turn("Plum", "Lead Pipe", "Kitchen", 4, "Plum")
    % , turn("Peacock", "Wrench", "Conservatory", 3, _)
    % , turn("Plum", "Revolver", "Dining Room", 4, _)

    % , turn("Orchid", "Revolver", "Billiard Room", 1, "Orchid")
    % , turn("Peacock", "Lead Pipe", "Kitchen")

    % player 1 wins
    ],

  initial_game_state(PlayerCount, FirstPlayer, IAmPlayer, OwnCards, State0),
  phrase(new_turns(Turns), [State0], [State]).

cluedo(Goal) :-
  example_game(State0),
  phrase(Goal, [State0], _).
