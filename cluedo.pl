:- module(cluedo, []).

% TODO:
% * recommend card to show (prefer cards opponent already knows to minimize information leakage)
% * recommend suggestions to make
% 	-> resolve as much uncertainty as possible
% 	-> if suspect is send to room (special game rule), suggest suspect to prevent him from making
%		suggestions in the room he really wants

:- dynamic player_count/1.
:- dynamic player_first/1.
:- dynamic own_cards/1.
:- dynamic turns/1.

i_am_player(0).

card(suspect, "Mustard").
card(suspect, "Scarlett").
card(suspect, "Peacock").
card(suspect, "Plum").
card(suspect, "Orchid").
card(suspect, "Green").

card(room, "Ballroom").
card(room, "Billiard Room").
card(room, "Conservatory").
card(room, "Dining Room").
card(room, "Hall").
card(room, "Kitchen").
card(room, "Library").
card(room, "Lounge").
card(room, "Study").

card(weapon, "Candlestick").
card(weapon, "Dagger").
card(weapon, "Lead Pipe").
card(weapon, "Revolver").
card(weapon, "Rope").
card(weapon, "Wrench").

all_cards(Type, List) :-
 	findall(X, card(Type, X), List).

player(Player) :-
  player_count(NumberOfPlayers),
  LastPlayer is NumberOfPlayers - 1,
  between(0, LastPlayer, Player).

% Total number of cards in the players hands excluding the 3
% solution cards in the envelope.
dealt_card_count(CardCount) :-
	all_cards(_, AllCards),
  length(AllCards, TotalCardCount),
  CardCount is TotalCardCount - 3.

% The number of hand cards of a certain player. If the cards
% can't be dealt evenly, some players have more cards than others.
player_card_count(Player, PlayerCardCount) :-
	player(Player),
  player_count(NumberOfPlayers),
  dealt_card_count(CardCount),
  Player < NumberOfPlayers - mod(CardCount, NumberOfPlayers),
  PlayerCardCount is div(CardCount, NumberOfPlayers) + 1.
player_card_count(Player, PlayerCardCount) :-
	player(Player),
  player_count(NumberOfPlayers),
  dealt_card_count(CardCount),
  Player >= NumberOfPlayers - mod(CardCount, NumberOfPlayers),
  PlayerCardCount is div(CardCount, NumberOfPlayers).

% Given a "turn" object, determine who was the player
% whos turn it was.
turn(Player, Turn) :-
	player(Player),
	player_count(NumberOfPlayers),
  player_first(SP),
  turns(Turns),
  nth0(Index, Turns, Turn),
	mod(Index - Player + SP, NumberOfPlayers) =:= 0.


% Players are arranged in a cyclic order with respect to clockwise turn order.
% So after the last player its the first players turn again.
% We use this predicate to check if a player is in-between two other players.
%  true:  player1 < player2 < player3
%  true:  player1 < player3 < player5
%  true:  player5 < player1 < player2
%  false: player1 < player3 < player2
cyclic_order(A, B, C) :- A < B, B < C.
cyclic_order(A, B, C) :- B < C, C < A.
cyclic_order(A, B, C) :- C < A, A < B.

sits_between(PlayerA, PlayerB, PlayerC) :-
  player(PlayerA),
  player(PlayerB),
  player(PlayerC),
  cyclic_order(PlayerA, PlayerB, PlayerC).

:- table hand/4.

hand(Player, true, Player, Card) :-
  i_am_player(Player),
  own_cards(Cards),
  member(Card, Cards).
% If a player disproves a suggestion, we know that he has at least one of the
% cards named in the suggestion. If we also know that he doesn't have two
% of the cards he must posess the remaining card.
hand(View, true, Player, Card) :-
  player(View), player(Player),
	turn(_, (suggestion(Suspect, Weapon, Room), disproof(Player))),
  select(Card, [Suspect, Weapon, Room], [Card2, Card3]),
	hand(View, false, Player, Card2),
  hand(View, false, Player, Card3).
% A player has a card if he uses it to disprove my suggestion.
hand(View, true, Player, Card) :-
  player(View), player(Player),
  turn(View, (_, disproof(Player, Card))).
% Useful to deduce other players knowledge:
% Say View makes suggestion ("Plum", "Wrench", "Study") disproved by Player.
% I know that Player doesn't have "Plum" and "Wrench" thus he must have showed
% "Study". Therefore we know View has determined "Study".
hand(View, true, Player, Card) :-
  player(View), player(Player),
	turn(View, (suggestion(Suspect, Weapon, Room), disproof(Player))),
  select(Card, [Suspect, Weapon, Room], [Card2, Card3]),
	hand(_, false, Player, Card2),
  hand(_, false, Player, Card3).
% Say a player has 4 cards and we have excluded all but 4 cards we know the
% player has exactly those cards.
hand(View, true, Player, Card) :-
  player(View),
  player_card_count(Player, PlayerCardCount),
  length(HandCards, PlayerCardCount),
  all_cards(_, AllCards),
  partition(hand(View, false, Player), AllCards, _, HandCards),
  member(Card, HandCards).
% A card is not in a players hand if it's already in another players hand.
hand(View, false, PlayerA, Card) :-
  player(View),
  player(PlayerA),
  player(PlayerB),
	hand(View, true, PlayerB, Card),
	PlayerA \= PlayerB.
% If PlayerA made a suggestion which was disproved by PlayerC,
% then any PlayerB sitting between the two was not able to
% dispove the suggestion and can therefore not possess any of the named cards
hand(_, false, PlayerB, Card) :-
	player(PlayerB),
  turn(PlayerA, (suggestion(Suspect, Weapon, Room), disproof(PlayerC))),
  sits_between(PlayerA, PlayerB, PlayerC),
  member(Card, [Suspect, Weapon, Room]).
hand(_, false, PlayerB, Card) :-
	player(PlayerB),
  turn(PlayerA, (suggestion(Suspect, Weapon, Room), disproof(PlayerC, _))),
  sits_between(PlayerA, PlayerB, PlayerC),
  member(Card, [Suspect, Weapon, Room]).
% If Player made a suggestion that nobody could disproof then
% every OtherPlayer does not have any of the named cards.
hand(_, false, OtherPlayer, Card) :-
	player(OtherPlayer),
  turn(Player, (suggestion(Suspect, Weapon, Room), disproof)),
  Player \= OtherPlayer,
  member(Card, [Suspect, Weapon, Room]).
% If we know all of a players cards all other cards can be excluded. Implies
% that all cards not in MY hand are automatically excluded.
hand(View, false, Player, Card) :-
  player(View),
  player_card_count(Player, PlayerCardCount),
  length(HandCards, PlayerCardCount),
  all_cards(_, AllCards),
  partition(hand(View, true, Player), AllCards, HandCards, OtherCards),
  member(Card, OtherCards).
% Players want to learn something about cards they don't have by making
% suggestions. Thus, we assume that at least one of the cards named
% is a suggestion is not in his hand. If we also know that the player has
% determined two of the cards, we deduce the remaining card is not in his hand.
hand(_, false, Player, Card) :-
  player(Player),
  turn(Player, (suggestion(Suspect, Weapon, Room), _)),
	select(Card, [Suspect, Weapon, Room], [Card2, Card3]),
  % NOTE: we assume opponents use the same reasoning as we do. If the player
  % uses weaker reasoning we might make wrong deductions.
  hand(Player, true, _, Card2),
  hand(Player, true, _, Card3).


% If we know that a card is in a players hand we consider it determined.
card_determined(View, Card) :-
  player(View),
  card(_, Card),
  player(Player),
  hand(View, true, Player, Card).
% If we know, say the murder weapon, all other weapons are determined.
% We may not know who has which weapon but we don't care about that
% information anymore.
card_determined(View, Card) :-
  card(Type, Card),
  card(Type, EnvCard),
  envelope(View, EnvCard).


hand_include(View, Player, Card) :-
  card(_, Card),
  player(Player),
  not(hand(View, false, Player, Card)).


envelope(View, Card) :-
  player(View),
	card(_, Card),
  forall(player(Player), hand(View, false, Player, Card)).
envelope(View, Card) :-
  player(View),
  all_cards(_, All),
  select(Card, All, AllExceptOne),
  forall(member(C, AllExceptOne), hand(View, true, _, C)).

envelope(View, Suspect, Weapon, Room) :-
  player(View),
  card(suspect, Suspect),
  card(weapon, Weapon),
  card(room, Room),
  envelope(View, Suspect),
  envelope(View, Weapon),
  envelope(View, Room).


% suggest cards first that are "more" determined. E.g. when all players but one
% are already excluded.
suggest_card(Card) :-
  i_am_player(View),
  player_count(NumberOfPlayers),
  Max is NumberOfPlayers - 1,
  between(1, Max, PlayerIncludeCount),
  card(_, Card),
  not(card_determined(View, Card)),
  findall(Player, hand_include(View, Player, Card), Players),
  length(Players, PlayerIncludeCount).
% Say we know the murder weapon is the "Dagger" but we don't know who has the
% "Wrench". We have to keep making suggestions to figure out suspect and room.
% But we want to avoid including "Wrench" in our suggestions because if we are
% shown that card we wasted a suggestions. Thus we deliberatly use the solution
% card ("Dagger") or cards in our hand.
suggest_card(Card) :-
  i_am_player(View),
  envelope(View, Card).
suggest_card(Card) :-
  i_am_player(View),
  hand(View, true, View, Card).

% Generate options for the next move. If envelope is determined
% always and only recommend accusation.
next_move(accusation(Suspect, Weapon, Room)) :-
  i_am_player(View),
  envelope(View, Suspect, Weapon, Room), !.
% Otherwise generate suggestions with focus on rooms. Rooms are
% the hardest to deduce. There are more rooms than suspects
% and weapons and you can only suggest a room if you manage to
% enter that room. Hence, the first generated suggestion might
% not have a feasible room. Therefore the next suggestion
% should always be a different room.
% TODO: what if no undetermined room reachable but weapon/suspect already clear?
% => still more useful to ask for knnown weapon/room (even if envelope already
% deduced) because eg. we learn a new weapon on the hand of another player and
% thereby know all the cards in his hand we also can exclude that he has any
% of the remaining rooms.
next_move(suggestion(Suspect, Weapon, Room)) :-
  % suspect_card rules first so attractive suggestions are generated first and
  % only later validated to actually be suspect, weapon, room triples.
  suggest_card(Suspect),
  suggest_card(Weapon),
  suggest_card(Room),
  card(suspect, Suspect),
  card(weapon, Weapon),
  card(room, Room). % room last!


% note_sheet_cell(Card, Player, "Y") :-
%   hand(Player, Card).
% note_sheet_cell(Card, Player, "N") :-
%   hand_exclude(Player, Card).
% note_sheet_cell(Card, Player, " ") :-
%   not(hand(Player, Card)),
%   not(hand_exclude(Player, Card)).

% note_sheet_row(Card, Cells) :-
%   player_count(PlayerCount),
%   note_sheet_row(Card, PlayerCount, Cells).

% note_sheet_row(_, 0, []).
% note_sheet_row(Card, PlayerBefore, [Cell|Cells]) :-
%   Player is PlayerBefore - 1,
%   note_sheet_cell(Card, Player, Cell),
%   note_sheet_row(Card, Player, Cells).

% note_sheet([], []).
% note_sheet([Card|Cards], [Row|Rows]) :-
%     note_sheet_row(Card, 0, Row),
%     note_sheet(Cards, Rows).

% note_sheet(Rows) :-
%     all_cards(_, Cards),
%     note_sheet(Cards, Rows).
