
% TODO:
% * recommend card to show (prefer cards opponent already knows to minimize information leakage)
% * recommend suggestions to make
% 	-> revolve as much uncertainty as possible
% 	-> if suspect is send to room (special game rule), suggest suspect to prevent him from making
%		suggestions in the room he really wants

% :- dynamic number_of_players/1.
% :- dynamic i_am_player/1.
% :- dynamic starting_player/1.
% :- dynamic my_cards/1.
% :- dynamic turns/3.
% :- dynamic suspect/1.
% :- dynamic room/1.
% :- dynamic weapon/1.

card(Card):- suspect(Card).
card(Card):- weapon(Card).
card(Card):- room(Card).

card_type(Card, suspect) :- suspect(Card).
card_type(Card, weapon) :- weapon(Card).
card_type(Card, room) :- room(Card).

all_cards(List) :-
 	findall(X, card(X), List).

all_cards(List, suspect) :-
    findall(C, suspect(C), List).
all_cards(List, weapon) :-
    findall(C, weapon(C), List).
all_cards(List, room) :-
    findall(C, room(C), List).

player(Player) :-
    number_of_players(NumberOfPlayers),
    LastPlayer is NumberOfPlayers - 1,
    between(0, LastPlayer, Player).

dealt_card_count(CardCount) :-
	all_cards(AllCards),
  	length(AllCards, TotalCardCount),
    CardCount is TotalCardCount - 3. % subtract cards in envelope

player_card_count(Player, PlayerCardCount) :-
	player(Player),
    number_of_players(NumberOfPlayers),
    dealt_card_count(CardCount),
    Player < NumberOfPlayers - mod(CardCount, NumberOfPlayers),
    PlayerCardCount is div(CardCount, NumberOfPlayers) + 1.
player_card_count(Player, PlayerCardCount) :-
	player(Player),
    number_of_players(NumberOfPlayers),
    dealt_card_count(CardCount),
    Player >= NumberOfPlayers - mod(CardCount, NumberOfPlayers),
    PlayerCardCount is div(CardCount, NumberOfPlayers).

turn(Player, Turn) :-
	player(Player),
	number_of_players(NumberOfPlayers),
    starting_player(SP),
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

:- table hand/2, hand_exclude/2.

hand(Player, Card) :-
    i_am_player(Player),
    my_cards(Cards),
    member(Card, Cards).
% If a player disproves a suggestion, we know that he has at least one of the
% cards named in the suggestion. If we also know that he doesn't have two
% of the cards he must posess the remaining card.
hand(Player, Card) :-
    player(Player),
	turn(_, (suggestion(Suspect, Weapon, Room), disproof(Player))),
    select(Card, [Suspect, Weapon, Room], [Card2, Card3]),
	hand_exclude(Player, Card2),
    hand_exclude(Player, Card3).
% A player has a card if he uses it to disprove my suggestion.
hand(Player, Card) :-
    player(Player),
    turn(_, (_, disproof(Player, Card))).

% If PlayerA made a suggestion which was disproved by PlayerC,
% then any PlayerB sitting between the two was not able to
% dispove the suggestion and can therefore not possess any of the named cards
hand_exclude_all(PlayerB, [Suspect, Weapon, Room]) :-
	player(PlayerB),
    turn(PlayerA, (suggestion(Suspect, Weapon, Room), disproof(PlayerC))),
  	sits_between(PlayerA, PlayerB, PlayerC).
hand_exclude_all(PlayerB, [Suspect, Weapon, Room]) :-
	player(PlayerB),
    turn(PlayerA, (suggestion(Suspect, Weapon, Room), disproof(PlayerC, _))),
  	sits_between(PlayerA, PlayerB, PlayerC).
% If Player made a suggestion that nobody could disproof then
% every OtherPlayer does not have any of the named cards.
hand_exclude_all(OtherPlayer, [Suspect, Weapon, Room]) :-
	player(OtherPlayer),
    turn(Player, (suggestion(Suspect, Weapon, Room), disproof)),
    Player \= OtherPlayer.
% If a Player makes a final accusation he certainly has non of the named cards
hand_exclude_all(Player, [Suspect, Weapon, Room]) :-
    turn(Player, (_, _, accusation(Suspect, Weapon, Room))).
% If we know all of a players cards all other cards can be excluded. Implies
% that all cards not in MY hand are automatically excluded.
hand_exclude_all(Player, OtherCards) :-
    player_card_count(Player, PlayerCardCount),
    length(HandCards, PlayerCardCount),
    all_cards(AllCards),
    partition(hand(Player), AllCards, HandCards, OtherCards).

% Players want to learn something about cards they don't have by making
% suggestions. Thus, we assume that at least one of the cards named
% is a suggestion is not in his hand. NOTE: If a player does it anyway, this
% introduces contradictions into the knowlage base.
hand_exclude_some(Player, [Suspect, Weapon, Room]) :-
	turn(Player, (suggestion(Suspect, Weapon, Room), _)).

% A card is not in a players hand if it's already in another players hand.
hand_exclude(PlayerA, Card) :-
    player(PlayerA),
    player(PlayerB),
	hand(PlayerB, Card),
	PlayerA \= PlayerB.
hand_exclude(Player, Card) :-
    player(Player),
    hand_exclude_all(Player, CardList),
    member(Card, CardList).
hand_exclude(Player, Card) :-
    player(Player),
    hand_exclude_some(Player, CardList),
	select(Card, CardList, [Card2, Card3]),
    % TOOD: is it necessary to know that the other cards are in his hand?
    % NO! If we know the player does not have the cards himself but we know
    % he has already seen them, he is probably interested in other cards
	hand(Player, Card2),
    hand(Player, Card3).


hand_include(Player, Card) :-
    card(Card),
    player(Player),
    not(hand_exclude(Player, Card)).

n_subset(0, [], []).
n_subset(N, Xs, [_|Ys]) :-
    n_subset(N, Xs, Ys).
n_subset(N, [X|Xs], [X|Ys]) :-
    NN is N - 1,
    n_subset(NN, Xs, Ys).

full_hand_candidate(Player, Cards) :-
    findall(Card, hand_include(Player, Card), AllCandidates),
    partition(hand(Player), AllCandidates, KnownCards, PossibleCards),
    player_card_count(Player, CardCount),
    length(KnownCards, KnownCount),
    RemainingCount is CardCount - KnownCount,
    n_subset(RemainingCount, RemainingCards, PossibleCards),
    append(KnownCards, RemainingCards, Cards).

envelope(Card) :-
	card(Card),
   	forall(player(Player), hand_exclude(Player, Card)).
envelope(Card) :-
    all_cards(All, _),
    select(Card, All, AllExceptOne),
    forall(member(C, AllExceptOne), hand(_, C)).

envelope(Suspect, Weapon, Room) :-
    suspect(Suspect),
    weapon(Weapon),
    room(Room),
    envelope(Suspect),
    envelope(Weapon),
    envelope(Room).


% If we know that a card is in a players hand we consider it determined.
card_determined(Card) :-
   	card(Card),
    player(Player),
    hand(Player, Card).
% If we know, say the murder weapon, all other weapons are determined.
% We may not know who has which weapon but we don't care about that
% information anymore.
card_determined(Card) :-
    card_type(Card, Type),
    card_type(EnvCard, Type),
    envelope(EnvCard).


% suggest cards first that are "more" determined? E.g. when all players but one
% are already excluded.
suggest_card(Card) :-
    number_of_players(NumberOfPlayers),
    Max is NumberOfPlayers - 1,
    between(1, Max, PlayerIncludeCount),
    card(Card),
    not(card_determined(Card)),
    findall(Player, hand_include(Player, Card), Players),
    length(Players, PlayerIncludeCount).
% Say we know the murder weapon is the "Dagger" but we don't know who has the
% "Wrench". We have to keep making suggestions to figure out suspect and room.
% But we want to avoid including "Wrench" in our suggestions because if we are
% shown that card we wasted a suggestions. Thus we deliberatly use the solution
% card ("Dagger") or cards in our hand.
suggest_card(Card) :-
    envelope(Card).
suggest_card(Card) :-
    i_am_player(Player),
    hand(Player, Card).

% Generate options for the next move. If envelope is determined
% always and only recommend accusation.
next_move(accusation(Suspect, Weapon, Room)) :-
    envelope(Suspect, Weapon, Room), !.
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
    suspect(Suspect),
    weapon(Weapon),
   	room(Room). % room last!


% sample game:
suspect("Mustard").
suspect("Scarlett").
suspect("Peacock").
suspect("Plum").
suspect("Orchid").
suspect("Green").

room("Ballroom").
room("Billiard Room").
room("Conservatory").
room("Dining Room").
room("Hall").
room("Kitchen").
room("Library").
room("Lounge").
room("Study").

weapon("Candlestick").
weapon("Dagger").
weapon("Lead Pipe").
weapon("Revolver").
weapon("Rope").
weapon("Wrench").

number_of_players(4).
starting_player(3).
i_am_player(0).
my_cards(["Orchid", "Dagger", "Wrench", "Dining Room", "Library"]).
turns(
    [ ( suggestion("Green", "Dagger", "Library"), disproof(0, "Dagger") )
    , ( suggestion("Mustard", "Candlestick", "Ballroom"), disproof(1, "Mustard") )
    , ( suggestion("Orchid", "Revolver", "Kitchen"), disproof(0, "Orchid") )
    , ( suggestion("Orchid", "Wrench", "Study"), disproof(3) )

    , ( suggestion("Orchid", "Wrench", "Hall"), disproof(0, "Wrench") )
    , ( suggestion("Scarlett", "Revolver", "Kitchen"), disproof(1, "Revolver") )
    , ( suggestion("Green", "Lead Pipe", "Ballroom"), disproof(2) )
    , ( suggestion("Orchid", "Revolver", "Kitchen"), disproof(0, "Orchid") )

    , ( suggestion("Orchid", "Revolver", "Dining Room"), disproof(0, "Dining Room") )
    , ( suggestion("Scarlett", "Candlestick", "Ballroom"), disproof(1, "Scarlett") )
    , ( suggestion("Orchid", "Rope", "Library"), disproof(2) )
    , ( suggestion("Orchid", "Wrench", "Kitchen"), disproof(0, "Wrench") )

    , ( suggestion("Orchid", "Rope", "Billiard Room"), disproof(0, "Orchid") )
    , ( suggestion("Scarlett", "Candlestick", "Kitchen"), disproof(1, "Candlestick") )
    , ( suggestion("Plum", "Wrench", "Lounge"), disproof(3) )
    , ( suggestion("Orchid", "Revolver", "Lounge"), disproof(3) )

    , ( suggestion("Orchid", "Rope", "Ballroom"), disproof(0, "Orchid") )
    , ( suggestion("Plum", "Lead Pipe", "Kitchen"), disproof(3, "Plum") )
    , ( suggestion("Peacock", "Wrench", "Conservatory"), disproof(2) )
    , ( suggestion("Plum", "Revolver", "Dining Room"), disproof(3) )

    , ( suggestion("Orchid", "Revolver", "Billiard Room"), disproof(0, "Orchid") )
    % , ( suggestion("Peacock", "Lead Pipe", "Kitchen"), disproof )
    ]
).
