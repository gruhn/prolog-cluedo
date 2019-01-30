
% game facts
suspect(miss_scarlett).
suspect(professor_plum).
suspect(mrs_peacock).
suspect(reverend_green).
suspect(colonel_mustard).
suspect(miss_white).

room(kitchen).
room(ballroom).
room(conservatory).
room(dining_room).
room(billiard_room).
room(library).
room(lounge).
room(hall).
room(study).

weapon(candlestick).
weapon(dagger).
weapon(lead_pipe).
weapon(revolver).
weapon(rope).
weapon(spanner).


card(Card):-
  suspect(Card).
card(Card):-
  room(Card).
card(Card):-
  weapon(Card).


% Assuming 6 player
player(player1).
player(player2).
player(player3).
player(player4).
player(player5).
player(player6).

%%% Example data %%%

% envelope(miss_white, study, spanner).

hand(player1, miss_scarlett).
hand(player1, kitchen).
hand(player1, candlestick).

hand(player2, professor_plum).
hand(player2, ballroom).
hand(player2, dagger).

hand(player3, mrs_peacock).
hand(player3, conservatory).
hand(player3, lead_pipe).

hand(player4, reverend_green).
hand(player4, colonel_mustard).
hand(player4, revolver).

hand(player5, dining_room).
hand(player5, billiard_room).
hand(player5, rope).

hand(player6, library).
hand(player6, lounge).
hand(player6, hall).

hand(Player, Card):-
  player(Player),
  card(Card).
  % dif(Player, OtherPlayer),
  % neg(hand(OtherPlayer, Card)).

envelope(Suspect, Room, Weapon):-
  suspect(Suspect),
  room(Room),
  weapon(Weapon),
  hand(Player, Suspect),
  hand(Player, Room),
  hand(Player, Weapon).

/*
suggestion(Player, Suspect, Room, Weapon):-
  player(Player),
  suspect(Suspect),
  room(Room),
  weapon(Weapon).
*/
