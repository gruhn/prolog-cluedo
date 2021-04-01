:- use_module(cluedo).

?- cluedo:assertz(player_count(4)).
?- cluedo:assertz(player_first(1)).
?- cluedo:assertz(own_cards(["Peacock", "Orchid", "Billiard Room", "Library", "Lounge"])).
?- cluedo:assertz(turns(
  [ ( suggestion("Peacock", "Revolver", "Study"), disproof(3) )
  , ( suggestion("Orchid", "Rope", "Library"), disproof(0, "Orchid") )
  , ( suggestion("Green", "Wrench", "Kitchen"), disproof(1) )
  , ( suggestion("Mustard", "Revolver", "Ballroom"), disproof(3, "Revolver") )

  , ( suggestion("Plum", "Revolver", "Conservatory"), disproof(3) )
  , ( suggestion("Green", "Wrench", "Study"), disproof(3) )
  , ( suggestion("Orchid", "Wrench", "Dining Room"), disproof(0, "Orchid") )
  , ( suggestion("Mustard", "Rope", "Ballroom"), disproof(2, "Rope") )

  , ( suggestion("Orchid", "Rope", "Billiard Room"), disproof(2) )
  , ( suggestion("Green", "Wrench", "Library"), disproof(0, "Library") )
  , ( suggestion("Plum", "Revolver", "Lounge"), disproof(0, "Lounge") )
  , ( suggestion("Mustard", "Wrench", "Conservatory"), disproof(3, "Mustard") )

  , ( suggestion("Orchid", "Rope", "Conservatory"), disproof(2) )
  , ( suggestion("Green", "Lead Pipe", "Billiard Room"), disproof(0, "Billiard Room") )
  , ( suggestion("Plum", "Rope", "Dining Room"), disproof(1) )
  , ( suggestion("Plum", "Wrench", "Dining Room"), disproof(1, "Dining Room") )

  , ( suggestion("Orchid", "Revolver", "Lounge"), disproof(3) )
  , ( suggestion("Green", "Wrench", "Conservatory"), disproof(1) )
  , ( suggestion("Plum", "Revolver", "Kitchen"), disproof(2) )
  % player 3 wins
  ]
)).
