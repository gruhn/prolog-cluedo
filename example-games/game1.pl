:- use_module(cluedo).

?- cluedo:assertz(player_count(4)).
?- cluedo:assertz(player_first(3)).
?- cluedo:assertz(own_cards(["Orchid", "Dagger", "Wrench", "Dining Room", "Library"])).

?- cluedo:assertz(turns(
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
  , ( suggestion("Peacock", "Lead Pipe", "Kitchen"), disproof )
  % player 0 wins
  ]
)).
