:- use_module(cluedo).

?- cluedo:retractall(player_count(_)).
?- cluedo:assertz(player_count(4)).

?- cluedo:retractall(player_first(_)).
?- cluedo:assertz(player_first(1)).

?- cluedo:retractall(own_cards(_)).
?- cluedo:assertz(own_cards(["Hall", "Conservatory", "Dagger", "Wrench", "Revolver"])).

?- cluedo:retractall(turns(_)).
?- cluedo:assertz(turns(
  [ ( suggestion("Orchid", "Rope", "Ballroom"), disproof(2) )
  , ( suggestion("Mustard", "Wrench", "Study"), disproof(0, "Wrench") )
  , ( suggestion("Orchid", "Revolver", "Ballroom"), disproof(0, "Revolver") )
  , ( suggestion("Mustard", "Candlestick", "Ballroom"), disproof(1, "Mustard") )

  , ( suggestion("Green", "Wrench", "Kitchen"), disproof(2) )
  , ( suggestion("Peacock", "Wrench", "Library"), disproof(3) )
  , ( suggestion("Peacock", "Rope", "Study"), disproof(1) )
  , ( suggestion("Scarlett", "Candlestick", "Billiard Room"), disproof(2, "Billiard Room") )

  , ( suggestion("Plum", "Wrench", "Billiard Room"), disproof(2) )
  , ( suggestion("Orchid", "Rope", "Conservatory"), disproof(0, "Conservatory") )
  , ( suggestion("Orchid", "Wrench", "Hall"), disproof(0, "Wrench") )
  , ( suggestion("Scarlett", "Candlestick", "Kitchen"), disproof(2, "Kitchen") )

  , ( suggestion("Peacock", "Wrench", "Library"), disproof(3) )
  , ( suggestion("Plum", "Rope", "Study"), disproof(1) )
  , ( suggestion("Plum", "Dagger", "Study"), disproof(0, "Dagger") )
  , ( suggestion("Scarlett", "Candlestick", "Library"), disproof(3, "Scarlett") )

  , ( suggestion("Plum", "Wrench", "Hall"), disproof(0, "Wrench") )
  , ( suggestion("Plum", "Dagger", "Hall"), disproof(0, "Dagger") )
  , ( suggestion("Plum", "Lead Pipe", "Lounge"), disproof(1) )
  , ( suggestion("Plum", "Candlestick", "Dining Room"), disproof(1, "Dining Room") )

  , ( nothing )
  , ( suggestion("Plum", "Revolver", "Dining Room"), disproof(0, "Revolver") )
  , ( suggestion("Orchid", "Rope", "Conservatory"), disproof(0, "Conservatory") )
  , ( suggestion("Plum", "Candlestick", "Lounge"), disproof(2, "Lounge") )

  , ( suggestion("Plum", "Revolver", "Library"), disproof(3) )
  , ( suggestion("Orchid", "Lead Pipe", "Hall"), disproof(0, "Hall") )
  , ( suggestion("Peacock", "Dagger", "Lounge"), disproof(0, "Dagger") )
  , ( suggestion("Plum", "Candlestick", "Study"), disproof )
  % player 0 wins!
  ]
)).
