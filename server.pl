:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(cluedo).

:- http_handler('/', http_reply_file('index.html', []), []).
:- http_handler('/api', handle, []).

server :-
  http_server(http_dispatch, [ port(80) ]).

handle(Request) :-
  http_read_json_dict(Request, DictIn),
  compute(DictIn, DictOut),
  reply_json(DictOut).

compute(DictIn, DictOut) :-
  _{ player_count: PlayerCount
   , player_first: PlayerFirst
   , turns: Turns
   , own_cards: OwnCards
   } :< DictIn, !,

  cluedo:assertz(player_count(PlayerCount)),
  cluedo:assertz(player_first(PlayerFirst)),
  cluedo:assertz(turns(Turns)),
  cluedo:assertz(own_cards(OwnCards)),

  DictOut = _{}.

