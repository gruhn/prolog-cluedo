A toy project to learn Prolog and my secret plan to not get wrecked by my sister in Cluedo anymore.

### Rules

https://en.wikipedia.org/wiki/Cluedo

### Start

```sh
docker-compose run swipl
```

### Features

#### What it can do

* deduce all possible hands of players and the envelopes content. Take into account:
  * cards you have seen
  * cards other players might have seen
  * cards other players don't have (when he couldn't disprove a suggestion)
  * cards named in suggestions by other players
  * suggestions other players made
  * accusations other players made

#### What it can't do

* Take the board, figures and their positions into account
* Recommend the next move

### Contributing

I'm both a Prolog newbie and bad in Cluedo so there is a lot of room for improvement. All contributions are welcome. Make sure to provide detailed explanations though. I'm more interested in learning then having a killer Cluedo AI.
