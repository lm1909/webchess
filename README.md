# webchess
A web-based chess platform with human-human & human-AI Games that allows for specators and has many social features, such as rankings, a lobby and more.

Webchess is written completely in Haskell for the final project of the course *Fortgeschrittene funktionale Programmierung* @LMU Munich by Maximilian Lutz.

## build
easiest to build with stack via:
```
stack build webchess --ghc-options "-O2 -threaded"
```
```
stack exec webchess --rts-options "-N4"
```

## features

###### accounts & elo system
Users can create an account and modify their profile. There is an elo system that allows for ranking the users.
###### human-human games
Users can play against each other. The board is clickable, there is a spectator mode (that means everybody can follow the game with live autoupdates by just visiting the stable link of the game) There is account verification, so only the authorized players can make moves.
###### AI games
The user can choose to play against two available AIs.\
**Easy** The easy AI is just a vanilla min-max-Search (with a depth of 4). This AI is parallelized and uses all available cores on the server system.\
**Medium** The medium AI uses an alpha-beta-pruning algorithm with dynamical search depth, move ordering, horizontal pruning & quiescence search (quiescence yet to be implemented). Moreover the medium AI utilizes an opening book. Static board evaluation is done by piece-square tables with game phase differentiation.
###### platform features
The platforms allows to see profiles of other users, join a lobby to find other users that are currently searching to be challenged to a game, provides a list of running games (for spectators) and a highscore ranking (ranked by elo) of the best users on the platform.
