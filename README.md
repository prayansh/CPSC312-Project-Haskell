# CPSC312-Project-Haskell
Project for CPSC 312
## Authors
- [@guilhermelameira](https://github.com/guilhermelameira)
- [@mingyugao](https://github.com/mingyugao)
- [@prayansh](https://github.com/prayansh)

## Contribution
- `master` branch is protected so, every major change will require a 
pull request, this is so as to prevent master branch from breaking 

## How to Run
`ghc` is required to run this project
To start a default game between a Human and AI player(dumb) use `startDefault`
You can start the game with your choice of players with the following command
```start players <fastMode?>``` where `players` is a list of 
length 2 with elements of type `Player` and `fastMode` is a Boolean 
to specify verbose output or a quick play

## Current Player Agents 
All of these can found in `AIPlayer.hs`
- Human Player - `human_player`
- Choose First Action Player - `unintelligent_player`
- Random Action Player - `random_player`
- Simple Player (chooses actions in specified order) - `simple_player`
- Heuristic Player (uses a heuristic function) - `my_hmm_player`
- Minimax Player (uses the minimax algorithm) - `my_mm_player`