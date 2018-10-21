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
- Load main module via `:l GameIO`
- To start a default game between a Human and AI player(dumb) use `startDefault`
- You can start the game with your choice of players with the following command
```start [players] <fastMode?>``` where `players` is a list of 
length 2 with elements of type `Player` and `fastMode` is a Boolean 
to specify verbose output or a quick play
- A tournament between two players of `n` games can be started using ```startTournament n (p1,p2)```

## Current Player Agents 
All of these can found in `AIPlayer.hs`
- Human Player - `human_player`
- Choose First Action (Reflex) Player - `unintelligent_player`
- Random Action Player - `random_player`
- Simple Player (chooses actions in specified order) - `simple_player`
- Heuristic Player (uses a heuristic function) - `my_hmm_player`
- Smarter Heuristic Player (uses a win sequencing heuristic function) - `smart_player`
- Minimax Player (uses the minimax algorithm) - `my_mm_player`
- Gui Player with options (uses depth limited minimax with a original heuristic that takes 2 parameters) - `(gui_player_with_options (heuristic_option1, heuristic_option2) depth)`
- Gui Player (Gui Player with default options and depth 2) - `gui_player`
