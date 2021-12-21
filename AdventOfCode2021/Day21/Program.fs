open System
open System.Collections.Generic

module Day21 = 

    type Player = { Position: int; Score: int }

    type Game = { Player1: Player; Player2: Player; TargetScore: int } with
        member this.player1Wins = this.Player1.Score >= this.TargetScore
        member this.player2Wins = this.Player2.Score >= this.TargetScore

    let createGame position1 position2 targetScore = 
        { Player1 = { Position = position1; Score = 0 };
          Player2 = { Position = position2; Score = 0 };
          TargetScore = targetScore }

    let step steps player =
        let position = (player.Position + steps - 1) % 10 + 1
        let score = player.Score + position
        { Position = position; Score = score }

    let play1 game =
        
        let rec nextRound (rounds: int) (player1: bool) (game: Game) =
            if game.player1Wins || game.player2Wins then
                (game, rounds)
            else 
                let deterministicDiceSteps = (3 * rounds + 6 - 1) % 100 + 1
                if player1 then
                    { game with Player1 = game.Player1 |> step deterministicDiceSteps } |> nextRound (rounds + 3) false
                else
                    { game with Player2 = game.Player2 |> step deterministicDiceSteps } |> nextRound (rounds + 3) true 

        game |> nextRound 0 true

    let play2 game =
        
        let memoization f =
            let cache = Dictionary<_, _>()
            fun arg1 arg2 ->
                let found, value = cache.TryGetValue((arg1, arg2))
                match found with
                | true -> value
                | _ -> 
                    let value = f arg1 arg2
                    cache.Add ((arg1, arg2), value)
                    value

        let diracDiceSteps = [
            for i in [1..3] do
                for j in [1..3] do
                    for k in [1..3] do
                        (i + j + k)
        ]

        let rec nextRound (player1: bool) (game: Game) =
            if (game.player1Wins) then 
                (1UL, 0UL)
            else if (game.player2Wins) then
                (0UL, 1UL)
            else
                diracDiceSteps |> List.fold (fun (totalWins1, totalWins2) steps -> 
                    let (wins1, wins2) = 
                        if player1 then
                            { game with Player1 = game.Player1 |> step steps } |> nextRoundMemoization false
                        else
                            { game with Player2 = game.Player2 |> step steps } |> nextRoundMemoization true
                    (totalWins1 + wins1, totalWins2 + wins2)) (0UL, 0UL)
        
        and nextRoundMemoization = memoization nextRound

        game |> nextRoundMemoization true

let (game, rounds) = Day21.createGame 3 5 1000 |> Day21.play1

printfn "%i" (rounds * min game.Player1.Score game.Player2.Score) // 720750

let (player1, player2) = Day21.createGame 3 5 21 |> Day21.play2

printfn "%i" (max player1 player2) // 275067741811212