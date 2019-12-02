// Learn more about F# at http://fsharp.org

open System
open System.IO

let inp = File.ReadLines "input1-1"

[<EntryPoint>]
let main argv =
    let j = inp |> Seq.map Int32.Parse

    let mutable continueLooping = true
    let mutable freq = 0
    let mutable mp : Set<int> = set []
    while continueLooping do
        for x in j do
            freq <- freq + x
            if mp.Contains(freq) && continueLooping then 
                printfn "%d" freq
                continueLooping <- false
            else
                mp <- mp.Add(freq)
        
    0 // return an integer exit code
