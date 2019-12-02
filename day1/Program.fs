// Learn more about F# at http://fsharp.org

open System
open System.IO

let inp = File.ReadLines "input1-1"

let fuel x = int (floor (float x / 3.0) - 2.0)

let totalFuel = inp |> Seq.map Int32.Parse
                     |> Seq.map fuel
                     |> Seq.sum

let rec calcFuel tot x = 
    //printfn "%A" x
    match fuel x with 
    | a when a <= 0 -> tot
    | a -> calcFuel (tot + a) a

let totalFuel2 = inp |> Seq.map Int32.Parse
                     |> Seq.map (fun x -> calcFuel 0 x)
                     |> Seq.sum

[<EntryPoint>]
let main argv =
    //printfn "%A" totalFuel
    printfn "%A" totalFuel2
    0 // return an integer exit code
