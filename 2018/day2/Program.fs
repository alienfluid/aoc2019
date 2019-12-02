// Learn more about F# at http://fsharp.org

open System
open System.IO

let inp = File.ReadLines "input2-1"

let freq = inp |> Seq.map (fun x -> Seq.countBy (id) x)
           
[<EntryPoint>]
let main argv =
    let prg = freq |> Seq.map (Seq.filter (fun (l, f) -> (f = 2 || f = 3)))
                   |> Seq.filter (fun x -> Seq.length x > 0)
                   |> Seq.map (Seq.map (fun (l, f) -> f))

    let c2 = prg |> Seq.filter (Seq.contains 2) |> Seq.length
    let c3 = prg |> Seq.filter (Seq.contains 3) |> Seq.length

    printfn "%d" (c2 * c3)           

    //let prg2 = prg |> Seq.iter (fun x -> for (l,f) in x do printfn  "%A %A" l f)
    0 // return an integer exit code
