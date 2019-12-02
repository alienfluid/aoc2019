// Learn more about F# at http://fsharp.org

open System
open System.IO

exception InnerError of string

let inp = File.ReadLines "input2-1"

// Test cases
//let inp = [|"1,0,0,0,99"|]
//let inp = [|"2,3,0,3,99"|]
//let inp = [|"2,4,4,5,99,0"|]
//let inp = [|"1,1,1,4,99,5,6,0,99"|]

let program = Seq.map Int32.Parse ((Seq.head inp).Split ',') |> Seq.toArray

let rec part1 prog pos =
    match Array.get prog pos with
    | 99 -> prog
    | 1 -> 
        Array.set prog (Array.get prog (pos + 3)) (Array.get prog (Array.get prog (pos + 1)) + Array.get prog (Array.get prog (pos + 2)))
        part1 prog (pos + 4) 
    | 2 ->
        Array.set prog (Array.get prog (pos + 3)) (Array.get prog (Array.get prog (pos + 1)) * Array.get prog (Array.get prog (pos + 2)))
        part1 prog (pos + 4)
    | _ -> raise (InnerError("Wrong opcode")) 

let part2 =
    for noun in 0 .. 100 do
        for verb in 0 .. 100 do
            let temp = Array.copy program
            Array.set temp 1 noun
            Array.set temp 2 verb
            match Seq.head (part1 temp 0) with
            | 19690720 -> 
                printfn "%d %d" noun verb
                raise (InnerError("Done!")) 
            | _ -> ()       

[<EntryPoint>]
let main argv =
    //Array.set program 1 12
    //Array.set program 2 2
    //printfn "%A" (part1 program 0).[0]
    part2
    0 // return an integer exit code
