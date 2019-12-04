// Learn more about F# at http://fsharp.org

open System

exception InnerError of string

let rec getDigits (n: int) acc =
    match n with 
    | 0 -> acc
    | a when a > 0 -> getDigits (a / 10) (a % 10 :: acc)
    | a -> raise (InnerError("unexpected"))

let getPairs nl =
    List.zip (List.take (List.length nl - 1) nl) (List.tail nl)

let checkIncrease x =
    let temp = Seq.filter (fun (a,b) -> a > b) x
    Seq.isEmpty temp

let checkRecurring x =
    let temp = Seq.filter (fun (a,b) -> a = b) x
    not (Seq.isEmpty temp)

let checkRecurring2 x =
    let temp = Seq.filter (fun (a,b) -> a = b) x |> Seq.countBy id
                                                 |> Seq.filter (fun (p, c) -> c = 1)
    not (Seq.isEmpty temp)

let isEligible (n: int) = 
    let temp = getDigits n [] |> getPairs 
    checkIncrease temp && checkRecurring2 temp

[<EntryPoint>]
let main argv =
    let t = [for x in 171309 .. 643603 do yield x] |> Seq.map isEligible
                                                   |> Seq.map (fun t -> Convert.ToInt32(t))
                                                   |> Seq.sum
    printfn "%A" t                                              
    0 // return an integer exit code
