// Learn more about F# at http://fsharp.org

open System
open System.IO 

exception InnerError of string

let w = 25
let h = 6
let size = w * h
let inp = Seq.head (File.ReadLines "input8-1")
let test = "0222112222120000"
let ilist = inp.ToCharArray() |> Seq.map Char.GetNumericValue |> Seq.map int |> List.ofSeq

let rec getLayers (sz: int) (i: int list) (layers) =
    match i with
    | [] -> List.rev layers
    | h :: tl -> 
        let temp = List.splitAt sz i
        getLayers sz (snd temp)  ((fst temp) :: layers)

let renderLayer (lay: int list) =
    let t = Array.ofList lay
    for x in [1 .. size] do
        if t.[x-1] = 1 then printf "*" else printf " "
        if x % 25 = 0 then printf "%s" "\n"

let pickColor i1 i2 =
    match i1 with
    | 0 | 1 -> i1
    | 2 -> i2 
    | _ -> raise (InnerError("Wrong color"))

let combineLayer (layer1: int list) (layer2: int list) =
    //printfn "%A %A" layer1 layer2
    match layer1 with
    | [] -> layer2
    | _ -> List.ofSeq ((List.zip layer1 layer2) |> Seq.map (fun (i1, i2) -> pickColor i1 i2))

let rec combineLayers layers res =
    match layers with
    | [] -> res 
    | h :: t -> combineLayers t (combineLayer res h)

[<EntryPoint>]
let main argv =
    let temp =  (getLayers size ilist [])
    let minZero = temp |> Seq.map (List.countBy id)
                       |> Seq.map (fun a -> 
                                    let cnt = List.minBy fst a |> snd
                                    (cnt, a)) 
                       |> Seq.minBy fst 
                       |> snd
    //printfn "%A" temp                       
    let out = combineLayers temp []       
    //printfn "%A" out                       
    renderLayer out
    0 // return an integer exit code