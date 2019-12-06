// Learn more about F# at http://fsharp.org

open System
open System.IO

exception InnerError of string

let inp = List.ofSeq (File.ReadLines "input6-1" |> Seq.map (fun a -> a.Split ')'))
let test = List.ofSeq (File.ReadLines "test6-1" |> Seq.map (fun a -> a.Split ')'))

let rec omap (lst: string [] list) (mp: Map<string, string>) =
    match lst with
    | [] -> mp
    | h :: t -> 
        let x = mp.Add(h.[1], h.[0])
        omap t x

let rec getPathLength (obj: string) (om: Map<string, string>) (len: int) =
    match obj with 
    | "COM" -> len
    | a -> 
        let nobj = om.TryFind a
        //printfn "%A" a
        match nobj with 
        | Some x -> getPathLength x om (1 + len)
        | None -> raise (InnerError("Could not find object"))

let rec getPath (obj: string) (om: Map<string, string>) (path: string list) =
    match obj with
    | "COM" -> path
    | a ->
        let nobj = om.TryFind a
        match nobj with 
        | Some x -> getPath x om (x :: path)
        | None -> raise (InnerError("Could not find object"))

[<EntryPoint>]
let main argv =
    let orbits = omap inp Map.empty
    let torbits = omap test Map.empty

    let objs = Set.ofList (List.concat (inp |> Seq.map (fun a -> List.ofArray a)))
    let norb = objs |> Seq.map (fun a -> getPathLength a orbits 0) |> Seq.sum
    printfn "%A" norb

    let youpath = getPath "YOU" orbits []
    let sanpath = getPath "SAN" orbits []

    let comb = List.ofSeq ((List.append youpath sanpath) |> Seq.countBy (id) |> Seq.filter (fun (v, c) -> c = 1))
    printfn "%A" (List.length comb)
    0 // return an integer exit code
