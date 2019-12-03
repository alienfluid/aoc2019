// Learn more about F# at http://fsharp.org

open System
open System.IO

type Pos = {X: int; Y: int}
exception InnerError of string

let inp = File.ReadLines "input3-1"

let route1 = Seq.toList ((Seq.head inp).Split ',')
let route2 = Seq.toList ((Seq.head (Seq.tail inp)).Split ',')

let test1 = Seq.toList ("R75,D30,R83,U83,L12,D49,R71,U7,L72".Split ',')
let test2 = Seq.toList ("U62,R66,U55,R34,D71,R55,D58,R83".Split ',')
//let test1 = Seq.toList ("R8,U5,L5,D3".Split ',')
//let test2 = Seq.toList ("U7,R6,D4,L4".Split ',')

let getPath (pos: Pos) (dir: string) =
    let d = Seq.head dir
    let steps = Int32.Parse (dir.Substring 1)
    match d with
    | 'U' ->
        [for t in pos.Y + 1 .. pos.Y + steps do yield {X = pos.X; Y = t;} ]
    | 'D' -> 
        List.rev [for t in pos.Y - steps .. pos.Y - 1 do yield {X = pos.X; Y = t;} ]
    | 'L' ->
        List.rev [for t in pos.X - steps .. pos.X - 1  do yield {X = t; Y = pos.Y;} ]
    | 'R' -> 
        [for t in pos.X + 1 .. pos.X + steps do yield {X = t; Y = pos.Y;} ]
    | _ -> raise (InnerError("Incorrect direction"))

let rec path route (cpos: Pos) acc =
    match route with
    | [] -> acc
    | hd :: tl -> 
        let temp = getPath cpos hd
        path tl (Seq.last temp) (List.append acc temp)

let getSteps (pos: Pos) pth =
    (List.findIndex (fun x -> x = pos) pth) + 1

[<EntryPoint>]
let main argv =
    //printfn "%A" (path test1 {X = 0; Y = 0} [])
    let path1 = path route1 {X = 0; Y = 0} []
    let path2 = path route2 {X = 0; Y = 0} []
    //printfn "%A" path2
    let t1 = Set.ofList path1
    let t2 = Set.ofList path2
    let intersect = Set.intersect t1 t2
    
    let minDist = intersect |> Seq.map (fun (p: Pos) -> getSteps p path1 + getSteps p path2 )
                            |> Seq.min 
    //printfn "%A" intersect
    //let minDist = intersect |> Seq.map (fun (p: Pos) -> abs p.X + abs p.Y)
    //                        |> Seq.min
    printfn "%A" minDist
    0 // return an integer exit code
