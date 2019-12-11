// Learn more about F# at http://fsharp.org

open System
open System.IO

exception InnerError of string

type Parameter = {param: int; mode: int}
type Instruction = {opcode: int; pms: Parameter[]; offset: int}

let inp = File.ReadLines "input7-1"

let program = Seq.map Int32.Parse ((Seq.head inp).Split ',') |> Seq.toArray
let test = Seq.map Int32.Parse (("3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5").Split ',') |> Seq.toArray

let getDigit (n: int) pos =
    let s = string n
    //printfn "%s %d %d" s s.Length pos
    match s.Length - pos with
    | a when a < 0 -> 0
    | a -> Int32.Parse (string (s.Chars(a))) 
    
let getInstruction (prog: int[]) (pointer: int) =
    //printfn "Pointer: %A" pointer
    //printfn "Opcode: %A" prog.[pointer]
    let opc = prog.[pointer] % 100
    let off = 
        match opc with 
        | 99 -> -1
        | 1 | 2 | 7 | 8 -> 4
        | 3 | 4 -> 2
        | 5 | 6 -> 3
        | _ -> raise (InnerError("Incorrect opcode"))
    let pm = 
        match opc with
        | 99 -> [||]
        | 1 | 2 | 7 | 8-> [|{param=prog.[pointer + 1]; mode= getDigit prog.[pointer] 3}; 
                            {param=prog.[pointer + 2]; mode= getDigit prog.[pointer] 4}; 
                            {param=prog.[pointer + 3]; mode= getDigit prog.[pointer] 5}|]
        | 3 | 4 -> [|{param=prog.[pointer + 1]; mode= getDigit prog.[pointer] 3};|]
        | 5 | 6 -> [|{param=prog.[pointer + 1]; mode= getDigit prog.[pointer] 3}; 
                            {param=prog.[pointer + 2]; mode= getDigit prog.[pointer] 4}|]
        | _ -> raise (InnerError("Incorrect opcode"))        
    {opcode= opc; pms= pm; offset= off}

let getVal (prog: int[]) (pm: Parameter) =
    match pm.mode with
    | 0 -> Array.get prog pm.param
    | 1 -> pm.param
    | _ -> raise (InnerError("incorrect parameter mode"))

let rec execute prog pos (inputs: int list) (lastval: int) =
    let inst = getInstruction prog pos
    //printfn "%A" prog
    //printfn "%A" inst
    match inst.opcode with
    | 99 -> lastval
    | 1 -> 
        let v1 = getVal prog inst.pms.[0]
        let v2 = getVal prog inst.pms.[1]
        let addr = inst.pms.[2].param
        //printfn "%d %d %d" v1 v2 addr
        Array.set prog addr (v1 + v2)
        execute prog (pos + inst.offset) inputs lastval
    | 2 ->
        let v1 = getVal prog inst.pms.[0]
        let v2 = getVal prog inst.pms.[1]
        let addr = inst.pms.[2].param
        Array.set prog addr (v1 * v2)
        execute prog (pos + inst.offset) inputs lastval
    | 3 -> 
        //printfn "%A" inputs 
        let inp = (List.head inputs)
        let addr = inst.pms.[0].param
        Array.set prog addr inp 
        execute prog (pos + inst.offset) (List.tail inputs) lastval
    | 4 ->
        let v = getVal prog inst.pms.[0]
        //printfn "%A" v
        execute prog (pos + inst.offset) inputs v     
    | 5 ->
        let v = getVal prog inst.pms.[0]
        match v with
        | a when a <> 0 -> 
            let ptr = getVal prog inst.pms.[1]
            execute prog (ptr) inputs lastval
        | a -> execute prog (pos + inst.offset) inputs lastval
    | 6 ->
        let v = getVal prog inst.pms.[0]
        //printfn "%A" v
        match v with
        | a when a = 0 -> 
            let ptr = getVal prog inst.pms.[1]
            //printfn "%A" off
            execute prog (ptr) inputs lastval
        | a -> execute prog (pos + inst.offset) inputs lastval
    | 7 ->
        let v1 = getVal prog inst.pms.[0]
        let v2 = getVal prog inst.pms.[1]
        if v1 < v2 then 
            Array.set prog inst.pms.[2].param 1
        else
            Array.set prog inst.pms.[2].param 0
        execute prog (pos + inst.offset) inputs lastval
    | 8 ->
        let v1 = getVal prog inst.pms.[0]
        let v2 = getVal prog inst.pms.[1]
        if v1 = v2 then 
            Array.set prog inst.pms.[2].param 1
        else
            Array.set prog inst.pms.[2].param 0
        execute prog (pos + inst.offset) inputs lastval
    | _ -> raise (InnerError("Wrong opcode")) 

(*
let part1 = 
    let mutable signals = 0
    for a in [0 .. 4] do
        for b in [0 .. 4] do 
            for c in [0 .. 4] do
                for d in [0 .. 4] do
                    for e in [0 .. 4] do
                        let phases = [a;b;c;d;e]
                        if (Set.ofList phases).Count = 5 then
                            let r1 = execute program 0 [a; 0] 0
                            let r2 = execute program 0 [b; r1] 0
                            let r3 = execute program 0 [c; r2] 0
                            let r4 = execute program 0 [d; r3] 0
                            let r5 = execute program 0 [e; r4] 0
                            if r5 > signals then signals <- r5 else ()
                        else ()
    signals 
*)


let part2 = 
    let mutable signals = 0
    for a in [5 .. 9] do
        for b in [5 .. 9] do 
            for c in [5 .. 9] do
                for d in [5 .. 9] do
                    for e in [5 .. 9] do
                        let phases = [a;b;c;d;e]
                        if (Set.ofList phases).Count = 5 then
                            let r1 = execute program 0 [a; 0] 0
                            let r2 = execute program 0 [b; r1] 0
                            let r3 = execute program 0 [c; r2] 0
                            let r4 = execute program 0 [d; r3] 0
                            let r5 = execute program 0 [e; r4] 0
                            if r5 > signals then signals <- r5 else ()
                        else ()
    signals                


[<EntryPoint>]
let main argv =
    //let p = part1                        
    //printfn "%A" part1

    let v = part2
    printfn "%A" v
    0 // return an integer exit code
