// Learn more about F# at http://fsharp.org

open System
open System.IO

exception InnerError of string

type Parameter = {param: int; mode: int}
type Instruction = {opcode: int; pms: Parameter[]; offset: int}

let inp = File.ReadLines "input5-1"

let program = Seq.map Int32.Parse ((Seq.head inp).Split ',') |> Seq.toArray
let test = Seq.map Int32.Parse (("3,3,1105,-1,9,1101,0,0,12,4,12,99,1").Split ',') |> Seq.toArray

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

let rec execute prog pos =
    let inst = getInstruction prog pos
    //printfn "%A" prog
    //printfn "%A" inst
    match inst.opcode with
    | 99 -> inst
    | 1 -> 
        let v1 = getVal prog inst.pms.[0]
        let v2 = getVal prog inst.pms.[1]
        let addr = inst.pms.[2].param
        //printfn "%d %d %d" v1 v2 addr
        Array.set prog addr (v1 + v2)
        execute prog (pos + inst.offset) 
    | 2 ->
        let v1 = getVal prog inst.pms.[0]
        let v2 = getVal prog inst.pms.[1]
        let addr = inst.pms.[2].param
        Array.set prog addr (v1 * v2)
        execute prog (pos + inst.offset)
    | 3 -> 
        let inp = 5
        let addr = inst.pms.[0].param
        Array.set prog addr inp 
        execute prog (pos + inst.offset)
    | 4 ->
        let v = getVal prog inst.pms.[0]
        printfn "%A" v
        execute prog (pos + inst.offset)      
    | 5 ->
        let v = getVal prog inst.pms.[0]
        match v with
        | a when a <> 0 -> 
            let ptr = getVal prog inst.pms.[1]
            execute prog (ptr)
        | a -> execute prog (pos + inst.offset)
    | 6 ->
        let v = getVal prog inst.pms.[0]
        //printfn "%A" v
        match v with
        | a when a = 0 -> 
            let ptr = getVal prog inst.pms.[1]
            //printfn "%A" off
            execute prog (ptr)
        | a -> execute prog (pos + inst.offset)
    | 7 ->
        let v1 = getVal prog inst.pms.[0]
        let v2 = getVal prog inst.pms.[1]
        if v1 < v2 then 
            Array.set prog inst.pms.[2].param 1
        else
            Array.set prog inst.pms.[2].param 0
        execute prog (pos + inst.offset)
    | 8 ->
        let v1 = getVal prog inst.pms.[0]
        let v2 = getVal prog inst.pms.[1]
        if v1 = v2 then 
            Array.set prog inst.pms.[2].param 1
        else
            Array.set prog inst.pms.[2].param 0
        execute prog (pos + inst.offset)
    | _ -> raise (InnerError("Wrong opcode")) 

[<EntryPoint>]
let main argv =
    //printfn "%A" test
    //printfn "%A" (getInstruction test 0)
    let _ = execute program 0
    0 // return an integer exit code
