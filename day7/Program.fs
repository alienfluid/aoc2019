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

type Payload = int * AsyncReplyChannel<string>
type Message = 
    | Combined of Payload * (MailboxProcessor<Message> option)

type Agent = MailboxProcessor<Message>

type Count = int * AsyncReplyChannel<string>

let referee = MailboxProcessor<Count>.Start(fun inbox ->
    let mutable highest = 0
    let rec loop () =
        async {
            let! (i, rc) = inbox.Receive()
            if i > highest then 
                highest <- i
                //printfn "New highest: %A" highest
            elif i = 777 then
                printfn "Highest: %A" highest 
            rc.Reply(String.Format("Received {0}", i))
            return! loop ()
        }
    loop ()
)

let agent prog pos num = MailboxProcessor<Message>.Start(fun inbox ->
    //printfn "Starting agent %d ...\n" num
    let rec execute prog pos (input: int) (lastval: int) (output) =
        async {
            let mutable npos = pos
            let mutable ninp = input
            let mutable lval = lastval
            let mutable nout = output

            let inst = getInstruction prog pos
            
            //if num = 5 then 
            //    printfn "Num: %d\n%A" num inst
            
            match inst.opcode with
            | 99 -> //printfn "Num: %d halted. Last val =%A" num lval
                    if num = 5 then 
                        //printfn "%A" lval
                        referee.PostAndReply(fun rc -> lval, rc) |> ignore
                    raise (InnerError("Thread done."))
            | 1 -> 
                let v1 = getVal prog inst.pms.[0]
                let v2 = getVal prog inst.pms.[1]
                let addr = inst.pms.[2].param
                Array.set prog addr (v1 + v2)
                npos <- pos + inst.offset
            | 2 ->
                let v1 = getVal prog inst.pms.[0]
                let v2 = getVal prog inst.pms.[1]
                let addr = inst.pms.[2].param
                Array.set prog addr (v1 * v2)
                npos <- pos + inst.offset
            | 3 ->      
                //printfn "Waiting for input ..."    
                let! inp = inbox.Receive()
                //printfn "%d Message received: %A" num inp
                match inp with
                | Combined (payload, out) -> 
                    let (i, rc) = payload
                    ninp <- i
                    match out with
                    | Some x -> nout <- x :: nout
                    | None -> ()
                    rc.Reply(String.Format("{0} message received", num))

                let addr = inst.pms.[0].param
                Array.set prog addr ninp

                npos <- pos + inst.offset
            | 4 ->
                lval <- getVal prog inst.pms.[0]

                //printfn "Output= %A" lval
                //printfn "%A" nout

                let ochan = List.head nout
                let reply = ochan.TryPostAndReply((fun rc -> Combined (Payload (lval, rc), None)), 100)
                //printfn "reply= %A" reply

                npos <- pos + inst.offset               
            | 5 ->
                let v = getVal prog inst.pms.[0]
                match v with
                | a when a <> 0 -> 
                    let ptr = getVal prog inst.pms.[1]
                    npos <- ptr
                | a -> npos <- pos + inst.offset
            | 6 ->
                let v = getVal prog inst.pms.[0]
                //printfn "%A" v
                match v with
                | a when a = 0 -> 
                    let ptr = getVal prog inst.pms.[1]
                    //printfn "%A" off
                    npos <- ptr
                | a -> npos <- pos + inst.offset
            | 7 ->
                let v1 = getVal prog inst.pms.[0]
                let v2 = getVal prog inst.pms.[1]
                if v1 < v2 then 
                    Array.set prog inst.pms.[2].param 1
                else
                    Array.set prog inst.pms.[2].param 0
                npos <- pos + inst.offset                   
            | 8 ->
                let v1 = getVal prog inst.pms.[0]
                let v2 = getVal prog inst.pms.[1]
                if v1 = v2 then 
                    Array.set prog inst.pms.[2].param 1
                else
                    Array.set prog inst.pms.[2].param 0
                npos <- pos + inst.offset    
            | _ -> raise (InnerError("Wrong opcode")) 

            //printfn "%A %A %A %A" npos ninp lval nout
            return! execute prog npos ninp lval nout
        }

    //printfn "Here"
    execute prog pos 0 0 []
)

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

let makeAgent id = async {
    agent program 0 id |> ignore
}

[<EntryPoint>]
let main argv =
    //let p = part1                        
    //printfn "%A" part1
    let mutable signals = 0
    for a in [5 .. 9] do
        for b in [5 .. 9] do 
            for c in [5 .. 9] do
                for d in [5 .. 9] do
                    for e in [5 .. 9] do
                        let phases = [a;b;c;d;e]
                        if (Set.ofList phases).Count = 5 then
                            
                            printfn "%A" phases

                            let v1 = agent program 0 1
                            let v2 = agent program 0 2
                            let v3 = agent program 0 3
                            let v4 = agent program 0 4
                            let v5 = agent program 0 5

                            let reply = v1.PostAndReply(fun rc -> Combined (Payload (a, rc), Some v2))
                            //printfn "reply= %A" reply

                            let reply = v2.PostAndReply(fun rc -> Combined (Payload (b, rc), Some v3))
                            //printfn "reply= %A" reply

                            let reply = v3.PostAndReply(fun rc -> Combined (Payload (c, rc), Some v4))
                            //printfn "reply= %A" reply

                            let reply = v4.PostAndReply(fun rc -> Combined (Payload (d, rc), Some v5))
                            //printfn "reply= %A" reply

                            let reply = v5.PostAndReply(fun rc -> Combined (Payload (e, rc), Some v1))
                            //printfn "reply= %A" reply

                            let reply = v1.PostAndReply(fun rc -> Combined (Payload (0, rc), None))
                            //printfn "reply= %A" reply
                            
                            //printfn "Waiting"
                            //let x = Console.ReadLine()
                            System.Threading.Thread.Sleep(1000)
                            ()    
                        else ()

    referee.PostAndReply(fun rc -> 777, rc) |> ignore
    let x = Console.ReadLine()

    0 // return an integer exit code
