let aout = System.IO.File.ReadAllBytes "a.out"
let read16 (a:byte[]) b =
    (int a.[b]) ||| ((int a.[b + 1]) <<< 8)
let tsize = read16 aout 2
let dsize = read16 aout 4
let mem = aout.[16 .. 16 + tsize + dsize - 1]
let mutable ip = 0
let show len dis =
    let bin = [ for b in mem.[ip .. ip + len - 1] -> sprintf "%02x" b ]
    printfn "%04x: %-12s  %s" ip (String.concat "" bin) dis
    ip <- ip + len
while ip < tsize do
    match int mem.[ip] with
    | 0xb8 ->
        let n = read16 mem (ip + 1)
        show 3 (sprintf "mov ax, %04x" n)
    | 0xcd ->
        let n = int mem.[ip + 1]
        show 2 (sprintf "int %x" n)
        if n = 7 then
            match int mem.[ip] with
            | 1 ->
                show 1 "; exit"
            | 4 ->
                show 1 "; write"
                show 2 "; arg"
                show 2 "; arg"
            | _ ->
                show 1 "; ???"
    | _ ->
        show 1 "???"
