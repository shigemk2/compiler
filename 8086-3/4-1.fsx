let aout = System.IO.File.ReadAllBytes "write-2.out"
let read16 (a:byte[]) b =
    (int a.[b]) ||| ((int a.[b + 1]) <<< 8)
let tsize = read16 aout 2
let dsize = read16 aout 4
let mem = Array.zeroCreate<byte> 0x10000
mem.[0 .. tsize + dsize - 1] <- aout.[16 .. 16 + tsize + dsize - 1]
let mutable ip = 0
let show len dis =
    let bin = [ for b in mem.[ip .. ip + len - 1] -> sprintf "%02x" b ]
    printfn "%04x: %-12s  %s" ip (String.concat "" bin) dis
    ip <- ip + len
while ip < tsize do
    match int mem.[ip], int mem.[ip + 1] with
    | 0xb8, _ ->
        show 3 (sprintf "mov ax, %04x" (read16 mem (ip + 1)))
    | 0xbb, _ ->
        show 3 (sprintf "mov bx, %04x" (read16 mem (ip + 1)))
    | 0xc7, 0x07 ->
        show 4 (sprintf "mov [bx], %04x" (read16 mem (ip + 2)))
    | 0xc7, 0x47 ->
        show 5 (sprintf "mov [bx+%x], %04x" mem.[ip + 2] (read16 mem (ip + 3)))
    | 0xcd, 0x07 ->
        show 2 "int 7"
        match int mem.[ip] with
        | 1 ->
            show 1 "; exit"
        | 4 ->
            show 1 "; write"
            show 2 "; arg"
            show 2 "; arg"
        | _ ->
            show 1 "; ???"
    | 0xcd, n ->
        show 2 (sprintf "int %x" n)
    | _ ->
        show 1 "???"
