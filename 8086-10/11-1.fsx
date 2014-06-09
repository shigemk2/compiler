let aout = System.IO.File.ReadAllBytes "regs.out"
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
    | (x, y) when ((x - 0xb8) = 0) -> show 3 (sprintf "mov ax, %04x" (read16 mem (ip + 1)))
    | 0xb9, _ ->
        show 3 (sprintf "mov cx, %04x" (read16 mem (ip + 1)))
    | 0xba, _ ->
        show 3 (sprintf "mov dx, %04x" (read16 mem (ip + 1)))
    | 0xbb, _ ->
        show 3 (sprintf "mov bx, %04x" (read16 mem (ip + 1)))
    | 0xbc, _ ->
        show 3 (sprintf "mov sp, %04x" (read16 mem (ip + 1)))
    | 0xbd, _ ->
        show 3 (sprintf "mov bp, %04x" (read16 mem (ip + 1)))
    | 0xbe, _ ->
        show 3 (sprintf "mov si, %04x" (read16 mem (ip + 1)))
    | 0xbf, _ ->
        show 3 (sprintf "mov di, %04x" (read16 mem (ip + 1)))
    | 0xc7, w ->
        match w with
        | 0x07 -> show 4 (sprintf "mov [bx], %04x" (read16 mem (ip + 2)))
        | 0x47 -> show 5 (sprintf "mov [bx+%x], %04x" mem.[ip + 2] (read16 mem (ip + 3)))
        | 0x06 -> show 6 (sprintf "mov [%04x], %04x" (read16 mem (ip + 2)) (read16 mem (ip + 4)))
        | _ -> show 4 "??"
    | 0xc6, w ->
        match w with
        | 0x07 -> show 3 (sprintf "mov byte [bx], %02x" mem.[ip + 2])
        | 0x47 -> show 4 (sprintf "mov byte [bx+%x], %02x" mem.[ip + 2] mem.[ip + 3])
        | 0x06 -> show 5 (sprintf "mov byte [%04x], %02x" (read16 mem (ip + 2)) mem.[ip + 4])
        | _ -> show 4 "??"
    | 0x89, w ->
        match w with
        | 0x07 -> show 2 (sprintf "mov [bx], ax")
        | 0x4f -> show 3 (sprintf "mov [bx+%x], cx" mem.[ip + 2])
        | 0x0f -> show 2 (sprintf "mov [bx], cx")
        | _ -> show 2 "??"
    | 0x88, w ->
        match w with
        | 0x07 -> show 2 (sprintf "mov [bx], al")
        | 0x67 -> show 3 (sprintf "mov [bx+%x], ah" mem.[ip + 2])
        | _ -> show 2 "??"
    | 0xb5, _ ->
        show 2 (sprintf "mov ch, %02x" mem.[ip + 1])
    | 0xb1, _ ->
        show 2 (sprintf "mov cl, %02x" mem.[ip + 1])
    | 0x81, 0x2e ->
        show 6 (sprintf "sub [%04x], %04x" (read16 mem (ip + 2)) (read16 mem (ip + 4)))
    | 0x80, 0x2e ->
        show 5 (sprintf "sub byte[%04x], %02x" (read16 mem (ip + 2)) mem.[ip + 4])
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
