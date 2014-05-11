let aout = System.IO.File.ReadAllBytes "a.out"
let read16a (a:byte[]) b =
    (int a.[b]) ||| ((int a.[b+1]) <<< 8)
let read16b (a:byte[]) b =
    ((int a.[b]) <<< 8) ||| (int a.[b+1])
let tsize = read16a aout 2
let dsize = read16a aout 4
let mem = aout.[16 .. 16 + tsize + dsize - 1]
let mutable pc = 0
let showa len dis =
    let words = [ for i in pc .. 2 .. pc + len - 1 -> sprintf "%04x" (read16b mem i) ]
    printfn "%04x: %-14s  %s" pc (String.concat " " words) dis
    pc <- pc + len
let showb len dis =
    let words = [ for i in pc .. 2 .. pc + len - 1 -> sprintf "%06x" (read16b mem i) ]
    printfn "%04x: %-14s  %s" pc (String.concat " " words) dis
    pc <- pc + len + 1
let showc len dis =
    printfn "%04x: %02x              %s" pc mem.[pc] dis
    pc <- pc + len
while pc < tsize do
    match read16b mem pc with
    | w when (w >>> 8 = 04) ->
        showc 1 "; sys write"
    | w when (w >>> 8 = 01) ->
        showc 1 "; sys exit"
    | 0xb801 ->
        showb 2 (sprintf "mov ax, %04x" mem.[pc + 1])
    | 0xb800 ->
        showb 2 (sprintf "mov ax, %04x" mem.[pc + 1])
    | 0xcd07 ->
        showa 2 "int 7"
    | 0x1000 ->
        showa 2 "; arg"
    | 0x0600 ->
        showa 2 "; arg"
    | _ ->
        showa 2 "???"
