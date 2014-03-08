let aout = System.IO.File.ReadAllBytes "write-7.out"
let read16 (a:byte[]) b =
    (int a.[b]) ||| ((int a.[b+1]) <<< 8)
let tsize = read16 aout 2
let dsize = read16 aout 4
let mem = aout.[16 .. 16 + tsize + dsize - 1]
let mutable pc = 0
let show len dis =
    let words = [ for i in pc .. 2 .. pc + len - 1 -> sprintf "%04x" (read16 mem i) ]
    printfn "%04x: %-14s  %s" pc (String.concat " " words) dis
    pc <- pc + len
while pc < tsize do
    let w = read16 mem pc
    match w with
    | 0o010011 ->
        show 2 "mov r0, (r1)"
    | 0o010261 ->
        show 4 (sprintf "mov r2, %x(r1)" (read16 mem (pc + 2)))
    | 0o012700
    | 0o012701
    | 0o012702 ->
        show 4 (sprintf "mov $%x, r%d" (read16 mem (pc + 2)) (w &&& 0o7))
    | 0o012711 ->
        show 4 (sprintf "mov $%x, (r1)" (read16 mem (pc + 2)))
    | 0o112761 ->
        show 6 (sprintf "movb $%x, %x(r1)" (read16 mem (pc + 2)) (read16 mem (pc + 4)))
    | 0o112711 ->
        show 4 (sprintf "movb $%x, (r1)" (read16 mem (pc + 2)))
    | 0o110011 ->
        show 2 "mov r0, (r1)"
    | 0o110061 ->
        show 4 (sprintf "movb r0, %x(r1)" (read16 mem (pc + 2)))
    | 0o000300 ->
        show 2 "swab r0"
    | 0o104401 ->
        show 2 "sys 1 ; exit"
    | 0o104404 ->
        show 2 "sys 4 ; write"
        show 2 "; arg"
        show 2 "; arg"
    | 0o012767 ->
        show 6 (sprintf "mov $%x, %04x" (read16 mem (pc + 2)) (pc + 6 + read16 mem (pc + 4)))
    | 0o112767 ->
        show 6 (sprintf "movb $%x, %04x" (read16 mem (pc + 2)) (pc + 6 + read16 mem (pc + 4)))
    | 0o162767 ->
        show 6 (sprintf "sub $%x, %04x" (read16 mem (pc + 2)) (pc + 6 + read16 mem (pc + 4)))
    | _ ->
        show 2 "???"
