let aout = System.IO.File.ReadAllBytes "write-8.out"
let read16 (a:byte[]) b =
    (int a.[b]) ||| ((int a.[b+1]) <<< 8)
let read8 (a:byte[]) b =
    sprintf "%06o" (read16 a b)

let tsize = read16 aout 2
let dsize = read16 aout 4
let mem = aout.[16 .. 16 + tsize + dsize - 1]
let mutable pc = 0
let show len dis =
    let words = [ for i in pc .. 2 .. pc + len - 1 -> sprintf "%04x" (read16 mem i) ]
    printfn "%04x: %-14s  %s" pc (String.concat " " words) dis
    pc <- pc + len

let one (a:byte[]) b =
    (read8 a b).[0..1]
let two (a:byte[]) b =
    (read8 a b).[2..3]
let three (a:byte[]) b =
    (read8 a b).[4..5]

while pc < tsize do
    printfn "%s" (read8 mem pc)
    printfn "%s" (one mem pc)
    printfn "%s" (two mem pc)
    printfn "%s" (three mem pc)
    match read16 mem pc with
    | 0x1009 ->
        show 2 "mov r0, (r1)"
    | 0x10b1 ->
        show 4 (sprintf "mov r2, %x(r1)" (read16 mem (pc + 2)))
    | 0x15c0 ->
        show 4 (sprintf "mov $%x, r0" (read16 mem (pc + 2)))
    | 0x15c1 ->
        show 4 (sprintf "mov $%x, r1" (read16 mem (pc + 2)))
    | 0x15c2 ->
        show 4 (sprintf "mov $%x, r2" (read16 mem (pc + 2)))
    | 0x15c9 ->
        show 4 (sprintf "mov $%x, (r1)" (read16 mem (pc + 2)))
    | 0x95f1 ->
        show 6 (sprintf "movb $%x, %x(r1)" (read16 mem (pc + 2)) (read16 mem (pc + 4)))
    | 0x95c9 ->
        show 4 (sprintf "movb $%x, (r1)" (read16 mem (pc + 2)))
    | 0x9009 ->
        show 2 "mov r0, (r1)"
    | 0x9031 ->
        show 4 (sprintf "movb r0, %x(r1)" (read16 mem (pc + 2)))
    | 0x00c0 ->
        show 2 "swab r0"
    | 0x8901 ->
        show 2 "sys 1 ; exit"
    | 0x8904 ->
        show 2 "sys 4 ; write"
        show 2 "; arg"
        show 2 "; arg"
    | 0x15f7 ->
        show 6 (sprintf "mov $%x, %04x" (read16 mem (pc + 2)) (pc + 6 + read16 mem (pc + 4)))
    | 0x95f7 ->
        show 6 (sprintf "movb $%x, %04x" (read16 mem (pc + 2)) (pc + 6 + read16 mem (pc + 4)))
    | 0xe5f7 ->
        show 6 (sprintf "sub $%x, %04x" (read16 mem (pc + 2)) (pc + 6 + read16 mem (pc + 4)))
    | _ ->
        show 2 "???"
