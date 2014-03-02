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

let operation10 (a:byte[]) b =
    match (read8 a b).[2..3] with
    | "00" ->
        sprintf "bpl"
    | "04" ->
        sprintf "bmi"
    | "10" ->
        sprintf "bhi"
    | "44" | "45" | "46" | "47" ->
        sprintf "trap"

    | _ ->
        sprintf "??"

let operation (a:byte[]) b =
    match (read8 a b).[0..1] with
    | "01" ->
        sprintf "mov"
    | "10" ->
        operation10 a b
    | "11" ->
        sprintf "movb"
    | "16" ->
        sprintf "sub"
    | _ ->
        sprintf "??"

let mode7 (a:byte[]) b c =
    match (read8 a b).[c] with
    | '2' ->
        sprintf "#(%c)" (read8 a b).[c]
    | '3' ->
        sprintf "@#(%c)" (read8 a b).[c]
    | '6' ->
        sprintf "%c" (read8 a b).[c]
    | '7' ->
        sprintf "%c" (read8 a b).[c]

let mode (a:byte[]) b c =
    if (read8 a b).[c + 1] = '7' then
        mode7 a b c
    else
        match (read8 a b).[c] with
        | '0' ->
            sprintf "r%c" (read8 a b).[c + 1]
        | '1' ->
            sprintf "(r%c)" (read8 a b).[c + 1]
        | '2' ->
            sprintf "(r%c)+" (read8 a b).[c + 1]
        | '3' ->
            sprintf "@(r%c)+" (read8 a b).[c + 1]
        | '4' ->
            sprintf "-(r%c)" (read8 a b).[c + 1]
        | '5' ->
            sprintf "@-(r%c)" (read8 a b).[c + 1]
        | '6' ->
            sprintf "X(r%c)" (read8 a b).[c + 1]
        | '7' ->
            sprintf "@X(r%c)" (read8 a b).[c + 1]

while pc < tsize do
    printfn "%s %s, %s" (operation mem pc) (mode mem pc 2) (mode mem pc 4)
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
