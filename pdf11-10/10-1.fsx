let aout = System.IO.File.ReadAllBytes "write-8.out"
let read16 (a:byte[]) b =
    (int a.[b]) ||| ((int a.[b+1]) <<< 8)
let tsize = read16 aout 2
let dsize = read16 aout 4
let mem = aout.[16 .. 16 + tsize + dsize - 1]
let mutable pc = 0
let show oldpc len dis =
    let words = [ for i in oldpc .. 2 .. oldpc + len - 1 -> sprintf "%04x" (read16 mem i) ]
    printfn "%04x: %-14s  %s" oldpc (String.concat " " words) dis
    pc <- oldpc + len
// fetch
let fetch() =
    let ret = read16 mem pc
    pc <- pc + 2
    ret
// operand取得 t type r register
let getopr t r =
    match t with
    | 0 -> sprintf "r%d" r
    | 1 -> sprintf "(r%d)" r
    | _ -> "??"

while pc < tsize do
    let oldpc = pc
    match fetch() with
    | 0o010011 ->
        show oldpc 2 "mov r0, (r1)"
    | 0o010261 ->
        show oldpc 4 (sprintf "mov r2, %x(r1)" (read16 mem (pc + 2)))
    | 0o012767 ->
        show oldpc 6 (sprintf "mov $%x, %04x" (read16 mem (pc + 2)) (pc + 6 + read16 mem (pc + 4)))
    // switch-caseの高級なやつで、式が書ける(パターンマッチ)
    | w when (w &&& 0o177770) = 0o012700 ->
        let w2 = read16 mem (pc + 2)
        let t, r = (w >>> 3 &&& 7), w &&& 7
        show oldpc 4 (sprintf "mov $%x, %s" w2 (getopr t r))
    | 0o112761 ->
        show oldpc 6 (sprintf "movb $%x, %x(r1)" (read16 mem (pc + 2)) (read16 mem (pc + 4)))
    | 0o112711 ->
        show oldpc 4 (sprintf "movb $%x, (r1)" (read16 mem (pc + 2)))
    | 0o110011 ->
        show oldpc 2 "movb r0, (r1)"
    | 0o110061 ->
        show oldpc 4 (sprintf "movb r0, %x(r1)" (read16 mem (pc + 2)))
    | 0o000300 ->
        show oldpc 2 "swab r0"
    | 0o104401 ->
        show oldpc 2 "sys 1 ; exit"
    | 0o104404 ->
        show oldpc 2 "sys 4 ; write"
        show pc 2 "; arg"
        show pc 2 "; arg"
    | 0o112767 ->
        show oldpc 6 (sprintf "movb $%x, %04x" (read16 mem (pc + 2)) (pc + 6 + read16 mem (pc + 4)))
    | 0o162767 ->
        show oldpc 6 (sprintf "sub $%x, %04x" (read16 mem (pc + 2)) (pc + 6 + read16 mem (pc + 4)))
    | _ ->
        show oldpc 2 "???"
