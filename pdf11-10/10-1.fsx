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
// fetch 中で変数の値を書き換えているので副作用が発生する
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
        show oldpc 4 (sprintf "mov r2, %x(r1)" (fetch()))
    | 0o012767 ->
        // let opr1, opr2 = fetch(), fetch() このような書き方だと環境依存で実行順序が保証されない
        let opr1 = fetch()
        let opr2 = fetch() // (pc + 6 + read16 mem (pc + 4)) fetchが全部済んだ段階のプログラムカウンター
        // 中で値をとったら進む、を繰り返す=fetch(CPU用語)
        show oldpc 6 (sprintf "mov $%x, %04x" opr1 (pc + opr2))
    // switch-caseの高級なやつで、式が書ける(パターンマッチ)
    | w when (w &&& 0o177770) = 0o012700 ->
        // let w2 = read16 mem (pc + 2)
        let t, r = (w >>> 3 &&& 7), w &&& 7
        show oldpc 4 (sprintf "mov $%x, %s" (fetch()) (getopr t r))
    | 0o112761 ->
        let opr1 = fetch()
        let opr2 = fetch()
        show oldpc 6 (sprintf "movb $%x, %x(r1)" opr1 opr2)
    | 0o112711 ->
        show oldpc 4 (sprintf "movb $%x, (r1)" (fetch()))
    | 0o110011 ->
        show oldpc 2 "movb r0, (r1)"
    | 0o110061 ->
        show oldpc 4 (sprintf "movb r0, %x(r1)" (fetch()))
    | 0o000300 ->
        show oldpc 2 "swab r0"
    | 0o104401 ->
        show oldpc 2 "sys 1 ; exit"
    | 0o104404 ->
        show oldpc 2 "sys 4 ; write"
        show pc 2 "; arg"
        show pc 2 "; arg"
    | 0o112767 ->
        // 中で値をとったら進む、を繰り返す=fetch(CPU用語)
        let opr1 = fetch()
        let opr2 = fetch()
        show oldpc 6 (sprintf "movb $%x, %04x" opr1 opr2)
    | 0o162767 ->
        // 中で値をとったら進む、を繰り返す=fetch(CPU用語)
        let opr1 = fetch()
        let opr2 = fetch()
        show oldpc 6 (sprintf "sub $%x, %04x" opr1 opr2)
    | _ ->
        show oldpc 2 "???"
