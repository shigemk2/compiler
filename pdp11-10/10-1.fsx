let aout = System.IO.File.ReadAllBytes "write-8.out"
let read16 (a:byte[]) b =
    (int a.[b]) ||| ((int a.[b+1]) <<< 8)
let tsize = read16 aout 2
let dsize = read16 aout 4
let mem = aout.[16 .. 16 + tsize + dsize - 1]
let mutable pc = 0
let show oldpc dis =
    let words = [ for i in oldpc .. 2 .. pc - 1 -> sprintf "%04x" (read16 mem i) ]
    printfn "%04x: %-14s  %s" oldpc (String.concat " " words) dis

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
        show oldpc "mov r0, (r1)"
    | 0o010261 ->
        show oldpc (sprintf "mov r2, %x(r1)" (fetch()))
    | 0o012767 ->
        // let opr1, opr2 = fetch(), fetch() このような書き方だと環境依存で実行順序が保証されない
        let opr1 = fetch()
        let opr2 = fetch() // (pc + 6 + read16 mem (pc + 4)) fetchが全部済んだ段階のプログラムカウンター
        // 中で値をとったら進む、を繰り返す=fetch(CPU用語)
        show oldpc (sprintf "mov $%x, %04x" opr1 (pc + opr2))
    // switch-caseの高級なやつで、式が書ける(パターンマッチ)
    | w when (w &&& 0o177770) = 0o012700 ->
        let opr1 = fetch()
        let t, r = (w >>> 3 &&& 7), w &&& 7
        show oldpc (sprintf "mov $%x, %s" opr1 (getopr t r))
    | 0o112761 ->
        let opr1 = fetch()
        let opr2 = fetch()
        show oldpc (sprintf "movb $%x, %x(r1)" opr1 opr2)
    | 0o112711 ->
        show oldpc (sprintf "movb $%x, (r1)" (fetch()))
    | 0o110011 ->
        show oldpc "movb r0, (r1)"
    | 0o110061 ->
        show oldpc (sprintf "movb r0, %x(r1)" (fetch()))
    | 0o000300 ->
        show oldpc "swab r0"
    | 0o104401 ->
        show oldpc "sys 1 ; exit"
    | 0o104404 ->
        show oldpc "sys 4 ; write"
        // シャドウイング 同じ名前の別の変数を定義する OCaml由来
        let oldpc = pc
        // 関数単体で呼び出して返り値を捨てるとThis expression should have type 'unit', but has type 'int'.って怒られる
        // パイプライン演算子|>を利用してignoreする
        // e.g. abc(2) => 2 |> abc
        // ignore(fetch())でも可
        // find . -type f | xargs grep 'hogehoge' っていう感じの着想
        fetch() |> ignore
        show oldpc "; arg"
        let oldpc = pc
        fetch() |> ignore
        show oldpc "; arg"
    | 0o112767 ->
        // 中で値をとったら進む、を繰り返す=fetch(CPU用語)
        let opr1 = fetch()
        let opr2 = fetch()
        show oldpc (sprintf "movb $%x, %04x" opr1 opr2)
    | 0o162767 ->
        // 中で値をとったら進む、を繰り返す=fetch(CPU用語)
        let opr1 = fetch()
        let opr2 = fetch()
        show oldpc (sprintf "sub $%x, %04x" opr1 opr2)
    | _ ->
        show oldpc "???"
