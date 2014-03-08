let args = System.Environment.GetCommandLineArgs()

let aout = System.IO.File.ReadAllBytes args.[2]
let read16 (a:byte[]) b =
    (int a.[b]) ||| ((int a.[b+1]) <<< 8)
let tsize = read16 aout 2
let dsize = read16 aout 4
let mem = aout.[16 .. 16 + tsize + dsize - 1]
let mutable pc = 0
let show oldpc dis =
    let words = [ for i in oldpc .. 2 .. pc - 1 -> sprintf "%04x" (read16 mem i) ]
    printfn "%04x: %-14s  %s" oldpc (String.concat " " words) dis

// 　　|95c9|←instr
// PC→|0048|←operand n 引数
// 　　|15c0|
// 012731 さらに先のアドレスを読むかどうかを決める

// fetch 中で変数の値を書き換えているので副作用が発生する
let fetch() =
    let ret = read16 mem pc
    pc <- pc + 2
    ret
// operand取得 t type r register
let getopr t r =
    if r = 7 then
        match t with
        | 2 -> sprintf "$%x" (fetch())
        | 6 -> let opr = fetch()
               sprintf "%04x" (pc + opr)
        | _ -> "??"
    else
        match t with
        | 0 -> sprintf "r%d" r
        | 1 -> sprintf "(r%d)" r
        | 2 -> sprintf "(r%d)+" r
        | 6 -> sprintf "%x(r%d)" (fetch()) r
        | _ -> "??"

// mnemonic
// SSDD系のoperationのメッセージを得る
let showssdd oldpc w mne =
    let opr1 = getopr (w >>> 9 &&& 7) (w >>> 6 &&& 7)
    let opr2 = getopr (w >>> 3 &&& 7) (w       &&& 7)
    show oldpc (sprintf "%s %s, %s" mne opr1 opr2)

// DD系のoperationのメッセージを得る
let showdd oldpc w mne =
    let opr1 = getopr (w >>> 9 &&& 7) (w >>> 6 &&& 7)
    show oldpc (sprintf "%s %s" mne opr1)

while pc < tsize do
    let oldpc = pc
    match fetch() with
    // let opr1, opr2 = fetch(), fetch() このような書き方だと環境依存で実行順序が保証されない
    // (pc + 6 + read16 mem (pc + 4)) fetchが全部済んだ段階のプログラムカウンター
    // 中で値をとったら進む、を繰り返す=fetch(CPU用語)
    // // switch-caseの高級なやつで、式が書ける(パターンマッチ)
    | w when (w >>> 6  = 0o0003) -> showdd oldpc w "swab"
    | w when (w >>> 12  = 0o01) -> showssdd oldpc w "mov"
    | w when (w >>> 12  = 0o11) -> showssdd oldpc w "movb"
    | w when (w >>> 12  = 0o16) -> showssdd oldpc w "sub"
    // 16進数 1桁4ビット
    // 8進数  1桁3ビット
    // 16ビット部分をまとめて命令
    // 命令の中身をopcodeとoperand
    // opcode operand
    // [89][01]
    // trap
    // 4444
    // その他
    // 133333
    // 104777 &&& 177400
    | 0o104401 (* 0x8901 *) ->
        show oldpc "sys 1 ; exit"
    | 0o104404 (* 0x8904 *) ->
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
    | _ ->
        show oldpc "???"
