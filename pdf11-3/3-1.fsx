// 逆アセンブラ 7run -d write-1.out
// 0000: 15c0 0001       mov $1, r0
// 0004: 8904            sys 4 ; write
// 0006: 0022            ; arg
// 0008: 0006            ; arg
// 000a: 15c1 0022       mov $22, r1
// 000e: 15c9 4548       mov $4548, (r1)
// 0012: 15c0 0001       mov $1, r0
// 0016: 8904            sys 4 ; write
// 0018: 0022            ; arg
// 001a: 0006            ; arg
// 001c: 15c0 0000       mov $0, r0
// 0020: 8901            sys 1 ; exit

let aout = System.IO.File.ReadAllBytes "write-1.out"
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
  match read16 mem pc with
  | 0x15c0 ->
    let n = read16 mem (pc + 2)
    show 4 (sprintf "mov $%x, r0" n)
  | 0x15c1 ->
    let n = read16 mem (pc + 2)
    show 4 (sprintf "mov $%x, r1" n)
  | 0x15c9 ->
    let n = read16 mem (pc + 2)
    show 4 (sprintf "mov $%x, r1" n)
  | 0x8901 ->
    show 2 "sys 1 ; exit"
  | 0x8904 ->
    show 2 "sys 4 ; write"
    show 2 "; arg"
    show 2 "; arg"
  | _ ->
    show 2 "???"
