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
  | 0x8901 ->
    show 2 "sys 1 ; exit"
  | 0x8904 ->
    show 2 "sys 4 ; write"
    show 2 "; arg"
    show 2 "; arg"
  | _ ->
    show 2 "???"
