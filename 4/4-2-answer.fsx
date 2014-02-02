let aout = System.IO.File.ReadAllBytes "write-2.out"
let read16 (a:byte[]) b =
  (int a.[b]) ||| ((int a.[b+1]) <<< 8)
let write16 (a:byte[]) b c =
  a.[b] <- byte c
  a.[b + 1] <- byte (c >>> 8)
let tsize = read16 aout 2
let dsize = read16 aout 4
let mem = aout.[16 .. 16 + tsize + dsize - 1]
let mutable r0, r1, pc = 0, 0, 0
while pc < tsize do
  match read16 mem pc with
  | 0x15c0 ->
    r0 <- read16 mem (pc + 2)
    pc <- pc + 4
  | 0x15c1 ->
    r1 <- read16 mem (pc + 2)
    pc <- pc + 4
  | 0x15c9 ->
    write16 mem r1 (read16 mem (pc + 2))
    pc <- pc + 4
  | 0x15f1 ->
    write16 mem (r1 + read16 mem (pc + 4)) (read16 mem (pc + 2))
    pc <- pc + 6
  | 0x8901 ->
    exit r0
  | 0x8904 ->
    let arg1 = read16 mem (pc + 2)
    let arg2 = read16 mem (pc + 4)
    let bytes = mem.[arg1 .. arg1 + arg2 - 1]
    printf "%s" (System.Text.Encoding.ASCII.GetString bytes)
    pc <- pc + 6
  | w ->
    printfn "%04x: %04x ???" pc w
    exit 1
