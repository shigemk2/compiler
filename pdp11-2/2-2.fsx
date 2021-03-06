let aout = System.IO.File.ReadAllBytes "write-1.out"
let read16 (a:byte[]) b =
    (int a.[b]) ||| ((int a.[b+1]) <<< 8)
let tsize = read16 aout 2
let dsize = read16 aout 4
let mem = aout.[16 .. 16 + tsize + dsize - 1]
let mutable r0, pc = 0, 0
while pc < tsize do
    match read16 mem pc with
    | 0x15c0 ->
        r0 <- read16 mem (pc + 2)
        pc <- pc + 4
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
