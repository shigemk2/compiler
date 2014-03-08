let args = System.Environment.GetCommandLineArgs()

let aout = System.IO.File.ReadAllBytes args.[2]
let read16 (a:byte[]) b =
    (int a.[b]) ||| ((int a.[b+1]) <<< 8)
let write16 (a:byte[]) b c =
    a.[b] <- byte c
    a.[b + 1] <- byte (c >>> 8)
let tsize = read16 aout 2
let dsize = read16 aout 4
let mem = aout.[16 .. 16 + tsize + dsize - 1]

let mutable r0, r1, r2, pc = 0, 0, 0, 0

let fetch() =
    let ret = read16 mem pc
    pc <- pc + 2
    ret

while pc < tsize do
    match read16 mem pc with
    | 0x1009 ->
        write16 mem r1 r0
        pc <- pc + 2
    | 0x10b1 ->
        write16 mem (r1 + read16 mem (pc + 2)) r2
        pc <- pc + 4
    | 0x15b1 ->
        r0 <- read16 mem (pc + 2)
        pc <- pc + 4
    // mov
    | 0x15c0 ->
        r0 <- read16 mem (pc + 2)
        pc <- pc + 4
    // mov
    | 0x15c1 ->
        r1 <- read16 mem (pc + 2)
        pc <- pc + 4
    | 0x15c2 ->
        r2 <- read16 mem (pc + 2)
        pc <- pc + 4
    // mov
    | 0x15c9 ->
        write16 mem r1 (read16 mem (pc + 2))
        pc <- pc + 4
    | 0x15f1 ->
        write16 mem (r1 + read16 mem (pc + 4)) (read16 mem (pc + 2))
        pc <- pc + 6
    | 0x95c9 ->
        mem.[r1] <- mem.[pc + 2]
        pc <- pc + 4
    | 0x95f1 ->
        mem.[r1 + read16 mem (pc + 4)] <- mem.[pc + 2]
        pc <- pc + 6
    // sys 1 ; exit
    | 0x8901 ->
        exit r0
    // sys 4 ; write
    | 0x8904 ->
        let arg1 = read16 mem (pc + 2)
        let arg2 = read16 mem (pc + 4)
        let bytes = mem.[arg1 .. arg1 + arg2 - 1]
        printf "%s" (System.Text.Encoding.ASCII.GetString bytes)
        pc <- pc + 6
    | 0x00c0 ->
        r0 <- ((r0 &&& 0xff) <<< 8) ||| ((r0 &&& 0xff00) >>> 8)
        pc <- pc + 2
    | 0x9009 ->
        mem.[r1] <- byte r0
        pc <- pc + 2
    | 0x9031 ->
        mem.[r1 + read16 mem (pc + 2)] <- byte r0
        pc <- pc + 4
    | 0x15f7 ->
        write16 mem (pc + 6 + read16 mem (pc + 4)) (read16 mem (pc + 2))
        pc <- pc + 6
    | 0x95f7 ->
        mem.[r1 + pc + 6 + read16 mem (pc + 4)] <- mem.[pc + 2]
        pc <- pc + 6
    | 0xe5f7 ->
        let addr = pc + 6 + read16 mem (pc + 4)
        write16 mem addr ((read16 mem addr) - (read16 mem (pc + 2)))
        pc <- pc + 6
    | w ->
        printfn "%04x: %04x ???" pc w
        exit 1
