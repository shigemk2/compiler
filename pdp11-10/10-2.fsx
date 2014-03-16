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
    match fetch() with
    | 0o010011 ->
        write16 mem r1 r0
        pc <- pc + 2
    | 0o010261 ->
        write16 mem (r1 + read16 mem (pc + 2)) r2
        pc <- pc + 4
    | 0o012661 ->
        r0 <- read16 mem (pc + 2)
        pc <- pc + 4
    // mov
    | 0o012700 ->
        r0 <- fetch()
    // mov
    | 0o012701 ->
        r1 <- fetch()
    | 0o012702 ->
        r2 <- read16 mem (pc + 2)
        pc <- pc + 4
    // mov
    | 0o012711 ->
        write16 mem r1 (fetch())
    | 0o012761 ->
        write16 mem (r1 + read16 mem (pc + 4)) (read16 mem (pc + 2))
        pc <- pc + 6
    | 0o112711 ->
        mem.[r1] <- mem.[pc + 2]
        pc <- pc + 4
    | 0o112761 ->
        mem.[r1 + read16 mem (pc + 4)] <- mem.[pc + 2]
        pc <- pc + 6
    // sys 1 ; exit
    | 0o104401 ->
        exit r0
    // sys 4 ; write
    | 0o104404 ->
        let arg1 = fetch()
        let arg2 = fetch()
        let bytes = mem.[arg1 .. arg1 + arg2 - 1]
        printf "%s" (System.Text.Encoding.ASCII.GetString bytes)
    | 0o003000 ->
        r0 <- ((r0 &&& 0xff) <<< 8) ||| ((r0 &&& 0xff00) >>> 8)
        pc <- pc + 2
    | 0o110011 ->
        mem.[r1] <- byte r0
        pc <- pc + 2
    | 0o110061 ->
        mem.[r1 + read16 mem (pc + 2)] <- byte r0
        pc <- pc + 4
    | 0o012767 ->
        write16 mem (pc + 6 + read16 mem (pc + 4)) (read16 mem (pc + 2))
        pc <- pc + 6
    | 0o112767 ->
        mem.[r1 + pc + 6 + read16 mem (pc + 4)] <- mem.[pc + 2]
        pc <- pc + 6
    | 0o162767 ->
        let addr = pc + 6 + read16 mem (pc + 4)
        write16 mem addr ((read16 mem addr) - (read16 mem (pc + 2)))
        pc <- pc + 6
    | w ->
        printfn "%04x: %04x ???" pc w
        exit 1
