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

let mutable pc = 0
let r = [| 0; 0; 0; |];;

let fetch() =
    let ret = read16 mem pc
    pc <- pc + 2
    ret

while pc < tsize do
    match read16 mem pc with
    | 0o010011 ->
        write16 mem r.[1] r.[0]
        pc <- pc + 2
    | 0o010261 ->
        write16 mem (r.[1] + read16 mem (pc + 2)) r.[2]
        pc <- pc + 4
    | 0o012661 ->
        r.[0] <- read16 mem (pc + 2)
        pc <- pc + 4
    // mov
    | w when (w >>> 3 = 0o01270) ->
        r.[(w &&& 7)] <- read16 mem (pc + 2)
        pc <- pc + 4
    | w when (w >>> 3 = 0o01271) ->
        write16 mem r.[(w &&& 7)] (read16 mem (pc + 2))
        pc <- pc + 4
    | w when (w >>> 3 = 0o01276) ->
        write16 mem (r.[(w &&& 7)] + read16 mem (pc + 4)) (read16 mem (pc + 2))
        pc <- pc + 6
    // movb
    | w when (w >>> 3 = 0o011271) ->
        mem.[r.[(w &&& 7)]] <- mem.[pc + 2]
        pc <- pc + 4
    | w when (w >>> 3 = 0o011276) ->
        mem.[r.[(w &&& 7)] + read16 mem (pc + 4)] <- mem.[pc + 2]
        pc <- pc + 6
    // sys 1 ; exit
    | 0o104401 ->
        exit r.[0]
    // sys 4 ; write
    | 0o104404 ->
        let arg1 = read16 mem (pc + 2)
        let arg2 = read16 mem (pc + 4)
        let bytes = mem.[arg1 .. arg1 + arg2 - 1]
        printf "%s" (System.Text.Encoding.ASCII.GetString bytes)
        pc <- pc + 6
    | 0o003000 ->
        r.[0] <- ((r.[0] &&& 0xff) <<< 8) ||| ((r.[0] &&& 0xff00) >>> 8)
        pc <- pc + 2
    | 0o110011 ->
        mem.[r.[1]] <- byte r.[0]
        pc <- pc + 2
    | 0o110061 ->
        mem.[r.[1] + read16 mem (pc + 2)] <- byte r.[0]
        pc <- pc + 4
    | 0o012767 ->
        write16 mem (pc + 6 + read16 mem (pc + 4)) (read16 mem (pc + 2))
        pc <- pc + 6
    | 0o112767 ->
        mem.[r.[1] + pc + 6 + read16 mem (pc + 4)] <- mem.[pc + 2]
        pc <- pc + 6
    | 0o162767 ->
        let addr = pc + 6 + read16 mem (pc + 4)
        write16 mem addr ((read16 mem addr) - (read16 mem (pc + 2)))
        pc <- pc + 6
    | w ->
        printfn "%04x: %04x ???" pc w
        exit 1
