module hoge
let mutable pc = 0
let r = [| 0; 0; 0; |]

let main file =
    let aout = System.IO.File.ReadAllBytes file

    let read16 (a:byte[]) b =
        (int a.[b]) ||| ((int a.[b+1]) <<< 8)
    let write16 (a:byte[]) b c =
        a.[b] <- byte c
        a.[b + 1] <- byte (c >>> 8)
    let tsize = read16 aout 2
    let dsize = read16 aout 4
    let mem = aout.[16 .. 16 + tsize + dsize - 1]


    let fetch() =
        let ret = read16 mem pc
        pc <- pc + 2
        ret

    let mutable running = true

    while running && pc < tsize do
        match fetch() with
        | 0o010011 ->
            write16 mem r.[1] r.[0]
        | 0o010261 ->
            write16 mem (r.[1] + fetch()) r.[2]
        | 0o012661 ->
            r.[0] <- read16 mem (pc + 2)
            pc <- pc + 4
        // mov
        | 0o012700 ->
            r.[0] <- fetch()
        // mov
        | 0o012701 ->
            r.[1] <- fetch()
        | 0o012702 ->
            r.[2] <- fetch()
        // mov
        | 0o012711 ->
            write16 mem r.[1] (fetch())
        | 0o012761 ->
            let w1 = fetch()
            let w2 = fetch()
            write16 mem (r.[1] + w2) w1
        | 0o112711 ->
            mem.[r.[1]] <- byte (fetch())
        | 0o112761 ->
            let w1 = fetch()
            let w2 = fetch()
            mem.[r.[1] + w2] <- byte w1
        // sys 1 ; exit
        | 0o104401 ->
            // exit r.[0]
            running <- false
        // sys 4 ; write
        | 0o104404 ->
            let arg1 = fetch()
            let arg2 = fetch()
            let bytes = mem.[arg1 .. arg1 + arg2 - 1]
            printf "%s" (System.Text.Encoding.ASCII.GetString bytes)
        | 0o000300 ->
            r.[0] <- ((r.[0] &&& 0xff) <<< 8) ||| ((r.[0] &&& 0xff00) >>> 8)
        | 0o110011 ->
            mem.[r.[1]] <- byte r.[0]
        | 0o110061 ->
            mem.[r.[1] + fetch()] <- byte r.[0]
        | 0o012767 ->
            let w1 = fetch()
            let w2 = fetch()
            write16 mem (pc + w2) w1
        | 0o112767 ->
            let w1 = fetch()
            let w2 = fetch()
            mem.[r.[1] + w2] <- byte w1
        | 0o162767 ->
            let w1 = fetch()
            let w2 = fetch()
            let addr = pc + w2
            write16 mem addr ((read16 mem addr) - (w1))
        | w ->
            printfn "%04x: %04x ???" pc w
            running <- false
