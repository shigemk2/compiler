module hoge
// プログラムカウンター=r7
let r = [| 0; 0; 0; 0; 0; 0; 0; 0 |]

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
        let ret = read16 mem r.[7]
        r.[7] <- r.[7] + 2
        ret

    let mutable running = true

    // dd書き込み w order v fetch
    let mov27 w v =
        let t = ((w >>> 3) &&& 7)
        let rn = w &&& 7
        match t with
        | 0 ->
            r.[rn] <- v
        | 1 ->
            write16 mem r.[rn] v
        // | 2 -> sprintf "(r%d)+" r
        | 6 ->
            let w1 = fetch()
            write16 mem (r.[rn] + w1) v
        | _ -> printfn "??"

    // dd書き込み w order v fetch
    let movb27 w v =
        let t = ((w >>> 3) &&& 7)
        let rn = w &&& 7
        match t with
        | 1 ->
            mem.[r.[rn]] <- byte (v)
        // | 2 -> sprintf "(r%d)+" r
        | 6 ->
            let w1 = fetch()
            mem.[r.[rn] + w1] <- byte v
        | _ -> printfn "??"

    // dd書き込み w order
    let movb00 w =
        let t = ((w >>> 3) &&& 7)
        let rn = w &&& 7
        match t with
        | 1 ->
            mem.[r.[1]] <- byte r.[0]
        // | 2 -> sprintf "(r%d)+" r
        | 6 ->
            mem.[r.[1] + fetch()] <- byte r.[0]
        | _ -> printfn "??"

    while running && r.[7] < tsize do
        match fetch() with
        | 0o010011 ->
            write16 mem r.[1] r.[0]
        | 0o010261 ->
            write16 mem (r.[1] + fetch()) r.[2]
        | 0o012661 ->
            r.[0] <- read16 mem (fetch())
        | w when (w >>> 6 = 0o0127) ->
            // mutableを付けない変数は中身が変わらないので、ビットシフト演算しても中身は変わらない
            mov27 w (fetch())
        | w when (w >>> 6 = 0o1127) ->
            movb27 w (fetch())
        | w when (w >>> 6 = 0o1100) ->
            movb00 w
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
        | 0o012767 ->
            let w1 = fetch()
            let w2 = fetch()
            write16 mem (r.[7] + w2) w1
        | 0o112767 ->
            let w1 = fetch()
            let w2 = fetch()
            mem.[r.[1] + w2] <- byte w1
        | 0o162767 ->
            let w1 = fetch()
            let w2 = fetch()
            let addr = r.[7] + w2
            write16 mem addr ((read16 mem addr) - (w1))
        | w ->
            printfn "%04x: %04x ???" r.[7] w
            running <- false
