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
    let mov w =
        let t1  = ((w >>> 9) &&& 7)
        let rn1 = ((w >>> 6) &&& 7)
        let t2  = ((w >>> 3) &&& 7)
        let rn2 = w &&& 7

        match t1 with
        | 0 ->
            match t2 with
            | 1 ->
                write16 mem r.[rn2] r.[rn1]
            | 2 ->
                let v   = fetch()
                write16 mem (r.[rn2] + v) r.[rn1]
            | 6 ->
                let v   = fetch()
                write16 mem (r.[rn2] + v) r.[rn1]
            | _ -> printfn "??"
        | 2 ->
            match t2 with
            | 6 ->
                let v   = fetch()
                r.[rn2] <- read16 mem v
            | _ -> printfn "??"
        | _ -> printfn "??"

    // dd書き込み w order v fetch
    let mov27 w v =
        let t = ((w >>> 3) &&& 7)
        let rn = w &&& 7
        if rn = 7 then
            match t with
            | 6 ->
                let w1 = fetch()
                write16 mem (r.[7] + w1) v
            | _ -> printfn "??"
        else
            match t with
            | 0 ->
                r.[rn] <- v
            | 1 ->
                write16 mem r.[rn] v
            | 6 ->
                let w1 = fetch()
                write16 mem (r.[rn] + w1) v
            | _ -> printfn "??"

    // dd書き込み w order v fetch
    let movb27 w v =
        let t = ((w >>> 3) &&& 7)
        let rn = w &&& 7
        if rn = 7 then
            match t with
            | 6 ->
                let w1 = fetch()
                mem.[r.[1] + w1] <- byte v
            | _ -> printfn "??"
        else
            match t with
            | 1 ->
                mem.[r.[rn]] <- byte (v)
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

    let swab w =
        let t = ((w >>> 3) &&& 7)
        let rn = w &&& 7
        match t with
        | 0 ->
             r.[rn] <- ((r.[rn] &&& 0xff) <<< 8) ||| ((r.[rn] &&& 0xff00) >>> 8)
        | _ -> printfn "??"

    let sub27 w =
        let t = ((w >>> 3) &&& 7)
        let rn = w &&& 7
        if rn = 7 then
            match t with
            | 6 ->
                 let w1 = fetch()
                 let w2 = fetch()
                 let addr = r.[7] + w2
                 write16 mem addr ((read16 mem addr) - (w1))
            | _ -> printfn "??"
        else
            match t with
            | _ -> printfn "??"

    while running && r.[7] < tsize do
        match fetch() with
        // mutableを付けない変数は中身が変わらないので、ビットシフト演算しても中身は変わらない
        | w when (w >>> 6 = 0o0127) -> mov27 w (fetch())
        | w when (w >>> 6 = 0o1127) -> movb27 w (fetch())
        | w when (w >>> 6 = 0o1100) -> movb00 w
        | w when (w >>> 6 = 0o1627) -> sub27 w
        | w when (w >>> 12 = 0o01) -> mov w
        | w when (w >>> 12 = 0o00) -> swab w
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
        | w ->
            printfn "%04x: %04x ???" r.[7] w
            running <- false
