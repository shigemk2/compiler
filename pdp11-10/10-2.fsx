module hoge

let main file =
    // プログラムカウンター=r7
    let r = [| 0; 0; 0; 0; 0; 0; 0; 0 |]

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

    // let mutable running = true
    // mutableと同じことができる。
    // mutableと違い、mutableと同じ場所で定義された関数の中で参照することができる
    let running = ref true

    let readopr t rn savepc =
        match t, rn with
        | 0, _ ->
            r.[rn]
        | 1, _ ->
            read16 mem r.[rn]
        | 6, _ ->
            let oldpc = r.[7]
            let v = fetch()
            let addr = r.[rn] + v
            if savepc then r.[7] <- oldpc
            read16 mem addr
        | 2, 7 ->
            fetch()
        | _, _ ->
            0

    let writeoprbase t rn value b =
        if b then
            match t, rn with
            | 0, _ ->
                r.[rn] <- int(byte value)
            | 1, _ ->
                mem.[r.[rn]] <- byte value
            | 6, _ ->
                let v   = fetch()
                mem.[r.[rn] + v] <- byte value
            | _ ->
                printfn "?? %x" r.[7]
                running := false
        else
            match t, rn with
            | 0, _ ->
                r.[rn] <- value
            | 1, _ ->
                write16 mem r.[rn] value
            | 6, _ ->
                let v   = fetch()
                write16 mem (r.[rn] + v) value
            | _ ->
                printfn "?? %x" r.[7]
                // running <- false
                running := false

    let writeopr w value b =
      writeoprbase ((w >>> 3) &&& 7) (w &&& 7) value b

    // dd書き込み w order v fetch
    let mov w =
        let src = readopr ((w >>> 9) &&& 7) ((w >>> 6) &&& 7) false
        writeopr w src false

    // dd書き込み w order v fetch
    let movb w =
        let src = readopr ((w >>> 9) &&& 7) ((w >>> 6) &&& 7) false
        writeopr w src true

    let swab w =
        let src = readopr ((w >>> 3) &&& 7) (w &&& 7) true
        let src = ((src &&& 0xff) <<< 8) ||| ((src &&& 0xff00) >>> 8)
        writeopr w src false

    // dd書き込み w order v fetch
    let sub w =
        let src = readopr ((w >>> 9) &&& 7) ((w >>> 6) &&& 7) false
        let dst = readopr ((w >>> 3) &&& 7)  (w        &&& 7) true
        writeopr w (dst - src) false

    while !running && r.[7] < tsize do
        match fetch() with
        // mutableを付けない変数は中身が変わらないので、ビットシフト演算しても中身は変わらない
        | w when (w >>> 12 = 0o01) -> mov w
        | w when (w >>> 12 = 0o11) -> movb w
        | w when (w >>> 12 = 0o16) -> sub w
        | w when (w >>> 12 = 0o00) -> swab w
        // sys 1 ; exit
        | 0o104401 ->
            // exit r.[0]
            // running <- false
            running := false
        // sys 4 ; write
        | 0o104404 ->
            let arg1 = fetch()
            let arg2 = fetch()
            let bytes = mem.[arg1 .. arg1 + arg2 - 1]
            printf "%s" (System.Text.Encoding.ASCII.GetString bytes)
        | w ->
            printfn "%04x: %04x ???" r.[7] w
            // running <- false
            running := false

let test() =
    printfn "-------------------"
    main "../pdp11-3/write-1.out"
    printfn "-------------------"
    main "../pdp11-4/write-2.out"
    printfn "-------------------"
    main "../pdp11-5/write-3.out"
    printfn "-------------------"
    main "../pdp11-6/write-4.out"
    printfn "-------------------"
    main "../pdp11-7/write-5.out"
    printfn "-------------------"
    main "../pdp11-8/write-6.out"
    printfn "-------------------"
    main "../pdp11-9/write-7.out"
    printfn "-------------------"
    main "../pdp11-10/write-8.out"
    printfn "-------------------"

