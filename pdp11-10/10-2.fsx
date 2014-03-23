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

    let readopr t rn =
        match t, rn with
        | 0, _ ->
            r.[rn]
        | 1, _ ->
            read16 mem r.[rn]
        | 6, _ ->
            let v = fetch()
            read16 mem (r.[rn] + v)
        | 2, 7 ->
            fetch()
        | _, _ ->
            0

    // dd書き込み w order v fetch
    let mov w =
        // t type
        // rn register number
        let src = readopr ((w >>> 9) &&& 7) ((w >>> 6) &&& 7)

        match ((w >>> 3) &&& 7), (w &&& 7) with
        | 0, dr ->
            r.[dr] <- src
        | 1, dr ->
            write16 mem r.[dr] src
        | 6, dr ->
            let v   = fetch()
            write16 mem (r.[dr] + v) src
        | _ ->
            printfn "?? %x" r.[7]
            // running <- false
            running := false

    // dd書き込み w order v fetch
    let movb w =
        let src = readopr ((w >>> 9) &&& 7) ((w >>> 6) &&& 7)

        match ((w >>> 3) &&& 7), (w &&& 7) with
        | 1, dr ->
            mem.[r.[dr]] <- byte src
        | 6, dr ->
            let v   = fetch()
            mem.[r.[dr] + v] <- byte src
        | _ ->
            printfn "??"

    let swab w =
        let t = ((w >>> 3) &&& 7)
        let rn = w &&& 7
        match t with
        | 0 ->
             r.[rn] <- ((r.[rn] &&& 0xff) <<< 8) ||| ((r.[rn] &&& 0xff00) >>> 8)
        | _ -> printfn "??"

    // dd書き込み w order v fetch
    let sub w =
        let src = readopr ((w >>> 9) &&& 7) ((w >>> 6) &&& 7)

        // type of destination
        // register of destination
        match ((w >>> 3) &&& 7), (w &&& 7) with
        | 0, dr ->
            r.[dr] <- r.[dr] - src
        | 6, 7  ->
            let v   = fetch()
            let addr = r.[7] + v
            write16 mem addr ((read16 mem addr) - src)
        | _ -> printfn "?? %o" r.[7]

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

