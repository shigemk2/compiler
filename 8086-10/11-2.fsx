module hoge
let mutable ax, bx, cx, dx, sp, bp, si, di, ip = 0, 0, 0, 0, 0, 0, 0, 0, 0

let main file =
    ip <- 0
    let aout = System.IO.File.ReadAllBytes file
    let read16 (a:byte[]) b =
        (int a.[b]) ||| ((int a.[b + 1]) <<< 8)
    let write16 (a:byte[]) b c =
        a.[b] <- byte c
        a.[b + 1] <- byte (c >>> 8)

    let tsize = read16 aout 2
    let dsize = read16 aout 4
    let mem = Array.zeroCreate<byte> 0x10000
    mem.[0 .. tsize + dsize - 1] <- aout.[16 .. 16 + tsize + dsize - 1]

    let running = ref true

    while !running && ip < tsize do
        match int mem.[ip], int mem.[ip + 1] with
        | 0xb8, _ ->
            ax <- read16 mem (ip + 1)
            ip <- ip + 3
        | 0xbb, _ ->
            bx <- read16 mem (ip + 1)
            ip <- ip + 3
        | 0xc7, 0x07 ->
            write16 mem bx (read16 mem (ip + 2))
            ip <- ip + 4
        | 0xc7, 0x47 ->
            write16 mem (bx + (int mem.[ip + 2])) (read16 mem (ip + 3))
            ip <- ip + 5
        | 0xc6, 0x07 ->
            mem.[bx] <- mem.[ip + 2]
            ip <- ip + 3
        | 0xc6, 0x47 ->
            mem.[bx + (int mem.[ip + 2])] <- mem.[ip + 3]
            ip <- ip + 4
        | 0x89, 0x07 ->
            write16 mem bx ax
            ip <- ip + 2
        | 0x89, 0x4f ->
            write16 mem (bx + (int mem.[ip + 2])) cx
            ip <- ip + 3
        | 0xb9, _ ->
            cx <- read16 mem (ip + 1)
            ip <- ip + 3
        | 0x88, 0x07 ->
            mem.[bx] <- byte ax
            ip <- ip + 2
        | 0x88, 0x67 ->
            mem.[bx + int mem.[ip + 2]] <- byte (ax >>> 8)
            ip <- ip + 3
        | 0xb5, _ ->
            cx <- ((int mem.[ip + 1]) <<< 8) ||| (cx &&& 0xff)
            mem.[bx] <- byte ax
            ip <- ip + 2
        | 0xb1, _ ->
            cx <- (cx &&& 0xff00) ||| (int mem.[ip + 1])
            ip <- ip + 2
        | 0x89, 0x0f ->
            write16 mem bx cx
            ip <- ip + 2
        | 0xc7, 0x06 ->
            write16 mem (read16 mem (ip + 2)) (read16 mem (ip + 4))
            ip <- ip + 6
        | 0xc6, 0x06 ->
            mem.[read16 mem (ip + 2)] <- mem.[ip + 4]
            ip <- ip + 5
        | 0x81, 0x2e ->
            let addr = bx + read16 mem (ip + 2)
            write16 mem addr ((read16 mem addr) - (read16 mem (ip + 4)))
            ip <- ip + 6
        | 0x80, 0x2e ->
            let addr = read16 mem (ip + 2)
            mem.[bx + addr] <- mem.[addr] - mem.[ip + 4]
            ip <- ip + 5
        | 0xba, _ ->
            dx <- read16 mem (ip + 1)
            ip <- ip + 3
        | 0xbc, _ ->
            sp <- read16 mem (ip + 1)
            ip <- ip + 3
        | 0xbd, _ ->
            bp <- read16 mem (ip + 1)
            ip <- ip + 3
        | 0xbe, _ ->
            si <- read16 mem (ip + 1)
            ip <- ip + 3
        | 0xbf, _ ->
            di <- read16 mem (ip + 1)
            ip <- ip + 3
        | 0xcd, 0x07 ->
            match int mem.[ip + 2] with
            | 1 ->
                // exit ax
                running := false
            | 4 ->
                let arg1 = read16 mem (ip + 3)
                let arg2 = read16 mem (ip + 5)
                let bytes = mem.[arg1 .. arg1 + arg2 - 1]
                printf "%s" (System.Text.Encoding.ASCII.GetString bytes)
                ip <- ip + 7
            | sc ->
                printfn "%04x: ??? syscall %d" ip sc
                // exit 1
                running := false
        | 0xcd, n ->
            printfn "%04x: ??? int %x" ip n
            // exit 1
            running := false
        | b, _ ->
            printfn "%04x: %02x ???" ip b
            // exit 1
            running := false

let test() =
    printfn "-------------------"
    printfn "../8086-2/write-1.out"
    main "../8086-2/write-1.out"
    printfn "-------------------"
    printfn "../8086-3/write-2.out"
    main "../8086-3/write-2.out"
    printfn "-------------------"
    printfn "../8086-4/write-3.out"
    main "../8086-4/write-3.out"
    printfn "-------------------"
    printfn "../8086-5/write-4.out"
    main "../8086-5/write-4.out"
    printfn "-------------------"
    printfn "../8086-6/write-5.out"
    main "../8086-6/write-5.out"
    printfn "-------------------"
    printfn "../8086-7/write-6.out"
    main "../8086-7/write-6.out"
    printfn "-------------------"
    printfn "../8086-8/write-7.out"
    main "../8086-8/write-7.out"
    printfn "-------------------"
    printfn "../8086-9/regs.out"
    main "../8086-9/regs.out"
    printfn "-------------------"
    printfn "../8086-10/write-8.out"
    main "../8086-10/write-8.out"
    printfn "-------------------"
    printfn "../8086-10/regs.out"
    main "../8086-10/regs.out"
    printfn "-------------------"
    printfn "../8086-10/regs2.out"
    main "../8086-10/regs2.out"
    printfn "-------------------"
