module hoge
let mutable ip = 0

let main file =
    ip <- 0

    // reg16 "ax"; "cx"; "dx"; "bx"; "sp"; "bp"; "si"; "di"
    let reg16 = [| 0; 0; 0; 0; 0; 0; 0; 0 |]

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
            reg16.[0] <- read16 mem (ip + 1)
            ip <- ip + 3
        | 0xbb, _ ->
            reg16.[3] <- read16 mem (ip + 1)
            ip <- ip + 3
        | 0xc7, 0x07 ->
            write16 mem reg16.[3] (read16 mem (ip + 2))
            ip <- ip + 4
        | 0xc7, 0x47 ->
            write16 mem (reg16.[3] + (int mem.[ip + 2])) (read16 mem (ip + 3))
            ip <- ip + 5
        | 0xc6, 0x07 ->
            mem.[reg16.[3]] <- mem.[ip + 2]
            ip <- ip + 3
        | 0xc6, 0x47 ->
            mem.[reg16.[3] + (int mem.[ip + 2])] <- mem.[ip + 3]
            ip <- ip + 4
        | 0x89, 0x07 ->
            write16 mem reg16.[3] reg16.[0]
            ip <- ip + 2
        | 0x89, 0x4f ->
            write16 mem (reg16.[3] + (int mem.[ip + 2])) reg16.[1]
            ip <- ip + 3
        | 0xb9, _ ->
            reg16.[1] <- read16 mem (ip + 1)
            ip <- ip + 3
        | 0x88, 0x07 ->
            mem.[reg16.[3]] <- byte reg16.[0]
            ip <- ip + 2
        | 0x88, 0x67 ->
            mem.[reg16.[3] + int mem.[ip + 2]] <- byte (reg16.[0] >>> 8)
            ip <- ip + 3
        | 0xb5, _ ->
            reg16.[1] <- ((int mem.[ip + 1]) <<< 8) ||| (reg16.[1] &&& 0xff)
            mem.[reg16.[3]] <- byte reg16.[0]
            ip <- ip + 2
        | 0xb1, _ ->
            reg16.[1] <- (reg16.[1] &&& 0xff00) ||| (int mem.[ip + 1])
            ip <- ip + 2
        | 0x89, 0x0f ->
            write16 mem reg16.[3] reg16.[1]
            ip <- ip + 2
        | 0xc7, 0x06 ->
            write16 mem (read16 mem (ip + 2)) (read16 mem (ip + 4))
            ip <- ip + 6
        | 0xc6, 0x06 ->
            mem.[read16 mem (ip + 2)] <- mem.[ip + 4]
            ip <- ip + 5
        | 0x81, 0x2e ->
            let addr = reg16.[3] + read16 mem (ip + 2)
            write16 mem addr ((read16 mem addr) - (read16 mem (ip + 4)))
            ip <- ip + 6
        | 0x80, 0x2e ->
            let addr = read16 mem (ip + 2)
            mem.[reg16.[3] + addr] <- mem.[addr] - mem.[ip + 4]
            ip <- ip + 5
        | 0xba, _ ->
            reg16.[2] <- read16 mem (ip + 1)
            ip <- ip + 3
        | 0xbc, _ ->
            reg16.[4] <- read16 mem (ip + 1)
            ip <- ip + 3
        | 0xbd, _ ->
            reg16.[5] <- read16 mem (ip + 1)
            ip <- ip + 3
        | 0xbe, _ ->
            reg16.[6] <- read16 mem (ip + 1)
            ip <- ip + 3
        | 0xbf, _ ->
            reg16.[7] <- read16 mem (ip + 1)
            ip <- ip + 3
        | 0xcd, 0x07 ->
            match int mem.[ip + 2] with
            | 1 ->
                // exit reg16.[0]
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
