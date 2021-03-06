module hoge
let mutable ip = 0

let main file =
    ip <- 0
    let aout = System.IO.File.ReadAllBytes file
    let read16 (a:byte[]) b =
        (int a.[b]) ||| ((int a.[b + 1]) <<< 8)
    let tsize = read16 aout 2
    let dsize = read16 aout 4
    let mem = Array.zeroCreate<byte> 0x10000
    mem.[0 .. tsize + dsize - 1] <- aout.[16 .. 16 + tsize + dsize - 1]
    let show len dis =
        let bin = [ for b in mem.[ip .. ip + len - 1] -> sprintf "%02x" b ]
        printfn "%04x: %-12s  %s" ip (String.concat "" bin) dis
        ip <- ip + len

    // レジスタのサイズが16ビットor8ビット
    let reg16 = [|"ax"; "cx"; "dx"; "bx"; "sp"; "bp"; "si"; "di"|]
    let regad = [|"??"; "??"; "??"; "??"; "si"; "di"; "bp"; "bx"|]
    let reg8  = [|"al"; "cl"; "dl"; "bl"; "ah"; "ch"; "dh"; "bh"|]

    let movreg16 x y =
        let rn = x &&& 7
        show 3 (sprintf "mov %s, %04x" reg16.[rn] (read16 mem (ip + 1)))
    let movreg8 x y =
        let rn = x &&& 7
        show 2 (sprintf "mov %s, %01x" reg8.[rn] mem.[ip + 1])

    let running = ref true

    while !running && ip < tsize do
        match int mem.[ip], int mem.[ip + 1] with
        | (x, y) when x &&& 0b10111000 = 0b10111000 -> movreg16 x y
        | (x, y) when x &&& 0b10110000 = 0b10110000 -> movreg8 x y
        | 0xc7, 0x07 -> show 4 (sprintf "mov %s, %04x" reg16.[3] (read16 mem (ip + 2)))
        | 0xc7, 0x47 -> show 5 (sprintf "mov [%s+%x], %04x" reg16.[3] mem.[ip + 2] (read16 mem (ip + 3)))
        | 0xc7, 0x06 -> show 6 (sprintf "mov [%04x], %04x" (read16 mem (ip + 2)) (read16 mem (ip + 4)))
        | 0xc6, 0x07 -> show 3 (sprintf "mov byte [%s], %02x" reg16.[3] mem.[ip + 2])
        | 0xc6, 0x47 -> show 4 (sprintf "mov byte [%s+%x], %02x" reg16.[3] mem.[ip + 2] mem.[ip + 3])
        | 0xc6, 0x06 -> show 5 (sprintf "mov byte [%04x], %02x" (read16 mem (ip + 2)) mem.[ip + 4])
        | 0x89, op when op &&& 0b11000000 = 0b01000000 ->
            let rn1 = op &&& 7
            let rn2 = (op >>> 3) &&& 7
            if mem.[ip + 2] = byte 0 then
                show 3 (sprintf "mov [%s], %s" regad.[rn1] reg16.[rn2])
            else
                show 3 (sprintf "mov [%s+%x], %s" regad.[rn1] mem.[ip + 2] reg16.[rn2])
        | 0x89, op when op &&& 0b11000000 = 0b00000000 ->
            let rn1 = op &&& 7
            let rn2 = op >>> 3
            show 2 (sprintf "mov [%s], %s" regad.[rn1] reg16.[rn2])
        | 0x88, op when op &&& 0b11000111 = 0b00000111 ->
            let rn = (op >>> 3) &&& 7
            show 2 (sprintf "mov [bx], %s" reg8.[rn])
        | 0x88, 0x67 -> show 3 (sprintf "mov [%s+%x], ah" reg16.[3] mem.[ip + 2])
        | 0x81, 0x2e -> show 6 (sprintf "sub [%04x], %04x" (read16 mem (ip + 2)) (read16 mem (ip + 4)))
        | 0x80, 0x2e -> show 5 (sprintf "sub byte[%04x], %02x" (read16 mem (ip + 2)) mem.[ip + 4])
        | 0xcd, 0x07 ->
            show 2 "int 7"
            match int mem.[ip] with
            | 1 ->
                show 1 "; exit"
                running := false
            | 4 ->
                show 1 "; write"
                show 2 "; arg"
                show 2 "; arg"
            | _ ->
                show 1 "; ???"
                running := false
        | 0xcd, n ->
            show 2 (sprintf "int %x" n)
        | _ ->
            show 1 "???"

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

