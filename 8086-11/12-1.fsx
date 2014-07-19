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

    let modrm () =
        let mode = (int mem.[ip + 1]) >>> 6
        let rm = (int mem.[ip + 1]) &&& 7
        match mode, rm with
        | 0b00, 0b111 -> "[bx]", 0
        | 0b01, 0b111 -> sprintf "[bx+%x]" mem.[ip + 2], 1
        | 0b11, 0b111 -> sprintf "[bx+%x]" mem.[ip + 2], 1
        | _, _ -> "??", 0

    let movreg16 x y =
        let rn = x &&& 7
        show 3 (sprintf "mov %s, %04x" reg16.[rn] (read16 mem (ip + 1)))

    let movreg8 x y =
        let rn = x &&& 7
        let opr, len = modrm()
        show (2+len) (sprintf "mov %s, %s" opr reg8.[rn])

    let movimrmw x y =
        let rn = x &&& 7
        match y with
        | 0b00000111 -> show 4 (sprintf "mov %s, %04x" reg16.[3] (read16 mem (ip + 2)))
        | 0b01000111 -> show 5 (sprintf "mov [%s+%x], %04x" reg16.[3] mem.[ip + 2] (read16 mem (ip + 3)))
        | 0b00000110 -> show 6 (sprintf "mov [%04x], %04x" (read16 mem (ip + 2)) (read16 mem (ip + 4)))
        | _ -> show 1 "; ???"

    let movimrm x y =
        let rn = x &&& 7
        match y with
        | 0b00000111 -> show 3 (sprintf "mov byte [%s], %02x" reg16.[3] mem.[ip + 2])
        | 0b01000111 -> show 4 (sprintf "mov byte [%s+%x], %02x" reg16.[3] mem.[ip + 2] mem.[ip + 3])
        | 0b00000110 -> show 5 (sprintf "mov byte [%04x], %02x" (read16 mem (ip + 2)) mem.[ip + 4])
        | _ -> show 1 "; ???"

    let add x y =
        let rn = x &&& 7
        match y with
        | 0b00000111 -> show 2 (sprintf "add [%s], %s" reg16.[3] reg16.[0])
        | 0b00000110 -> show 4 (sprintf "add [%04x], %s" (read16 mem (ip + 2)) reg16.[0])
        | _ -> show 1 "; ???"

    let running = ref true

    while !running && ip < tsize do
        match int mem.[ip], int mem.[ip + 1] with
        | 0x80, 0x2e -> show 5 (sprintf "sub byte[%04x], %02x" (read16 mem (ip + 2)) mem.[ip + 4])
        | 0x81, 0x2e -> show 6 (sprintf "sub [%04x], %04x" (read16 mem (ip + 2)) (read16 mem (ip + 4)))
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
        | 0xa3, 0x2c -> show 3 (sprintf "mov [%04x], %s" (read16 mem (ip + 1)) reg16.[0])
        | 0xcd, 0x07 ->
            show 2 "int 7"
            match int mem.[ip] with
            | 1 ->
                show 1 "; exit"
            | 4 ->
                show 1 "; write"
                show 2 "; arg"
                show 2 "; arg"
            | _ ->
                show 1 "; ???"
                running := false
        | 0xcd, n ->
            show 2 (sprintf "int %x" n)
        | (x, y) when x &&& 0b10111000 = 0b10111000 -> movreg16 x y
        | (x, y) when x &&& 0b10110000 = 0b10110000 -> movreg8 x y
        | (x, y) when x &&& 0b10001000 = 0b10001000 -> movreg8 x y
        | (x, y) when x &&& 0b11000111 = 0b11000110 -> movimrm x y
        | (x, y) when x &&& 0b11000111 = 0b11000111 -> movimrmw x y
        | (x, y) when x &&& 0b00000001 = 0b00000001 -> add x y
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
    printfn "../8086-11/add.out"
    main "../8086-11/add.out"
    printfn "-------------------"

