module hoge
let mutable ip = 0

let main file =
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

    let op = [|"ax"; "cx"; "dx"; "bx"; "sp"; "bp"; "si"; "di"|]

    let movreg x y =
        let pc = x - 0xb8
        show 3 (sprintf "mov %s, %04x" op.[pc] (read16 mem (ip + 1)))

    let running = ref true

    while ip < tsize do
        match int mem.[ip], int mem.[ip + 1] with
        | (x, y) when ((0 <= (x - 0xb8)) && ((x - 0xb8) <= 7)) -> movreg x y
        | 0xc7, w ->
            match w with
            | 0x07 -> show 4 (sprintf "mov %s, %04x" op.[3] (read16 mem (ip + 2)))
            | 0x47 -> show 5 (sprintf "mov [%s+%x], %04x" op.[3] mem.[ip + 2] (read16 mem (ip + 3)))
            | 0x06 -> show 6 (sprintf "mov [%04x], %04x" (read16 mem (ip + 2)) (read16 mem (ip + 4)))
            | _ ->
                show 4 "??"
                running := false
        | 0xc6, w ->
            match w with
            | 0x07 -> show 3 (sprintf "mov byte [%s], %02x" op.[3] mem.[ip + 2])
            | 0x47 -> show 4 (sprintf "mov byte [%s+%x], %02x" op.[3] mem.[ip + 2] mem.[ip + 3])
            | 0x06 -> show 5 (sprintf "mov byte [%04x], %02x" (read16 mem (ip + 2)) mem.[ip + 4])
            | _ ->
                show 4 "??"
                running := false
        | 0x89, w ->
            match w with
            | 0x07 -> show 2 (sprintf "mov [%s], %s" op.[3] op.[0])
            | 0x4f -> show 3 (sprintf "mov [%s+%x], %s" op.[3] mem.[ip + 2] op.[1])
            | 0x0f -> show 2 (sprintf "mov [%s], %s" op.[3] op.[1])
            | _ ->
                show 2 "??"
                running := false
        | 0x88, w ->
            match w with
            | 0x07 -> show 2 (sprintf "mov [%s], al" op.[3])
            | 0x67 -> show 3 (sprintf "mov [%s+%x], ah" op.[3] mem.[ip + 2])
            | _ ->
                show 2 "??"
                running := false
        | 0xb5, _ ->
            show 2 (sprintf "mov ch, %02x" mem.[ip + 1])
        | 0xb1, _ ->
            show 2 (sprintf "mov cl, %02x" mem.[ip + 1])
        | 0x81, 0x2e ->
            show 6 (sprintf "sub [%04x], %04x" (read16 mem (ip + 2)) (read16 mem (ip + 4)))
        | 0x80, 0x2e ->
            show 5 (sprintf "sub byte[%04x], %02x" (read16 mem (ip + 2)) mem.[ip + 4])
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
            running := false

let test() =
    // printfn "-------------------"
    // printfn "../8086-2/write-1.out"
    // main "../8086-2/write-1.out"
    printfn "-------------------"
    printfn "../8086-3/write-2.out"
    main "../8086-3/write-2.out"
    // printfn "-------------------"
    // printfn "../8086-4/write-3.out"
    // main "../8086-4/write-3.out"
    // printfn "-------------------"
    // printfn "../8086-5/write-4.out"
    // main "../8086-5/write-4.out"
    // printfn "-------------------"
    // printfn "../8086-6/write-5.out"
    // main "../8086-6/write-5.out"
    // printfn "-------------------"
    // printfn "../8086-7/write-6.out"
    // main "../8086-7/write-6.out"
    // printfn "-------------------"
    // printfn "../8086-8/write-7.out"
    // main "../8086-8/write-7.out"
    // printfn "-------------------"
    // printfn "../8086-9/regs.out"
    // main "../8086-9/regs.out"
    // printfn "-------------------"
    // printfn "../8086-10/write-8.out"
    // main "../8086-10/write-8.out"
    // printfn "-------------------"

