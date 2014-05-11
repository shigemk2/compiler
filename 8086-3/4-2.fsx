let aout = System.IO.File.ReadAllBytes "write-2.out"
let read16 (a:byte[]) b =
    (int a.[b]) ||| ((int a.[b + 1]) <<< 8)
let write16 (a:byte[]) b c =
    a.[b] <- byte c
    a.[b + 1] <- byte (c >>> 8)
let tsize = read16 aout 2
let dsize = read16 aout 4
let mem = Array.zeroCreate<byte> 0x10000
mem.[0 .. tsize + dsize - 1] <- aout.[16 .. 16 + tsize + dsize - 1]
let mutable ax, bx, ip = 0, 0, 0
while ip < tsize do
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
    | 0xcd, 0x07 ->
        match int mem.[ip + 2] with
        | 1 ->
            exit ax
        | 4 ->
            let arg1 = read16 mem (ip + 3)
            let arg2 = read16 mem (ip + 5)
            let bytes = mem.[arg1 .. arg1 + arg2 - 1]
            printf "%s" (System.Text.Encoding.ASCII.GetString bytes)
            ip <- ip + 7
        | sc ->
            printfn "%04x: ??? syscall %d" ip sc
            exit 1
    | 0xcd, n ->
        printfn "%04x: ??? int %x" ip n
        exit 1
    | b, _ ->
        printfn "%04x: %02x ???" ip b
        exit 1
