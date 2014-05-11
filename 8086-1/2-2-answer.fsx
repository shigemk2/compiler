let aout = System.IO.File.ReadAllBytes "a.out"
let read16 (a:byte[]) b =
    (int a.[b]) ||| ((int a.[b + 1]) <<< 8)
let tsize = read16 aout 2
let dsize = read16 aout 4
let mem = aout.[16 .. 16 + tsize + dsize - 1]
let mutable ax, ip = 0, 0
while ip < tsize do
    match int mem.[ip] with
    | 0xb8 ->
        ax <- read16 mem (ip + 1)
        ip <- ip + 3
    | 0xcd ->
        let n = int mem.[ip + 1]
        if n <> 7 then
            printfn "%04x: ??? int %x" ip n
            exit 1
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
    | b ->
        printfn "%04x: %02x ???" ip b
        exit 1
