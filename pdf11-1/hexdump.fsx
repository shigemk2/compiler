let aout = System.IO.File.ReadAllBytes "../../a.out"
printfn "File Size = 0x%x" aout.Length
for i in 0 .. 16 .. aout.Length - 1 do
    printf "%04x " i
    for j in 0 .. 15 do
        if i + j < aout.Length then
            printf " %02x " aout.[i + j]
        else
            printf "    "
    for j in 0 .. 15 do
        if i + j < aout.Length then
            let n = int aout.[i + j]
            if 0x20 <= n && n <= 0x7e then
                printf "%c" (char n)
            else
            printf "."
    printfn ""
    