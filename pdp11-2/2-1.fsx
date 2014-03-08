let aout = System.IO.File.ReadAllBytes "../../a.out"

let read16 (src:byte[]) index =
    (int src.[index]) ||| ((int src.[index + 1]) <<< 8)

let textsize = read16 aout 2
printfn "textsize = %d (0x%x)" textsize textsize
let text = aout.[0x10 .. 0x10 + textsize - 1]
let mutable i = 0
while i < text.Length do
    let w = read16 text i
    if w = 0x15c0 then
        let v = read16 text (i + 2)
        printfn "%04x %04x %04x mov $%x, r0" i w v v
        i <- i + 4
    else
        printfn "%04x %04x     ?" i w
        i <- i + 2

