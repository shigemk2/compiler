let aout = System.IO.File.ReadAllBytes "a.out"

// 0x10=16おきにループを始める。
// 16なのはhexdumpの仕様
for i in 0 .. 0x10 .. aout.Length - 1 do
    printf "%08x " i
    // 左側のダンプ部分
    for j in 0 .. 0xf do
        if i + j < aout.Length then
            printf "%02x " aout.[i + j]
        else
            printf "   "
    // 右側の文字出力部分
    for j in 0 .. 0xf do
        if i + j < aout.Length then
            let ch = int aout.[i + j]
            // 32-63だったら文字
            if 0x20 <= ch && ch <= 0x7e then
                printf "%c" (char ch)
            else
                printf "."
    printfn ""
