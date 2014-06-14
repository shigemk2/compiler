mov ax, #1
mov cx, #1
mov dx, #1
mov bx, #1
mov sp, #1
mov bp, #1
mov si, #1
mov di, #1
mov (si), ax
mov (si), cx
mov (si), dx
mov (si), bx
mov (di), ax
mov (di), cx
mov (di), dx
mov (di), bx
mov (bp), ax
mov (bp), cx
mov (bp), dx
mov (bp), bx
mov (bx), ax
mov (bx), cx
mov (bx), dx
mov (bx), bx

.sect .data
hello: .ascii "hello\n"
