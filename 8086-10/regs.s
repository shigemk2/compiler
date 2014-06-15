mov ax, #1
mov cx, #1
mov dx, #1
mov bx, #1
mov sp, #1
mov bp, #1
mov si, #1
mov di, #1
mov (bx), ax
mov (bx), cx
mov (bx), dx
mov (bx), bx

.sect .data
hello: .ascii "hello\n"
