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
mov (bx), sp
mov (bx), bp
mov (bx), si
mov (bx), di

mov 2(bx), ax
mov 2(bx), cx
mov 2(bx), dx
mov 2(bx), bx
mov 2(bx), sp
mov 2(bx), bp
mov 2(bx), si
mov 2(bx), di

.sect .data
hello: .ascii "hello\n"
