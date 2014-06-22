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

mov (bp), ax
mov (bp), cx
mov (bp), dx
mov (bp), bx
mov (bp), sp
mov (bp), bp
mov (bp), si
mov (bp), di

mov (si), ax
mov (si), cx
mov (si), dx
mov (si), bx
mov (si), sp
mov (si), bp
mov (si), si
mov (si), di

mov (di), ax
mov (di), cx
mov (di), dx
mov (di), bx
mov (di), sp
mov (di), bp
mov (di), si
mov (di), di

mov 2(bx), ax
mov 2(bx), cx
mov 2(bx), dx
mov 2(bx), bx
mov 2(bx), sp
mov 2(bx), bp
mov 2(bx), si
mov 2(bx), di

mov 2(bp), ax
mov 2(bp), cx
mov 2(bp), dx
mov 2(bp), bx
mov 2(bp), sp
mov 2(bp), bp
mov 2(bp), si
mov 2(bp), di

mov 2(si), ax
mov 2(si), cx
mov 2(si), dx
mov 2(si), bx
mov 2(si), sp
mov 2(si), bp
mov 2(si), si
mov 2(si), di

mov 2(di), ax
mov 2(di), cx
mov 2(di), dx
mov 2(di), bx
mov 2(di), sp
mov 2(di), bp
mov 2(di), si
mov 2(di), di

movb (bx), al
movb (bx), cl
movb (bx), dl
movb (bx), bl
movb (bx), ah
movb (bx), ch
movb (bx), dh
movb (bx), bh

mov 0x1234, ax
mov 0x1234, cx
mov 0x1234, dx
mov 0x1234, bx
mov 0x1234, sp
mov 0x1234, bp
mov 0x1234, si
mov 0x1234, di

;; .sect .data
;; hello: .ascii "hello\n"
