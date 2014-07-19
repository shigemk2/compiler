! write(1, hello, 6);
mov ax, #1
int 7
.data1 4
.data2 hello, 6

! exit(0);
mov ax, #0
int 7
.data1 1

! bx = hello;
! ax = 0x4548;
! *(uint8_t *)bx = al;
! *(uint8_t *)(bx + 1) = ah;
mov bx, #hello
mov ax, #0x4548
mov (bx), ax
mov hello + 4, ax
add (bx), ax
add hello + 4, ax
int 7
.data1 4
.data2 hello, 6

.sect .data
hello: .ascii "hello\n"

