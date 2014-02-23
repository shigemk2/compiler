/ write(1, hello, 6);
mov $1, r0
sys write
hello
6


/ *(uint16_t *)hello = 0x4548;
mov $42510, hello

/ write(1, hello, 6);
mov $1, r0
sys write
hello
6


/ *(uint16_t *)(hello + 2) = 0x4c4c;
mov $46114, hello + 2

/ write(1, hello, 6);
mov $1, r0
sys write
hello
6


/ *(uint8_t *)(hello + 4) = 'O';
movb $117, hello + 4

/ write(1, hello, 6);
mov $1, r0
sys write
hello
6


/ exit(0);
mov $0, r0
sys exit


.data
hello: <hello\n>
