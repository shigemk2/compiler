/ write(1, hello, 6);
mov $1, r0
sys write
hello
6

/ r1 = hello;
/ r0 = 0x4548;
/ *(uint8_t *)r1 = r0;
/ r0 = (r0 << 8) | (r0 >> 8);
/ *(uint8_t *)(r1 + 1) = r0;
mov $hello, r1
mov $42510, r0
movb r0, (r1)
swab r0
movb r0, 1(r1)

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
