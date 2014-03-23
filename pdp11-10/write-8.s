/ write(1, hello, 6);
mov $1, r0
sys write
hello
6

/ r1 = hello;
/ r0 = 0x4548;
/ *(uint16_t *)r1 = r0;
mov $hello + 2, r1
mov (r1), r0
sub $20040, r0
mov r0, (r1)

/ write(1, hello, 6);
mov $1, r0
sys write
hello
6

mov $hello, r0
mov $hello + 2, r1
mov (r1), (r0)

/ write(1, hello, 6);
mov $1, r0
sys write
hello
6

mov $hello, r1
mov $42510, r0
mov r0, r2
mov r2, (r1)
//mov (r1), r0

/ write(1, hello, 6);
mov $1, r0
sys write
hello
6

/ r1 = hello;
/ r2 = 0x4c4c;
/ *(uint16_t *)(r1 + 2) = r2;
mov $hello, r1
mov $46114, r2
mov r2, 2(r1)

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
