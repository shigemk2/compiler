/ write(1, hello, 6);
mov $1, r0
sys write
hello
6


/ *(uint16_t *)hello -= 0x2020;
sub $20040, hello

/ write(1, hello, 6);
mov $1, r0
sys write
hello
6


/ *(uint16_t *)(hello + 2) -= 0x2020;
sub $20040, hello + 2

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
