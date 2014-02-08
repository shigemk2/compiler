/ write(1, hello, 6);
mov $1, r0
sys write
hello
6


/ r1 = hello;
/ *(uint8_t *)r1 = 'H';
mov $hello, r1
movb $110, (r1)

/ write(1, hello, 6);
mov $1, r0
sys write
hello
6


/ r1 = hello;
/ *(uint8_t *)(r1 + 2) = 'L';
mov $hello, r1
movb $114, 2(r1)

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
