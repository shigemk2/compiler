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
