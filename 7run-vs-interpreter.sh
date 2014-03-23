# !/bin/sh

./7runall.sh > 7run.out
cd pdp11-10/
fsharpi 10-2-execute.fsx > ../interpreter.out
cd ..
diff -u 7run.out interpreter.out
