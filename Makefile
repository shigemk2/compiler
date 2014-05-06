all:
	$(MAKE) -C pdp11-10

7run.out: 7runall.sh
	./7runall.sh > $@

interpreter.out: pdp11-10/10-2-execute.fsx pdp11-10/10-2.fsx
	cd pdp11-10/ && fsharpi 10-2-execute.fsx > ../$@

# make test
# ファイルが存在するかチェックが走る
# ファイルが存在しなかったら7run.out もしくはinterpreter.out を実行する
# ファイルが存在しても依存しているファイルが更新されている場合は実行し直す
# 故に毎回make cleanする必要はない
test: 7run.out interpreter.out
	diff -u 7run.out interpreter.out

# make clean
clean:
	rm -f 7run.out interpreter.out
