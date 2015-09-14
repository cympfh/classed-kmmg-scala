do:
	scalac src/kmmg.scala
	scalac main.scala
	time scala Main -K 3 < input/sample 2>&1 | tee log

bin:
	scalac src/*.scala
	scalac test/preceq.scala
	scala Test < test/preceq.input

.PHONY: clean

clean:
	rm -r CRP/ *.class
