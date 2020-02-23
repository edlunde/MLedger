
.PHONY: test
test: MLedger.m test/testScript.sh
	./test/testScript.sh

MLedger.m: src/*.m src/makeMLedger.sh
	src/makeMLedger.sh