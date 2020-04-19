
.PHONY: all
all: test quickstart

.PHONY: test
test: MLedger.m test/testScript.sh
	./test/testScript.sh

MLedger.m: src/*.m src/makeMLedger.sh
	src/makeMLedger.sh

##########################

quickstart_import_path = MLedger_Quickstart/toImport/
import_files = export0.csv stmt.txt

.PHONY: quickstart
quickstart: $(addprefix $(quickstart_import_path),$(import_files))

$(quickstart_import_path)%: test/testFiles/%
	cp $< $@ 