# This doesn't work yet :(
test:
	emacs -batch -l ert -l java-imports.el -l java-imports-tests.el -f ert-run-tests-batch-and-exit
