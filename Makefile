all: packages/sandbox/done
	@make --no-print-directory -C source $@

packages/sandbox/done:
	@cd packages && sh build.sh

.PHONY: all install
