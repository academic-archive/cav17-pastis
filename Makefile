all: packages/sandbox
	@make --no-print-directory -C source $@

packages/sandbox:
	@cd packages && sh build.sh

.PHONY: all install
