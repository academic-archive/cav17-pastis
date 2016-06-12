all: packages/sandbox/done
	@make --no-print-directory -C source $@

clean:
	@cd packages && sh build.sh clean
	@make --no-print-directory -C source clean

dist-clean: clean
	@rm -f config.mk

packages/sandbox/done:
	@cd packages && sh build.sh

.PHONY: all clean dist-clean install

config.mk:
	@./configure

include config.mk
