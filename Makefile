all: packages/sandbox/done
	@make --no-print-directory -C source $@

clean:
	@cd packages && sh build.sh clean
	@make --no-print-directory -C source clean

packages/sandbox/done:
	@cd packages && sh build.sh

.PHONY: all clean install
