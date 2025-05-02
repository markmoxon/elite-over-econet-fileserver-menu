BEEBASM?=beebasm
PYTHON?=python

variant-number=2
folder=menu

.PHONY:all
all:
	echo _VERSION=2 > 1-source-files/main-sources/elite-build-options.asm
	echo _VARIANT=$(variant-number) >> 1-source-files/main-sources/elite-build-options.asm
	$(BEEBASM) -i 1-source-files/main-sources/elite-loader.asm -v >> 3-assembled-output/compile.txt
	$(BEEBASM) -i 1-source-files/main-sources/elite-menu.asm -v >> 3-assembled-output/compile.txt
	$(BEEBASM) -i 1-source-files/main-sources/elite-disc.asm -do server-menu.ssd -boot MENU -title "E L I T E"
ifneq ($(verify), no)
	@$(PYTHON) 2-build-files/crc32.py 4-reference-binaries/$(folder) 3-assembled-output
endif

.PHONY:b2
b2:
	curl -G "http://localhost:48075/reset/b2"
	curl -H "Content-Type:application/binary" --upload-file "server-menu.ssd" "http://localhost:48075/run/b2?name=server-menu.ssd"
