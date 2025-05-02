BEEBASM?=beebasm
PYTHON?=python

# A make command with no arguments will build Teletext Elite with the standard
# commander
#
# Optional arguments for the make command are:
#
#   commander=max       Start with a maxed-out commander
#
# So, for example:
#
#   make commander=max
#
# will build Teletext Elite with a maxed-out commander

ifeq ($(commander), max)
  max-commander=TRUE
else
  max-commander=FALSE
endif

remove-checksums=FALSE
match-original-binaries=FALSE

variant-number=2
folder=sth
suffix=-sth

.PHONY:all
all:
	echo _VERSION=2 > 1-source-files/main-sources/elite-build-options.asm
	echo _VARIANT=$(variant-number) >> 1-source-files/main-sources/elite-build-options.asm
	echo _REMOVE_CHECKSUMS=$(remove-checksums) >> 1-source-files/main-sources/elite-build-options.asm
	echo _MATCH_ORIGINAL_BINARIES=$(match-original-binaries) >> 1-source-files/main-sources/elite-build-options.asm
	echo _MAX_COMMANDER=$(max-commander) >> 1-source-files/main-sources/elite-build-options.asm
	$(BEEBASM) -i 1-source-files/main-sources/elite-loader.asm -v >> 3-assembled-output/compile.txt
	$(BEEBASM) -i 1-source-files/main-sources/elite-menu.asm -v >> 3-assembled-output/compile.txt
	$(BEEBASM) -i 1-source-files/main-sources/elite-disc.asm -do teletext-elite.ssd -boot MENU -title "E L I T E"
ifneq ($(verify), no)
	@$(PYTHON) 2-build-files/crc32.py 4-reference-binaries/$(folder) 3-assembled-output
endif

.PHONY:b2
b2:
	curl -G "http://localhost:48075/reset/b2"
	curl -H "Content-Type:application/binary" --upload-file "teletext-elite.ssd" "http://localhost:48075/run/b2?name=teletext-elite.ssd"
