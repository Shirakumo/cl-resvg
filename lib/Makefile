ROOT_DIR:=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
OS = misc
ARCH = misc
EXT = so

UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
	OS = lin
endif
ifeq ($(UNAME_S),Darwin)
	OS = mac
	EXT = dylib
endif
ifneq ($(filter MINGW%,$(UNAME_S)),)
	OS = win
	EXT = dll
endif

UNAME_M := $(shell uname -m)
ifeq ($(UNAME_M),x86_64)
	ARCH = amd64
endif
ifneq ($(filter %86,$(UNAME_M)),)
	ARCH = i686
endif
ifneq ($(filter arm%,$(UNAME_M)),)
	ARCH = arm64
endif

all:
	mkdir -p ../static/
	cd crates/c-api; cargo build --release
	cp target/release/deps/libresvg.$(EXT) ../static/libresvg-$(OS)-$(ARCH).$(EXT)
