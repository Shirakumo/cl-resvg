all:
	mkdir -p ../static/
	$(MAKE) build TARGET=x86_64-unknown-linux-gnu PLATFORM=lin-amd64 EXT=so
	$(MAKE) build TARGET=aarch64-unknown-linux-gnu PLATFORM=lin-arm64 EXT=so RUSTFLAGS="-C linker=aarch64-linux-gnu-gcc"
	$(MAKE) build TARGET=x86_64-pc-windows-gnu PLATFORM=win-amd64 EXT=dll
	$(MAKE) build TARGET=x86_64-apple-darwin PLATFORM=mac-amd64 EXT=dylib

build:
	cd crates/c-api; cargo build --release --target $(TARGET) $(CARGO)
	cp target/$(TARGET)/release/*resvg.$(EXT) ../static/libresvg-$(PLATFORM).$(EXT)
