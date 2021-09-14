
CABAL_ARGS = -O2 --enable-executable-stripping
CABAL = cabal

build:
	$(CABAL) build $(CABAL_ARGS)

BIN_DIR = ./bin
install:
	mkdir -p $(BIN_DIR);
	cp `$(CABAL) list-bin xmonad-wm $(CABAL_ARGS)` $(BIN_DIR)
	cp `$(CABAL) list-bin xmobar $(CABAL_ARGS)` $(BIN_DIR)

