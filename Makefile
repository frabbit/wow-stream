test-mode=-fbyte-code # or -fobject-code

ghci-options= --no-build --ghc-options $(test-mode)

ghci-disabled-warnings=  --ghci-options -Wno-unused-matches \
	--ghci-options -Wno-redundant-constraints \
	--ghci-options -Wno-unused-binds \
	--ghci-options -Wno-partial-type-signatures \
	--ghci-options -Wno-unused-imports \
	--ghci-options -Wno-unused-foralls

.PHONY: ghci
ghci:
	stack ghci wow-stream:lib

.PHONY: test-unit-ghcid
test-unit-ghcid:
	reset && clear
	ghcid --warnings --restart=wow-stream.cabal --reload=.reload-ghcid --test=UnitSpec.main \
		--command="stack ghci \
			$(ghci-options) $(ghci-disabled-warnings) \
		wow-stream:wow-stream-test-all"
	reset && clear

.PHONY: test-e2e-ghcid
test-e2e-ghcid:
	reset && clear
	ghcid --warnings --restart=wow-stream.cabal --reload=.reload-ghcid --test=E2ESpec.main \
		--command="stack ghci \
			$(ghci-options) $(ghci-disabled-warnings) \
		wow-stream:wow-stream-test-all"
	reset && clear

.PHONY: test-compile-ghcid
test-compile-ghcid:
	reset && clear
	ghcid --restart=wow-stream.cabal --reload=.reload-ghcid \
		--command="stack ghci \
		$(ghci-options) \
		wow-stream:wow-stream-test-all"
	reset && clear

.PHONY: test-unit
test-unit:
	stack test --ghc-options "-O0 -j4 +RTS -A128m -n2m -RTS" wow-stream:wow-stream-test-unit

.PHONY: test-e2e
test-e2e:
	stack test --ghc-options "-O0 -j4 +RTS -A128m -n2m -RTS" wow-stream:wow-stream-test-e2e

build-run-docker:
	docker run --rm -it $(docker build -q .)