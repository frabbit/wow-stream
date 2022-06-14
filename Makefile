

ghci:
	stack ghci wow-stream:lib

test-unit-ghcid:
	reset && clear
	ghcid --warnings --restart=wow-stream.cabal --reload=.reload-ghcid --test=UnitSpec.main \
		--command="stack ghci \
			$(ghci-options) $(ghci-disabled-warnings) \
		wow-stream:wow-stream-test-all"
	reset && clear