.PHONY: watch
watch:
	stack exec ghcid -- --command "stack ghci" --test "update" --warnings