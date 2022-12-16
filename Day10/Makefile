#!/usr/bin/env make

SRC	:= $(wildcard *.hs **/*.hs)

.PHONY: default
default: check build test

.PHONY: check
check:	tags style lint

.PHONY: tags
tags:
	@echo tags ...
	@hasktags --ctags --extendedctag $(SRC)

.PHONY: style
style:
	@echo style ... $(SRC)
	@stylish-haskell --config=.stylish-haskell.yaml --inplace $(SRC)

.PHONY: lint
lint:
	@echo lint ...
	@hlint --color $(SRC)
	@cabal check

.PHONY: build
build:
	@echo build ...
	@stack build --verbosity info --pedantic

.PHONY: test
test:
	@echo test ...
	@stack test

.PHONY: exec
exec:
	@echo test data ...
	@stack exec main test.data
	@echo input data ...
	@stack exec main input.data

.PHONY: setup
setup:
	@stack path
	@stack query
	@stack ls dependencies

.PHONY: ghci
ghci:
	@stack ghci --ghci-options -Wno-type-defaults

clean:
	@stack clean
	@cabal clean
	@$(RM) tags
	@$(RM) $(wildcard *.hi **/*.hi)
	@$(RM) $(wildcard *.o **/*.o)
	@$(RM) $(wildcard *.prof **/*.prof)
	@$(RM) $(wildcard *.tix **/*.tix)

cleanall: clean
	@stack purge
