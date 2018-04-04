.PHONY: build
build:
	cabal build

.PHONY: run
run:
	cabal run asteroid

.PHONY: repl
repl:
	cabal exec -- ghci Asteroid

.PHONY: docs
docs:
	cabal haddock --executables
	rm -rf ../docs
	cp -r dist/doc/html/asteroid ../docs

.PHONY: clean
clean:
	cabal clean
	rm -rf ../docs

.PHONY: flushsandbox
flushsandbox:
	rm -rf cabal.sandbox.config
	rm -rf .cabal-sandbox
	cabal sandbox init
	cabal install --dependencies-only
