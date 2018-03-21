.PHONY: repl
repl:
	cabal exec -- ghci

.PHONY: docs
docs:
	cabal haddock
	cp -r dist/doc/html/asteroid ./docs

.PHONY: clean
clean:
	rm -rf docs

