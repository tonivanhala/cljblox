PACKAGE := app

.PHONY: test
test:
	mkdir -p target && clojure -M:test -m kaocha.runner

.PHONY: repl
repl:
	clj -M -m main

.PHONY: lint
lint:
	clojure -M:lint --lint src
