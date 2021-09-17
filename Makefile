PACKAGE := app

.PHONY: test
test:
	mkdir -p target && clojure -M:test -m kaocha.runner

.PHONY: lint
lint:
	clojure -M:lint --lint src
