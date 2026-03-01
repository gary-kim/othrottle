.PHONY: all
all: othrottle

.PHONY: othrottle
othrottle: .deps-installed
	dune build othrottle

.PHONY: othrottle_release
othrottle_release: .deps-installed
	dune build --release othrottle

.PHONY: watch
watch: dependencies
	dune build --watch @all

.PHONY: dependencies
dependencies: .deps-installed

.deps-installed: othrottle.opam
	opam install . --deps-only
	touch .deps-installed

othrottle.opam: dune-project
	dune build othrottle.opam

.PHONY: test
test: othrottle
	dune runtest --force

.PHONY: checks
checks: othrottle test
	dune build @fmt
