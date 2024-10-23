.PHONY: all
all: othrottle

.PHONY: bin/main.bc.exe
bin/main.bc.exe: .deps-installed
	dune build bin/main.bc.exe

.PHONY: bin/main.exe
bin/main.exe: .deps-installed
	dune build bin/main.exe

.PHONY: othrottle
othrottle: bin/main.exe .deps-installed
	rm -f othrottle
	ln -s bin/main.exe othrottle

.PHONY: othrottle_release
othrottle_release: .deps-installed
	rm -f othrottle_release
	dune build --release @all
	ln -s bin/main.bc.exe othrottle_release

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
