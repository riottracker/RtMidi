stack_build := "stack build --fast"
src_dirs := "Sound test examples"

# No default tasks
default:
  just --list

# Build and run tests
test:
  {{ stack_build }} --test

# Build only
build:
  {{ stack_build }} --test --no-run-tests

# Clean stack work
clean:
  stack clean --full

# Enter repl
ghci:
  stack ghci --test

# Open browser with generated docs
docs:
  stack haddock --open

# Install tool deps
deps:
  stack build --copy-compiler-tool hlint fourmolu

# Format with fourmolu
format:
  stack exec -- fourmolu --mode inplace {{ src_dirs }}

# Lint with hlint
lint:
  stack exec -- hlint {{ src_dirs }}

example-callback: build
  {{ stack_build }} --test --no-run-tests --exec rtmidi-callback

example-playback: build
  {{ stack_build }} --test --no-run-tests --exec rtmidi-playback

example-poll: build
  {{ stack_build }} --test --no-run-tests --exec rtmidi-poll

example-report: build
  {{ stack_build }} --test --no-run-tests --exec rtmidi-report

# Generate docs for hackage
gen-docs:
  rm -rf dist-newstyle/RtMidi-*-docs.tar.gz
  cabal test
  cabal haddock --haddock-for-hackage --haddock-option=--hyperlinked-source

# Upload docs to hackage
upload-docs: gen-docs
  cabal upload --publish -d dist-newstyle/RtMidi-*-docs.tar.gz
