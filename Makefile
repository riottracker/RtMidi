RT_MIDI_VERSION := 4.0.0
RT_MIDI_URL := https://raw.githubusercontent.com/thestk/rtmidi/$(RT_MIDI_VERSION)

.PHONY: update-sources
update-sources:
	# Update RtMidi sources from upstream
	curl --output rtmidi/RtMidi.cpp $(RT_MIDI_URL)/RtMidi.cpp
	curl --output rtmidi/RtMidi.h $(RT_MIDI_URL)/RtMidi.h
	curl --output rtmidi/rtmidi_c.cpp $(RT_MIDI_URL)/rtmidi_c.cpp
	curl --output rtmidi/rtmidi_c.h $(RT_MIDI_URL)/rtmidi_c.h

.PHONY: install-dev-deps
install-dev-deps:
	# Install some useful packages for development
	stack build --copy-compiler-tool ghcid hlint stylish-haskell

.PHONY: clean
clean:
	stack clean --full

.PHONY: build
build:
	stack build --test --no-run-tests

.PHONY: test
test:
	stack test

.PHONY: example-callback
example-callback: build
	stack exec -- rtmidi-callback

.PHONY: example-playback
example-playback: build
	stack exec -- rtmidi-playback

.PHONY: example-poll
example-poll: build
	stack exec -- rtmidi-poll

.PHONY: example-report
example-report: build
	stack exec -- rtmidi-report

.PHONY: format
format:
	# Run stylish-haskell to format our haskell source
	find Sound -name '*.hs' | xargs -t stack exec -- stylish-haskell -i

.PHONY: lint
lint:
	# Run hlint over our haskell source
	stack exec -- hlint -i 'Parse error' -i 'Reduce duplication' Sound

.PHONY: gen-docs
gen-docs:
	# Generate docs for hackage
	rm -rf dist-newstyle/RtMidi-*-docs.tar.gz
	cabal test
	cabal haddock --haddock-for-hackage --haddock-option=--hyperlinked-source

.PHONY: upload-docs
upload-docs: gen-docs
	# Upload docs to hackage
	cabal upload --publish -d dist-newstyle/RtMidi-*-docs.tar.gz
