# RtMidi

Haskell wrapper for [RtMidi](http://www.music.mcgill.ca/~gary/rtmidi/), the lightweight, cross-platform MIDI I/O library.

## Development

This project is tested with Cabal (latest versions of last two compiler lines) and Stack (latest LTS).

You can get started with development like so:

    # Build on OSX CoreMIDI support or Linux with ALSA support
    # You can also pass `--flag RtMidi:jack` for Jack support
    stack build

    # Verify that it works:
    stack exec -- rtmidi-query

There is also a `Dockerfile` in the `docker` directory and some `make` targets that can help you verify Linux builds:

    # Build the image and tag it `haskell-rtmidi-dev`
    make docker-build

    # Enter the docker image
    make docker-repl

    # Inside the docker image:
    cabal update && cabal build -fjack

(Note that you can't use any `RtMidi` functions in the containerized env unless you are running a Linux host, and
even then you'd probably have to start the process with something like `docker run --device /dev/snd`.)

## TODO

* Add Windows MM support. This should only require a few changes to the Cabal file.
* See if there is a way to autodetect Jack in the Cabal file.
* Publish new version to Hackage and eventually Stackage.
