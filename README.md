# RtMidi

[![CircleCI](https://circleci.com/gh/riottracker/RtMidi/tree/master.svg?style=svg)](https://circleci.com/gh/riottracker/RtMidi/tree/master)

Haskell wrapper for [RtMidi](http://www.music.mcgill.ca/~gary/rtmidi/), the lightweight, cross-platform MIDI I/O library.

## How to use

See [Hackage](https://hackage.haskell.org/package/RtMidi) for the latest released version and add `RtMidi` and `vector` to your library `build-depends`.

Follow the [callback](https://github.com/riottracker/RtMidi/blob/master/examples/callback.hs) example to receive messages.

Follow the [playback](https://github.com/riottracker/RtMidi/blob/master/examples/playback.hs) example to send messages.

## Development

This project is tested with Cabal (latest versions of last two compiler lines) and Stack (latest LTS).

You can get started with development like so:

    # Build and test on OSX CoreMIDI support or Linux with ALSA support.
    # (You can also manually invoke stack and pass `--flag RtMidi:jack` for Jack support.)
    make test

    # Print information about all accessible MIDI devices and all compiled APIs.
    make example-report

## TODO

* See if there is a way to autodetect Jack in the Cabal file.
