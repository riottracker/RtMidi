# RtMidi

[![CircleCI](https://circleci.com/gh/riottracker/RtMidi/tree/master.svg?style=svg)](https://circleci.com/gh/riottracker/RtMidi/tree/master)

Haskell wrapper for [RtMidi](http://www.music.mcgill.ca/~gary/rtmidi/), the lightweight, cross-platform MIDI I/O library.

## Development

This project is tested with Cabal (latest versions of last two compiler lines) and Stack (latest LTS).

You can get started with development like so:

    # Build and test on OSX CoreMIDI support or Linux with ALSA support.
    # (You can also manually invoke stack and pass `--flag RtMidi:jack` for Jack support.)
    make test

    # Print information about all accessible MIDI devices and all compiled APIs.
    make example-report

## TODO

* Add Windows MM support. This should only require a few changes to the Cabal file.
* See if there is a way to autodetect Jack in the Cabal file.
