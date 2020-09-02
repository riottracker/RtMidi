# RtMidi

Haskell wrapper for [RtMidi](http://www.music.mcgill.ca/~gary/rtmidi/), the lightweight, cross-platform MIDI I/O library.

## Development

This project is tested with Cabal (latest versions of last two compiler lines) and Stack (latest LTS).

Currently, you need to use flags to select the RtMidi backend. For example:

    # Build with OSX CoreMIDI support
    stack build --flag RtMidi:core

    # Verify that it works:
    stack exec -- rtmidi-query

We hope to auto-detect backends like [python-rtmidi](https://github.com/SpotlightKid/python-rtmidi/blob/master/setup.py) in the future.
