# RtMidi

Haskell wrapper for [RtMidi](http://www.music.mcgill.ca/~gary/rtmidi/), the lightweight, cross-platform MIDI I/O library.

## Version

The version of this library does not correspond to the RtMidi version (sorry). You can look at the `justfile` to see that we vendor sources
from version `6.0.0`.

## How to use

See [Hackage](https://hackage.haskell.org/package/RtMidi) for the latest released version and add `RtMidi` and `vector` to your library `build-depends`.

Follow the [callback](https://github.com/riottracker/RtMidi/blob/master/examples/callback.hs) example to receive messages.

Follow the [playback](https://github.com/riottracker/RtMidi/blob/master/examples/playback.hs) example to send messages.

## Development

This project is tested with Cabal (latest versions of last two compiler lines) and Stack (latest LTS).

You can get started with development like so (if you have `just` installed):

    # Build and test on OSX CoreMIDI support or Linux with ALSA support.
    # (You can also manually invoke stack and pass `--flag RtMidi:jack` for Jack support.)
    just test

    # Print information about all accessible MIDI devices and all compiled APIs.
    just example-report

## TODO

* See if there is a way to autodetect Jack in the Cabal file.
* See if there is a way to make OSX not take 3 seconds when initializing the MIDI subsystem.
