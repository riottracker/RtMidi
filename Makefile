RT_MIDI_VERSION := "4.0.0"
RT_MIDI_URL := "https://raw.githubusercontent.com/thestk/rtmidi/$(RT_MIDI_VERSION)"

.PHONY: update-sources
update-sources:
	# Update RtMidi sources from upstream
	curl --output rtmidi/RtMidi.cpp $(RT_MIDI_URL)/RtMidi.cpp
	curl --output rtmidi/RtMidi.h $(RT_MIDI_URL)/RtMidi.h
	curl --output rtmidi/rtmidi_c.cpp $(RT_MIDI_URL)/rtmidi_c.cpp
	curl --output rtmidi/rtmidi_c.h $(RT_MIDI_URL)/rtmidi_c.h
