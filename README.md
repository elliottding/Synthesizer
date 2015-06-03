# Synthesizer
CMSC 22311 Final Project

# Play commands

**play** _note1_ [_note2_] ...
Play a sequence of notes. Notes must be in scientific pitch notation, separated
by spaces. Ex:
    play A3 B3 C#4 D4 E4 F#4 G#4 A4
plays an A-major scale starting at the A below middle C.

**play** midi _file_
Play notes from a .midi file. All tracks in the .midi file are merged, and
the synthesizer plays notes in all of the tracks simultaneously. Note velocity
is ignored.

# Set commands

**set** _synth-param_ _value_
Set the specified _synth-param_ to the given _value_. Valid _synth-param_
specifiers are:
* samplerate: The samplerate used in playback.
* bpm: Beats per minute used for playback.
* amp: Volume of the synth.
* attack: Attack duration, in seconds, of the envelope.
* decay: Decay duration in seconds of the envelope.
* sustain: Sustain level of the envelope.
* release: Release duration in seconds of the envelope.

**set** osc _osc-index_ _osc-param_ _value_
Set the specified _osc-param_ of the oscillator at the given _osc-index_ to the
given _value_. Valid _osc-param_ specifiers are:
* amp: Volume of the oscillator.
* phase: Phase offset, in seconds, of the oscillator.
* wave: Wave table to be used by the oscillator. The wave table must name an
existing .wavetable file in the /data/ directory.

# Add commands

**add** osc _osc-type_
Add an oscillator that loads the wave table given by _osc-type_ to the synth.

# Output commands

**output** _note1_ [_note2_] ... _outfile_
Output all notes to the given _outfile_.

**output** midi _infile_ _outfile_
Load a .midi file, and output the sound to the _outfile_.
