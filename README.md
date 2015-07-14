# Scales

### Usage
    runhaskell Scales.hs <key> <scale> <mode>
    -- e. g.
    -- runhaskell Scles.hs C Major Ionian
Check out `Lib.DataTypes` to see the possible values

### tests
    runhaskell ScaleTests.hs
Mostly quickcheck.

### What is this?

This little project is excuse to learn Haskell as I dump stuff I'm learning
about music theory.

For the time being it's mostly about scales and modes, hence the highly
imaginative name I'm so proud of.

I should note that scales and modes are kind of mixed up as of right now, but
I'm too lazy to explain how they work within this little thing right now.
Suffice to say, maybe as I learn more I'll come up with a better way combine
them, but for now this will do.

### Features
So here's the list of current features:
* Display the full scale/mode combination for an arbitrary key
* Calculate the relative modes (e.g. "A Aeolian is the same as C Ionian")
* Calculate modulations (e.g. "If you're playing C Ionian and want to move to Dorian, you can use As Ionian")
* Display a rudimentary representation of a guitar neck to show the scale

TODO:
* Add an interactive menu to select things, instead (or on top of?) using command line arguments
* Add a piano representation on top of the guitar

TODO that would require at least a different name:
* Add chord builder
* ...Other stuff with chords I guess
