# split-session-channel

This program was built using the Haskell stack utility, Version 3.3.1
x86_64.

To build use
```
stack build
```
(The warnings issued by the compiler can be ignored.)

To run the three benchmarks use
```
stack run -- Prim
stack run -- Cell
stack run -- Split
```

We obtained the timings using the Unix `time` command.
