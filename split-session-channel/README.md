# Benchmark

This project contains the Haskell micro benchmark from Section 4.5 that
evaluates the performance of our semantics.

## Overview

The benchmark runs 200 sessions where each session contains 1000 send and 1000
receive operations, the latter in a different thread (no session types, just
plain parametric channels). It sends the numbers 1..1000 and the receiver
calculates the sum. The benchmark has three modes:

- Prim: using simple synchronous channels

- Cell: using the cell encoding outlined in the paper, but optimized in the
  sequential parts as outlined in (A-Q1)

- Split: using the full cell encoding explained in the paper

Split and Cell are implemented using Prim as primitive channels.

Benchmarking is done using the criterion framework:
https://hackage.haskell.org/package/criterion

## Results

Results are shown in the table below and have been obtained on a MacBook Pro,
2,4 GHz 8-Core Intel Core i9, 64 GB 2667 MHz DDR4 main memory without
using docker. 

```
% stack run -- --output benchout.html --csv benchout.csv
benchmarking channels/Prim
time                 2.034 s    (2.003 s .. 2.095 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 2.045 s    (2.036 s .. 2.060 s)
std dev              13.81 ms   (1.659 ms .. 17.67 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking channels/Splt
time                 2.154 s    (2.104 s .. 2.260 s)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 2.143 s    (2.108 s .. 2.166 s)
std dev              34.27 ms   (14.22 ms .. 47.23 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking channels/Cell
time                 2.087 s    (2.068 s .. 2.126 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 2.095 s    (2.085 s .. 2.105 s)
std dev              11.11 ms   (3.996 ms .. 15.30 ms)
variance introduced by outliers: 19% (moderately inflated)
```

(The generated output files `benchout.html` and `benchout.csv` are included.)

We calculate the overhead by $(T-T_0)/T_0$ where $T$ is the time of
the current benchmark and $T_0$ is the baseline for `Prim`.

| Mode  | Time   | Overhead |
|-------|--------|----------|
| Prim  | 2.034 s |     0.0% |
| Cell  | 2.087 s |   ~2.6% |
| Split | 2.154 s |   ~5.9% |

We believe this overhead is negligible in practice as this micro benchmark is
communication bound and we expect a real program to be much more computation
bound.

## Usage

### Docker

The following uses docker to enter an interactive `bash` shell where the
benchmark has been already compiled:

```bash
docker-compose build
docker-compose run bst-benchmark
```

The following command can then be used in the interactive shell to run the
benchmarks:

```bash
stack run --
```

### Manual Installation

The project uses the `stack` build system (Version 3.3.1).
Installation instructions for `stack` can be found here:
https://docs.haskellstack.org/en/stable/

To build the project, run:
```
stack build
```
(The warnings issued by the compiler can be ignored.)

To run the benchmarks use
```
stack run --
```

