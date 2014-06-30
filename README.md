## Carbon

A Haskell framework for (parallel) monte-carlo simulations.

### What is it?

Carbon is an open-source, Haskell framework aiming to provide easy access to parallel Monte Carlo simulations by providing a simple, but powerful compositional method for building simulations, high-level functions for running them, and efficient data structures for aggregating common data types.

Examples of simulations written in Carbon can be found at:
https://github.com/icasperzen/hs-carbon-examples

### Features

#### Monadic Composition

Carbon allows users to quickly describe Monte Carlo simulations using monadic composition (similar to parser combinators).
For example, starting from a Carbon primitive action, `randomR`, we can build a simulation which tests for membership in the unit circle.

```Haskell
mcSquareD :: RandomGen g => MonteCarlo g (Double,Double)
mcSquareD = liftM2 (,) (randomR (-1,1)) (randomR (-1,1))

inUnitCircle :: RandomGen g => MonteCarlo g Bool
inUnitCircle = do
    (x,y) <- mcSquareD
    return $ x*x + y*y <= 1
```

We build `mcSquareD`, which returns a random 2D coordinate in the unit square, from `randomR`.
Next, we build `inUnitCircle` using `mcSquareD`, returning a `Bool` if the sampled 2D point lies within the unit circle.

#### High Level Function

Carbon aims to capture common usage patterns in high level "skeletons".
Currently, just one is provided (and its sequential equivalent):

```Haskell
experimentP :: (R.RandomGen g, Result s)
            => MonteCarlo g (Obs s) -> Int -> Int -> g -> s
```

For example, using `inUnitCircle`, we can compute the value of pi.

```Haskell
let res = experimentP inUnitCircle noRuns chSize gen
    pi  = 4 * sampleMean res
```

#### Data Aggregation

Descriptions of simulations should be completely independent of the results desired.
Carbon enforces this separation of concerns by using type families.
Every simulation will require a type annotation describing the desired output.
The particular instance of the type family describes how data aggregation is to occur (what to do with each observation).
For example, the following two simulations use the same `MonteCarlo` action, but deliver very different results.

```Haskell
let bl = experimentP inUnitCircle noRuns chnkSize gen :: [Bool]
let bs = experimentP inUnitCircle noRuns chnkSize gen :: BoolSumm
```

`[Bool]` is a list containing every observation from the simulation.
This is inefficient, however, as it is rarely necessary to maintain each and every observation in memory.
Instead, we can fold it into a clever data structure.
`BoolSumm` is a space-efficient representation of binary observations.

The `Result` typeclass captures this functionality.

#### Basic Statistics

Carbon provides functionality for computing basic statistics on some instances of `Result`.
The `Summary` typeclass is meant to capture some of the common statistical measures one might be interested in.
Creating new instances of `Summary` is easy; and extra functions can be written for a `Result` instance to capture even more complex statistical functionality.

### Status

_Updated: 2014/06/30_

Version 0.1.0 released!

Current features:
* support for parallelism (using GpH Strategies)
* high-level function for describing a simulation
* efficient data aggregation techniques