## Carbon

A Haskell framework for (parallel) monte-carlo simulations.

### What is it?

Carbon is an open-source, Haskell framework aiming to provide easy access to parallel Monte Carlo simulations by providing a simple, but powerful compositional method for building simulations and high-level functions for running them.

Examples of simulations written in Carbon can be found at: https://github.com/icasperzen/hs-carbon-examples

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

#### Data Aggregation

Descriptions of simulations should be completely independent of the results desired.
Carbon enforces this separation of concerns by using type families.
Every simulation will require a type annotation describing the desired output.
The particular instance of the type family describes how data aggregation is to occur (what to do with each observation).
For example, the following two simulations use the same `MonteCarlo` action, but different data aggregation techniques:

```Haskell
let s = experimentP inUnitCircle noRuns chnkSize gen :: BoolSumm
let t = experimentP inUnitCircle noRuns chnkSize gen :: [Bool]
```

`BoolSumm` is an instance of the `Result` type class/family.

### Status

_Updated: 2014/06/24_

Version 0.0.1.0 released!

Current features:
* support for parallelism (using 'Control.Parallel.Strategies.Eval')
* high-level function for describing a simulation
* 2 data aggregation techniques

Features coming soon:
* more high-level functions
* more aggregation techniques
* improved performance

Features for the future:
* suspend/resume
* monad transformer or IO wrapper
