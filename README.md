## Carbon

A Haskell framework for (parallel) monte-carlo simulations.

### What is it?

Carbon is an open-source, Haskell framework aiming to provide easy access to parallel Monte Carlo simulations by providing a simple, but powerful compo- sitional method for building simulations and high-level functions for running them.

### Status

_Updated: 2014/06/10_

Version 0.0.0.2 released!

Current features:
* support for parallelism (using 'Control.Parallel.Strategies.Eval')
* 1 high-level function for describing a simulation
* 2 aggregation techniques

Features coming soon:
* more high-level functions
* more aggregation techniques
* improved performance (hopefully)

Features for the future:
* suspend/resume
* monad transformer or IO wrapper