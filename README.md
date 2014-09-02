# What is this?

The code here is a fork of [Jérémie Chassaing](http://twitter.com/thinkb4coding)'s [FsUno.Prod](https://github.com/thinkbeforecoding/FsUno.Prod) 
Functional Event Sourcing sample.

NB be sure to read the [FsUno.Prod Journey](http://thinkbeforecoding.github.io/FsUno.Prod/).

# Why ?

_Most importantly_, it contains [NEventStore](https://github.com/NEventStore/NEventStore) v5 integration with the aim of integrating that via a PR against `FsUno.Prod`

As part of this, the Command Handler and other aspects have been revised to be able to work consistenly across both GES and NES (and assumes stores to be `async`).

**NB THIS IS ONLY A FORK - while most ideas will hopefully make it in (as issues/PRs) into `FsUno.Prod`, the aim continues to be to have the actual showcase be `FsUno.Prod`**

# Feature Notes vs FsUno.Prod

1. Supports 2 store types (NES, GES)
2. Demonstrates event type naming encoding scheme
3. Demonstrates selective projection scheme (all but neutral to whether one is using NES or GES)
4. Separates Event Encoding from Serialization
5. Separates Serialization from Event Storage
6. Does not rely on `empty` State elements or `initial` method on State - `state` arguments supplied to `handle` and `evolve` are `option`s

# Major TODOs

1. Further pursuit of generalized Command Processing strategies (outside of just rambling 'Acceptance Test' code)
	e.g. enriching commands etc.
2. Further pursuit of generalized Projection Processing strategies inc 
	e.g. awaiting in-memory projection completion 
3. Experiment more with the [unquote based] test DSLs and/or layer on some wrappers
4. Expose access to store-specific headers etc.

# Style notes vs FsUno.Prod

There are also some style variances from FsUno.Prod. In order of my desire to getting them embodied in the FsUno.Prod codebase:

1. `Uno.fs` should not have any code - Builders are separated
2. I've 'already' renamed `apply` to `evolve` (not yet agreed in FsUno.Prod)
3. No .Domain suffixes on assemblies or namespace
4. I've used unquote for the tests to show an alternate (slightly quirky) DSL. The big advantage is that there's no other test infrastructure to write / copy between test suites
5. I've pushed the Command messages out into the UL file too
6. Does not rely on `empty` State elements or `initial` method on State - `state` arguments supplied to `handle` and `evolve` are `option`s

# Open questions

1. is `handle state command` (which I ended up using in this codebase by accident in the pursuit of brevity) more correct than `handle command event` ? (if it is, that's an FsUno.Prod issue; if not, more brackets in the test-DSL)
2. Is the [arguably positive] tension (one'll be forced to pull stuff out of the body if you want it to stay screen-sized) caused by having all command-handlers in a single pattern match vs having a [handler mapping function](https://github.com/thinkbeforecoding/FsUno.Prod/blob/61515550855e413306de20acfad316311425188e/FsUno.Domain/Game.fs#L160-L162) a good tradeoff ?