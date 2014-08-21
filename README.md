# What is this?

The code here is a fork of [Jérémie Chassaing](http://twitter.com/thinkb4coding)'s [FsUno.Prod](https://github.com/thinkbeforecoding/FsUno.Prod) 
Functional Event Sourcing sample.

NB be sure to read the [FsUno.Prod Journey](http://thinkbeforecoding.github.io/FsUno.Prod/).

# Why ?

Most importantly, it contains a spike of [NEventStore](https://github.com/NEventStore/NEventStore) v5 integration for FsUno.Prod 
(though depending on timelines the actual integration will be against v6 which will incorporate async support)

This should be considered a fork that will eventually expire - some ideas will hopefully make it in (as issues/PRs) into FsUno.Prod.

# Style notes

There are also some style variances from FsUno.Prod. In order of my desire to getting them embodied in the FsUno.Prod codebase:

1. Uno.fs should not have any code - Builders are separated
2. I've 'already' renamed apply to evolve (not yet agreed in FsUno.Prod)
3. No .Domain suffixes on assemblies or namespace
4. I've used unquote for the tests to show an alternate (slightly quirky) DSL. The big advantage is that there's nothing else to write
5. I've pushed the command messages out into the UL file too

# Open questions

1. is `handle state command` (which I ended up using in this codebase by accident in the pursuit of brevity) more correct than `handle command event` ? (if it is, that's an FsUno.Prod issue; if not, more brackets in the test-DSL)
2. Is the [arguably positive] tension (one'll be forced to pull stuff out of the body if you want it to stay screen-sized) caused by having all command-handlers in a single pattern match vs having a [handler mapping function](https://github.com/thinkbeforecoding/FsUno.Prod/blob/61515550855e413306de20acfad316311425188e/FsUno.Domain/Game.fs#L160-L162) a good tradeoff ?