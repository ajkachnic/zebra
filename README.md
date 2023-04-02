# zebra

Zebra is a research-project programming languages. It's design is based upon the
mantra:

> Optimize for developer happiness

## Project Goals

The goal *is **not** to create a production-ready language*. Rather, I'm
exploring some ideas I had for a language in one place.

## `zebrac`

`zebrac` is the compiler for the Zebra language. It's WIP, but when it's
finished, it will use Cranelift to generate machine code. Zebra's mantra also
applies to the compiler, so the compiler:

- Must to be fast (tight feedback loop)
- Must generate reasonably fast code (*80% of the optimizations in 20% of the time*)
- Must provide helpful error messages

Of course, these goals are also a work in progress

