# Standard Library

Zebra has a number of potentially conflicting goals. We aim to provide high performance, low level control, and enable a vast and powerful ecosystem. In order achieve those goals, we settled on this structure for the standard library. Zebra's standard library is divided into three sections:

1. `rt`
2. `std`
3. `contrib`

## `rt`

This contains the language runtime, and other libraries that Zebra needs to run. It has the basic building blocks like memory management and concurrency primitives. Knobs to tweak the async runtime and garbage collector with also be featured here.

```rust
import rt

fn main() {
  rt.sync.setMaxThreads(1) // Set the max number of threads for the async runtime
  rt.gc.collect() // Run garbage collection
}
```

## `std`

This makes up the bulk of the standard library, and is what would traditionally called the *standard library*. You could implement these yourself if you wanted to, but you probably shouldn't without good reason. This includes:

- File system driver
- Command line IO
- Networking
- Data structures like dynamic arrays and hashmaps
- Higher level concurrency primitives
- Low level time/date

## `contrib`

Contains libraries that could be implemented separately, but are standardized for the sake of the language and its ecosystem. These are the batteries from *batteries included*

- HTTP client/server
- Serialization framework (could also be in `std`)
- Logging
- High level time/date
