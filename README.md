# packed

Data packed tight like a tin can of sardines.

## Motivation
The libraries `bytestring`, `text`, `aeson`, and `attoparsec` all have a few
things in common. They are known to be some of the fastest Haskell libraries
to exist for widespread production use, they are well-vetted by the Haskell
community, and their approaches to their internal API are old. `packed` implements
its own `bytestring`, `text`, `aeson`, and parsers (not necessarily in the spirit
of `attoparsec`, also there are a number of parsers in this library).
For `packed`, we want to avoid using the C FFI, instead wishing to implement things
in pure GHC Haskell. Most things in `packed` are implemented using `-XUnboxedSums`
and `-XUnboxedTuples`. The aforementioned libraries were created when `stream fusion`
was still a new hype-word. Our implementations of `ByteString` and `Text` don't provide
slicing information in the datatype - since this is usually just a source of extra
allocation, as (in our experience) slicing is not needed so often in a practical setting.

In short, `packed` is a different design of the APIs of these libraries, and
the benchmarks show promising results. Most operations in the benchmarks currently
show a 2x to 10x speedup over the current de-facto libraries.

## Building and Stability
Do not attempt to build this library with versions of GHC earlier than 8.6.
The constraint on `base` in the cabal file should prevent this from happening.
There is nothing from `base-4.12` that is actually needed here. The reason
for this constraint is that the `UnboxedSums` extension, which was introduced
in GHC 8.2, produced corrupted code that crashed at runtime. One such problem
was corrected in GHC 8.4. Soon after, another was discovered. It was fixed
in GHC 8.6. The maintainers of this library are unaware of any other problems
with `UnboxedSums`. However, keep in mind that this extension is not widely used,
and there is a possibility, however remote we hope it is, that a sufficiently
complex use of `UnboxedSums` may unearth yet another problem.

The API of `packed` is unstable, and will most likely remain so until the nearing
of GHC 8.8 or 9.0.
This repository currently contains a bunch of libraries which will be split
off into their own libraries at some point, but for now, because of the
instability of the API, it is easier to keep them all under one roof.