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
and `-XUnboxedTuples`. The aforementioned libraries were created before
of `UnboxedSums` and before `resizeMutableByteArray#`. These push the design
decisions of `packed` in a different direction. Additionally, we do not
support any stream fusion. It is unclear whether users of `text` and `bytestring`
benefit from stream fusion in practice, and it would complicate the implementation.
We provide both sliced and unsliced variants of `ByteString` and `Text`:

- Sliced bytes: `Bytes`
- Unsliced bytes: `ByteArray`
- Sliced text: `Text`
- Unsliced text: `SmallText`

In certain situations, the metadata needed for slicing is an unnecessary
source of allocations. Users of `packed` are expected to know what they need.

In short, `packed` is a different design of the APIs of these libraries, and
some preliminary benchmarks on the parser show promising results.

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
