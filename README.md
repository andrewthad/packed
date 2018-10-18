# packed

Data packed tight like a tin can of sardines.

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
