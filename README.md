# rearrange-afrp

This is the code associated with the paper _Functional Reactive Programming, Rearranged_ - an implementation of Oxbow.

## Running this code

To run it, perform one of the following:

* `stack run l rf` will generate `generated/Test0.hs` as in Section 6.1 of the paper. This contains a Yampa implementation and an Oxbow implementation of a randomly generated proc-notation program.
* `stack bench` will benchmark the current `generated/Test0.hs`, as in Section 6.2.
* `stack repl` will give you an interactive prompt, as per usual. You can type `runExamples` to see a few examples (stored in `app/Examples.hs`) compile and run. If you want to build your own programs, you can do so from here or in another file.

## Writing your own programs

To build your own programs, import the following:

```haskell
import RAFRP -- for the running functions
import AFRP -- for the AFRP combinators
import GenProc.ProcTH (gap) -- if you want to use our proc desugaring given by [gap| ... ]
```

Then you can just use the arrow combinators as you are used to.

## File structure

The structure of files is as follows:

* `Data.Memory` and `Data.Type` contain a subset of the implementation of Keating and Gale's rearrange \[Keating21]. Parts of their implementation which we do not use are removed, and their main `Memory` monad has been replaced with ` MIO `. `Control.Effect` contains the main `Effect` definition from Orchard and Petricek's implementation of graded monads \[Orchard14].
* `GenProc` contains our implementation of a proc desugaring using `haskell-src-exts`, as briefly discussed in Section 5. It is adapted from a more complete proc desugaring written by the authors.
* The top level files (those in `src/` but no subfolder) are our implementation of SFRP using rearrange.
    * `AFRP` contains all of the type and GADT definitions defined throughout Section 4.
    * `GraphRearrange` contains the value-level implementation of rearrange, as mentioned at the end of Section 6.2. This was used so we could compile larger programs without running out of memory.
    * `MakeMIO` contains the definition of `ToMIO`. (Section 4.3.)
    * `Naming` contains the definition of `AssignMemory`. (Section 4.2.)
    * `RAFRP` contains the final definitions for running the whole transformation, as in Section 4.4. `makeAFRP` utilises the rearrange library, while `makeRearrangeable` utilises our runtime version.
    * `Rearrange` is from the rearrange library, and provides a handy interface for importing all of its definitions.

## Data collection

Data from the benchmarks is available in `test-results.ods` (LibreOffice Calc, but can be opened in other spreadsheet software), with a CSV version containing just the raw data in `test-results.csv`.
The corpus was generated via `many_gen.sh`, and the benchmark results collected using `bench_corpus.sh`. This took some inspiration from [Chupin and Nilsson's SFRP implementation](https://gitlab.com/chupin/scalable-frp/-/tree/master?ref_type=heads).

## Bibliography

\[Keating21]: Keating, F. and Gale, M.B., 2021, August. Graded monads and type-level programming for dependence analysis. In Proceedings of the 14th ACM SIGPLAN International Symposium on Haskell (pp. 27-40).
\[Orchard14]: Orchard, D. and Petricek, T., 2014, September. Embedding effect systems in Haskell. In Proceedings of the 2014 ACM SIGPLAN Symposium on Haskell (pp. 13-24).