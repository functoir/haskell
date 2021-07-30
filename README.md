# haskell

## Elementary Haskell

***

NOTE: These projects were implemented from a learning standpoint, so some best practices may have been foregone, or, *there is just a better, different way to do it.* That's okay. I'm not at the top of the Haskell learning curve. I would be happy to hear suggestions or ideas, or general comments about my code. Thank you!

#### Here's how the packages are organized:

1. Each directory for individual concepts has an accompanying [Makefile](https://en.wikipedia.org/wiki/Make_(software))

   * To build, run `make`.

   * To run a test routine, run `make test`.

   * To clean up, run `make clean`.

   * NOTE: `make test` automatically builds before and cleans up after itself.

2. Larger directories implementing modules are configured to use [Cabal](https://www.haskell.org/cabal/), Haskell's compile manager.

   * To build, run `cabal build`.

   * To run a test routine, run `cabal run`.

3. **BUT, which is which?**

   * All Makefile directories do not have subdirectories. (While it is possible to use `GNU Make` with subdirectories, why not let `cabal` handle all the chaos?)
   * Likewise, all `cabal` directories have at least one subdirectory, as `cabal` requires one to hold all the source files.

4. **"Oh my god, this is confusing"**

   * Package handling tends to get *reasonably* messy as the project size grows. And, which is more, the cabal directories would be a clusterfuck if they used make, and the Makefile directories would be unneededly overcomplicated if they depended on cabal. TL;DR: It is what it is -- get used to it.

***

&copy; [siavava](https://siavava.me)
