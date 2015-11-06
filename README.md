# CIS 194 homework solutions

These are my solutions to the [CIS 194 Haskell course](http://www.seas.upenn.edu/~cis194/spring13/lectures.html).

**IMPORTANT**: I am a complete beginner in Haskell, so do not take these as the "correct" answers or the best way to do things. :)

## Quick start

You will need to have [Stack](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md) installed (you can also follow [this guide](http://seanhess.github.io/2015/08/04/practical-haskell-getting-started.html) to get up and running).

Clone this repo then run:

```bash
$ stack setup
```

This can take a minute the first time you run it.

When set up is complete, launch the REPL with:

```bash
$ stack ghci
```

In the REPL, load an exercise with:

```haskell
Prelude> :l Homework.A.CreditCard
```

Build and run the tests with:

```bash
$ stack test
```

