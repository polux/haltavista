# Haltavista #

Search for haskell functions in installed libraries by providing input/output
examples.

## Requirements ##

[Hoogle] has to be installed.

## Usage ##

Example session 1:

    brauner@worf:~$ haltavista
    2 2 4
    <EOF>

    Prelude (*)
    Prelude (+)
    Prelude (^)

Example session 2 (refining search 1):

    brauner@worf:~$ haltavista
    2 2 4
    1 2 3
    <EOF>

    Prelude (+)

Example session 3 (higher-order functions):

    brauner@worf:~$ haltavista 
    (+1) (+2) (1,1) (2,3)
    <EOF>

    Data.Graph.Inductive.Query.Monad (><)

Under the hood, uses:

  - hint for type inference;
  - hoogle to get a list of candidate functions;
  - hint for testing.

## Credits ##

Hoogle calling facility has been copy-pasted (and later modified) from the [Yi]
project.

## Todo ##

- catch stack overflows (is that possible?)
- catch other errors (should do but doesn't for some reason)

[Hoogle]: http://hackage.haskell.org/package/hoogle
[Yi]: http://www.haskell.org/haskellwiki/Yi
