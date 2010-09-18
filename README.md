# Haltavista #

Search for haskell functions in installed libraries by providing input/output
examples.

## Usage ##

Example session : 

    brauner@worf:~$ haltavista
    1 1 2
    1 2 3
    <EOF>

    Prelude (+)

Under the hood, uses:

  - hint for type inference;
  - hoogle for a list of candidates;
  - hint for testing.

## Credits ##

Hoogle calling facility has been copy-pasted (and later modified) from the [Yi]
project.

[Yi]: http://www.haskell.org/haskellwiki/Yi
