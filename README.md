# ft_turing
Functionnal implementation of a single infinite tape Turing machine.

## Install Haskell & Cabal
```bash
curl https://get-ghcup.haskell.org -sSf | sh
```

## Compile & Run
```bash
cabal run exe:turing configFile tape
```

## Machines

### Unary Substraction
```bash
unary_sub.json 111-11=  -> 1
unary_sub.json 11-11=   ->
unary_sub.json 111-1=   -> 11
```

### Unary Addition
```bash
unary_sub.json 111+11=  -> 11111
unary_sub.json 1+1=     -> 11
```

### Is Palindrome
```bash
is_palindrome.json aba  -> y
is_palindrome.json abba -> y
is_palindrome.json abab -> n
```

### Is 0n1n
```bash
is_0n1n.json 000111     -> y
is_0n1n.json 011        -> y
is_0n1n.json 011100     -> n
is_0n1n.json 1          -> n
```

### Is 02n
```bash
is_0n1n.json 0          -> n
is_0n1n.json 00         -> y
is_0n1n.json 000        -> n
is_0n1n.json 0000       -> y
```

## Test
```bash
cabal run test:turing-test
```

## Misc
```hs
usefulResources :: [String]
usefulResources = [
    "http://dev.stephendiehl.com/hask/",
    "https://hackage.haskell.org/package/CheatSheet-1.11/src/CheatSheet.pdf"
    ]
```
