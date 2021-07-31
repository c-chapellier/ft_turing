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
