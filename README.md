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
n + n * n / 2 -> O(n^2)

### Universal Unary Substraction
```bash
./app/configs/universal.json '(.|e|a.a.>1a1>-a->=b.<|b1c=<-e.<|c1c1<-d-<|d.d.<1a.>!@111-11=)'   -> 1
./app/configs/universal.json '(.|e|a.a.>1a1>-a->=b.<|b1c=<-e.<|c1c1<-d-<|d.d.<1a.>!@11-11=)'    ->
./app/configs/universal.json '(.|e|a.a.>1a1>-a->=b.<|b1c=<-e.<|c1c1<-d-<|d.d.<1a.>!@111-1=)'    -> 11
```
unary sub complexity * (20 + n) -> (n + n * n / 2) * (20 + n) -> O(n^3)

### Unary Addition
```bash
unary_sub.json 111+11=  -> 11111
unary_sub.json 1+1=     -> 11
```
3n -> O(n)

### Is Palindrome
```bash
is_palindrome.json aba  -> y
is_palindrome.json abba -> y
is_palindrome.json abab -> n
```
somme(1..n) + 3n/2  -> (n * (n + 1) / 2) + 3n/2 -> O(n^2)

### Is 0n1n
```bash
is_0n1n.json 000111     -> y
is_0n1n.json 011        -> y
is_0n1n.json 011100     -> n
is_0n1n.json 1          -> n
```
2n -> O(n)

### Is 02n
```bash
is_0n1n.json 0          -> n
is_0n1n.json 00         -> y
is_0n1n.json 000        -> n
is_0n1n.json 0000       -> y
```
2n -> O(n)

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
