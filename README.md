This repo is a result of my attemptation for understanding Normalization by Evaluation. It's a combination of Chalmers's Programming Language Technology on implementing a call by need programming language and Checking Dependent Types with Normalization by Evaluation: A Tutorial (Haskell Version) done by David Thrane Christiansen. 


How to use this repo?
1. nix develop .#haskell 
2. make 
3. a linker file, named `nbe-lambda`, points to an exectuable built by the `cabal build` in the root directory
4. run `./nbe-lambda test.hs`
    


