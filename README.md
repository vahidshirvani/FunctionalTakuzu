FunctionalTakuzu
================
Course: Advanced Functional Programming

Project: Takuzu (Haskell)

Group: 1

Authors: Emanuel Stöckli, Vahid Shirvani

## Installation and deployment

Execute the following commands after opening a terminal and moving to the project folder.

```bash 
$ cabal sandbox init 
$ cabal install --enable-tests
```

It will first create a folder .cabal-sandbox and then build the whole project with its dependencies in this folder.
If everything worked fine you should find an executable with the name tic-tac-logic.

```bash 
$ ls .cabal-sandbox/bin
```

To deploy the project outside of the sandbox just execute the following command 

```bash 
$ cabal build
```

### Testing

```bash 
$ cabal build test
$ cabal test --show-details=always
```
