# Lambda-unif


## Setup
First you need have `ghc` installed:

### Mac OS
```bash
brew install ghc
``` 

## Build
To generates a binary `lambda-unif`:
```bash
cd .../lambda-unif/

make
```

## Run tests
```bash
make test
```

## Clean
To remove auxiliary and binary files:
```bash
make clean
```


## Development guide

### Add new test cases:
  To do this, you need:
  - Go to `.../test/TestMain.hs`
  - Import your new test file
  - Add it to `runAllTests` definition
