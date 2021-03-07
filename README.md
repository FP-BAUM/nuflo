<p align="center">
  <img width="150" height="150" src='./logo.png'>
</p>

[![Build Status](https://travis-ci.org/FP-BAUM/nuflo.svg?branch=master)](https://travis-ci.org/FP-BAUM/nuflo)
# Ñuflo language (~)

## Setup
First you need have `ghc` installed:

### Mac OS
```bash
brew install ghc
```

### Windows

- Install `chocolatey` using `powershell`

- Install `ghc` and `make` using `chocolatey`

- Change the default output encoding to `utf8`:
```bash
& "$env:windir\system32\chcp.com" 65001
```

## Build
To generates a binary `nuflo`:
```bash
cd .../nuflo/

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
