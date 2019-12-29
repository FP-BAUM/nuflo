# Lambda-unif

### How to use

For more information, see the [Stack guide](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

#### Install
  - install stack from your OS
  - run `stack setup`
  - run `stack install`
  - run `stack build`

#### Add new depedencies
  - run `stack install depedency_name`

#### Load a single file
  - run `stack ghci`
  - run `:set -package necessary_package_name` for example `:set -package HUnit`
  - run `:l path`

#### Run tests
  - run `stack ghci`
  - run `:set -package HUnit`
  - run `:l test/spec_name.spec.hs `
  - run `runTestTT tests`
