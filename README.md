# `jq` implementation in Haskell

A clone of `jq` done as part of the Functional Programming course of TUDelft.

## Build
```
> stack build
```

## Install

```
> stack install
```

this installs your executable to `~/.local/bin` by default (on *nix), make sure it's in $PATH

## Use

```
> echo '{"this" : "that"}' | jq-clone '.this'
```

or

```
> echo '{"this" : "that"}' | stack run -- '.this'
```
