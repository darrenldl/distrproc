# Distrproc

Prototype of Erlang style process/actor model library in OCaml

## Demo

- See `debug/main.ml` for a runnable example

## Technical details/discussions

- Uses `Eio` under the hood

- Not sure how to distribute tasks across domains - probably need
  upstream support from `Eio`

- `Eio` fiber API is flexible enough to mimic selective receive
  with timeout (see `Distrproc.Selective.recv`)
