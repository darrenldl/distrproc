# Distrproc

Prototype of Erlang style process/actor model library in OCaml

## Technical details/discussions

- Right now using Domainslib task pool under the hood

- Not very clear if we can mimic Erlang's `receive ... after` syntax
  (or Ada's `select ... end` syntax) without having control over
  effects ourselves,
  as we need to be able to distinguish "waiting for message" and
  "waiting on work" promises.

