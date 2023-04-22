FROM docker.io/ocaml/opam:alpine-ocaml-5.1
USER root
RUN opam init --disable-sandboxing
RUN opam install dune containers fmt
RUN opam install utop ocp-indent
RUN opam install cmdliner
RUN opam install angstrom
RUN opam install oseq
RUN opam install eio
RUN apk add linux-headers
RUN opam install eio_main
RUN opam install timedesc
RUN opam install domainslib
