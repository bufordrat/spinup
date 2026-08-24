FROM alpine:3.20 AS base

FROM base AS builder

RUN apk update && \
    apk upgrade && \
    apk add build-base --no-cache opam git

RUN addgroup ocamldev && adduser -G ocamldev -D ocamldev

USER ocamldev

RUN opam init --bare -a -y --disable-sandboxing \
    && opam update

RUN opam switch create static-5.2.0 ocaml-variants.5.2.0+options ocaml-option-static

WORKDIR /app

RUN mkdir ./app ./lib

COPY dune-project ##[cl,pname].opam ./

COPY app/* ./app

COPY lib/* ./lib

COPY test/* ./test

RUN opam repository add dldc https://dldc.lib.uchicago.edu/opam && \
    opam update && \
    eval $(opam env) && \
    opam install . --deps-only --yes

RUN opam exec -- dune build --release && \
    cp app/##[cl,pname].exe /app/##[cl,pname]

FROM builder AS test

CMD ##[left] "opam", "exec", "--", "dune", "runtest", "--release" ##[right]

FROM base AS runtime

COPY --from=builder /app/##[cl,pname] /usr/local/bin/##[cl,pname]

CMD ##[left] "/usr/local/bin/##[cl,pname]" ##[right]
