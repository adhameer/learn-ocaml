FROM ocaml/opam2:alpine-3.7-ocaml-4.05 as compilation
LABEL Description="learn-ocaml building" Vendor="OCamlPro"

WORKDIR learn-ocaml

COPY learn-ocaml.opam learn-ocaml.opam.locked ./
RUN sudo chown -R opam:nogroup .

ENV OPAMYES true
RUN echo 'archive-mirrors: [ "https://opam.ocaml.org/cache" ]' >> ~/.opam/config
RUN opam switch 4.05
RUN echo 'pre-session-commands: ["sudo" "apk" "add" depexts]' >>~/.opam/config
RUN opam update
RUN opam install . --deps-only --locked

ADD static static
ADD translations translations
ADD src src
ADD scripts scripts
ADD Makefile Makefile
ADD demo-repository demo-repository
RUN echo "bytelink += [\"-custom\"]" >addflags.ocp
RUN sudo chown -R opam:nogroup .

ENV OPAMVERBOSE 1
RUN opam install . --destdir /home/opam/install-prefix




FROM alpine:3.7 as program
LABEL Description="learn-ocaml app manager" Vendor="OCamlPro"

RUN apk update
RUN apk add ncurses-libs libev dumb-init git
RUN addgroup learn-ocaml
RUN adduser learn-ocaml -DG learn-ocaml

VOLUME ["/repository"]
RUN mkdir -p /sync && chown learn-ocaml:learn-ocaml /sync
VOLUME ["/sync"]
EXPOSE 8080
EXPOSE 8443

USER learn-ocaml
WORKDIR /home/learn-ocaml

COPY --from=compilation /home/opam/install-prefix /usr

CMD ["build","serve"]
ENTRYPOINT ["dumb-init","learn-ocaml","--sync-dir=/sync","--repo=/repository"]
