# Redis protocol library for OCaml

ocaml-redis-protocol is a tiny (~100 loc) self-contained implementation of the [Redis Serialization Protocol (RESP)](http://redis.io/topics/protocol) for OCaml. It is not a Redis client and does not provide (or assume) any network functionality - it simply encodes Redis commands and data.

For example, you could use it as base for a redis (or [Disque](https://github.com/antirez/disque)) client or if you want to use RESP for your own server application.

## Example usage

Decoding redis values from RESP:

    >> open Redis_protocol
    >> "*2\r\n*3\r\n:1\r\n:2\r\n:3\r\n*2\r\n+Foo\r\n-Bar\r\n"
       |> Resp.decode_exn;;
    Array (Some [|Array (Some [|Integer "1";Integer "2";Integer "3"|]);
                  Array (Some [|Simple_string "Foo";Error "Bar"|])|])

Encoding redis values to RESP:

    >> open Redis_protocol
    >> Array (Some [|Simple_string "Foo";Error "Bar"|])
       |> Resp.encode_exn;;
    "*2\r\n+Foo\r\n-Bar\r\n"

Encoding redis commands to RESP:

    >> open Redis_protocol
    >> Redis_command.build "SET" ["FOO_KEY";"BAR_VALUE"]
        |> Resp.encode_exn
    "*3\r\n$3\r\nSET\r\n$6\r\nFOO_KEY\r\n$8\r\nBAR_VALUE\r\n"

Pretty-printing redis values:

    >> open Redis_protocol
    >> "*3\r\n+Foo\r\n:123\r\n-Bar\r\n"
       |> Redis_protocol.Resp.decode_exn
       |> Redis_protocol.to_string_hum
       |> Printf.printf "%s";;
    1) "Foo"
    2) 123
    3) ERR Bar

For the full API see the [.mli](https://github.com/sgreben/ocaml-redis-protocol/blob/master/src/redis_protocol.mli).

## Linking with your app

Assuming you've installed it (see below), you can use the library by loading the package `redis-protocol` with ocamlfind:

    ocamlbuild -use-ocamlfind -pkg redis-protocol my_app.native

## Build

ocaml-redis-protocol is built with [dune](https://github.com/ocaml/dune). To build the libary:

    dune build

The resulting library files will be in `_build/default/src`

### Docs

Then to compile the html docs:

    dune build @doc

The resulting documentation files will be in `_build/default/_doc`

An HTML page is generated with _odoc_ - browse it (e.g. with _Google Chrome_):

    google-chrome ./_build/default/_doc/index.html

## Test

    dune runtest

## Install

    dune install

If you're running opam, the library will be installed into the opam-defined lib path.
