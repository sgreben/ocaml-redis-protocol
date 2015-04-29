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
        |> String.escaped
        |> Printf.printf "\"%s\"";;
    "*3\r\n$3\r\nSET\r\n$6\r\nFOO_KEY\r\n$8\r\nBAR_VALUE\r\n"

Pretty-printing redis values:

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

ocaml-redis-protocol is built with ocamlbuild and has a Makefile that calls it. To build the libary and the docs, do

    $ make

The documentation is in `redis_protocol.docdir/index.html`, so to view it use e.g.

    $ google-chrome redis_protocol.docdir/index.html

## Test

    $ make test

## Install

ocaml-redis-protocol supports ocamlfind. To install, just do

    $ make install

If you're running opam, the library will be installed into the opam-defined lib path.
