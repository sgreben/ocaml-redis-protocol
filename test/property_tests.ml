open Redis_protocol
open Redis_protocol.Resp

let passed = ref 0
let max_encoding_length = ref 0
let log s = Printf.printf "%s" s; flush stdout
let assert_equal ~actual ~expected = assert (actual = expected); incr passed

module Well_formed = struct

  let _ = log "well-formed data tests...\n"

  let check_via_inverse ts =
    for i = 0 to Array.length ts - 1 do
      let t = ts.(i) in
      let s = Resp.encode_exn t in
      let n = Bytes.length s in
      if n > !max_encoding_length then max_encoding_length := n;
      assert_equal ~actual:(Resp.decode_exn s) ~expected:t
    done

  let check pairs =
    for i = 0 to Array.length pairs - 1 do
      let t,str = pairs.(i) in
      (assert_equal ~actual:(Resp.decode_exn str) ~expected:t;
       assert_equal ~actual:(Resp.encode_exn t) ~expected:str;)
    done

  let simple_strings =
    [|Simple_string "", "+\r\n";
      Simple_string "\x00", "+\x00\r\n";
      Simple_string "abc", "+abc\r\n";
      Simple_string "abc123", "+abc123\r\n";
      Simple_string "1034123113512312361231337123", "+1034123113512312361231337123\r\n";
      Simple_string "abc123abc123abc123abc123", "+abc123abc123abc123abc123\r\n";
      Simple_string "abc123abc123abc123abc123", "+abc123abc123abc123abc123\r\n"; |]
  let _ = log "\tsimple strings..."
  let _ = check simple_strings
  let _ = log "done\n"

  let error_strings =
    [|Error "", "-\r\n";
      Error "abc", "-abc\r\n";
      Error "\x00", "-\x00\r\n";
      Error "abc123", "-abc123\r\n";
      Error "abc123abc123abc123abc123", "-abc123abc123abc123abc123\r\n";
      Error "abc123abc123abc123abc123", "-abc123abc123abc123abc123\r\n";|]
  let _ = log "\terror strings..."
  let _ = check error_strings
  let _ = log "done\n"

  let integers =
    [|Integer "0", ":0\r\n";
      Integer "-123", ":-123\r\n";
      Integer "1234567890", ":1234567890\r\n";
      Integer "1234567890123456789012345678901234567890", ":1234567890123456789012345678901234567890\r\n";
      Integer "1234567890123456789012345678901234567890", ":1234567890123456789012345678901234567890\r\n";|]
  let _ = log "\tintegers..."
  let _ = check integers
  let _ = log "done\n"

  let bulk_strings =
    [|Bulk_string None, "$-1\r\n";
      Bulk_string (Some ""), "$0\r\n\r\n";
      Bulk_string (Some "foobar"), "$6\r\nfoobar\r\n";
      Bulk_string (Some "a"), "$1\r\na\r\n";
      Bulk_string (Some "\x00"), "$1\r\n\x00\r\n";
      Bulk_string (Some "\x01"), "$1\r\n\x01\r\n";
      Bulk_string (Some "\x02"), "$1\r\n\x02\r\n";
      Bulk_string (Some "\x03"), "$1\r\n\x03\r\n";
      Bulk_string (Some "\x00\x01\x02"), "$3\r\n\x00\x01\x02\r\n";
      Bulk_string (Some "\r\n\x00\x01\x02"), "$5\r\n\r\n\x00\x01\x02\r\n";
      Bulk_string (Some "ab"), "$2\r\nab\r\n";
      Bulk_string (Some "abc"), "$3\r\nabc\r\n";
      Bulk_string (Some "\r\n"), "$2\r\n\r\n\r\n";
      Bulk_string (Some "\r\n\r\n"),"$4\r\n\r\n\r\n\r\n";
      Bulk_string (Some "ab\r\n"), "$4\r\nab\r\n\r\n";
      Bulk_string (Some "abc\r\n"), "$5\r\nabc\r\n\r\n";
      Bulk_string (Some "1234567890123456789012345678901234567890"), "$40\r\n1234567890123456789012345678901234567890\r\n";
      Bulk_string (Some "12345678901234567890123456789012345678901234567890123456789012345678901234567890"), "$80\r\n12345678901234567890123456789012345678901234567890123456789012345678901234567890\r\n";|]
  let _ = log "\tbulk strings..."
  let _ = check bulk_strings
  let _ = log "done\n"

  let arrays = [|Array (Some [|Array (Some [|Integer "1";Integer "2";Integer "3"|]);Array (Some [|Simple_string "Foo";Error "Bar"|]);|]),
                 "*2\r\n*3\r\n:1\r\n:2\r\n:3\r\n*2\r\n+Foo\r\n-Bar\r\n";
                 Array (Some [|Bulk_string (Some "foo"); Bulk_string (Some "bar")|]),
                 "*2\r\n$3\r\nfoo\r\n$3\r\nbar\r\n";
                 Array (Some [|Bulk_string (Some "SET"); Bulk_string (Some "key"); Bulk_string (Some "value")|]),
                 "*3\r\n$3\r\nSET\r\n$3\r\nkey\r\n$5\r\nvalue\r\n";
                 Array None,
                 "*-1\r\n";
                 Array (Some [||]),
                 "*0\r\n";|]
  let _ = log "\tarrays..."
  let _ = check arrays
  let _ = log "done\n"

  let arrays =
    let rec array = function
      | 0 -> Array (Some [||])
      | (-1) -> Array None
      | n -> Array (Some (Array.init n (fun i -> match (n+i) mod 7 with
        | 0 -> fst simple_strings.(i mod Array.length simple_strings)
        | 1 -> fst error_strings.(i mod Array.length error_strings)
        | 2 -> fst integers.(i mod Array.length integers)
        | 3 -> fst bulk_strings.(i mod Array.length bulk_strings)
        | 4 -> fst arrays.(i mod Array.length arrays)
        | 5 -> array (n / 2)
        | _ -> array (-1))))
    in Array.init 200 array
  let _ = log "\tlarge arrays round-trip test..."
  let _ = check_via_inverse arrays
  let _ = log "done\n"

end

module Not_well_formed = struct
  exception Not_a_redis_exception

  let _ = log "ill-formed data tests...\n"

  module Encode = struct
    let _ = log "\t(encoding)\n"

    let check = Array.iter (fun (s,e_expected) ->
      try (let _ = Resp.encode_exn s in raise Not_a_redis_exception)
      with e -> assert_equal ~actual:e ~expected:e_expected)

    let bad_simple_strings =
      [|Simple_string "\r", Simple_string_contains_CR_or_LF;
        Simple_string "abc\n", Simple_string_contains_CR_or_LF;
        Simple_string "\r\n", Simple_string_contains_CR_or_LF;
        Simple_string "abc123abc123abc123abc123\r\n", Simple_string_contains_CR_or_LF;
        Simple_string "a\nbc123abc123ab\rc123abc123", Simple_string_contains_CR_or_LF |]
    let _ = log "\tsimple strings..."
    let _ = check bad_simple_strings
    let _ = log "done\n"

    let bad_error_strings =
      [|Error "\r", Simple_string_contains_CR_or_LF;
        Error "abc\n", Simple_string_contains_CR_or_LF;
        Error "\r\n", Simple_string_contains_CR_or_LF;
        Error "abc123abc123abc123abc123\r\n", Simple_string_contains_CR_or_LF;
        Error "a\nbc123abc123ab\rc123abc123", Simple_string_contains_CR_or_LF |]
    let _ = log "\terror strings..."
    let _ = check bad_error_strings
    let _ = log "done\n"

  end

  module Decode = struct
    let _ = log "\t(decoding)\n"

    let check = Array.iter (fun s ->
      try (let _ = Resp.decode_exn s in raise Not_a_redis_exception)
      with e -> assert_equal ~actual:e ~expected:Invalid_encoding)

    let bad_simple_strings = [|"+\r\r\n";"+\r\r\n";"+";"+\r";"+\n";"+";"";|]
    let _ = log "\tsimple strings..."
    let _ = check bad_simple_strings
    let _ = log "done\n"

    let bad_error_strings = [|"-\r\r\n";"-\r\r\n";"-";"-\r";"-\n";"-";""|]
    let _ = log "\terror strings..."
    let _ = check bad_error_strings
    let _ = log "done\n"

    let bad_integers = [|":";":123";":0"|]
    let _ = log "\tintegers..."
    let _ = check bad_integers
    let _ = log "done\n"

    let bad_bulk_strings = [|"$0aaa\r\n";"$100aaa\r\n";"$-1abc\r\n";"$0aaa\r\n";"$3\r\naaaX\r\n";"$300\r\nabc\r\n"|]
    let _ = log "\tbulk strings..."
    let _ = check bad_bulk_strings
    let _ = log "done\n"

    let bad_arrays = [|"*3\r\n:0\r\n:1\r\n";"*3\r\n:0\r\n:1\r\n\r\n";"*-13\r\n:0\r\n:1\r\n\r\n";"*-22\r\n:0\r\n:1\r\n\r\n";|]
    let _ = log "\tarrays..."
    let _ = check bad_arrays
    let _ = log "done\n"

  end

end

let _ = Printf.printf "%d tests passed (max encoding length tested: %f mb).\n\n" !passed ((float_of_int !max_encoding_length)/.1024.0/.1024.0)