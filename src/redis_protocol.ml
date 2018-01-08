type t =
  | Simple_string of string
  | Error of string
  | Integer of string
  | Bulk_string of string option
  | Array of t array option

module Resp = struct

  exception Simple_string_contains_CR_or_LF

  let rec encoding_length = function
    | Simple_string s      -> 3+String.length s
    | Error s              -> 3+String.length s
    | Integer s            -> 3+String.length s
    | Bulk_string None     -> 5
    | Bulk_string (Some s) -> 6+String.length (String.length s |> string_of_int)+String.length s
    | Array None           -> 5
    | Array (Some ss)      -> 3+(Array.length ss |> string_of_int |> String.length)+Array.fold_left (fun n t -> n+encoding_length t) 0 ss

  let ensure_no_crlf = String.iter (function
    | '\r' | '\n' -> raise Simple_string_contains_CR_or_LF
    | _ -> ())

  let rec encode_to_buffer_exn t b =
    let add_string s = Buffer.add_string b s in
    let crlf = "\r\n" in
    match t with
    | Simple_string s ->
      ensure_no_crlf s; add_string "+"; add_string s; add_string crlf
    | Error s ->
      ensure_no_crlf s; add_string "-"; add_string s; add_string crlf
    | Integer s ->
      ensure_no_crlf s; add_string ":"; add_string s; add_string crlf
    | Bulk_string (Some s) ->
      let n = String.length s |> string_of_int in
      add_string "$"; add_string n; add_string crlf; add_string s; add_string crlf
    | Array (Some ss) ->
      let n = Array.length ss |> string_of_int in
      add_string "*"; add_string n; add_string crlf; Array.iter (fun t -> encode_to_buffer_exn t b) ss
    | Bulk_string None -> add_string "$-1\r\n"
    | Array None -> add_string "*-1\r\n"

  let encode_exn t = encoding_length t |> Buffer.create |> fun b -> encode_to_buffer_exn t b; Buffer.to_bytes b |> Bytes.to_string

  let encode t = try Some (encode_exn t) with Simple_string_contains_CR_or_LF -> None

  exception Invalid_encoding

  exception Trailing_garbage of t*int

  let check b i c = if b.[i] <> c then raise Invalid_encoding

  let find_and_skip_crlf b i = let j = String.index_from b i '\r' in check b (j+1) '\n'; j+2

  let decode_length b = let rec loop n i = if b.[i] <> '\r' then loop (10*n+(Char.code b.[i]-48)) (i+1) else (check b (i+1) '\n';i+2,n) in loop 0

  let decode_simple_string b i = find_and_skip_crlf b i |> fun j -> j,String.sub b i (j-i-2)

  let decode_bulk_string b i = let j,n = decode_length b i in let s = String.sub b j n in check b (j+n) '\r';check b (j+n+1) '\n'; j+n+2,s

  let rec decode_array b i =
    let rec loop acc j n = if n = 0 then j,acc else let j,t = decode_from b j in loop (t::acc) j (n-1) in
    let j,n = decode_length b i in
    let j,ts = loop [] j n in j,List.rev ts |> Array.of_list

  and decode_from b i =
    match b.[i],b.[i+1] with
    | '+', _   (* simple string *) -> let j,s = decode_simple_string b (i+1) in j,Simple_string s
    | '-', _   (* error string *)  -> let j,s = decode_simple_string b (i+1) in j,Error s
    | ':', _   (* integer *)       -> let j,s = decode_simple_string b (i+1) in j,Integer s
    | '$', '-' (* bulk null *)     -> check b (i+2) '1';check b (i+3) '\r';check b (i+4) '\n';(i+5),Bulk_string None
    | '$', _   (* bulk string *)   -> let j,s = decode_bulk_string b (i+1) in j,Bulk_string (Some s)
    | '*', '-' (* array null *)    -> check b (i+2) '1';check b (i+3) '\r';check b (i+4) '\n';(i+5),Array None
    | '*', _   (* array *)         -> let j,ss = decode_array b (i+1) in j,Array (Some ss)
    | _                            -> raise Invalid_encoding

  let decode_exn b =
    let i,t = try decode_from b 0 with _ -> raise Invalid_encoding
    in if i <> String.length b then raise (Trailing_garbage (t,i)) else t

  let decode b = try Some (decode_exn b) with Invalid_encoding -> None

end

module Redis_command = struct

  let bs s = Bulk_string (Some s)
  let array a = Array (Some a)

  let build ~command arguments =
    array (Array.of_list @@ (Bulk_string (Some command))::(List.map (fun s -> (bs s)) arguments))
  let build0 command = array [|bs command|]
  let build1 ~command arg1 = array [|bs command; bs arg1|]
  let build2 ~command arg1 arg2 = array [|bs command; bs arg1; bs arg2|]
  let build3 ~command arg1 arg2 arg3 = array [|bs command; bs arg1; bs arg2; bs arg3|]
  let build4 ~command arg1 arg2 arg3 arg4 = array [|bs command; bs arg1; bs arg2; bs arg3; bs arg4|]
  let build5 ~command arg1 arg2 arg3 arg4 arg5 = array [|bs command; bs arg1; bs arg2; bs arg3; bs arg4; bs arg5|]
  let build6 ~command arg1 arg2 arg3 arg4 arg5 arg6 = array [|bs command; bs arg1; bs arg2; bs arg3; bs arg4; bs arg5; bs arg6|]

end

let rec to_string = function
  | Simple_string s -> Printf.sprintf "Simple_string \"%s\"" (String.escaped s)
  | Error s -> Printf.sprintf "Error \"%s\"" (String.escaped s)
  | Integer s -> Printf.sprintf "Integer \"%s\"" (String.escaped s)
  | Bulk_string (Some s) -> Printf.sprintf "Bulk_string (Some \"%s\")" (String.escaped s)
  | Bulk_string None -> Printf.sprintf "Bulk_string None"
  | Array (Some s) -> Printf.sprintf "Array (Some [|%s|])" (Array.to_list s |> List.map to_string |> String.concat ";")
  | Array None -> Printf.sprintf "Array None"

let rec to_string_hum' top i =
  let indent' = String.init i (fun _ -> ' ') in
  let indent = if (not top) then "" else indent' in
  function
  | Simple_string s -> Printf.sprintf "%s\"%s\"" indent (String.escaped s)
  | Error s -> Printf.sprintf "%sERR %s" indent (String.escaped s)
  | Integer s -> Printf.sprintf "%s%s" indent s
  | Bulk_string (Some s) -> Printf.sprintf "%s\"%s\"" indent (String.escaped s)
  | Bulk_string None -> Printf.sprintf "%s(nil)" indent
  | Array (Some s) -> Array.to_list s
    |> List.mapi (fun j t -> Printf.sprintf "%s%d) %s" (if j = 0 && (not top) then "" else indent') (j+1) (to_string_hum' false (i+1) t))
    |> String.concat "\n"
  | Array None -> Printf.sprintf "%s(nil)" indent

let to_string_hum = to_string_hum' true 0
