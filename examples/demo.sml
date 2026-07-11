(* demo.sml - parse a CSV document with a header and a quoted field
   containing an embedded comma and doubled quotes, inspect it via
   `parse`/`parseNamed`, then round-trip it back to text with `writeWith`
   under both CRLF and LF line endings. Deterministic: identical output on
   every run and both compilers. *)

structure C = Csv

val input =
  "name,city,note\r\n" ^
  "Alice,\"Springfield, IL\",plain\r\n" ^
  "Bob,Chicago,\"he said \"\"hi\"\"\"\r\n"

val doc = C.parse { delim = #",", hasHeader = true } input

val () = print "Parsed CSV (header + quoted field with embedded comma):\n"
val () = print ("  header: " ^ String.concatWith " | " (valOf (#header doc)) ^ "\n")
val () =
  List.app
    (fn row => print ("  row:    " ^ String.concatWith " | " row ^ "\n"))
    (#rows doc)

val () = print "\nNamed rows (parseNamed):\n"
val named = C.parseNamed { delim = #"," } input
val () =
  List.app
    (fn fields =>
       print ("  " ^ String.concatWith ", "
                        (List.map (fn (k, v) => k ^ "=" ^ v) fields) ^ "\n"))
    named

val () = print "\nRound trip via writeWith (CRLF vs LF):\n"
val allRows = valOf (#header doc) :: #rows doc
val crlfOut = C.writeWith { delim = #",", newline = C.CRLF } allRows
val lfOut   = C.writeWith { delim = #",", newline = C.LF } allRows
val () = print ("  CRLF length = " ^ Int.toString (String.size crlfOut) ^ "\n")
val () = print ("  LF length   = " ^ Int.toString (String.size lfOut) ^ "\n")
val () = print ("  parseRows (writeWith LF) = original rows: "
                ^ Bool.toString (C.parseRows #"," lfOut = allRows) ^ "\n")
