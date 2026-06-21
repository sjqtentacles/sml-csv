(* Tests for sml-csv. Parsing/quoting behaviour follows RFC 4180 with the
   leniencies documented in csv.sig. Output is deterministic across compilers. *)

structure CsvTests =
struct
  open Harness

  (* Helpers to render rows for failure messages and equality checks. *)
  fun rowToString xs = "[" ^ String.concatWith "," (List.map (fn s => "\"" ^ s ^ "\"") xs) ^ "]"
  fun rowsToString rs = "[" ^ String.concatWith ";" (List.map rowToString rs) ^ "]"

  fun checkRows name (expected, actual) =
    if expected = actual then check name true
    else (check name false; print ("    " ^ rowsToString expected ^ " <> " ^ rowsToString actual ^ "\n"))

  val comma = #","
  fun parseC s = Csv.parseRows comma s

  fun run () =
    let
      val () = section "parse: basic unquoted fields"
      val () = checkRows "single row" ([["a", "b", "c"]], parseC "a,b,c")
      val () = checkRows "two rows (LF)" ([["a", "b"], ["c", "d"]], parseC "a,b\nc,d")
      val () = checkRows "empty fields" ([["", "", ""]], parseC ",,")
      val () = checkRows "leading/trailing empty" ([["", "a", ""]], parseC ",a,")
      val () = checkRows "single field single row" ([["hello"]], parseC "hello")

      val () = section "parse: line endings & trailing newline"
      val () = checkRows "CRLF rows" ([["a", "b"], ["c", "d"]], parseC "a,b\r\nc,d")
      val () = checkRows "CRLF == LF"
                 (parseC "x,y\nz,w", parseC "x,y\r\nz,w")
      val () = checkRows "trailing LF no spurious row" ([["a"]], parseC "a\n")
      val () = checkRows "trailing CRLF no spurious row" ([["a", "b"]], parseC "a,b\r\n")
      val () = checkRows "empty input -> no rows" ([], parseC "")
      val () = checkRows "blank line is an empty single field" ([["a"], [""], ["b"]], parseC "a\n\nb")

      val () = section "parse: quoted fields"
      val () = checkRows "simple quoted" ([["a", "b"]], parseC "\"a\",\"b\"")
      val () = checkRows "quoted comma" ([["a,b", "c"]], parseC "\"a,b\",c")
      val () = checkRows "doubled quote -> literal quote" ([["a\"b"]], parseC "\"a\"\"b\"")
      val () = checkRows "embedded newline in quotes" ([["line1\nline2", "x"]], parseC "\"line1\nline2\",x")
      val () = checkRows "embedded CRLF in quotes" ([["a\r\nb"]], parseC "\"a\r\nb\"")
      val () = checkRows "quoted then more rows"
                 ([["a,b", "c"], ["d", "e"]], parseC "\"a,b\",c\nd,e")
      val () = checkRows "all the things"
                 ([["he said \"hi\", then left", "ok"]],
                  parseC "\"he said \"\"hi\"\", then left\",ok")

      val () = section "parse: errors"
      val () = checkRaises "unterminated quote" (fn () => parseC "\"abc")
      val () = checkRaises "unterminated quote mid-row" (fn () => parseC "a,\"bc\ndef")

      val () = section "parse: header handling"
      val doc = Csv.parse { delim = comma, hasHeader = true } "name,age\nAlice,30\nBob,25"
      val () = checkBool "header present"
                 (true, #header doc = SOME ["name", "age"])
      val () = checkRows "header rows"
                 ([["Alice", "30"], ["Bob", "25"]], #rows doc)
      val doc2 = Csv.parse { delim = comma, hasHeader = false } "a,b\nc,d"
      val () = checkBool "no header NONE" (true, #header doc2 = NONE)
      val () = checkRows "no header rows" ([["a", "b"], ["c", "d"]], #rows doc2)
      val docE = Csv.parse { delim = comma, hasHeader = true } ""
      val () = checkBool "empty input header NONE" (true, #header docE = NONE)
      val () = checkRows "empty input rows" ([], #rows docE)

      val () = section "parseNamed"
      val named = Csv.parseNamed { delim = comma } "name,age\nAlice,30\nBob,25"
      val () = checkBool "first record mapped"
                 (true, List.nth (named, 0) = [("name", "Alice"), ("age", "30")])
      val () = checkBool "second record mapped"
                 (true, List.nth (named, 1) = [("name", "Bob"), ("age", "25")])
      val () = checkInt "parseNamed count" (2, List.length named)
      val () = checkBool "parseNamed empty -> []"
                 (true, Csv.parseNamed { delim = comma } "" = [])

      val () = section "write & round-trip"
      val () = checkString "write simple" ("a,b,c", Csv.write { delim = comma } [["a", "b", "c"]])
      val () = checkString "write two rows (CRLF)"
                 ("a,b\r\nc,d", Csv.write { delim = comma } [["a", "b"], ["c", "d"]])
      val () = checkString "write quotes embedded comma"
                 ("\"a,b\",c", Csv.write { delim = comma } [["a,b", "c"]])
      val () = checkString "write doubles quotes"
                 ("\"a\"\"b\"", Csv.write { delim = comma } [["a\"b"]])
      val () = checkString "write quotes newline"
                 ("\"a\nb\"", Csv.write { delim = comma } [["a\nb"]])
      val () = checkString "writeWith LF"
                 ("a,b\nc,d", Csv.writeWith { delim = comma, newline = Csv.LF } [["a", "b"], ["c", "d"]])

      (* Round-trip property: parse (write rows) = rows *)
      val roundTripCases =
        [ [["a", "b", "c"]]
        , [["a", "b"], ["c", "d"]]
        , [["a,b", "c"]]
        , [["a\"b", "c"]]
        , [["line1\nline2", "x"], ["y", "z"]]
        , [["he said \"hi\", then left", "ok"]]
        , [["", "", ""]]
        , [["with\r\ncrlf", "tab\tinside"]]
        , [["solo"]]
        ]
      val rtOk =
        List.all
          (fn rows => parseC (Csv.write { delim = comma } rows) = rows)
          roundTripCases
      val () = check "round-trip parse (write rows) = rows" rtOk

      val () = section "ragged rows"
      val () = checkRows "ragged preserved"
                 ([["a", "b", "c"], ["d"], ["e", "f"]], parseC "a,b,c\nd\ne,f")
      val ragNamed = Csv.parseNamed { delim = comma } "a,b,c\n1,2\n3,4,5,6"
      val () = checkBool "ragged named short row"
                 (true, List.nth (ragNamed, 0) = [("a", "1"), ("b", "2")])
      val () = checkBool "ragged named extra field"
                 (true, List.nth (ragNamed, 1) = [("a", "3"), ("b", "4"), ("c", "5"), ("", "6")])

      val () = section "TSV"
      val tab = #"\t"
      val () = checkRows "tsv parse"
                 ([["a", "b"], ["c", "d"]], Csv.parseRows tab "a\tb\nc\td")
      val () = checkString "tsv write"
                 ("a\tb", Csv.write { delim = tab } [["a", "b"]])
      val () = checkBool "tsv round-trip"
                 (true, Csv.parseRows tab (Csv.write { delim = tab } [["a,comma", "b"]]) = [["a,comma", "b"]])
    in
      ()
    end
end
