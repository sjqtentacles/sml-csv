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

      val () = section "sml-check properties"

      (* Field-character generator weighted toward plain printable ASCII but
         also regularly emitting the delimiter, the quote character, CR and
         LF, so the quoting/escaping path is exercised, not just the plain
         one. `write` is documented to quote any field containing `delim`, a
         double-quote, CR, or LF and to double embedded quotes, so this
         should round-trip regardless of which of these a field contains. *)
      val genFieldChar =
        Check.frequency
          [ (80, Check.charRange (#" ", #"~"))
          , (5, Check.pure #",")
          , (5, Check.pure (Char.chr 34))   (* '"' *)
          , (5, Check.pure (Char.chr 13))   (* CR *)
          , (5, Check.pure (Char.chr 10))   (* LF *)
          ]
      val genField = Check.stringOf genFieldChar
      (* Rows have at least one field: `write` of a zero-field row produces
         "", and a blank input line parses back to a single empty-string
         field (see csv.sig), so a truly empty row can never round-trip. *)
      val genRow = Check.nonEmptyListOf genField
      (* A trailing row that is exactly one empty field contributes nothing
         but a bare record separator to `write`'s output, so it is
         textually indistinguishable from "no trailing record" and gets
         swallowed by the "a single trailing record separator does NOT
         produce a spurious empty final record" rule on reparse (csv.sig).
         `parse (write rows) = rows` therefore cannot hold whenever the last
         row is [""]; exclude that one degenerate shape. *)
      fun lastRowIsBlank rows =
        case List.rev rows of
            (r :: _) => r = [""]
          | [] => false
      val genRows = Check.filter (fn rows => not (lastRowIsBlank rows)) (Check.listOf genRow)

      fun showRows rs =
        "[" ^ String.concatWith ";"
                (List.map (fn r => "[" ^ String.concatWith ","
                                            (List.map (fn f => "\"" ^ f ^ "\"") r) ^ "]") rs)
              ^ "]"

      (* prop: parse (write rows) = rows for the comma delimiter, over
         arbitrary generated fields (including embedded delimiters, quotes,
         CR and LF). *)
      val () =
        Harness.check "prop: parseC (Csv.write rows) = rows (comma)"
          (case Check.quickCheck
                  (Check.forAll genRows showRows
                     (fn rows => parseC (Csv.write { delim = comma } rows) = rows)) of
               Check.Passed _ => true
             | Check.Failed _ => false)

      (* prop: the same round-trip holds for an arbitrary delimiter (TSV
         here), showing the quoting logic isn't hard-coded to comma. *)
      val () =
        Harness.check "prop: parseRows tab (Csv.write rows) = rows (tab)"
          (case Check.quickCheck
                  (Check.forAll genRows showRows
                     (fn rows => Csv.parseRows tab (Csv.write { delim = tab } rows) = rows)) of
               Check.Passed _ => true
             | Check.Failed _ => false)

      (* prop: writeWith with an LF record separator round-trips exactly like
         the CRLF default. *)
      val () =
        Harness.check "prop: parseC (Csv.writeWith {..LF} rows) = rows"
          (case Check.quickCheck
                  (Check.forAll genRows showRows
                     (fn rows =>
                        parseC (Csv.writeWith { delim = comma, newline = Csv.LF } rows) = rows)) of
               Check.Passed _ => true
             | Check.Failed _ => false)

      (* prop: a field forced to contain the delimiter, a quote, CR and LF
         all at once still round-trips correctly when embedded next to a
         plain neighbor field. *)
      val genNastyField = Check.map (fn s => ",\"\r\n" ^ s) genField
      val () =
        Harness.check "prop: field with delim+quote+CR+LF round-trips"
          (case Check.quickCheck
                  (Check.forAll genNastyField (fn s => "\"" ^ s ^ "\"")
                     (fn s => parseC (Csv.write { delim = comma } [[s, "x"]]) = [[s, "x"]])) of
               Check.Passed _ => true
             | Check.Failed _ => false)
    in
      ()
    end
end
