(* csv.sml

   RFC 4180 CSV/TSV reader and writer, dependency-free (Basis only).
   The parser is a small character-level state machine over the input
   string; see csv.sig for the data model and the documented leniencies. *)

structure Csv :> CSV =
struct
  exception Csv of string

  type field = string
  type row   = field list
  type document = { header : row option, rows : row list }

  datatype newline = CRLF | LF

  val quote = #"\""
  val cr    = #"\r"
  val lf    = #"\n"

  (* Parse the whole input into a list of records. The result never
     contains a spurious trailing empty record produced by a final record
     separator; a wholly empty input yields []. *)
  fun parseAll delim input : row list =
    let
      val n = String.size input
      fun at i = String.sub (input, i)

      (* Accumulate fields of the current record (reversed) and completed
         records (reversed). `rev`-ing happens once at the end. *)

      (* Scan an unquoted field starting at i; returns (fieldChars, nextIndex,
         atRecordEnd, atInputEnd). The field runs to the next delim or to a
         record separator (CRLF or bare LF). *)
      fun scanUnquoted i acc =
        if i >= n then (String.implode (rev acc), i, true, true)
        else
          let val c = at i in
            if c = delim then (String.implode (rev acc), i + 1, false, false)
            else if c = lf then (String.implode (rev acc), i + 1, true, false)
            else if c = cr andalso i + 1 < n andalso at (i + 1) = lf
                 then (String.implode (rev acc), i + 2, true, false)
            else scanUnquoted (i + 1) (c :: acc)
          end

      (* Scan a quoted field; i points just past the opening quote. A
         doubled quote is a literal quote; a lone closing quote ends the
         field. After the closing quote we expect a delim, a record
         separator, or end of input. *)
      fun scanQuoted i acc =
        if i >= n then raise Csv "unterminated quoted field"
        else
          let val c = at i in
            if c = quote then
              if i + 1 < n andalso at (i + 1) = quote
              then scanQuoted (i + 2) (quote :: acc)        (* escaped quote *)
              else
                let val s = String.implode (rev acc)
                    val j = i + 1
                in
                  if j >= n then (s, j, true, true)
                  else
                    let val d = at j in
                      if d = delim then (s, j + 1, false, false)
                      else if d = lf then (s, j + 1, true, false)
                      else if d = cr andalso j + 1 < n andalso at (j + 1) = lf
                           then (s, j + 2, true, false)
                      else (* RFC 4180 forbids text after a closing quote. *)
                           raise Csv "unexpected text after quoted field"
                    end
                end
            else scanQuoted (i + 1) (c :: acc)
          end

      (* Parse one field starting at i. *)
      fun scanField i =
        if i < n andalso at i = quote then scanQuoted (i + 1) []
        else scanUnquoted i []

      (* Parse records. `fields` is the reversed accumulator of the current
         record; `rows` the reversed accumulator of completed records.
         `pending` is true when a field is owed -- at record start or just
         after a delimiter -- so a trailing delimiter still yields a final
         empty field, while a trailing record separator does not produce a
         spurious empty record. *)
      fun loop i pending fields rows =
        if i >= n then
          (if pending
           then rev (rev (("" : field) :: fields) :: rows)
           else case fields of
                    [] => rev rows
                  | _  => rev (rev fields :: rows))
        else
          let val (f, j, recEnd, _) = scanField i in
            if recEnd
            then loop j false [] (rev (f :: fields) :: rows)
            else loop j true (f :: fields) rows
          end
    in
      if n = 0 then [] else loop 0 true [] []
    end

  fun parseRows delim input = parseAll delim input

  fun parse { delim, hasHeader } input =
    let val all = parseAll delim input in
      if hasHeader
      then (case all of
                []      => { header = NONE, rows = [] }
              | h :: rs => { header = SOME h, rows = rs })
      else { header = NONE, rows = all }
    end

  (* Zip a header against a record, pairing extra fields beyond the header
     with the empty name "". A short record simply yields fewer pairs. *)
  fun zipNamed (h :: hs) (v :: vs) = (h, v) :: zipNamed hs vs
    | zipNamed []        (v :: vs) = ("", v) :: zipNamed [] vs
    | zipNamed _         []        = []

  fun parseNamed { delim } input =
    case parseAll delim input of
        []           => []
      | hdr :: recs  => List.map (fn r => zipNamed hdr r) recs

  (* ---- writing ---- *)

  fun newlineStr CRLF = "\r\n"
    | newlineStr LF   = "\n"

  fun needsQuote delim s =
    let
      fun bad c = c = delim orelse c = quote orelse c = cr orelse c = lf
    in
      List.exists bad (String.explode s)
    end

  fun escapeField delim s =
    if needsQuote delim s
    then
      let
        (* double every embedded quote *)
        val body =
          String.translate (fn c => if c = quote then "\"\"" else String.str c) s
      in
        "\"" ^ body ^ "\""
      end
    else s

  fun writeWith { delim, newline } rows =
    let
      val sep = String.str delim
      fun writeRow r = String.concatWith sep (List.map (escapeField delim) r)
    in
      String.concatWith (newlineStr newline) (List.map writeRow rows)
    end

  fun write { delim } rows = writeWith { delim = delim, newline = CRLF } rows
end
