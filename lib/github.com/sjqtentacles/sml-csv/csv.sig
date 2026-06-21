(* csv.sig

   A small, dependency-free CSV/TSV reader and writer for Standard ML,
   following RFC 4180 with a few documented leniencies.

   Data model
   ----------
   A CSV document is a list of records; each record is a list of string
   fields (`string list`). No interpretation is placed on the field
   contents -- they are raw, unquoted strings. The empty document parses
   to no rows.

   Parsing (RFC 4180 + lenient)
   ----------------------------
   - Fields are separated by `delim` (`#","` for CSV, `#"\t"` for TSV).
   - A field may be quoted with the double-quote character `#"\""`. Inside
     a quoted field a doubled quote `""` denotes a single literal quote,
     and the field may freely contain the delimiter, CR and LF.
   - An unquoted field runs up to the next delimiter or record separator.
   - Records are separated by CRLF or by a bare LF; both are accepted and
     treated identically. A lone CR (not followed by LF) inside unquoted
     text is kept verbatim as data.
   - A single trailing record separator at the very end of the input does
     NOT produce a spurious empty final record. (An input that is entirely
     empty parses to zero rows; `"a\n"` parses to one row `["a"]`.)
   - Malformed input -- currently only an unterminated quoted field -- raises
     `Csv`.

   Writing
   -------
   - Fields are joined with `delim` and records with the configured line
     ending (CRLF by default, per RFC 4180).
   - A field is quoted iff it contains `delim`, a double-quote, CR or LF;
     embedded quotes are escaped by doubling. Other fields are emitted as-is.
   - `write` does not append a trailing line ending after the final record,
     so `parse (write rows) = rows` holds (see the README round-trip note). *)

signature CSV =
sig
  (* Raised on malformed input (e.g. an unterminated quoted field). *)
  exception Csv of string

  type field = string
  type row   = field list

  (* Result of a parse: an optional header row (present iff `hasHeader`
     was true and the input was non-empty) plus the remaining data rows. *)
  type document = { header : row option, rows : row list }

  (* Parse `input` using `delim` as the field separator. When `hasHeader`
     is true the first record is returned as `header` and the rest as
     `rows`; otherwise `header` is NONE and every record is in `rows`. *)
  val parse : { delim : char, hasHeader : bool } -> string -> document

  (* Convenience: parse with no header, returning every record. *)
  val parseRows : char -> string -> row list

  (* Parse using the first record as a header and map each subsequent
     record to (name, value) pairs. A short record is zipped against the
     header up to its own length; extra fields beyond the header are
     paired with the empty name "". An empty input yields []. *)
  val parseNamed : { delim : char } -> string -> (string * string) list list

  (* Line endings used by `write`. *)
  datatype newline = CRLF | LF

  (* Serialize rows, quoting fields per RFC 4180. Records are joined with
     CRLF; no trailing line ending is emitted. *)
  val write : { delim : char } -> row list -> string

  (* Like `write` but with a configurable record separator. *)
  val writeWith : { delim : char, newline : newline } -> row list -> string
end
