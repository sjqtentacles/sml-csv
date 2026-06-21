# sml-csv

[![CI](https://github.com/sjqtentacles/sml-csv/actions/workflows/ci.yml/badge.svg)](https://github.com/sjqtentacles/sml-csv/actions/workflows/ci.yml)

A small CSV/TSV reader and writer for Standard ML, following
[RFC 4180](https://www.rfc-editor.org/rfc/rfc4180) with a few documented
leniencies.

A document is a list of records and each record is a list of string fields
(`string list`). Quoted fields, embedded delimiters, doubled quotes and
embedded newlines are handled; both CRLF and bare LF are accepted as record
separators. TSV is just CSV with `delim = #"\t"`.

## Portability

Pure Standard ML using only the Basis library -- no FFI, no threads. Verified
on **MLton** and **Poly/ML**.

## Building and testing

```sh
make test        # build + run the suite under MLton (default)
make test-poly   # run the suite under Poly/ML
make all-tests   # run under both
make clean
```

## Installing with smlpkg

```sh
smlpkg add github.com/sjqtentacles/sml-csv
smlpkg sync
```

Then reference the library basis from your own `.mlb`:

```
lib/github.com/sjqtentacles/sml-csv/sml-csv.mlb
```

For Poly/ML, `use` the `csv.sig` and `csv.sml` sources in order.

## Usage

```sml
(* Parse, treating the first record as a header. *)
val doc = Csv.parse { delim = #",", hasHeader = true }
            "name,age\nAlice,30\nBob,25"
(* #header doc = SOME ["name", "age"]                       *)
(* #rows doc   = [["Alice", "30"], ["Bob", "25"]]           *)

(* Map each record against the header. *)
val rows = Csv.parseNamed { delim = #"," }
             "name,age\nAlice,30\nBob,25"
(* [[("name","Alice"),("age","30")], [("name","Bob"),("age","25")]] *)

(* Quoted fields: commas, doubled quotes and newlines are preserved. *)
val one = Csv.parseRows #"," "\"a,b\",\"he said \"\"hi\"\"\",\"x\ny\""
(* [["a,b", "he said \"hi\"", "x\ny"]] *)

(* Serialize, quoting where required (CRLF between records). *)
val out = Csv.write { delim = #"," } [["a,b", "c"], ["d\"e", "f"]]
(* "\"a,b\",c\r\n\"d\"\"e\",f" *)

(* TSV. *)
val tsv = Csv.parseRows #"\t" "a\tb\nc\td"   (* [["a","b"],["c","d"]] *)
```

## API summary

| Function | Description |
| --- | --- |
| `parse : {delim, hasHeader} -> string -> document` | Parse into `{header, rows}`. |
| `parseRows : char -> string -> row list` | Parse every record (no header). |
| `parseNamed : {delim} -> string -> (string * string) list list` | Map records against the first record as header. |
| `write : {delim} -> row list -> string` | Serialize; CRLF between records. |
| `writeWith : {delim, newline} -> row list -> string` | Serialize with `CRLF` or `LF`. |
| `exception Csv of string` | Raised on malformed input. |

## RFC 4180 notes & decisions

- **Quoting on parse.** A field may be quoted with `"`; inside quotes a
  doubled `""` is one literal quote, and the delimiter, CR and LF are kept
  verbatim. An unquoted field runs to the next delimiter or record separator.
- **Line endings on parse.** CRLF and bare LF are both accepted and treated
  identically, so the same data parses the same regardless of newline style.
- **Trailing newline.** A single trailing record separator does *not* create a
  spurious empty final record: `"a\n"` parses to `[["a"]]`, and a wholly empty
  input parses to `[]`. A *trailing delimiter* does produce a final empty field
  (`",,"` -> `[["", "", ""]]`).
- **Line ending on write.** `write` joins records with CRLF (RFC 4180) and does
  *not* append a trailing line ending, so `parseRows delim (write {delim} rows)
  = rows`. Use `writeWith` to choose `LF` instead.
- **Quoting on write.** A field is quoted iff it contains the delimiter, a
  double-quote, CR or LF; embedded quotes are doubled.
- **Errors.** An unterminated quoted field (or text after a closing quote)
  raises `Csv`.

## License

MIT. See [LICENSE](LICENSE).
