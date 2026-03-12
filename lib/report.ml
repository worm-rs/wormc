(* converts span to grace's range *)
let span_to_range (source: Grace.Source.t ref) (span: Span.t) : Grace.Range.t =
  Grace.Range.create ~source: !source
    (Grace.Byte_index.of_int span.start_pos)
    (Grace.Byte_index.of_int span.end_pos)

(* constructs diagnostic *)
let diag
  ~(notes: Grace.Diagnostic.Message.t list)
  ~(labels: Grace.Diagnostic.Label.t list)
  ~(severity: Grace.Diagnostic.Severity.t)
  ~(code: int)
  ~(msg: Grace.Diagnostic.Message.t) =
  Grace.Diagnostic.create ~notes ~labels ~code severity msg

(* reports diagnostic *)
let report (diag: int Grace.Diagnostic.t) =
  Format.printf
    "%a@."
    (* lambda that prints passed diag to formatter *)
    (fun ppf diag -> Grace_ansi_renderer.pp_diagnostic
      ppf
      diag
      (* prints code in format `0004` *)
      ~code_to_string:(fun idx -> Format.sprintf "%04d" idx))
    diag;
  exit 1
