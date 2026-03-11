(* converts span to grace's range *)
let span_to_range (source: Grace.Source.t ref) (span: Common.span) : Grace.Range.t =
  Grace.Range.create ~source: !source
    (Grace.Byte_index.of_int span.start_pos)
    (Grace.Byte_index.of_int span.end_pos)

(* constructs diagnostic *)
let diag (severity: Grace.Diagnostic.Severity.t) (msg: Grace.Diagnostic.Message.t) (labels: Grace.Diagnostic.Label.t list) =
