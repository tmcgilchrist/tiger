open Syntax

let quick =
  let open Generators in
  QCheck.Test.make ~count:1000
    ~name:"QC - parse arith"
    QCheck.(arbitrary_arith) (fun l ->
      let a = Parser.prog Lexer.lexer @@ Lexing.from_string @@ Syntax.Pretty.print_to_string @@ l in
      (* a == l *)

      (* TODO Suspect line numbers are off and the comparison is wrong *)
      match a.L.item = l.L.item with
      | true -> true
      | false -> let _ =
        Core_kernel.(Out_channel.printf "expected: %s %s\n" (Syntax.Pretty.print_to_string l) (Location.to_string l.L.loc);
                     Out_channel.printf "got: %s %s\n" (Syntax.Pretty.print_to_string a) (Location.to_string a.L.loc);)
                in false
    )
let _ =
  QCheck_runner.run_tests_main [quick]
