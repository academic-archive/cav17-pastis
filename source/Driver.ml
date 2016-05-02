(* Quentin Carbonneaux - 2016 *)

let input_file = ref ""
let main_func = ref ""

let usage_msg = "usage: pasta [OPTIONS] FILE\n"
let argspec = Arg.align
  [ "-func", Arg.String (fun s -> main_func := s),
    "<name>: Analyze this function body"
  ]

letr main () =
  let set_input s =
    if !input_file <> "" then
      raise (Arg.Bad "too many input files");
    input_file := s
  in
  Arg.parse argspec set_input usage_msg;
  if !input_file = "" then begin
    Arg.usage argspec usage_msg;
    exit 1;
  end;
  try
    let efmt = Format.err_formatter in
    let imp_file = IMP.parse_file efmt !input_file in
    let _g_file = List.map Graph.from_imp imp_file in
    ()
  with
  | Utils.Error -> exit 1

let () = main ()
