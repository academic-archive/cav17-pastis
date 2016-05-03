(* Quentin Carbonneaux - 2016 *)

let input_file = ref ""
let main_func = ref ""

let usagemsg = "usage: pasta [OPTIONS] FILE\n"
let argspec = Arg.align
  [ "-func", Arg.String (fun s -> main_func := s),
    "<name>: Analyze this function body"
  ]
let annonarg s =
  if !input_file <> "" then
    raise (Arg.Bad "too many input files");
  input_file := s
let failarg msg =
  Printf.eprintf "%s: %s\n" Sys.argv.(0) msg;
  Arg.usage argspec usagemsg;
  exit 1

let main () =
  Arg.parse argspec annonarg usagemsg;
  if !input_file = "" then failarg "no input file provided";
  try
    let efmt = Format.err_formatter in
    let imp_file = IMP.parse_file efmt !input_file in
    let g_file = List.map Graph.from_imp imp_file in
    let _ = Graph.AbsInt.analyze ~debug:true g_file "start" in
    ()
  with
  | Utils.Error -> exit 1

let () = main ()
