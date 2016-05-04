(* Quentin Carbonneaux - 2016 *)

let input_file = ref ""
let main_func = ref None
let dump_ai = ref false

let usagemsg = "usage: pasta [OPTIONS] FILE\n"
let argspec = Arg.align
  [ "-func", Arg.String (fun s -> main_func := Some s),
    "<name>: Analyze this function body"
  ; "-dump-ai", Arg.Set dump_ai,
    " Display abstract interpretation output"
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
    let fstart =
      match !main_func with
      | Some f -> f
      | None ->
        if List.length imp_file = 1
        then (List.hd imp_file).Types.fun_name
        else "start"
    in
    if not (List.exists (fun f -> f.Types.fun_name = fstart) imp_file) then
      failarg (Printf.sprintf "cannot find function '%s' to analyze" fstart);
    let g_file = List.map Graph.from_imp imp_file in
    let _ = Graph.AbsInt.analyze ~dump:!dump_ai g_file fstart in
    0
  with
  | Utils.Error -> 1

let () = Utils.exit (main ())
