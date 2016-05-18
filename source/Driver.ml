(* Quentin Carbonneaux - 2016 *)

let input_file = ref ""
let main_func = ref None
let dump_ai = ref false
let dump_stats = ref false

let usagemsg = Printf.sprintf "usage: %s [OPTIONS] FILE\n" Sys.argv.(0)
let argspec = Arg.align
  [ "-func", Arg.String (fun s -> main_func := Some s),
    "<fn> Analyze the specified function"
  ; "-dump-ai", Arg.Set dump_ai,
    " Display abstract interpretation results"
  ; "-dump-stats", Arg.Set dump_stats,
    " Display statistics of the size-tracking analysis"
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
    let imp_file = IMP.parse_file !input_file in
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
    let ai_results = Graph.AbsInt.analyze ~dump:!dump_ai g_file fstart in
    let query =
      let open Polynom in
        (Poly.of_monom (Monom.of_var "z") (+1.))
    in
    let st_results = Analysis.run ai_results g_file fstart query in
    begin match st_results with
    | None ->
      Format.printf "Sorry, I could not find a bound.@."
    | Some p ->
      Format.printf "Upper bound for %a: %a@."
        Polynom.Poly.print query
        Polynom.Poly.print p
    end;
    if !dump_stats then begin
      let open Format in
      let { Analysis.num_lpvars; num_lpcons; max_focus } = Analysis.stats in
      printf "@.Statistics:@.    @[<v>";
      printf "Number of LP variables: %d@ " num_lpvars;
      printf "Number of LP constraints: %d@ " num_lpcons;
      printf "Maximum focus functions in use: %d" max_focus;
      printf "@]@.";
    end;
    0
  with Utils.Error -> 1

let () = Utils.exit (main ())
