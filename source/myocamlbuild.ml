open Ocamlbuild_plugin
open Command

let sandbox =
  try Sys.getenv "SANDBOX" with Not_found ->
  Printf.eprintf "\nBuild error: $SANDBOX environment variable not found.\n\n";
  exit 1

let apron_libs = [ "bigarray"; "gmp"; "apron"; "box"; "polka" ]

let flag_inc = [ A "-I"; A (sandbox ^ "/lib") ]
let flag_nat = flag_inc @ List.map (fun lib -> A (lib ^ ".cmxa")) apron_libs
let flag_byt = flag_inc @ List.map (fun lib -> A (lib ^ ".cma")) apron_libs

let () =
  dispatch begin function
  | After_rules ->
    flag ["compile"; "use_apron"] (S flag_inc);
    flag ["link"; "ocaml"; "native"; "use_apron"] (S flag_nat);
    flag ["link"; "ocaml"; "byte"; "use_apron"] (S flag_byt);
  | _ -> ()
  end
