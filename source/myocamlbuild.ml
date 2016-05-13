open Ocamlbuild_plugin
open Command

let debug = false

let sandbox =
  try Sys.getenv "SANDBOX" with Not_found ->
  Printf.eprintf "\nBuild error: $SANDBOX environment variable not found.\n\n";
  exit 1

let cxx = try Sys.getenv "CXX" with Not_found -> "g++"

(* Flag to use ocaml libraries and headers in the sandbox. *)
let flag_sandbox_lib = [ A "-I"; A (sandbox ^ "/lib") ]
let flag_sandbox_include = [ A "-ccopt"; A ("-I" ^ sandbox ^ "/include") ]

(* Apron specific flags. *)
let apron_libs = [ "bigarray"; "gmp"; "apron"; "box"; "polka" ]
let flag_apron_nat = flag_sandbox_lib @ List.map (fun lib -> A (lib ^ ".cmxa")) apron_libs
let flag_apron_byt = flag_sandbox_lib @ List.map (fun lib -> A (lib ^ ".cma")) apron_libs

(* Clp specific flags. *)
let clp_stubs = "Clp_Stubs.o"
let clp_c_static_libs = [ "Clp"; "CoinUtils" ]
let clp_c_dynamic_libs = [ "bz2"; "z" ]
let flag_clp_nat =
  [ if debug then A "-verbose" else N ] @
  [ A clp_stubs; A "-cc"; A cxx ] @
  List.concat (
    List.map (fun lib -> [ A "-cclib"; A (sandbox ^ "/lib/lib" ^ lib ^ ".a") ])
      clp_c_static_libs ) @
  List.concat (
    List.map (fun lib -> [ A "-cclib"; A ("-l" ^ lib) ])
      clp_c_dynamic_libs )
let flag_clp_byt = A "-custom" :: flag_clp_nat

let () =
  dispatch begin function
  | After_rules ->
    (* Make OCaml files with flag use_clp depend on Clp_Stubs.o. *)
    dep ["compile"; "ocaml"; "use_clp"] [clp_stubs];

    flag ["compile"; "ocaml"; "use_apron"] (S flag_sandbox_lib);
    flag ["compile"; "c"; "use_clp"] (S flag_sandbox_include);
    flag ["link"; "ocaml"; "native"; "use_apron"] (S flag_apron_nat);
    flag ["link"; "ocaml"; "byte"; "use_apron"] (S flag_apron_byt);
    flag ["link"; "ocaml"; "native"; "use_clp"] (S flag_clp_nat);
    flag ["link"; "ocaml"; "byte"; "use_clp"] (S flag_clp_byt);

  | _ -> ()
  end
