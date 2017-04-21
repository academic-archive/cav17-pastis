Require Import pasta.Pasta.

Inductive proc: Type :=
  P_gx_default_print_page_copies.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_gx_default_print_page_copies_z := 1%positive.
Notation V_gx_default_print_page_copies__tmp := 2%positive.
Notation V_gx_default_print_page_copies_code := 3%positive.
Notation V_gx_default_print_page_copies_i := 4%positive.
Notation V_gx_default_print_page_copies_num_copies := 5%positive.
Notation V_gx_default_print_page_copies_pdev := 6%positive.
Notation V_gx_default_print_page_copies_prn_stream := 7%positive.
Definition Pedges_gx_default_print_page_copies: list (edge proc) :=
  (EA 1 (AAssign V_gx_default_print_page_copies_z (Some (ENum (0)))) 2)::
  (EA 2 (AAssign V_gx_default_print_page_copies__tmp
  (Some (EVar V_gx_default_print_page_copies_num_copies))) 3)::(EA 3 (AAssign
  V_gx_default_print_page_copies_i
  (Some (EVar V_gx_default_print_page_copies__tmp))) 4)::(EA 4 (AAssign
  V_gx_default_print_page_copies_code (Some (ENum (0)))) 5)::(EA 5 ANone 6)::
  (EA 6 AWeaken 7)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_gx_default_print_page_copies_code) s) >=
  (eval (ENum (0)) s))%Z)) 9)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_gx_default_print_page_copies_code) s) <
  (eval (ENum (0)) s))%Z)) 8)::(EA 8 AWeaken 15)::(EA 9 AWeaken 10)::
  (EA 10 (AAssign V_gx_default_print_page_copies_i
  (Some (EAdd (EVar V_gx_default_print_page_copies_i) (ENum (-1))))) 11)::
  (EA 11 ANone 12)::(EA 12 AWeaken 13)::(EA 13 (AGuard
  (fun s => ((eval (EVar V_gx_default_print_page_copies_i) s) >
  (eval (ENum (0)) s))%Z)) 16)::(EA 13 (AGuard
  (fun s => ((eval (EVar V_gx_default_print_page_copies_i) s) <=
  (eval (ENum (0)) s))%Z)) 14)::(EA 14 AWeaken 15)::(EA 16 AWeaken 17)::
  (EA 17 (AAssign V_gx_default_print_page_copies_code None) 18)::
  (EA 18 ANone 19)::(EA 19 ANone 20)::(EA 20 (AAssign
  V_gx_default_print_page_copies_z (Some (EAdd (ENum (1))
  (EVar V_gx_default_print_page_copies_z)))) 21)::(EA 21 AWeaken 7)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_gx_default_print_page_copies => Pedges_gx_default_print_page_copies
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_gx_default_print_page_copies => 15
     end)%positive;
  var_global := var_global
}.

Definition ai_gx_default_print_page_copies (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_gx_default_print_page_copies_z <= 0 /\ -1 * s V_gx_default_print_page_copies_z <= 0)%Z
   | 3 => (-1 * s V_gx_default_print_page_copies_z <= 0 /\ 1 * s V_gx_default_print_page_copies_z <= 0)%Z
   | 4 => (1 * s V_gx_default_print_page_copies_z <= 0 /\ -1 * s V_gx_default_print_page_copies_z <= 0)%Z
   | 5 => (-1 * s V_gx_default_print_page_copies_z <= 0 /\ 1 * s V_gx_default_print_page_copies_z <= 0 /\ 1 * s V_gx_default_print_page_copies_code <= 0 /\ -1 * s V_gx_default_print_page_copies_code <= 0)%Z
   | 6 => (-1 * s V_gx_default_print_page_copies_code <= 0 /\ 1 * s V_gx_default_print_page_copies_code <= 0 /\ 1 * s V_gx_default_print_page_copies_z <= 0 /\ -1 * s V_gx_default_print_page_copies_z <= 0)%Z
   | 7 => (-1 * s V_gx_default_print_page_copies_z <= 0)%Z
   | 8 => (-1 * s V_gx_default_print_page_copies_z <= 0 /\ 1 * s V_gx_default_print_page_copies_code + 1 <= 0)%Z
   | 9 => (-1 * s V_gx_default_print_page_copies_z <= 0 /\ -1 * s V_gx_default_print_page_copies_code <= 0)%Z
   | 10 => (-1 * s V_gx_default_print_page_copies_code <= 0 /\ -1 * s V_gx_default_print_page_copies_z <= 0)%Z
   | 11 => (-1 * s V_gx_default_print_page_copies_z <= 0 /\ -1 * s V_gx_default_print_page_copies_code <= 0)%Z
   | 12 => (-1 * s V_gx_default_print_page_copies_code <= 0 /\ -1 * s V_gx_default_print_page_copies_z <= 0)%Z
   | 13 => (-1 * s V_gx_default_print_page_copies_z <= 0 /\ -1 * s V_gx_default_print_page_copies_code <= 0)%Z
   | 14 => (-1 * s V_gx_default_print_page_copies_code <= 0 /\ -1 * s V_gx_default_print_page_copies_z <= 0 /\ 1 * s V_gx_default_print_page_copies_i <= 0)%Z
   | 15 => (-1 * s V_gx_default_print_page_copies_z <= 0)%Z
   | 16 => (-1 * s V_gx_default_print_page_copies_code <= 0 /\ -1 * s V_gx_default_print_page_copies_z <= 0 /\ -1 * s V_gx_default_print_page_copies_i + 1 <= 0)%Z
   | 17 => (-1 * s V_gx_default_print_page_copies_i + 1 <= 0 /\ -1 * s V_gx_default_print_page_copies_z <= 0 /\ -1 * s V_gx_default_print_page_copies_code <= 0)%Z
   | 18 => (-1 * s V_gx_default_print_page_copies_z <= 0 /\ -1 * s V_gx_default_print_page_copies_i + 1 <= 0)%Z
   | 19 => (-1 * s V_gx_default_print_page_copies_i + 1 <= 0 /\ -1 * s V_gx_default_print_page_copies_z <= 0)%Z
   | 20 => (-1 * s V_gx_default_print_page_copies_z <= 0 /\ -1 * s V_gx_default_print_page_copies_i + 1 <= 0)%Z
   | 21 => (-1 * s V_gx_default_print_page_copies_i + 1 <= 0 /\ -1 * s V_gx_default_print_page_copies_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_gx_default_print_page_copies (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_gx_default_print_page_copies_num_copies) <= z)%Q
   | 2 => (max0(s V_gx_default_print_page_copies_num_copies)
           + max0(s V_gx_default_print_page_copies_z) <= z)%Q
   | 3 => (max0(s V_gx_default_print_page_copies__tmp)
           + max0(s V_gx_default_print_page_copies_z) <= z)%Q
   | 4 => (max0(s V_gx_default_print_page_copies_i)
           + max0(s V_gx_default_print_page_copies_z) <= z)%Q
   | 5 => (max0(s V_gx_default_print_page_copies_i)
           + max0(s V_gx_default_print_page_copies_z) <= z)%Q
   | 6 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_gx_default_print_page_copies_i) (-1
                                                                    + s V_gx_default_print_page_copies_i))]
     (max0(s V_gx_default_print_page_copies_i)
      + max0(s V_gx_default_print_page_copies_z) <= z)%Q
   | 7 => (max0(-1 + s V_gx_default_print_page_copies_i)
           + max0(s V_gx_default_print_page_copies_z) <= z)%Q
   | 8 => hints
     [(*-1 0*) F_max0_ge_0 (-1 + s V_gx_default_print_page_copies_i);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_gx_default_print_page_copies_z)) (F_check_ge (s V_gx_default_print_page_copies_z) (0))]
     (max0(-1 + s V_gx_default_print_page_copies_i)
      + max0(s V_gx_default_print_page_copies_z) <= z)%Q
   | 9 => (max0(-1 + s V_gx_default_print_page_copies_i)
           + max0(s V_gx_default_print_page_copies_z) <= z)%Q
   | 10 => (max0(-1 + s V_gx_default_print_page_copies_i)
            + max0(s V_gx_default_print_page_copies_z) <= z)%Q
   | 11 => (max0(s V_gx_default_print_page_copies_i)
            + max0(s V_gx_default_print_page_copies_z) <= z)%Q
   | 12 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_gx_default_print_page_copies_z)) (F_check_ge (s V_gx_default_print_page_copies_z) (0))]
     (max0(s V_gx_default_print_page_copies_i)
      + max0(s V_gx_default_print_page_copies_z) <= z)%Q
   | 13 => (s V_gx_default_print_page_copies_z
            + max0(s V_gx_default_print_page_copies_i) <= z)%Q
   | 14 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_gx_default_print_page_copies_i) (-1
                                                                    + s V_gx_default_print_page_copies_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V_gx_default_print_page_copies_i)]
     (s V_gx_default_print_page_copies_z
      + max0(s V_gx_default_print_page_copies_i) <= z)%Q
   | 15 => (s V_gx_default_print_page_copies_z <= z)%Q
   | 16 => (s V_gx_default_print_page_copies_z
            + max0(s V_gx_default_print_page_copies_i) <= z)%Q
   | 17 => (s V_gx_default_print_page_copies_z
            + max0(s V_gx_default_print_page_copies_i) <= z)%Q
   | 18 => (s V_gx_default_print_page_copies_z
            + max0(s V_gx_default_print_page_copies_i) <= z)%Q
   | 19 => (s V_gx_default_print_page_copies_z
            + max0(s V_gx_default_print_page_copies_i) <= z)%Q
   | 20 => (s V_gx_default_print_page_copies_z
            + max0(s V_gx_default_print_page_copies_i) <= z)%Q
   | 21 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_gx_default_print_page_copies_i) (1);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_gx_default_print_page_copies_z) (0))) (F_max0_ge_0 (s V_gx_default_print_page_copies_z))]
     (-(1 # 1) + s V_gx_default_print_page_copies_z
      + max0(s V_gx_default_print_page_copies_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_gx_default_print_page_copies =>
    [mkPA Q (fun n z s => ai_gx_default_print_page_copies n s /\ annot0_gx_default_print_page_copies n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_gx_default_print_page_copies (proc_start P_gx_default_print_page_copies) s1 (proc_end P_gx_default_print_page_copies) s2 ->
    (s2 V_gx_default_print_page_copies_z <= max0(s1 V_gx_default_print_page_copies_num_copies))%Q.
Proof.
  prove_bound ipa admissible_ipa P_gx_default_print_page_copies.
Qed.
