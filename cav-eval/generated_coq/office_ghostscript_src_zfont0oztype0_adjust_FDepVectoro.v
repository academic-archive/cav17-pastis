Require Import pasta.Pasta.

Inductive proc: Type :=
  P_ztype0_adjust_FDepVector.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_ztype0_adjust_FDepVector_z := 1%positive.
Notation V_ztype0_adjust_FDepVector__tmp := 2%positive.
Notation V_ztype0_adjust_FDepVector_code := 3%positive.
Notation V_ztype0_adjust_FDepVector_fdep_size := 4%positive.
Notation V_ztype0_adjust_FDepVector_i := 5%positive.
Notation V_ztype0_adjust_FDepVector_pfont_dref_off280_off56 := 6%positive.
Notation V_ztype0_adjust_FDepVector_pfont := 7%positive.
Definition Pedges_ztype0_adjust_FDepVector: list (edge proc) :=
  (EA 1 (AAssign V_ztype0_adjust_FDepVector_z (Some (ENum (0)))) 2)::
  (EA 2 (AGuard (fun s => ((eval (EVar V_ztype0_adjust_FDepVector_i) s) >=
  (eval (ENum (0)) s))%Z)) 3)::(EA 3 (AGuard
  (fun s => ((eval (EVar V_ztype0_adjust_FDepVector_fdep_size) s) >=
  (eval (ENum (0)) s))%Z)) 4)::(EA 4 AWeaken 5)::(EA 5 (AAssign
  V_ztype0_adjust_FDepVector_fdep_size
  (Some (EVar V_ztype0_adjust_FDepVector_pfont_dref_off280_off56))) 6)::
  (EA 6 (AAssign V_ztype0_adjust_FDepVector_code None) 7)::(EA 7 AWeaken 8)::
  (EA 8 (AGuard (fun s => ((eval (EVar V_ztype0_adjust_FDepVector_code) s) <
  (eval (ENum (0)) s))%Z)) 25)::(EA 8 (AGuard
  (fun s => ((eval (EVar V_ztype0_adjust_FDepVector_code) s) >=
  (eval (ENum (0)) s))%Z)) 9)::(EA 9 AWeaken 10)::(EA 10 (AAssign
  V_ztype0_adjust_FDepVector_i (Some (ENum (0)))) 11)::(EA 11 ANone 12)::
  (EA 12 AWeaken 13)::(EA 13 (AGuard
  (fun s => ((eval (EVar V_ztype0_adjust_FDepVector_i) s) <
  (eval (EVar V_ztype0_adjust_FDepVector_fdep_size) s))%Z)) 18)::
  (EA 13 (AGuard (fun s => ((eval (EVar V_ztype0_adjust_FDepVector_i) s) >=
  (eval (EVar V_ztype0_adjust_FDepVector_fdep_size) s))%Z)) 14)::
  (EA 14 AWeaken 15)::(EA 15 (AAssign V_ztype0_adjust_FDepVector__tmp
  None) 16)::(EA 16 ANone 17)::(EA 17 AWeaken 29)::(EA 18 AWeaken 19)::
  (EA 19 ANone 20)::(EA 20 (AAssign V_ztype0_adjust_FDepVector_i
  (Some (EAdd (EVar V_ztype0_adjust_FDepVector_i) (ENum (1))))) 21)::
  (EA 21 ANone 22)::(EA 22 ANone 23)::(EA 23 (AAssign
  V_ztype0_adjust_FDepVector_z (Some (EAdd (ENum (1))
  (EVar V_ztype0_adjust_FDepVector_z)))) 24)::(EA 24 AWeaken 13)::
  (EA 25 AWeaken 26)::(EA 26 (AAssign V_ztype0_adjust_FDepVector__tmp
  (Some (EVar V_ztype0_adjust_FDepVector_code))) 27)::(EA 27 ANone 28)::
  (EA 28 AWeaken 29)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_ztype0_adjust_FDepVector => Pedges_ztype0_adjust_FDepVector
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_ztype0_adjust_FDepVector => 29
     end)%positive;
  var_global := var_global
}.

Definition ai_ztype0_adjust_FDepVector (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_ztype0_adjust_FDepVector_z <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_z <= 0)%Z
   | 3 => (-1 * s V_ztype0_adjust_FDepVector_z <= 0 /\ 1 * s V_ztype0_adjust_FDepVector_z <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_i <= 0)%Z
   | 4 => (-1 * s V_ztype0_adjust_FDepVector_i <= 0 /\ 1 * s V_ztype0_adjust_FDepVector_z <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_z <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_fdep_size <= 0)%Z
   | 5 => (-1 * s V_ztype0_adjust_FDepVector_fdep_size <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_z <= 0 /\ 1 * s V_ztype0_adjust_FDepVector_z <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_i <= 0)%Z
   | 6 => (-1 * s V_ztype0_adjust_FDepVector_i <= 0 /\ 1 * s V_ztype0_adjust_FDepVector_z <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_z <= 0)%Z
   | 7 => (-1 * s V_ztype0_adjust_FDepVector_z <= 0 /\ 1 * s V_ztype0_adjust_FDepVector_z <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_i <= 0)%Z
   | 8 => (-1 * s V_ztype0_adjust_FDepVector_i <= 0 /\ 1 * s V_ztype0_adjust_FDepVector_z <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_z <= 0)%Z
   | 9 => (-1 * s V_ztype0_adjust_FDepVector_z <= 0 /\ 1 * s V_ztype0_adjust_FDepVector_z <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_i <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_code <= 0)%Z
   | 10 => (-1 * s V_ztype0_adjust_FDepVector_code <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_i <= 0 /\ 1 * s V_ztype0_adjust_FDepVector_z <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_z <= 0)%Z
   | 11 => (-1 * s V_ztype0_adjust_FDepVector_z <= 0 /\ 1 * s V_ztype0_adjust_FDepVector_z <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_code <= 0 /\ 1 * s V_ztype0_adjust_FDepVector_i <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_i <= 0)%Z
   | 12 => (-1 * s V_ztype0_adjust_FDepVector_i <= 0 /\ 1 * s V_ztype0_adjust_FDepVector_i <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_code <= 0 /\ 1 * s V_ztype0_adjust_FDepVector_z <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_z <= 0)%Z
   | 13 => (-1 * s V_ztype0_adjust_FDepVector_z <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_i <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_code <= 0)%Z
   | 14 => (-1 * s V_ztype0_adjust_FDepVector_code <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_i <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_z <= 0 /\ 1 * s V_ztype0_adjust_FDepVector_fdep_size+ -1 * s V_ztype0_adjust_FDepVector_i <= 0)%Z
   | 15 => (1 * s V_ztype0_adjust_FDepVector_fdep_size+ -1 * s V_ztype0_adjust_FDepVector_i <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_z <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_i <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_code <= 0)%Z
   | 16 => (-1 * s V_ztype0_adjust_FDepVector_code <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_i <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_z <= 0 /\ 1 * s V_ztype0_adjust_FDepVector_fdep_size+ -1 * s V_ztype0_adjust_FDepVector_i <= 0)%Z
   | 17 => (1 * s V_ztype0_adjust_FDepVector_fdep_size+ -1 * s V_ztype0_adjust_FDepVector_i <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_z <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_i <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_code <= 0)%Z
   | 18 => (-1 * s V_ztype0_adjust_FDepVector_code <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_i <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_z <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_fdep_size+ 1 * s V_ztype0_adjust_FDepVector_i + 1 <= 0)%Z
   | 19 => (-1 * s V_ztype0_adjust_FDepVector_fdep_size+ 1 * s V_ztype0_adjust_FDepVector_i + 1 <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_z <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_i <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_code <= 0)%Z
   | 20 => (-1 * s V_ztype0_adjust_FDepVector_code <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_i <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_z <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_fdep_size+ 1 * s V_ztype0_adjust_FDepVector_i + 1 <= 0)%Z
   | 21 => (-1 * s V_ztype0_adjust_FDepVector_z <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_code <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_i + 1 <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_fdep_size+ 1 * s V_ztype0_adjust_FDepVector_i <= 0)%Z
   | 22 => (-1 * s V_ztype0_adjust_FDepVector_fdep_size+ 1 * s V_ztype0_adjust_FDepVector_i <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_i + 1 <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_code <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_z <= 0)%Z
   | 23 => (-1 * s V_ztype0_adjust_FDepVector_z <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_code <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_i + 1 <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_fdep_size+ 1 * s V_ztype0_adjust_FDepVector_i <= 0)%Z
   | 24 => (-1 * s V_ztype0_adjust_FDepVector_fdep_size+ 1 * s V_ztype0_adjust_FDepVector_i <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_i + 1 <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_code <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_z + 1 <= 0)%Z
   | 25 => (-1 * s V_ztype0_adjust_FDepVector_z <= 0 /\ 1 * s V_ztype0_adjust_FDepVector_z <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_i <= 0 /\ 1 * s V_ztype0_adjust_FDepVector_code + 1 <= 0)%Z
   | 26 => (1 * s V_ztype0_adjust_FDepVector_code + 1 <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_i <= 0 /\ 1 * s V_ztype0_adjust_FDepVector_z <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_z <= 0)%Z
   | 27 => (-1 * s V_ztype0_adjust_FDepVector_z <= 0 /\ 1 * s V_ztype0_adjust_FDepVector_z <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_i <= 0 /\ 1 * s V_ztype0_adjust_FDepVector_code + 1 <= 0 /\ 1 * s V_ztype0_adjust_FDepVector__tmp + 1 <= 0)%Z
   | 28 => (1 * s V_ztype0_adjust_FDepVector__tmp + 1 <= 0 /\ 1 * s V_ztype0_adjust_FDepVector_code + 1 <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_i <= 0 /\ 1 * s V_ztype0_adjust_FDepVector_z <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_z <= 0)%Z
   | 29 => (-1 * s V_ztype0_adjust_FDepVector_z <= 0 /\ -1 * s V_ztype0_adjust_FDepVector_i <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_ztype0_adjust_FDepVector (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_ztype0_adjust_FDepVector_pfont_dref_off280_off56) <= z)%Q
   | 2 => (s V_ztype0_adjust_FDepVector_z
           + max0(s V_ztype0_adjust_FDepVector_pfont_dref_off280_off56) <= z)%Q
   | 3 => (s V_ztype0_adjust_FDepVector_z
           + max0(s V_ztype0_adjust_FDepVector_pfont_dref_off280_off56) <= z)%Q
   | 4 => (s V_ztype0_adjust_FDepVector_z
           + max0(s V_ztype0_adjust_FDepVector_pfont_dref_off280_off56) <= z)%Q
   | 5 => (s V_ztype0_adjust_FDepVector_z
           + max0(s V_ztype0_adjust_FDepVector_pfont_dref_off280_off56) <= z)%Q
   | 6 => (s V_ztype0_adjust_FDepVector_z
           + max0(s V_ztype0_adjust_FDepVector_fdep_size) <= z)%Q
   | 7 => (s V_ztype0_adjust_FDepVector_z
           + max0(s V_ztype0_adjust_FDepVector_fdep_size) <= z)%Q
   | 8 => (s V_ztype0_adjust_FDepVector_z
           + max0(s V_ztype0_adjust_FDepVector_fdep_size) <= z)%Q
   | 9 => (s V_ztype0_adjust_FDepVector_z
           + max0(s V_ztype0_adjust_FDepVector_fdep_size) <= z)%Q
   | 10 => (s V_ztype0_adjust_FDepVector_z
            + max0(s V_ztype0_adjust_FDepVector_fdep_size) <= z)%Q
   | 11 => (s V_ztype0_adjust_FDepVector_z
            + max0(s V_ztype0_adjust_FDepVector_fdep_size
                   - s V_ztype0_adjust_FDepVector_i) <= z)%Q
   | 12 => (s V_ztype0_adjust_FDepVector_z
            + max0(s V_ztype0_adjust_FDepVector_fdep_size
                   - s V_ztype0_adjust_FDepVector_i) <= z)%Q
   | 13 => (s V_ztype0_adjust_FDepVector_z
            + max0(s V_ztype0_adjust_FDepVector_fdep_size
                   - s V_ztype0_adjust_FDepVector_i) <= z)%Q
   | 14 => (s V_ztype0_adjust_FDepVector_z
            + max0(s V_ztype0_adjust_FDepVector_fdep_size
                   - s V_ztype0_adjust_FDepVector_i) <= z)%Q
   | 15 => (s V_ztype0_adjust_FDepVector_z
            + max0(s V_ztype0_adjust_FDepVector_fdep_size
                   - s V_ztype0_adjust_FDepVector_i) <= z)%Q
   | 16 => (s V_ztype0_adjust_FDepVector_z
            + max0(s V_ztype0_adjust_FDepVector_fdep_size
                   - s V_ztype0_adjust_FDepVector_i) <= z)%Q
   | 17 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_ztype0_adjust_FDepVector_fdep_size
                                             - s V_ztype0_adjust_FDepVector_i) (-1
                                                                    + s V_ztype0_adjust_FDepVector_fdep_size
                                                                    - s V_ztype0_adjust_FDepVector_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V_ztype0_adjust_FDepVector_fdep_size
                            - s V_ztype0_adjust_FDepVector_i)]
     (s V_ztype0_adjust_FDepVector_z
      + max0(s V_ztype0_adjust_FDepVector_fdep_size
             - s V_ztype0_adjust_FDepVector_i) <= z)%Q
   | 18 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_ztype0_adjust_FDepVector_fdep_size
                                       - s V_ztype0_adjust_FDepVector_i) (1)]
     (s V_ztype0_adjust_FDepVector_z
      + max0(s V_ztype0_adjust_FDepVector_fdep_size
             - s V_ztype0_adjust_FDepVector_i) <= z)%Q
   | 19 => ((1 # 1) + s V_ztype0_adjust_FDepVector_z
            + max0(-1 + s V_ztype0_adjust_FDepVector_fdep_size
                   - s V_ztype0_adjust_FDepVector_i) <= z)%Q
   | 20 => ((1 # 1) + s V_ztype0_adjust_FDepVector_z
            + max0(-1 + s V_ztype0_adjust_FDepVector_fdep_size
                   - s V_ztype0_adjust_FDepVector_i) <= z)%Q
   | 21 => ((1 # 1) + s V_ztype0_adjust_FDepVector_z
            + max0(s V_ztype0_adjust_FDepVector_fdep_size
                   - s V_ztype0_adjust_FDepVector_i) <= z)%Q
   | 22 => ((1 # 1) + s V_ztype0_adjust_FDepVector_z
            + max0(s V_ztype0_adjust_FDepVector_fdep_size
                   - s V_ztype0_adjust_FDepVector_i) <= z)%Q
   | 23 => ((1 # 1) + s V_ztype0_adjust_FDepVector_z
            + max0(s V_ztype0_adjust_FDepVector_fdep_size
                   - s V_ztype0_adjust_FDepVector_i) <= z)%Q
   | 24 => (s V_ztype0_adjust_FDepVector_z
            + max0(s V_ztype0_adjust_FDepVector_fdep_size
                   - s V_ztype0_adjust_FDepVector_i) <= z)%Q
   | 25 => (s V_ztype0_adjust_FDepVector_z
            + max0(s V_ztype0_adjust_FDepVector_fdep_size) <= z)%Q
   | 26 => (s V_ztype0_adjust_FDepVector_z
            + max0(s V_ztype0_adjust_FDepVector_fdep_size) <= z)%Q
   | 27 => (s V_ztype0_adjust_FDepVector_z
            + max0(s V_ztype0_adjust_FDepVector_fdep_size) <= z)%Q
   | 28 => hints
     [(*-1 0*) F_max0_ge_0 (s V_ztype0_adjust_FDepVector_fdep_size)]
     (s V_ztype0_adjust_FDepVector_z
      + max0(s V_ztype0_adjust_FDepVector_fdep_size) <= z)%Q
   | 29 => (s V_ztype0_adjust_FDepVector_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_ztype0_adjust_FDepVector =>
    [mkPA Q (fun n z s => ai_ztype0_adjust_FDepVector n s /\ annot0_ztype0_adjust_FDepVector n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_ztype0_adjust_FDepVector (proc_start P_ztype0_adjust_FDepVector) s1 (proc_end P_ztype0_adjust_FDepVector) s2 ->
    (s2 V_ztype0_adjust_FDepVector_z <= max0(s1 V_ztype0_adjust_FDepVector_pfont_dref_off280_off56))%Q.
Proof.
  prove_bound ipa admissible_ipa P_ztype0_adjust_FDepVector.
Qed.
