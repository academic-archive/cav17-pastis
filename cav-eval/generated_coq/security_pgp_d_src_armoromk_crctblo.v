Require Import pasta.Pasta.

Inductive proc: Type :=
  P_mk_crctbl.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_mk_crctbl_z := 1%positive.
Notation V_mk_crctbl__tmp := 2%positive.
Notation V_mk_crctbl_i := 3%positive.
Notation V_mk_crctbl_t := 4%positive.
Notation V_mk_crctbl_poly := 5%positive.
Definition Pedges_mk_crctbl: list (edge proc) :=
  (EA 1 (AAssign V_mk_crctbl_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_mk_crctbl__tmp (Some (EVar V_mk_crctbl_poly))) 3)::(EA 3 (AAssign
  V_mk_crctbl_i (Some (ENum (1)))) 4)::(EA 4 ANone 5)::(EA 5 AWeaken 6)::
  (EA 6 (AGuard (fun s => ((eval (EVar V_mk_crctbl_i) s) < (eval (ENum (128))
  s))%Z)) 9)::(EA 6 (AGuard (fun s => ((eval (EVar V_mk_crctbl_i) s) >=
  (eval (ENum (128)) s))%Z)) 7)::(EA 7 AWeaken 8)::(EA 9 AWeaken 10)::
  (EA 10 (AAssign V_mk_crctbl_t None) 11)::(EA 11 AWeaken 12)::
  (EA 12 ANone 15)::(EA 12 ANone 13)::(EA 13 (AAssign V_mk_crctbl_t
  None) 14)::(EA 14 ANone 17)::(EA 15 (AAssign V_mk_crctbl_t None) 16)::
  (EA 16 ANone 17)::(EA 17 ANone 18)::(EA 18 (AAssign V_mk_crctbl_i
  (Some (EAdd (EVar V_mk_crctbl_i) (ENum (1))))) 19)::(EA 19 ANone 20)::
  (EA 20 ANone 21)::(EA 21 (AAssign V_mk_crctbl_z (Some (EAdd (ENum (1))
  (EVar V_mk_crctbl_z)))) 22)::(EA 22 AWeaken 6)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_mk_crctbl => Pedges_mk_crctbl
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_mk_crctbl => 8
     end)%positive;
  var_global := var_global
}.

Definition ai_mk_crctbl (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_mk_crctbl_z <= 0 /\ -1 * s V_mk_crctbl_z <= 0)%Z
   | 3 => (-1 * s V_mk_crctbl_z <= 0 /\ 1 * s V_mk_crctbl_z <= 0)%Z
   | 4 => (1 * s V_mk_crctbl_z <= 0 /\ -1 * s V_mk_crctbl_z <= 0 /\ 1 * s V_mk_crctbl_i + -1 <= 0 /\ -1 * s V_mk_crctbl_i + 1 <= 0)%Z
   | 5 => (-1 * s V_mk_crctbl_i + 1 <= 0 /\ 1 * s V_mk_crctbl_i + -1 <= 0 /\ -1 * s V_mk_crctbl_z <= 0 /\ 1 * s V_mk_crctbl_z <= 0)%Z
   | 6 => (-1 * s V_mk_crctbl_z <= 0 /\ -1 * s V_mk_crctbl_i + 1 <= 0 /\ 1 * s V_mk_crctbl_i + -128 <= 0)%Z
   | 7 => (1 * s V_mk_crctbl_i + -128 <= 0 /\ -1 * s V_mk_crctbl_z <= 0 /\ -1 * s V_mk_crctbl_i + 128 <= 0)%Z
   | 8 => (-1 * s V_mk_crctbl_i + 128 <= 0 /\ -1 * s V_mk_crctbl_z <= 0 /\ 1 * s V_mk_crctbl_i + -128 <= 0)%Z
   | 9 => (-1 * s V_mk_crctbl_i + 1 <= 0 /\ -1 * s V_mk_crctbl_z <= 0 /\ 1 * s V_mk_crctbl_i + -127 <= 0)%Z
   | 10 => (1 * s V_mk_crctbl_i + -127 <= 0 /\ -1 * s V_mk_crctbl_z <= 0 /\ -1 * s V_mk_crctbl_i + 1 <= 0)%Z
   | 11 => (-1 * s V_mk_crctbl_i + 1 <= 0 /\ -1 * s V_mk_crctbl_z <= 0 /\ 1 * s V_mk_crctbl_i + -127 <= 0)%Z
   | 12 => (1 * s V_mk_crctbl_i + -127 <= 0 /\ -1 * s V_mk_crctbl_z <= 0 /\ -1 * s V_mk_crctbl_i + 1 <= 0)%Z
   | 13 => (-1 * s V_mk_crctbl_i + 1 <= 0 /\ -1 * s V_mk_crctbl_z <= 0 /\ 1 * s V_mk_crctbl_i + -127 <= 0)%Z
   | 14 => (1 * s V_mk_crctbl_i + -127 <= 0 /\ -1 * s V_mk_crctbl_z <= 0 /\ -1 * s V_mk_crctbl_i + 1 <= 0)%Z
   | 15 => (-1 * s V_mk_crctbl_i + 1 <= 0 /\ -1 * s V_mk_crctbl_z <= 0 /\ 1 * s V_mk_crctbl_i + -127 <= 0)%Z
   | 16 => (1 * s V_mk_crctbl_i + -127 <= 0 /\ -1 * s V_mk_crctbl_z <= 0 /\ -1 * s V_mk_crctbl_i + 1 <= 0)%Z
   | 17 => (-1 * s V_mk_crctbl_i + 1 <= 0 /\ -1 * s V_mk_crctbl_z <= 0 /\ 1 * s V_mk_crctbl_i + -127 <= 0)%Z
   | 18 => (1 * s V_mk_crctbl_i + -127 <= 0 /\ -1 * s V_mk_crctbl_z <= 0 /\ -1 * s V_mk_crctbl_i + 1 <= 0)%Z
   | 19 => (-1 * s V_mk_crctbl_z <= 0 /\ 1 * s V_mk_crctbl_i + -128 <= 0 /\ -1 * s V_mk_crctbl_i + 2 <= 0)%Z
   | 20 => (-1 * s V_mk_crctbl_i + 2 <= 0 /\ 1 * s V_mk_crctbl_i + -128 <= 0 /\ -1 * s V_mk_crctbl_z <= 0)%Z
   | 21 => (-1 * s V_mk_crctbl_z <= 0 /\ 1 * s V_mk_crctbl_i + -128 <= 0 /\ -1 * s V_mk_crctbl_i + 2 <= 0)%Z
   | 22 => (-1 * s V_mk_crctbl_i + 2 <= 0 /\ 1 * s V_mk_crctbl_i + -128 <= 0 /\ -1 * s V_mk_crctbl_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_mk_crctbl (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((127 # 1) <= z)%Q
   | 2 => ((127 # 1) + s V_mk_crctbl_z <= z)%Q
   | 3 => ((127 # 1) + s V_mk_crctbl_z <= z)%Q
   | 4 => ((128 # 1) - s V_mk_crctbl_i + s V_mk_crctbl_z <= z)%Q
   | 5 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (128
                                                               - s V_mk_crctbl_i) (0))) (F_max0_ge_0 (128
                                                                    - s V_mk_crctbl_i))]
     ((128 # 1) - s V_mk_crctbl_i + s V_mk_crctbl_z <= z)%Q
   | 6 => (s V_mk_crctbl_z + max0(128 - s V_mk_crctbl_i) <= z)%Q
   | 7 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (128 - s V_mk_crctbl_i) (127
                                                                    - 
                                                                    s V_mk_crctbl_i));
      (*-1 0*) F_max0_ge_0 (127 - s V_mk_crctbl_i)]
     (s V_mk_crctbl_z + max0(128 - s V_mk_crctbl_i) <= z)%Q
   | 8 => (s V_mk_crctbl_z <= z)%Q
   | 9 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (127
                                                              - s V_mk_crctbl_i) (0))) (F_max0_ge_0 (127
                                                                    - s V_mk_crctbl_i))]
     (s V_mk_crctbl_z + max0(128 - s V_mk_crctbl_i) <= z)%Q
   | 10 => (-(127 # 1) + s V_mk_crctbl_i + s V_mk_crctbl_z
            + max0(127 - s V_mk_crctbl_i) + max0(128 - s V_mk_crctbl_i) <= z)%Q
   | 11 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (128 - s V_mk_crctbl_i)) (F_check_ge (128
                                                                    - s V_mk_crctbl_i) (0))]
     (-(127 # 1) + s V_mk_crctbl_i + s V_mk_crctbl_z
      + max0(127 - s V_mk_crctbl_i) + max0(128 - s V_mk_crctbl_i) <= z)%Q
   | 12 => ((1 # 1) + s V_mk_crctbl_z + max0(127 - s V_mk_crctbl_i) <= z)%Q
   | 13 => ((1 # 1) + s V_mk_crctbl_z + max0(127 - s V_mk_crctbl_i) <= z)%Q
   | 14 => ((1 # 1) + s V_mk_crctbl_z + max0(127 - s V_mk_crctbl_i) <= z)%Q
   | 15 => ((1 # 1) + s V_mk_crctbl_z + max0(127 - s V_mk_crctbl_i) <= z)%Q
   | 16 => ((1 # 1) + s V_mk_crctbl_z + max0(127 - s V_mk_crctbl_i) <= z)%Q
   | 17 => ((1 # 1) + s V_mk_crctbl_z + max0(127 - s V_mk_crctbl_i) <= z)%Q
   | 18 => ((1 # 1) + s V_mk_crctbl_z + max0(127 - s V_mk_crctbl_i) <= z)%Q
   | 19 => ((1 # 1) + s V_mk_crctbl_z + max0(128 - s V_mk_crctbl_i) <= z)%Q
   | 20 => ((1 # 1) + s V_mk_crctbl_z + max0(128 - s V_mk_crctbl_i) <= z)%Q
   | 21 => ((1 # 1) + s V_mk_crctbl_z + max0(128 - s V_mk_crctbl_i) <= z)%Q
   | 22 => (s V_mk_crctbl_z + max0(128 - s V_mk_crctbl_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_mk_crctbl =>
    [mkPA Q (fun n z s => ai_mk_crctbl n s /\ annot0_mk_crctbl n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_mk_crctbl (proc_start P_mk_crctbl) s1 (proc_end P_mk_crctbl) s2 ->
    (s2 V_mk_crctbl_z <= (127 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_mk_crctbl.
Qed.
