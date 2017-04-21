Require Import pasta.Pasta.

Inductive proc: Type :=
  P_cie_exec_tpqr.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_cie_exec_tpqr_z := 1%positive.
Notation V_cie_exec_tpqr__tmp := 2%positive.
Notation V_cie_exec_tpqr_i := 3%positive.
Notation V_cie_exec_tpqr_space := 4%positive.
Notation V_cie_exec_tpqr_op := 5%positive.
Definition Pedges_cie_exec_tpqr: list (edge proc) :=
  (EA 1 (AAssign V_cie_exec_tpqr_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_cie_exec_tpqr_space None) 3)::(EA 3 AWeaken 4)::(EA 4 ANone 28)::
  (EA 4 ANone 5)::(EA 5 ANone 6)::(EA 6 AWeaken 7)::(EA 7 ANone 25)::
  (EA 7 ANone 8)::(EA 8 ANone 9)::(EA 9 ANone 10)::(EA 10 (AAssign
  V_cie_exec_tpqr_i (Some (ENum (0)))) 11)::(EA 11 ANone 12)::
  (EA 12 AWeaken 13)::(EA 13 (AGuard
  (fun s => ((eval (EVar V_cie_exec_tpqr_i) s) < (eval (ENum (4))
  s))%Z)) 18)::(EA 13 (AGuard (fun s => ((eval (EVar V_cie_exec_tpqr_i) s) >=
  (eval (ENum (4)) s))%Z)) 14)::(EA 14 AWeaken 15)::(EA 15 (AAssign
  V_cie_exec_tpqr__tmp None) 16)::(EA 16 ANone 17)::(EA 17 AWeaken 31)::
  (EA 18 AWeaken 19)::(EA 19 ANone 20)::(EA 20 (AAssign V_cie_exec_tpqr_i
  (Some (EAdd (EVar V_cie_exec_tpqr_i) (ENum (1))))) 21)::(EA 21 ANone 22)::
  (EA 22 ANone 23)::(EA 23 (AAssign V_cie_exec_tpqr_z (Some (EAdd (ENum (1))
  (EVar V_cie_exec_tpqr_z)))) 24)::(EA 24 AWeaken 13)::(EA 25 (AAssign
  V_cie_exec_tpqr__tmp (Some (ENum (-16)))) 26)::(EA 26 ANone 27)::
  (EA 27 AWeaken 31)::(EA 28 (AAssign V_cie_exec_tpqr__tmp
  (Some (ENum (-17)))) 29)::(EA 29 ANone 30)::(EA 30 AWeaken 31)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_cie_exec_tpqr => Pedges_cie_exec_tpqr
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_cie_exec_tpqr => 31
     end)%positive;
  var_global := var_global
}.

Definition ai_cie_exec_tpqr (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_cie_exec_tpqr_z <= 0 /\ -1 * s V_cie_exec_tpqr_z <= 0)%Z
   | 3 => (-1 * s V_cie_exec_tpqr_z <= 0 /\ 1 * s V_cie_exec_tpqr_z <= 0)%Z
   | 4 => (1 * s V_cie_exec_tpqr_z <= 0 /\ -1 * s V_cie_exec_tpqr_z <= 0)%Z
   | 5 => (-1 * s V_cie_exec_tpqr_z <= 0 /\ 1 * s V_cie_exec_tpqr_z <= 0)%Z
   | 6 => (1 * s V_cie_exec_tpqr_z <= 0 /\ -1 * s V_cie_exec_tpqr_z <= 0)%Z
   | 7 => (-1 * s V_cie_exec_tpqr_z <= 0 /\ 1 * s V_cie_exec_tpqr_z <= 0)%Z
   | 8 => (1 * s V_cie_exec_tpqr_z <= 0 /\ -1 * s V_cie_exec_tpqr_z <= 0)%Z
   | 9 => (-1 * s V_cie_exec_tpqr_z <= 0 /\ 1 * s V_cie_exec_tpqr_z <= 0)%Z
   | 10 => (1 * s V_cie_exec_tpqr_z <= 0 /\ -1 * s V_cie_exec_tpqr_z <= 0)%Z
   | 11 => (-1 * s V_cie_exec_tpqr_z <= 0 /\ 1 * s V_cie_exec_tpqr_z <= 0 /\ 1 * s V_cie_exec_tpqr_i <= 0 /\ -1 * s V_cie_exec_tpqr_i <= 0)%Z
   | 12 => (-1 * s V_cie_exec_tpqr_i <= 0 /\ 1 * s V_cie_exec_tpqr_i <= 0 /\ 1 * s V_cie_exec_tpqr_z <= 0 /\ -1 * s V_cie_exec_tpqr_z <= 0)%Z
   | 13 => (-1 * s V_cie_exec_tpqr_z <= 0 /\ -1 * s V_cie_exec_tpqr_i <= 0 /\ 1 * s V_cie_exec_tpqr_i + -4 <= 0)%Z
   | 14 => (1 * s V_cie_exec_tpqr_i + -4 <= 0 /\ -1 * s V_cie_exec_tpqr_z <= 0 /\ -1 * s V_cie_exec_tpqr_i + 4 <= 0)%Z
   | 15 => (-1 * s V_cie_exec_tpqr_i + 4 <= 0 /\ -1 * s V_cie_exec_tpqr_z <= 0 /\ 1 * s V_cie_exec_tpqr_i + -4 <= 0)%Z
   | 16 => (1 * s V_cie_exec_tpqr_i + -4 <= 0 /\ -1 * s V_cie_exec_tpqr_z <= 0 /\ -1 * s V_cie_exec_tpqr_i + 4 <= 0)%Z
   | 17 => (-1 * s V_cie_exec_tpqr_i + 4 <= 0 /\ -1 * s V_cie_exec_tpqr_z <= 0 /\ 1 * s V_cie_exec_tpqr_i + -4 <= 0)%Z
   | 18 => (-1 * s V_cie_exec_tpqr_i <= 0 /\ -1 * s V_cie_exec_tpqr_z <= 0 /\ 1 * s V_cie_exec_tpqr_i + -3 <= 0)%Z
   | 19 => (1 * s V_cie_exec_tpqr_i + -3 <= 0 /\ -1 * s V_cie_exec_tpqr_z <= 0 /\ -1 * s V_cie_exec_tpqr_i <= 0)%Z
   | 20 => (-1 * s V_cie_exec_tpqr_i <= 0 /\ -1 * s V_cie_exec_tpqr_z <= 0 /\ 1 * s V_cie_exec_tpqr_i + -3 <= 0)%Z
   | 21 => (-1 * s V_cie_exec_tpqr_z <= 0 /\ -1 * s V_cie_exec_tpqr_i + 1 <= 0 /\ 1 * s V_cie_exec_tpqr_i + -4 <= 0)%Z
   | 22 => (1 * s V_cie_exec_tpqr_i + -4 <= 0 /\ -1 * s V_cie_exec_tpqr_i + 1 <= 0 /\ -1 * s V_cie_exec_tpqr_z <= 0)%Z
   | 23 => (-1 * s V_cie_exec_tpqr_z <= 0 /\ -1 * s V_cie_exec_tpqr_i + 1 <= 0 /\ 1 * s V_cie_exec_tpqr_i + -4 <= 0)%Z
   | 24 => (1 * s V_cie_exec_tpqr_i + -4 <= 0 /\ -1 * s V_cie_exec_tpqr_i + 1 <= 0 /\ -1 * s V_cie_exec_tpqr_z + 1 <= 0)%Z
   | 25 => (1 * s V_cie_exec_tpqr_z <= 0 /\ -1 * s V_cie_exec_tpqr_z <= 0)%Z
   | 26 => (-1 * s V_cie_exec_tpqr_z <= 0 /\ 1 * s V_cie_exec_tpqr_z <= 0 /\ 1 * s V_cie_exec_tpqr__tmp + 16 <= 0 /\ -1 * s V_cie_exec_tpqr__tmp + -16 <= 0)%Z
   | 27 => (-1 * s V_cie_exec_tpqr__tmp + -16 <= 0 /\ 1 * s V_cie_exec_tpqr__tmp + 16 <= 0 /\ 1 * s V_cie_exec_tpqr_z <= 0 /\ -1 * s V_cie_exec_tpqr_z <= 0)%Z
   | 28 => (-1 * s V_cie_exec_tpqr_z <= 0 /\ 1 * s V_cie_exec_tpqr_z <= 0)%Z
   | 29 => (1 * s V_cie_exec_tpqr_z <= 0 /\ -1 * s V_cie_exec_tpqr_z <= 0 /\ 1 * s V_cie_exec_tpqr__tmp + 17 <= 0 /\ -1 * s V_cie_exec_tpqr__tmp + -17 <= 0)%Z
   | 30 => (-1 * s V_cie_exec_tpqr__tmp + -17 <= 0 /\ 1 * s V_cie_exec_tpqr__tmp + 17 <= 0 /\ -1 * s V_cie_exec_tpqr_z <= 0 /\ 1 * s V_cie_exec_tpqr_z <= 0)%Z
   | 31 => (-1 * s V_cie_exec_tpqr_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_cie_exec_tpqr (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((4 # 1) <= z)%Q
   | 2 => ((4 # 1) + s V_cie_exec_tpqr_z <= z)%Q
   | 3 => ((4 # 1) + s V_cie_exec_tpqr_z <= z)%Q
   | 4 => ((4 # 1) + s V_cie_exec_tpqr_z <= z)%Q
   | 5 => ((4 # 1) + s V_cie_exec_tpqr_z <= z)%Q
   | 6 => ((4 # 1) + s V_cie_exec_tpqr_z <= z)%Q
   | 7 => ((4 # 1) + s V_cie_exec_tpqr_z <= z)%Q
   | 8 => ((4 # 1) + s V_cie_exec_tpqr_z <= z)%Q
   | 9 => ((4 # 1) + s V_cie_exec_tpqr_z <= z)%Q
   | 10 => ((4 # 1) + s V_cie_exec_tpqr_z <= z)%Q
   | 11 => (s V_cie_exec_tpqr_z + max0(4 - s V_cie_exec_tpqr_i) <= z)%Q
   | 12 => (s V_cie_exec_tpqr_z + max0(4 - s V_cie_exec_tpqr_i) <= z)%Q
   | 13 => (s V_cie_exec_tpqr_z + max0(4 - s V_cie_exec_tpqr_i) <= z)%Q
   | 14 => (s V_cie_exec_tpqr_z + max0(4 - s V_cie_exec_tpqr_i) <= z)%Q
   | 15 => (s V_cie_exec_tpqr_z + max0(4 - s V_cie_exec_tpqr_i) <= z)%Q
   | 16 => (s V_cie_exec_tpqr_z + max0(4 - s V_cie_exec_tpqr_i) <= z)%Q
   | 17 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (4 - s V_cie_exec_tpqr_i) (3
                                                                    - s V_cie_exec_tpqr_i));
      (*-1 0*) F_max0_ge_0 (3 - s V_cie_exec_tpqr_i)]
     (s V_cie_exec_tpqr_z + max0(4 - s V_cie_exec_tpqr_i) <= z)%Q
   | 18 => hints
     [(*0 1*) F_max0_pre_decrement 1 (4 - s V_cie_exec_tpqr_i) (1)]
     (s V_cie_exec_tpqr_z + max0(4 - s V_cie_exec_tpqr_i) <= z)%Q
   | 19 => ((1 # 1) + s V_cie_exec_tpqr_z + max0(3 - s V_cie_exec_tpqr_i) <= z)%Q
   | 20 => ((1 # 1) + s V_cie_exec_tpqr_z + max0(3 - s V_cie_exec_tpqr_i) <= z)%Q
   | 21 => ((1 # 1) + s V_cie_exec_tpqr_z + max0(4 - s V_cie_exec_tpqr_i) <= z)%Q
   | 22 => ((1 # 1) + s V_cie_exec_tpqr_z + max0(4 - s V_cie_exec_tpqr_i) <= z)%Q
   | 23 => ((1 # 1) + s V_cie_exec_tpqr_z + max0(4 - s V_cie_exec_tpqr_i) <= z)%Q
   | 24 => (s V_cie_exec_tpqr_z + max0(4 - s V_cie_exec_tpqr_i) <= z)%Q
   | 25 => ((4 # 1) + s V_cie_exec_tpqr_z <= z)%Q
   | 26 => ((4 # 1) + s V_cie_exec_tpqr_z <= z)%Q
   | 27 => hints
     [(*-4 0*) F_one]
     ((4 # 1) + s V_cie_exec_tpqr_z <= z)%Q
   | 28 => ((4 # 1) + s V_cie_exec_tpqr_z <= z)%Q
   | 29 => ((4 # 1) + s V_cie_exec_tpqr_z <= z)%Q
   | 30 => hints
     [(*-4 0*) F_one]
     ((4 # 1) + s V_cie_exec_tpqr_z <= z)%Q
   | 31 => (s V_cie_exec_tpqr_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_cie_exec_tpqr =>
    [mkPA Q (fun n z s => ai_cie_exec_tpqr n s /\ annot0_cie_exec_tpqr n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_cie_exec_tpqr (proc_start P_cie_exec_tpqr) s1 (proc_end P_cie_exec_tpqr) s2 ->
    (s2 V_cie_exec_tpqr_z <= (4 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_cie_exec_tpqr.
Qed.
