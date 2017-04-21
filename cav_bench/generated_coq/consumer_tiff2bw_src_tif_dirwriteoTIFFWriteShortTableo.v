Require Import pasta.Pasta.

Inductive proc: Type :=
  P_TIFFWriteShortTable.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_TIFFWriteShortTable_z := 1%positive.
Notation V_TIFFWriteShortTable__tmp := 2%positive.
Notation V_TIFFWriteShortTable__tmp1 := 3%positive.
Notation V_TIFFWriteShortTable__tmp2 := 4%positive.
Notation V_TIFFWriteShortTable_i := 5%positive.
Notation V_TIFFWriteShortTable_off := 6%positive.
Notation V_TIFFWriteShortTable_dir := 7%positive.
Notation V_TIFFWriteShortTable_n := 8%positive.
Notation V_TIFFWriteShortTable_table := 9%positive.
Notation V_TIFFWriteShortTable_tag := 10%positive.
Notation V_TIFFWriteShortTable_tif := 11%positive.
Definition Pedges_TIFFWriteShortTable: list (edge proc) :=
  (EA 1 (AAssign V_TIFFWriteShortTable_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_TIFFWriteShortTable_i) s) >= (eval (ENum (0))
  s))%Z)) 3)::(EA 3 (AGuard
  (fun s => ((eval (EVar V_TIFFWriteShortTable__tmp) s) >= (eval (ENum (0))
  s))%Z)) 4)::(EA 4 AWeaken 5)::(EA 5 (AAssign V_TIFFWriteShortTable__tmp2
  (Some (EVar V_TIFFWriteShortTable_tag))) 6)::(EA 6 (AAssign
  V_TIFFWriteShortTable__tmp (Some (EVar V_TIFFWriteShortTable_n))) 7)::
  (EA 7 (AAssign V_TIFFWriteShortTable_off None) 8)::(EA 8 (AAssign
  V_TIFFWriteShortTable_i (Some (ENum (0)))) 9)::(EA 9 ANone 10)::
  (EA 10 AWeaken 11)::(EA 11 (AGuard
  (fun s => ((eval (EVar V_TIFFWriteShortTable_i) s) <
  (eval (EVar V_TIFFWriteShortTable__tmp) s))%Z)) 16)::(EA 11 (AGuard
  (fun s => ((eval (EVar V_TIFFWriteShortTable_i) s) >=
  (eval (EVar V_TIFFWriteShortTable__tmp) s))%Z)) 12)::(EA 12 AWeaken 13)::
  (EA 13 (AAssign V_TIFFWriteShortTable__tmp1 (Some (ENum (1)))) 14)::
  (EA 14 ANone 15)::(EA 15 AWeaken 21)::(EA 16 AWeaken 17)::
  (EA 17 ANone 22)::(EA 17 ANone 18)::(EA 18 (AAssign
  V_TIFFWriteShortTable__tmp1 (Some (ENum (0)))) 19)::(EA 19 ANone 20)::
  (EA 20 AWeaken 21)::(EA 22 ANone 23)::(EA 23 (AAssign
  V_TIFFWriteShortTable_i (Some (EAdd (EVar V_TIFFWriteShortTable_i)
  (ENum (1))))) 24)::(EA 24 ANone 25)::(EA 25 ANone 26)::(EA 26 (AAssign
  V_TIFFWriteShortTable_z (Some (EAdd (ENum (1))
  (EVar V_TIFFWriteShortTable_z)))) 27)::(EA 27 AWeaken 11)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_TIFFWriteShortTable => Pedges_TIFFWriteShortTable
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_TIFFWriteShortTable => 21
     end)%positive;
  var_global := var_global
}.

Definition ai_TIFFWriteShortTable (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_TIFFWriteShortTable_z <= 0 /\ -1 * s V_TIFFWriteShortTable_z <= 0)%Z
   | 3 => (-1 * s V_TIFFWriteShortTable_z <= 0 /\ 1 * s V_TIFFWriteShortTable_z <= 0 /\ -1 * s V_TIFFWriteShortTable_i <= 0)%Z
   | 4 => (-1 * s V_TIFFWriteShortTable_i <= 0 /\ 1 * s V_TIFFWriteShortTable_z <= 0 /\ -1 * s V_TIFFWriteShortTable_z <= 0 /\ -1 * s V_TIFFWriteShortTable__tmp <= 0)%Z
   | 5 => (-1 * s V_TIFFWriteShortTable__tmp <= 0 /\ -1 * s V_TIFFWriteShortTable_z <= 0 /\ 1 * s V_TIFFWriteShortTable_z <= 0 /\ -1 * s V_TIFFWriteShortTable_i <= 0)%Z
   | 6 => (-1 * s V_TIFFWriteShortTable_i <= 0 /\ 1 * s V_TIFFWriteShortTable_z <= 0 /\ -1 * s V_TIFFWriteShortTable_z <= 0 /\ -1 * s V_TIFFWriteShortTable__tmp <= 0)%Z
   | 7 => (-1 * s V_TIFFWriteShortTable_z <= 0 /\ 1 * s V_TIFFWriteShortTable_z <= 0 /\ -1 * s V_TIFFWriteShortTable_i <= 0)%Z
   | 8 => (-1 * s V_TIFFWriteShortTable_i <= 0 /\ 1 * s V_TIFFWriteShortTable_z <= 0 /\ -1 * s V_TIFFWriteShortTable_z <= 0)%Z
   | 9 => (-1 * s V_TIFFWriteShortTable_z <= 0 /\ 1 * s V_TIFFWriteShortTable_z <= 0 /\ 1 * s V_TIFFWriteShortTable_i <= 0 /\ -1 * s V_TIFFWriteShortTable_i <= 0)%Z
   | 10 => (-1 * s V_TIFFWriteShortTable_i <= 0 /\ 1 * s V_TIFFWriteShortTable_i <= 0 /\ 1 * s V_TIFFWriteShortTable_z <= 0 /\ -1 * s V_TIFFWriteShortTable_z <= 0)%Z
   | 11 => (-1 * s V_TIFFWriteShortTable_z <= 0 /\ -1 * s V_TIFFWriteShortTable_i <= 0)%Z
   | 12 => (-1 * s V_TIFFWriteShortTable_i <= 0 /\ -1 * s V_TIFFWriteShortTable_z <= 0 /\ 1 * s V_TIFFWriteShortTable__tmp+ -1 * s V_TIFFWriteShortTable_i <= 0)%Z
   | 13 => (1 * s V_TIFFWriteShortTable__tmp+ -1 * s V_TIFFWriteShortTable_i <= 0 /\ -1 * s V_TIFFWriteShortTable_z <= 0 /\ -1 * s V_TIFFWriteShortTable_i <= 0)%Z
   | 14 => (-1 * s V_TIFFWriteShortTable_i <= 0 /\ -1 * s V_TIFFWriteShortTable_z <= 0 /\ 1 * s V_TIFFWriteShortTable__tmp+ -1 * s V_TIFFWriteShortTable_i <= 0 /\ 1 * s V_TIFFWriteShortTable__tmp1 + -1 <= 0 /\ -1 * s V_TIFFWriteShortTable__tmp1 + 1 <= 0)%Z
   | 15 => (-1 * s V_TIFFWriteShortTable__tmp1 + 1 <= 0 /\ 1 * s V_TIFFWriteShortTable__tmp1 + -1 <= 0 /\ 1 * s V_TIFFWriteShortTable__tmp+ -1 * s V_TIFFWriteShortTable_i <= 0 /\ -1 * s V_TIFFWriteShortTable_z <= 0 /\ -1 * s V_TIFFWriteShortTable_i <= 0)%Z
   | 16 => (-1 * s V_TIFFWriteShortTable_i <= 0 /\ -1 * s V_TIFFWriteShortTable_z <= 0 /\ -1 * s V_TIFFWriteShortTable__tmp+ 1 * s V_TIFFWriteShortTable_i + 1 <= 0)%Z
   | 17 => (-1 * s V_TIFFWriteShortTable__tmp+ 1 * s V_TIFFWriteShortTable_i + 1 <= 0 /\ -1 * s V_TIFFWriteShortTable_z <= 0 /\ -1 * s V_TIFFWriteShortTable_i <= 0)%Z
   | 18 => (-1 * s V_TIFFWriteShortTable_i <= 0 /\ -1 * s V_TIFFWriteShortTable_z <= 0 /\ -1 * s V_TIFFWriteShortTable__tmp+ 1 * s V_TIFFWriteShortTable_i + 1 <= 0)%Z
   | 19 => (-1 * s V_TIFFWriteShortTable__tmp+ 1 * s V_TIFFWriteShortTable_i + 1 <= 0 /\ -1 * s V_TIFFWriteShortTable_z <= 0 /\ -1 * s V_TIFFWriteShortTable_i <= 0 /\ 1 * s V_TIFFWriteShortTable__tmp1 <= 0 /\ -1 * s V_TIFFWriteShortTable__tmp1 <= 0)%Z
   | 20 => (-1 * s V_TIFFWriteShortTable__tmp1 <= 0 /\ 1 * s V_TIFFWriteShortTable__tmp1 <= 0 /\ -1 * s V_TIFFWriteShortTable_i <= 0 /\ -1 * s V_TIFFWriteShortTable_z <= 0 /\ -1 * s V_TIFFWriteShortTable__tmp+ 1 * s V_TIFFWriteShortTable_i + 1 <= 0)%Z
   | 21 => (1 * s V_TIFFWriteShortTable__tmp1 + -1 <= 0 /\ -1 * s V_TIFFWriteShortTable_z <= 0 /\ -1 * s V_TIFFWriteShortTable_i <= 0 /\ -1 * s V_TIFFWriteShortTable__tmp1 <= 0)%Z
   | 22 => (-1 * s V_TIFFWriteShortTable_i <= 0 /\ -1 * s V_TIFFWriteShortTable_z <= 0 /\ -1 * s V_TIFFWriteShortTable__tmp+ 1 * s V_TIFFWriteShortTable_i + 1 <= 0)%Z
   | 23 => (-1 * s V_TIFFWriteShortTable__tmp+ 1 * s V_TIFFWriteShortTable_i + 1 <= 0 /\ -1 * s V_TIFFWriteShortTable_z <= 0 /\ -1 * s V_TIFFWriteShortTable_i <= 0)%Z
   | 24 => (-1 * s V_TIFFWriteShortTable_z <= 0 /\ -1 * s V_TIFFWriteShortTable__tmp+ 1 * s V_TIFFWriteShortTable_i <= 0 /\ -1 * s V_TIFFWriteShortTable_i + 1 <= 0)%Z
   | 25 => (-1 * s V_TIFFWriteShortTable_i + 1 <= 0 /\ -1 * s V_TIFFWriteShortTable__tmp+ 1 * s V_TIFFWriteShortTable_i <= 0 /\ -1 * s V_TIFFWriteShortTable_z <= 0)%Z
   | 26 => (-1 * s V_TIFFWriteShortTable_z <= 0 /\ -1 * s V_TIFFWriteShortTable__tmp+ 1 * s V_TIFFWriteShortTable_i <= 0 /\ -1 * s V_TIFFWriteShortTable_i + 1 <= 0)%Z
   | 27 => (-1 * s V_TIFFWriteShortTable_i + 1 <= 0 /\ -1 * s V_TIFFWriteShortTable__tmp+ 1 * s V_TIFFWriteShortTable_i <= 0 /\ -1 * s V_TIFFWriteShortTable_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_TIFFWriteShortTable (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_TIFFWriteShortTable_n) <= z)%Q
   | 2 => (s V_TIFFWriteShortTable_z + max0(s V_TIFFWriteShortTable_n) <= z)%Q
   | 3 => (s V_TIFFWriteShortTable_z + max0(s V_TIFFWriteShortTable_n) <= z)%Q
   | 4 => (s V_TIFFWriteShortTable_z + max0(s V_TIFFWriteShortTable_n) <= z)%Q
   | 5 => (s V_TIFFWriteShortTable_z + max0(s V_TIFFWriteShortTable_n) <= z)%Q
   | 6 => (s V_TIFFWriteShortTable_z + max0(s V_TIFFWriteShortTable_n) <= z)%Q
   | 7 => (s V_TIFFWriteShortTable_z + max0(s V_TIFFWriteShortTable__tmp) <= z)%Q
   | 8 => (s V_TIFFWriteShortTable_z + max0(s V_TIFFWriteShortTable__tmp) <= z)%Q
   | 9 => (s V_TIFFWriteShortTable_z
           + max0(s V_TIFFWriteShortTable__tmp - s V_TIFFWriteShortTable_i) <= z)%Q
   | 10 => (s V_TIFFWriteShortTable_z
            + max0(s V_TIFFWriteShortTable__tmp - s V_TIFFWriteShortTable_i) <= z)%Q
   | 11 => (s V_TIFFWriteShortTable_z
            + max0(s V_TIFFWriteShortTable__tmp - s V_TIFFWriteShortTable_i) <= z)%Q
   | 12 => (s V_TIFFWriteShortTable_z
            + max0(s V_TIFFWriteShortTable__tmp - s V_TIFFWriteShortTable_i) <= z)%Q
   | 13 => (s V_TIFFWriteShortTable_z
            + max0(s V_TIFFWriteShortTable__tmp - s V_TIFFWriteShortTable_i) <= z)%Q
   | 14 => (s V_TIFFWriteShortTable_z
            + max0(s V_TIFFWriteShortTable__tmp - s V_TIFFWriteShortTable_i) <= z)%Q
   | 15 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_TIFFWriteShortTable__tmp
                                             - s V_TIFFWriteShortTable_i) (-1
                                                                    + s V_TIFFWriteShortTable__tmp
                                                                    - s V_TIFFWriteShortTable_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V_TIFFWriteShortTable__tmp
                            - s V_TIFFWriteShortTable_i)]
     (s V_TIFFWriteShortTable_z
      + max0(s V_TIFFWriteShortTable__tmp - s V_TIFFWriteShortTable_i) <= z)%Q
   | 16 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_TIFFWriteShortTable__tmp
                                       - s V_TIFFWriteShortTable_i) (1)]
     (s V_TIFFWriteShortTable_z
      + max0(s V_TIFFWriteShortTable__tmp - s V_TIFFWriteShortTable_i) <= z)%Q
   | 17 => ((1 # 1) + s V_TIFFWriteShortTable_z
            + max0(-1 + s V_TIFFWriteShortTable__tmp
                   - s V_TIFFWriteShortTable_i) <= z)%Q
   | 18 => ((1 # 1) + s V_TIFFWriteShortTable_z
            + max0(-1 + s V_TIFFWriteShortTable__tmp
                   - s V_TIFFWriteShortTable_i) <= z)%Q
   | 19 => ((1 # 1) + s V_TIFFWriteShortTable_z
            + max0(-1 + s V_TIFFWriteShortTable__tmp
                   - s V_TIFFWriteShortTable_i) <= z)%Q
   | 20 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_max0_ge_0 (-1 + s V_TIFFWriteShortTable__tmp
                            - s V_TIFFWriteShortTable_i)]
     ((1 # 1) + s V_TIFFWriteShortTable_z
      + max0(-1 + s V_TIFFWriteShortTable__tmp - s V_TIFFWriteShortTable_i) <= z)%Q
   | 21 => (s V_TIFFWriteShortTable_z <= z)%Q
   | 22 => ((1 # 1) + s V_TIFFWriteShortTable_z
            + max0(-1 + s V_TIFFWriteShortTable__tmp
                   - s V_TIFFWriteShortTable_i) <= z)%Q
   | 23 => ((1 # 1) + s V_TIFFWriteShortTable_z
            + max0(-1 + s V_TIFFWriteShortTable__tmp
                   - s V_TIFFWriteShortTable_i) <= z)%Q
   | 24 => ((1 # 1) + s V_TIFFWriteShortTable_z
            + max0(s V_TIFFWriteShortTable__tmp - s V_TIFFWriteShortTable_i) <= z)%Q
   | 25 => ((1 # 1) + s V_TIFFWriteShortTable_z
            + max0(s V_TIFFWriteShortTable__tmp - s V_TIFFWriteShortTable_i) <= z)%Q
   | 26 => ((1 # 1) + s V_TIFFWriteShortTable_z
            + max0(s V_TIFFWriteShortTable__tmp - s V_TIFFWriteShortTable_i) <= z)%Q
   | 27 => (s V_TIFFWriteShortTable_z
            + max0(s V_TIFFWriteShortTable__tmp - s V_TIFFWriteShortTable_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_TIFFWriteShortTable =>
    [mkPA Q (fun n z s => ai_TIFFWriteShortTable n s /\ annot0_TIFFWriteShortTable n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_TIFFWriteShortTable (proc_start P_TIFFWriteShortTable) s1 (proc_end P_TIFFWriteShortTable) s2 ->
    (s2 V_TIFFWriteShortTable_z <= max0(s1 V_TIFFWriteShortTable_n))%Q.
Proof.
  prove_bound ipa admissible_ipa P_TIFFWriteShortTable.
Qed.
