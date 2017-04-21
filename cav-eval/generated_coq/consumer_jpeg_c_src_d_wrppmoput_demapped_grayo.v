Require Import pasta.Pasta.

Inductive proc: Type :=
  P_put_demapped_gray.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_put_demapped_gray_z := 1%positive.
Notation V_put_demapped_gray__tmp := 2%positive.
Notation V_put_demapped_gray_cinfo_dref_off128 := 3%positive.
Notation V_put_demapped_gray_col := 4%positive.
Notation V_put_demapped_gray_cinfo := 5%positive.
Notation V_put_demapped_gray_dinfo := 6%positive.
Notation V_put_demapped_gray_rows_supplied := 7%positive.
Definition Pedges_put_demapped_gray: list (edge proc) :=
  (EA 1 (AAssign V_put_demapped_gray_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_put_demapped_gray_col) s) >= (eval (ENum (0))
  s))%Z)) 3)::(EA 3 AWeaken 4)::(EA 4 (AAssign V_put_demapped_gray__tmp
  (Some (EVar V_put_demapped_gray_rows_supplied))) 5)::(EA 5 (AAssign
  V_put_demapped_gray_col
  (Some (EVar V_put_demapped_gray_cinfo_dref_off128))) 6)::(EA 6 ANone 7)::
  (EA 7 AWeaken 8)::(EA 8 (AGuard
  (fun s => ((eval (EVar V_put_demapped_gray_col) s) > (eval (ENum (0))
  s))%Z)) 11)::(EA 8 (AGuard (fun s => ((eval (EVar V_put_demapped_gray_col)
  s) <= (eval (ENum (0)) s))%Z)) 9)::(EA 9 AWeaken 10)::(EA 11 AWeaken 12)::
  (EA 12 ANone 13)::(EA 13 (AAssign V_put_demapped_gray_col
  (Some (EAdd (EVar V_put_demapped_gray_col) (ENum (-1))))) 14)::
  (EA 14 ANone 15)::(EA 15 ANone 16)::(EA 16 (AAssign V_put_demapped_gray_z
  (Some (EAdd (ENum (1)) (EVar V_put_demapped_gray_z)))) 17)::
  (EA 17 AWeaken 8)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_put_demapped_gray => Pedges_put_demapped_gray
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_put_demapped_gray => 10
     end)%positive;
  var_global := var_global
}.

Definition ai_put_demapped_gray (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_put_demapped_gray_z <= 0 /\ -1 * s V_put_demapped_gray_z <= 0)%Z
   | 3 => (-1 * s V_put_demapped_gray_z <= 0 /\ 1 * s V_put_demapped_gray_z <= 0 /\ -1 * s V_put_demapped_gray_col <= 0)%Z
   | 4 => (-1 * s V_put_demapped_gray_col <= 0 /\ 1 * s V_put_demapped_gray_z <= 0 /\ -1 * s V_put_demapped_gray_z <= 0)%Z
   | 5 => (-1 * s V_put_demapped_gray_z <= 0 /\ 1 * s V_put_demapped_gray_z <= 0 /\ -1 * s V_put_demapped_gray_col <= 0)%Z
   | 6 => (1 * s V_put_demapped_gray_z <= 0 /\ -1 * s V_put_demapped_gray_z <= 0)%Z
   | 7 => (-1 * s V_put_demapped_gray_z <= 0 /\ 1 * s V_put_demapped_gray_z <= 0)%Z
   | 8 => (-1 * s V_put_demapped_gray_z <= 0)%Z
   | 9 => (-1 * s V_put_demapped_gray_z <= 0 /\ 1 * s V_put_demapped_gray_col <= 0)%Z
   | 10 => (1 * s V_put_demapped_gray_col <= 0 /\ -1 * s V_put_demapped_gray_z <= 0)%Z
   | 11 => (-1 * s V_put_demapped_gray_z <= 0 /\ -1 * s V_put_demapped_gray_col + 1 <= 0)%Z
   | 12 => (-1 * s V_put_demapped_gray_col + 1 <= 0 /\ -1 * s V_put_demapped_gray_z <= 0)%Z
   | 13 => (-1 * s V_put_demapped_gray_z <= 0 /\ -1 * s V_put_demapped_gray_col + 1 <= 0)%Z
   | 14 => (-1 * s V_put_demapped_gray_z <= 0 /\ -1 * s V_put_demapped_gray_col <= 0)%Z
   | 15 => (-1 * s V_put_demapped_gray_col <= 0 /\ -1 * s V_put_demapped_gray_z <= 0)%Z
   | 16 => (-1 * s V_put_demapped_gray_z <= 0 /\ -1 * s V_put_demapped_gray_col <= 0)%Z
   | 17 => (-1 * s V_put_demapped_gray_col <= 0 /\ -1 * s V_put_demapped_gray_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_put_demapped_gray (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_put_demapped_gray_cinfo_dref_off128) <= z)%Q
   | 2 => (s V_put_demapped_gray_z
           + max0(s V_put_demapped_gray_cinfo_dref_off128) <= z)%Q
   | 3 => (s V_put_demapped_gray_z
           + max0(s V_put_demapped_gray_cinfo_dref_off128) <= z)%Q
   | 4 => (s V_put_demapped_gray_z
           + max0(s V_put_demapped_gray_cinfo_dref_off128) <= z)%Q
   | 5 => (s V_put_demapped_gray_z
           + max0(s V_put_demapped_gray_cinfo_dref_off128) <= z)%Q
   | 6 => (s V_put_demapped_gray_z + max0(s V_put_demapped_gray_col) <= z)%Q
   | 7 => (s V_put_demapped_gray_z + max0(s V_put_demapped_gray_col) <= z)%Q
   | 8 => (s V_put_demapped_gray_z + max0(s V_put_demapped_gray_col) <= z)%Q
   | 9 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_put_demapped_gray_col) (-1
                                                                    + s V_put_demapped_gray_col));
      (*-1 0*) F_max0_ge_0 (-1 + s V_put_demapped_gray_col)]
     (s V_put_demapped_gray_z + max0(s V_put_demapped_gray_col) <= z)%Q
   | 10 => (s V_put_demapped_gray_z <= z)%Q
   | 11 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_put_demapped_gray_col) (1)]
     (s V_put_demapped_gray_z + max0(s V_put_demapped_gray_col) <= z)%Q
   | 12 => ((1 # 1) + s V_put_demapped_gray_z
            + max0(-1 + s V_put_demapped_gray_col) <= z)%Q
   | 13 => ((1 # 1) + s V_put_demapped_gray_z
            + max0(-1 + s V_put_demapped_gray_col) <= z)%Q
   | 14 => ((1 # 1) + s V_put_demapped_gray_z
            + max0(s V_put_demapped_gray_col) <= z)%Q
   | 15 => ((1 # 1) + s V_put_demapped_gray_z
            + max0(s V_put_demapped_gray_col) <= z)%Q
   | 16 => ((1 # 1) + s V_put_demapped_gray_z
            + max0(s V_put_demapped_gray_col) <= z)%Q
   | 17 => (s V_put_demapped_gray_z + max0(s V_put_demapped_gray_col) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_put_demapped_gray =>
    [mkPA Q (fun n z s => ai_put_demapped_gray n s /\ annot0_put_demapped_gray n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_put_demapped_gray (proc_start P_put_demapped_gray) s1 (proc_end P_put_demapped_gray) s2 ->
    (s2 V_put_demapped_gray_z <= max0(s1 V_put_demapped_gray_cinfo_dref_off128))%Q.
Proof.
  prove_bound ipa admissible_ipa P_put_demapped_gray.
Qed.
