Require Import pasta.Pasta.

Inductive proc: Type :=
  P_get_8bit_gray_row.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_get_8bit_gray_row_z := 1%positive.
Notation V_get_8bit_gray_row_cinfo_dref_off40 := 2%positive.
Notation V_get_8bit_gray_row_col := 3%positive.
Notation V_get_8bit_gray_row_cinfo := 4%positive.
Notation V_get_8bit_gray_row_sinfo := 5%positive.
Definition Pedges_get_8bit_gray_row: list (edge proc) :=
  (EA 1 (AAssign V_get_8bit_gray_row_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_get_8bit_gray_row_col) s) >= (eval (ENum (0))
  s))%Z)) 3)::(EA 3 AWeaken 4)::(EA 4 (AAssign V_get_8bit_gray_row_col
  (Some (EVar V_get_8bit_gray_row_cinfo_dref_off40))) 5)::(EA 5 ANone 6)::
  (EA 6 AWeaken 7)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_get_8bit_gray_row_col) s) > (eval (ENum (0))
  s))%Z)) 10)::(EA 7 (AGuard (fun s => ((eval (EVar V_get_8bit_gray_row_col)
  s) <= (eval (ENum (0)) s))%Z)) 8)::(EA 8 AWeaken 9)::(EA 10 AWeaken 11)::
  (EA 11 ANone 12)::(EA 12 (AAssign V_get_8bit_gray_row_col
  (Some (EAdd (EVar V_get_8bit_gray_row_col) (ENum (-1))))) 13)::
  (EA 13 ANone 14)::(EA 14 ANone 15)::(EA 15 (AAssign V_get_8bit_gray_row_z
  (Some (EAdd (ENum (1)) (EVar V_get_8bit_gray_row_z)))) 16)::
  (EA 16 AWeaken 7)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_get_8bit_gray_row => Pedges_get_8bit_gray_row
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_get_8bit_gray_row => 9
     end)%positive;
  var_global := var_global
}.

Definition ai_get_8bit_gray_row (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_get_8bit_gray_row_z <= 0 /\ -1 * s V_get_8bit_gray_row_z <= 0)%Z
   | 3 => (-1 * s V_get_8bit_gray_row_z <= 0 /\ 1 * s V_get_8bit_gray_row_z <= 0 /\ -1 * s V_get_8bit_gray_row_col <= 0)%Z
   | 4 => (-1 * s V_get_8bit_gray_row_col <= 0 /\ 1 * s V_get_8bit_gray_row_z <= 0 /\ -1 * s V_get_8bit_gray_row_z <= 0)%Z
   | 5 => (-1 * s V_get_8bit_gray_row_z <= 0 /\ 1 * s V_get_8bit_gray_row_z <= 0)%Z
   | 6 => (1 * s V_get_8bit_gray_row_z <= 0 /\ -1 * s V_get_8bit_gray_row_z <= 0)%Z
   | 7 => (-1 * s V_get_8bit_gray_row_z <= 0)%Z
   | 8 => (-1 * s V_get_8bit_gray_row_z <= 0 /\ 1 * s V_get_8bit_gray_row_col <= 0)%Z
   | 9 => (1 * s V_get_8bit_gray_row_col <= 0 /\ -1 * s V_get_8bit_gray_row_z <= 0)%Z
   | 10 => (-1 * s V_get_8bit_gray_row_z <= 0 /\ -1 * s V_get_8bit_gray_row_col + 1 <= 0)%Z
   | 11 => (-1 * s V_get_8bit_gray_row_col + 1 <= 0 /\ -1 * s V_get_8bit_gray_row_z <= 0)%Z
   | 12 => (-1 * s V_get_8bit_gray_row_z <= 0 /\ -1 * s V_get_8bit_gray_row_col + 1 <= 0)%Z
   | 13 => (-1 * s V_get_8bit_gray_row_z <= 0 /\ -1 * s V_get_8bit_gray_row_col <= 0)%Z
   | 14 => (-1 * s V_get_8bit_gray_row_col <= 0 /\ -1 * s V_get_8bit_gray_row_z <= 0)%Z
   | 15 => (-1 * s V_get_8bit_gray_row_z <= 0 /\ -1 * s V_get_8bit_gray_row_col <= 0)%Z
   | 16 => (-1 * s V_get_8bit_gray_row_col <= 0 /\ -1 * s V_get_8bit_gray_row_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_get_8bit_gray_row (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_get_8bit_gray_row_cinfo_dref_off40) <= z)%Q
   | 2 => (s V_get_8bit_gray_row_z
           + max0(s V_get_8bit_gray_row_cinfo_dref_off40) <= z)%Q
   | 3 => (s V_get_8bit_gray_row_z
           + max0(s V_get_8bit_gray_row_cinfo_dref_off40) <= z)%Q
   | 4 => (s V_get_8bit_gray_row_z
           + max0(s V_get_8bit_gray_row_cinfo_dref_off40) <= z)%Q
   | 5 => (s V_get_8bit_gray_row_z + max0(s V_get_8bit_gray_row_col) <= z)%Q
   | 6 => (s V_get_8bit_gray_row_z + max0(s V_get_8bit_gray_row_col) <= z)%Q
   | 7 => (s V_get_8bit_gray_row_z + max0(s V_get_8bit_gray_row_col) <= z)%Q
   | 8 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_get_8bit_gray_row_col) (-1
                                                                    + s V_get_8bit_gray_row_col));
      (*-1 0*) F_max0_ge_0 (-1 + s V_get_8bit_gray_row_col)]
     (s V_get_8bit_gray_row_z + max0(s V_get_8bit_gray_row_col) <= z)%Q
   | 9 => (s V_get_8bit_gray_row_z <= z)%Q
   | 10 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_get_8bit_gray_row_col) (1)]
     (s V_get_8bit_gray_row_z + max0(s V_get_8bit_gray_row_col) <= z)%Q
   | 11 => ((1 # 1) + s V_get_8bit_gray_row_z
            + max0(-1 + s V_get_8bit_gray_row_col) <= z)%Q
   | 12 => ((1 # 1) + s V_get_8bit_gray_row_z
            + max0(-1 + s V_get_8bit_gray_row_col) <= z)%Q
   | 13 => ((1 # 1) + s V_get_8bit_gray_row_z
            + max0(s V_get_8bit_gray_row_col) <= z)%Q
   | 14 => ((1 # 1) + s V_get_8bit_gray_row_z
            + max0(s V_get_8bit_gray_row_col) <= z)%Q
   | 15 => ((1 # 1) + s V_get_8bit_gray_row_z
            + max0(s V_get_8bit_gray_row_col) <= z)%Q
   | 16 => (s V_get_8bit_gray_row_z + max0(s V_get_8bit_gray_row_col) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_get_8bit_gray_row =>
    [mkPA Q (fun n z s => ai_get_8bit_gray_row n s /\ annot0_get_8bit_gray_row n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_get_8bit_gray_row (proc_start P_get_8bit_gray_row) s1 (proc_end P_get_8bit_gray_row) s2 ->
    (s2 V_get_8bit_gray_row_z <= max0(s1 V_get_8bit_gray_row_cinfo_dref_off40))%Q.
Proof.
  prove_bound ipa admissible_ipa P_get_8bit_gray_row.
Qed.
