Require Import pasta.Pasta.

Inductive proc: Type :=
  P_expand_bottom_edge.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_expand_bottom_edge_z := 1%positive.
Notation V_expand_bottom_edge__tmp := 2%positive.
Notation V_expand_bottom_edge__tmp1 := 3%positive.
Notation V_expand_bottom_edge__tmp2 := 4%positive.
Notation V_expand_bottom_edge_row := 5%positive.
Notation V_expand_bottom_edge_image_data := 6%positive.
Notation V_expand_bottom_edge_input_rows := 7%positive.
Notation V_expand_bottom_edge_num_cols := 8%positive.
Notation V_expand_bottom_edge_output_rows := 9%positive.
Definition Pedges_expand_bottom_edge: list (edge proc) :=
  (EA 1 (AAssign V_expand_bottom_edge_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_expand_bottom_edge__tmp2
  (Some (EVar V_expand_bottom_edge_num_cols))) 3)::(EA 3 (AAssign
  V_expand_bottom_edge__tmp1
  (Some (EVar V_expand_bottom_edge_input_rows))) 4)::(EA 4 (AAssign
  V_expand_bottom_edge__tmp
  (Some (EVar V_expand_bottom_edge_output_rows))) 5)::(EA 5 (AAssign
  V_expand_bottom_edge_row (Some (EVar V_expand_bottom_edge__tmp1))) 6)::
  (EA 6 ANone 7)::(EA 7 AWeaken 8)::(EA 8 (AGuard
  (fun s => ((eval (EVar V_expand_bottom_edge_row) s) <
  (eval (EVar V_expand_bottom_edge__tmp) s))%Z)) 11)::(EA 8 (AGuard
  (fun s => ((eval (EVar V_expand_bottom_edge_row) s) >=
  (eval (EVar V_expand_bottom_edge__tmp) s))%Z)) 9)::(EA 9 AWeaken 10)::
  (EA 11 AWeaken 12)::(EA 12 ANone 13)::(EA 13 (AAssign
  V_expand_bottom_edge_row (Some (EAdd (EVar V_expand_bottom_edge_row)
  (ENum (1))))) 14)::(EA 14 ANone 15)::(EA 15 ANone 16)::(EA 16 (AAssign
  V_expand_bottom_edge_z (Some (EAdd (ENum (1))
  (EVar V_expand_bottom_edge_z)))) 17)::(EA 17 AWeaken 8)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_expand_bottom_edge => Pedges_expand_bottom_edge
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_expand_bottom_edge => 10
     end)%positive;
  var_global := var_global
}.

Definition ai_expand_bottom_edge (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_expand_bottom_edge_z <= 0 /\ -1 * s V_expand_bottom_edge_z <= 0)%Z
   | 3 => (-1 * s V_expand_bottom_edge_z <= 0 /\ 1 * s V_expand_bottom_edge_z <= 0)%Z
   | 4 => (1 * s V_expand_bottom_edge_z <= 0 /\ -1 * s V_expand_bottom_edge_z <= 0)%Z
   | 5 => (-1 * s V_expand_bottom_edge_z <= 0 /\ 1 * s V_expand_bottom_edge_z <= 0)%Z
   | 6 => (1 * s V_expand_bottom_edge_z <= 0 /\ -1 * s V_expand_bottom_edge_z <= 0)%Z
   | 7 => (-1 * s V_expand_bottom_edge_z <= 0 /\ 1 * s V_expand_bottom_edge_z <= 0)%Z
   | 8 => (-1 * s V_expand_bottom_edge_z <= 0)%Z
   | 9 => (-1 * s V_expand_bottom_edge_z <= 0 /\ 1 * s V_expand_bottom_edge__tmp+ -1 * s V_expand_bottom_edge_row <= 0)%Z
   | 10 => (1 * s V_expand_bottom_edge__tmp+ -1 * s V_expand_bottom_edge_row <= 0 /\ -1 * s V_expand_bottom_edge_z <= 0)%Z
   | 11 => (-1 * s V_expand_bottom_edge_z <= 0 /\ -1 * s V_expand_bottom_edge__tmp+ 1 * s V_expand_bottom_edge_row + 1 <= 0)%Z
   | 12 => (-1 * s V_expand_bottom_edge__tmp+ 1 * s V_expand_bottom_edge_row + 1 <= 0 /\ -1 * s V_expand_bottom_edge_z <= 0)%Z
   | 13 => (-1 * s V_expand_bottom_edge_z <= 0 /\ -1 * s V_expand_bottom_edge__tmp+ 1 * s V_expand_bottom_edge_row + 1 <= 0)%Z
   | 14 => (-1 * s V_expand_bottom_edge_z <= 0 /\ -1 * s V_expand_bottom_edge__tmp+ 1 * s V_expand_bottom_edge_row <= 0)%Z
   | 15 => (-1 * s V_expand_bottom_edge__tmp+ 1 * s V_expand_bottom_edge_row <= 0 /\ -1 * s V_expand_bottom_edge_z <= 0)%Z
   | 16 => (-1 * s V_expand_bottom_edge_z <= 0 /\ -1 * s V_expand_bottom_edge__tmp+ 1 * s V_expand_bottom_edge_row <= 0)%Z
   | 17 => (-1 * s V_expand_bottom_edge__tmp+ 1 * s V_expand_bottom_edge_row <= 0 /\ -1 * s V_expand_bottom_edge_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_expand_bottom_edge (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(-s V_expand_bottom_edge_input_rows
                + s V_expand_bottom_edge_output_rows) <= z)%Q
   | 2 => (s V_expand_bottom_edge_z
           + max0(-s V_expand_bottom_edge_input_rows
                  + s V_expand_bottom_edge_output_rows) <= z)%Q
   | 3 => (s V_expand_bottom_edge_z
           + max0(-s V_expand_bottom_edge_input_rows
                  + s V_expand_bottom_edge_output_rows) <= z)%Q
   | 4 => (s V_expand_bottom_edge_z
           + max0(-s V_expand_bottom_edge__tmp1
                  + s V_expand_bottom_edge_output_rows) <= z)%Q
   | 5 => (s V_expand_bottom_edge_z
           + max0(s V_expand_bottom_edge__tmp - s V_expand_bottom_edge__tmp1) <= z)%Q
   | 6 => (s V_expand_bottom_edge_z
           + max0(s V_expand_bottom_edge__tmp - s V_expand_bottom_edge_row) <= z)%Q
   | 7 => (s V_expand_bottom_edge_z
           + max0(s V_expand_bottom_edge__tmp - s V_expand_bottom_edge_row) <= z)%Q
   | 8 => (s V_expand_bottom_edge_z
           + max0(s V_expand_bottom_edge__tmp - s V_expand_bottom_edge_row) <= z)%Q
   | 9 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_expand_bottom_edge__tmp
                                             - s V_expand_bottom_edge_row) (-1
                                                                    + s V_expand_bottom_edge__tmp
                                                                    - s V_expand_bottom_edge_row));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                 + s V_expand_bottom_edge__tmp
                                                 - s V_expand_bottom_edge_row)) (F_check_ge (0) (0))]
     (s V_expand_bottom_edge_z
      + max0(s V_expand_bottom_edge__tmp - s V_expand_bottom_edge_row) <= z)%Q
   | 10 => (s V_expand_bottom_edge_z <= z)%Q
   | 11 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_expand_bottom_edge__tmp
                                       - s V_expand_bottom_edge_row) (1)]
     (s V_expand_bottom_edge_z
      + max0(s V_expand_bottom_edge__tmp - s V_expand_bottom_edge_row) <= z)%Q
   | 12 => ((1 # 1) + s V_expand_bottom_edge_z
            + max0(-1 + s V_expand_bottom_edge__tmp
                   - s V_expand_bottom_edge_row) <= z)%Q
   | 13 => ((1 # 1) + s V_expand_bottom_edge_z
            + max0(-1 + s V_expand_bottom_edge__tmp
                   - s V_expand_bottom_edge_row) <= z)%Q
   | 14 => ((1 # 1) + s V_expand_bottom_edge_z
            + max0(s V_expand_bottom_edge__tmp - s V_expand_bottom_edge_row) <= z)%Q
   | 15 => ((1 # 1) + s V_expand_bottom_edge_z
            + max0(s V_expand_bottom_edge__tmp - s V_expand_bottom_edge_row) <= z)%Q
   | 16 => ((1 # 1) + s V_expand_bottom_edge_z
            + max0(s V_expand_bottom_edge__tmp - s V_expand_bottom_edge_row) <= z)%Q
   | 17 => (s V_expand_bottom_edge_z
            + max0(s V_expand_bottom_edge__tmp - s V_expand_bottom_edge_row) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_expand_bottom_edge =>
    [mkPA Q (fun n z s => ai_expand_bottom_edge n s /\ annot0_expand_bottom_edge n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_expand_bottom_edge (proc_start P_expand_bottom_edge) s1 (proc_end P_expand_bottom_edge) s2 ->
    (s2 V_expand_bottom_edge_z <= max0(-s1 V_expand_bottom_edge_input_rows
                                       + s1 V_expand_bottom_edge_output_rows))%Q.
Proof.
  prove_bound ipa admissible_ipa P_expand_bottom_edge.
Qed.
