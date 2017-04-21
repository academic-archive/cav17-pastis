Require Import pasta.Pasta.

Inductive proc: Type :=
  P_build_ycc_rgb_table.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_build_ycc_rgb_table_z := 1%positive.
Notation V_build_ycc_rgb_table_i := 2%positive.
Notation V_build_ycc_rgb_table_x := 3%positive.
Notation V_build_ycc_rgb_table_cinfo := 4%positive.
Definition Pedges_build_ycc_rgb_table: list (edge proc) :=
  (EA 1 (AAssign V_build_ycc_rgb_table_z (Some (ENum (0)))) 2)::
  (EA 2 (AAssign V_build_ycc_rgb_table_i (Some (ENum (0)))) 3)::
  (EA 3 (AAssign V_build_ycc_rgb_table_x (Some (ENum (-128)))) 4)::
  (EA 4 ANone 5)::(EA 5 AWeaken 6)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_build_ycc_rgb_table_i) s) <= (eval (ENum (255))
  s))%Z)) 9)::(EA 6 (AGuard (fun s => ((eval (EVar V_build_ycc_rgb_table_i)
  s) > (eval (ENum (255)) s))%Z)) 7)::(EA 7 AWeaken 8)::(EA 9 AWeaken 10)::
  (EA 10 ANone 11)::(EA 11 (AAssign V_build_ycc_rgb_table_i
  (Some (EAdd (EVar V_build_ycc_rgb_table_i) (ENum (1))))) 12)::
  (EA 12 (AAssign V_build_ycc_rgb_table_x
  (Some (EAdd (EVar V_build_ycc_rgb_table_x) (ENum (1))))) 13)::
  (EA 13 ANone 14)::(EA 14 ANone 15)::(EA 15 (AAssign V_build_ycc_rgb_table_z
  (Some (EAdd (ENum (1)) (EVar V_build_ycc_rgb_table_z)))) 16)::
  (EA 16 AWeaken 6)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_build_ycc_rgb_table => Pedges_build_ycc_rgb_table
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_build_ycc_rgb_table => 8
     end)%positive;
  var_global := var_global
}.

Definition ai_build_ycc_rgb_table (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_build_ycc_rgb_table_z <= 0 /\ -1 * s V_build_ycc_rgb_table_z <= 0)%Z
   | 3 => (-1 * s V_build_ycc_rgb_table_z <= 0 /\ 1 * s V_build_ycc_rgb_table_z <= 0 /\ 1 * s V_build_ycc_rgb_table_i <= 0 /\ -1 * s V_build_ycc_rgb_table_i <= 0)%Z
   | 4 => (-1 * s V_build_ycc_rgb_table_i <= 0 /\ 1 * s V_build_ycc_rgb_table_i <= 0 /\ 1 * s V_build_ycc_rgb_table_z <= 0 /\ -1 * s V_build_ycc_rgb_table_z <= 0 /\ 1 * s V_build_ycc_rgb_table_x + 128 <= 0 /\ -1 * s V_build_ycc_rgb_table_x + -128 <= 0)%Z
   | 5 => (-1 * s V_build_ycc_rgb_table_x + -128 <= 0 /\ 1 * s V_build_ycc_rgb_table_x + 128 <= 0 /\ -1 * s V_build_ycc_rgb_table_z <= 0 /\ 1 * s V_build_ycc_rgb_table_z <= 0 /\ 1 * s V_build_ycc_rgb_table_i <= 0 /\ -1 * s V_build_ycc_rgb_table_i <= 0)%Z
   | 6 => (-1 * s V_build_ycc_rgb_table_i <= 0 /\ -1 * s V_build_ycc_rgb_table_z <= 0 /\ -1 * s V_build_ycc_rgb_table_x + -128 <= 0 /\ 1 * s V_build_ycc_rgb_table_i + -256 <= 0)%Z
   | 7 => (1 * s V_build_ycc_rgb_table_i + -256 <= 0 /\ -1 * s V_build_ycc_rgb_table_x + -128 <= 0 /\ -1 * s V_build_ycc_rgb_table_z <= 0 /\ -1 * s V_build_ycc_rgb_table_i + 256 <= 0)%Z
   | 8 => (-1 * s V_build_ycc_rgb_table_i + 256 <= 0 /\ -1 * s V_build_ycc_rgb_table_z <= 0 /\ -1 * s V_build_ycc_rgb_table_x + -128 <= 0 /\ 1 * s V_build_ycc_rgb_table_i + -256 <= 0)%Z
   | 9 => (-1 * s V_build_ycc_rgb_table_x + -128 <= 0 /\ -1 * s V_build_ycc_rgb_table_z <= 0 /\ -1 * s V_build_ycc_rgb_table_i <= 0 /\ 1 * s V_build_ycc_rgb_table_i + -255 <= 0)%Z
   | 10 => (1 * s V_build_ycc_rgb_table_i + -255 <= 0 /\ -1 * s V_build_ycc_rgb_table_i <= 0 /\ -1 * s V_build_ycc_rgb_table_z <= 0 /\ -1 * s V_build_ycc_rgb_table_x + -128 <= 0)%Z
   | 11 => (-1 * s V_build_ycc_rgb_table_x + -128 <= 0 /\ -1 * s V_build_ycc_rgb_table_z <= 0 /\ -1 * s V_build_ycc_rgb_table_i <= 0 /\ 1 * s V_build_ycc_rgb_table_i + -255 <= 0)%Z
   | 12 => (-1 * s V_build_ycc_rgb_table_z <= 0 /\ -1 * s V_build_ycc_rgb_table_x + -128 <= 0 /\ -1 * s V_build_ycc_rgb_table_i + 1 <= 0 /\ 1 * s V_build_ycc_rgb_table_i + -256 <= 0)%Z
   | 13 => (1 * s V_build_ycc_rgb_table_i + -256 <= 0 /\ -1 * s V_build_ycc_rgb_table_i + 1 <= 0 /\ -1 * s V_build_ycc_rgb_table_z <= 0 /\ -1 * s V_build_ycc_rgb_table_x + -127 <= 0)%Z
   | 14 => (-1 * s V_build_ycc_rgb_table_x + -127 <= 0 /\ -1 * s V_build_ycc_rgb_table_z <= 0 /\ -1 * s V_build_ycc_rgb_table_i + 1 <= 0 /\ 1 * s V_build_ycc_rgb_table_i + -256 <= 0)%Z
   | 15 => (1 * s V_build_ycc_rgb_table_i + -256 <= 0 /\ -1 * s V_build_ycc_rgb_table_i + 1 <= 0 /\ -1 * s V_build_ycc_rgb_table_z <= 0 /\ -1 * s V_build_ycc_rgb_table_x + -127 <= 0)%Z
   | 16 => (-1 * s V_build_ycc_rgb_table_x + -127 <= 0 /\ -1 * s V_build_ycc_rgb_table_i + 1 <= 0 /\ 1 * s V_build_ycc_rgb_table_i + -256 <= 0 /\ -1 * s V_build_ycc_rgb_table_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_build_ycc_rgb_table (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((256 # 1) <= z)%Q
   | 2 => ((256 # 1) + s V_build_ycc_rgb_table_z <= z)%Q
   | 3 => ((256 # 1) - s V_build_ycc_rgb_table_i + s V_build_ycc_rgb_table_z <= z)%Q
   | 4 => ((256 # 1) - s V_build_ycc_rgb_table_i + s V_build_ycc_rgb_table_z <= z)%Q
   | 5 => ((256 # 1) - s V_build_ycc_rgb_table_i + s V_build_ycc_rgb_table_z <= z)%Q
   | 6 => ((256 # 1) - s V_build_ycc_rgb_table_i + s V_build_ycc_rgb_table_z <= z)%Q
   | 7 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (256 - s V_build_ycc_rgb_table_i) (255
                                                                    - s V_build_ycc_rgb_table_i));
      (*-1 0*) F_max0_ge_0 (255 - s V_build_ycc_rgb_table_i);
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (256
                                                              - s V_build_ycc_rgb_table_i) (0))) (F_max0_ge_0 (256
                                                                    - s V_build_ycc_rgb_table_i))]
     ((256 # 1) - s V_build_ycc_rgb_table_i + s V_build_ycc_rgb_table_z <= z)%Q
   | 8 => (s V_build_ycc_rgb_table_z <= z)%Q
   | 9 => ((256 # 1) - s V_build_ycc_rgb_table_i + s V_build_ycc_rgb_table_z <= z)%Q
   | 10 => ((256 # 1) - s V_build_ycc_rgb_table_i + s V_build_ycc_rgb_table_z <= z)%Q
   | 11 => ((256 # 1) - s V_build_ycc_rgb_table_i + s V_build_ycc_rgb_table_z <= z)%Q
   | 12 => ((257 # 1) - s V_build_ycc_rgb_table_i + s V_build_ycc_rgb_table_z <= z)%Q
   | 13 => ((257 # 1) - s V_build_ycc_rgb_table_i + s V_build_ycc_rgb_table_z <= z)%Q
   | 14 => ((257 # 1) - s V_build_ycc_rgb_table_i + s V_build_ycc_rgb_table_z <= z)%Q
   | 15 => ((257 # 1) - s V_build_ycc_rgb_table_i + s V_build_ycc_rgb_table_z <= z)%Q
   | 16 => ((256 # 1) - s V_build_ycc_rgb_table_i + s V_build_ycc_rgb_table_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_build_ycc_rgb_table =>
    [mkPA Q (fun n z s => ai_build_ycc_rgb_table n s /\ annot0_build_ycc_rgb_table n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_build_ycc_rgb_table (proc_start P_build_ycc_rgb_table) s1 (proc_end P_build_ycc_rgb_table) s2 ->
    (s2 V_build_ycc_rgb_table_z <= (256 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_build_ycc_rgb_table.
Qed.
