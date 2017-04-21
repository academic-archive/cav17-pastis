Require Import pasta.Pasta.

Inductive proc: Type :=
  P_psw_put_bits.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_psw_put_bits_z := 1%positive.
Notation V_psw_put_bits__tmp := 2%positive.
Notation V_psw_put_bits__tmp1 := 3%positive.
Notation V_psw_put_bits__tmp2 := 4%positive.
Notation V_psw_put_bits__tmp3 := 5%positive.
Notation V_psw_put_bits_y := 6%positive.
Notation V_psw_put_bits_data := 7%positive.
Notation V_psw_put_bits_data_x_bit := 8%positive.
Notation V_psw_put_bits_height := 9%positive.
Notation V_psw_put_bits_raster := 10%positive.
Notation V_psw_put_bits_s := 11%positive.
Notation V_psw_put_bits_width_bits := 12%positive.
Definition Pedges_psw_put_bits: list (edge proc) :=
  (EA 1 (AAssign V_psw_put_bits_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_psw_put_bits__tmp3 (Some (EVar V_psw_put_bits_data_x_bit))) 3)::
  (EA 3 (AAssign V_psw_put_bits__tmp2
  (Some (EVar V_psw_put_bits_raster))) 4)::(EA 4 (AAssign
  V_psw_put_bits__tmp1 (Some (EVar V_psw_put_bits_width_bits))) 5)::
  (EA 5 (AAssign V_psw_put_bits__tmp
  (Some (EVar V_psw_put_bits_height))) 6)::(EA 6 (AAssign V_psw_put_bits_y
  (Some (ENum (0)))) 7)::(EA 7 ANone 8)::(EA 8 AWeaken 9)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_psw_put_bits_y) s) <
  (eval (EVar V_psw_put_bits__tmp) s))%Z)) 12)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_psw_put_bits_y) s) >=
  (eval (EVar V_psw_put_bits__tmp) s))%Z)) 10)::(EA 10 AWeaken 11)::
  (EA 12 AWeaken 13)::(EA 13 ANone 14)::(EA 14 (AAssign V_psw_put_bits_y
  (Some (EAdd (EVar V_psw_put_bits_y) (ENum (1))))) 15)::(EA 15 ANone 16)::
  (EA 16 ANone 17)::(EA 17 (AAssign V_psw_put_bits_z (Some (EAdd (ENum (1))
  (EVar V_psw_put_bits_z)))) 18)::(EA 18 AWeaken 9)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_psw_put_bits => Pedges_psw_put_bits
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_psw_put_bits => 11
     end)%positive;
  var_global := var_global
}.

Definition ai_psw_put_bits (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_psw_put_bits_z <= 0 /\ -1 * s V_psw_put_bits_z <= 0)%Z
   | 3 => (-1 * s V_psw_put_bits_z <= 0 /\ 1 * s V_psw_put_bits_z <= 0)%Z
   | 4 => (1 * s V_psw_put_bits_z <= 0 /\ -1 * s V_psw_put_bits_z <= 0)%Z
   | 5 => (-1 * s V_psw_put_bits_z <= 0 /\ 1 * s V_psw_put_bits_z <= 0)%Z
   | 6 => (1 * s V_psw_put_bits_z <= 0 /\ -1 * s V_psw_put_bits_z <= 0)%Z
   | 7 => (-1 * s V_psw_put_bits_z <= 0 /\ 1 * s V_psw_put_bits_z <= 0 /\ 1 * s V_psw_put_bits_y <= 0 /\ -1 * s V_psw_put_bits_y <= 0)%Z
   | 8 => (-1 * s V_psw_put_bits_y <= 0 /\ 1 * s V_psw_put_bits_y <= 0 /\ 1 * s V_psw_put_bits_z <= 0 /\ -1 * s V_psw_put_bits_z <= 0)%Z
   | 9 => (-1 * s V_psw_put_bits_z <= 0 /\ -1 * s V_psw_put_bits_y <= 0)%Z
   | 10 => (-1 * s V_psw_put_bits_y <= 0 /\ -1 * s V_psw_put_bits_z <= 0 /\ 1 * s V_psw_put_bits__tmp+ -1 * s V_psw_put_bits_y <= 0)%Z
   | 11 => (1 * s V_psw_put_bits__tmp+ -1 * s V_psw_put_bits_y <= 0 /\ -1 * s V_psw_put_bits_z <= 0 /\ -1 * s V_psw_put_bits_y <= 0)%Z
   | 12 => (-1 * s V_psw_put_bits_y <= 0 /\ -1 * s V_psw_put_bits_z <= 0 /\ -1 * s V_psw_put_bits__tmp+ 1 * s V_psw_put_bits_y + 1 <= 0)%Z
   | 13 => (-1 * s V_psw_put_bits__tmp+ 1 * s V_psw_put_bits_y + 1 <= 0 /\ -1 * s V_psw_put_bits_z <= 0 /\ -1 * s V_psw_put_bits_y <= 0)%Z
   | 14 => (-1 * s V_psw_put_bits_y <= 0 /\ -1 * s V_psw_put_bits_z <= 0 /\ -1 * s V_psw_put_bits__tmp+ 1 * s V_psw_put_bits_y + 1 <= 0)%Z
   | 15 => (-1 * s V_psw_put_bits_z <= 0 /\ -1 * s V_psw_put_bits_y + 1 <= 0 /\ -1 * s V_psw_put_bits__tmp+ 1 * s V_psw_put_bits_y <= 0)%Z
   | 16 => (-1 * s V_psw_put_bits__tmp+ 1 * s V_psw_put_bits_y <= 0 /\ -1 * s V_psw_put_bits_y + 1 <= 0 /\ -1 * s V_psw_put_bits_z <= 0)%Z
   | 17 => (-1 * s V_psw_put_bits_z <= 0 /\ -1 * s V_psw_put_bits_y + 1 <= 0 /\ -1 * s V_psw_put_bits__tmp+ 1 * s V_psw_put_bits_y <= 0)%Z
   | 18 => (-1 * s V_psw_put_bits__tmp+ 1 * s V_psw_put_bits_y <= 0 /\ -1 * s V_psw_put_bits_y + 1 <= 0 /\ -1 * s V_psw_put_bits_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_psw_put_bits (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_psw_put_bits_height) <= z)%Q
   | 2 => (s V_psw_put_bits_z + max0(s V_psw_put_bits_height) <= z)%Q
   | 3 => (s V_psw_put_bits_z + max0(s V_psw_put_bits_height) <= z)%Q
   | 4 => (s V_psw_put_bits_z + max0(s V_psw_put_bits_height) <= z)%Q
   | 5 => (s V_psw_put_bits_z + max0(s V_psw_put_bits_height) <= z)%Q
   | 6 => (s V_psw_put_bits_z + max0(s V_psw_put_bits__tmp) <= z)%Q
   | 7 => (s V_psw_put_bits_z
           + max0(s V_psw_put_bits__tmp - s V_psw_put_bits_y) <= z)%Q
   | 8 => (s V_psw_put_bits_z
           + max0(s V_psw_put_bits__tmp - s V_psw_put_bits_y) <= z)%Q
   | 9 => (s V_psw_put_bits_z
           + max0(s V_psw_put_bits__tmp - s V_psw_put_bits_y) <= z)%Q
   | 10 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_psw_put_bits__tmp
                                             - s V_psw_put_bits_y) (-1
                                                                    + 
                                                                    s V_psw_put_bits__tmp
                                                                    - 
                                                                    s V_psw_put_bits_y));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_psw_put_bits__tmp
                                                 - s V_psw_put_bits_y)) (F_check_ge (0) (0))]
     (s V_psw_put_bits_z + max0(s V_psw_put_bits__tmp - s V_psw_put_bits_y) <= z)%Q
   | 11 => (s V_psw_put_bits_z <= z)%Q
   | 12 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_psw_put_bits__tmp
                                       - s V_psw_put_bits_y) (1)]
     (s V_psw_put_bits_z + max0(s V_psw_put_bits__tmp - s V_psw_put_bits_y) <= z)%Q
   | 13 => ((1 # 1) + s V_psw_put_bits_z
            + max0(-1 + s V_psw_put_bits__tmp - s V_psw_put_bits_y) <= z)%Q
   | 14 => ((1 # 1) + s V_psw_put_bits_z
            + max0(-1 + s V_psw_put_bits__tmp - s V_psw_put_bits_y) <= z)%Q
   | 15 => ((1 # 1) + s V_psw_put_bits_z
            + max0(s V_psw_put_bits__tmp - s V_psw_put_bits_y) <= z)%Q
   | 16 => ((1 # 1) + s V_psw_put_bits_z
            + max0(s V_psw_put_bits__tmp - s V_psw_put_bits_y) <= z)%Q
   | 17 => ((1 # 1) + s V_psw_put_bits_z
            + max0(s V_psw_put_bits__tmp - s V_psw_put_bits_y) <= z)%Q
   | 18 => (s V_psw_put_bits_z
            + max0(s V_psw_put_bits__tmp - s V_psw_put_bits_y) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_psw_put_bits =>
    [mkPA Q (fun n z s => ai_psw_put_bits n s /\ annot0_psw_put_bits n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_psw_put_bits (proc_start P_psw_put_bits) s1 (proc_end P_psw_put_bits) s2 ->
    (s2 V_psw_put_bits_z <= max0(s1 V_psw_put_bits_height))%Q.
Proof.
  prove_bound ipa admissible_ipa P_psw_put_bits.
Qed.
