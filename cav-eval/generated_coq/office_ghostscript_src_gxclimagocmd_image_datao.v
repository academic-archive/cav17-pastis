Require Import pasta.Pasta.

Inductive proc: Type :=
  P_cmd_image_data.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_cmd_image_data_z := 1%positive.
Notation V_cmd_image_data__tmp := 2%positive.
Notation V_cmd_image_data__tmp1 := 3%positive.
Notation V_cmd_image_data__tmp2 := 4%positive.
Notation V_cmd_image_data__tmp3 := 5%positive.
Notation V_cmd_image_data__tmp4 := 6%positive.
Notation V_cmd_image_data_i := 7%positive.
Notation V_cmd_image_data_len := 8%positive.
Notation V_cmd_image_data_nbytes := 9%positive.
Notation V_cmd_image_data_bytes_per_row := 10%positive.
Notation V_cmd_image_data_cldev := 11%positive.
Notation V_cmd_image_data_data := 12%positive.
Notation V_cmd_image_data_data_x := 13%positive.
Notation V_cmd_image_data_h := 14%positive.
Notation V_cmd_image_data_pcls := 15%positive.
Notation V_cmd_image_data_raster := 16%positive.
Definition Pedges_cmd_image_data: list (edge proc) :=
  (EA 1 (AAssign V_cmd_image_data_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_cmd_image_data__tmp (Some (EVar V_cmd_image_data_data_x))) 3)::
  (EA 3 (AAssign V_cmd_image_data__tmp4
  (Some (EVar V_cmd_image_data_raster))) 4)::(EA 4 (AAssign
  V_cmd_image_data__tmp3 (Some (EVar V_cmd_image_data_bytes_per_row))) 5)::
  (EA 5 (AAssign V_cmd_image_data__tmp2
  (Some (EVar V_cmd_image_data_h))) 6)::(EA 6 (AAssign
  V_cmd_image_data_nbytes (Some (EMul (EVar V_cmd_image_data__tmp3)
  (EVar V_cmd_image_data__tmp2)))) 7)::(EA 7 AWeaken 8)::(EA 8 ANone 10)::
  (EA 8 ANone 9)::(EA 9 ANone 16)::(EA 10 ANone 11)::(EA 11 (AAssign
  V_cmd_image_data_len None) 12)::(EA 12 AWeaken 13)::(EA 13 (AGuard
  (fun s => True)) 15)::(EA 13 (AGuard (fun s => True)) 14)::
  (EA 14 AWeaken 21)::(EA 15 AWeaken 16)::(EA 16 ANone 17)::
  (EA 17 AWeaken 18)::(EA 18 ANone 47)::(EA 18 ANone 19)::(EA 19 ANone 20)::
  (EA 20 ANone 21)::(EA 21 ANone 22)::(EA 22 AWeaken 23)::(EA 23 ANone 44)::
  (EA 23 ANone 24)::(EA 24 ANone 25)::(EA 25 AWeaken 26)::(EA 26 ANone 28)::
  (EA 26 ANone 27)::(EA 27 ANone 29)::(EA 28 ANone 29)::(EA 29 (AAssign
  V_cmd_image_data_i (Some (ENum (0)))) 30)::(EA 30 ANone 31)::
  (EA 31 AWeaken 32)::(EA 32 (AGuard
  (fun s => ((eval (EVar V_cmd_image_data_i) s) <
  (eval (EVar V_cmd_image_data__tmp2) s))%Z)) 37)::(EA 32 (AGuard
  (fun s => ((eval (EVar V_cmd_image_data_i) s) >=
  (eval (EVar V_cmd_image_data__tmp2) s))%Z)) 33)::(EA 33 AWeaken 34)::
  (EA 34 (AAssign V_cmd_image_data__tmp1 (Some (ENum (0)))) 35)::
  (EA 35 ANone 36)::(EA 36 AWeaken 50)::(EA 37 AWeaken 38)::
  (EA 38 ANone 39)::(EA 39 (AAssign V_cmd_image_data_i
  (Some (EAdd (EVar V_cmd_image_data_i) (ENum (1))))) 40)::(EA 40 ANone 41)::
  (EA 41 ANone 42)::(EA 42 (AAssign V_cmd_image_data_z (Some (EAdd (ENum (1))
  (EVar V_cmd_image_data_z)))) 43)::(EA 43 AWeaken 32)::(EA 44 (AAssign
  V_cmd_image_data__tmp1 None) 45)::(EA 45 ANone 46)::(EA 46 AWeaken 50)::
  (EA 47 (AAssign V_cmd_image_data__tmp1 None) 48)::(EA 48 ANone 49)::
  (EA 49 AWeaken 50)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_cmd_image_data => Pedges_cmd_image_data
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_cmd_image_data => 50
     end)%positive;
  var_global := var_global
}.

Definition ai_cmd_image_data (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_cmd_image_data_z <= 0 /\ -1 * s V_cmd_image_data_z <= 0)%Z
   | 3 => (-1 * s V_cmd_image_data_z <= 0 /\ 1 * s V_cmd_image_data_z <= 0)%Z
   | 4 => (1 * s V_cmd_image_data_z <= 0 /\ -1 * s V_cmd_image_data_z <= 0)%Z
   | 5 => (-1 * s V_cmd_image_data_z <= 0 /\ 1 * s V_cmd_image_data_z <= 0)%Z
   | 6 => (1 * s V_cmd_image_data_z <= 0 /\ -1 * s V_cmd_image_data_z <= 0)%Z
   | 7 => (-1 * s V_cmd_image_data_z <= 0 /\ 1 * s V_cmd_image_data_z <= 0)%Z
   | 8 => (1 * s V_cmd_image_data_z <= 0 /\ -1 * s V_cmd_image_data_z <= 0)%Z
   | 9 => (-1 * s V_cmd_image_data_z <= 0 /\ 1 * s V_cmd_image_data_z <= 0)%Z
   | 10 => (-1 * s V_cmd_image_data_z <= 0 /\ 1 * s V_cmd_image_data_z <= 0)%Z
   | 11 => (1 * s V_cmd_image_data_z <= 0 /\ -1 * s V_cmd_image_data_z <= 0)%Z
   | 12 => (-1 * s V_cmd_image_data_z <= 0 /\ 1 * s V_cmd_image_data_z <= 0)%Z
   | 13 => (1 * s V_cmd_image_data_z <= 0 /\ -1 * s V_cmd_image_data_z <= 0)%Z
   | 14 => (-1 * s V_cmd_image_data_z <= 0 /\ 1 * s V_cmd_image_data_z <= 0)%Z
   | 15 => (-1 * s V_cmd_image_data_z <= 0 /\ 1 * s V_cmd_image_data_z <= 0)%Z
   | 16 => (1 * s V_cmd_image_data_z <= 0 /\ -1 * s V_cmd_image_data_z <= 0)%Z
   | 17 => (-1 * s V_cmd_image_data_z <= 0 /\ 1 * s V_cmd_image_data_z <= 0)%Z
   | 18 => (1 * s V_cmd_image_data_z <= 0 /\ -1 * s V_cmd_image_data_z <= 0)%Z
   | 19 => (-1 * s V_cmd_image_data_z <= 0 /\ 1 * s V_cmd_image_data_z <= 0)%Z
   | 20 => (1 * s V_cmd_image_data_z <= 0 /\ -1 * s V_cmd_image_data_z <= 0)%Z
   | 21 => (-1 * s V_cmd_image_data_z <= 0 /\ 1 * s V_cmd_image_data_z <= 0)%Z
   | 22 => (1 * s V_cmd_image_data_z <= 0 /\ -1 * s V_cmd_image_data_z <= 0)%Z
   | 23 => (-1 * s V_cmd_image_data_z <= 0 /\ 1 * s V_cmd_image_data_z <= 0)%Z
   | 24 => (1 * s V_cmd_image_data_z <= 0 /\ -1 * s V_cmd_image_data_z <= 0)%Z
   | 25 => (-1 * s V_cmd_image_data_z <= 0 /\ 1 * s V_cmd_image_data_z <= 0)%Z
   | 26 => (1 * s V_cmd_image_data_z <= 0 /\ -1 * s V_cmd_image_data_z <= 0)%Z
   | 27 => (-1 * s V_cmd_image_data_z <= 0 /\ 1 * s V_cmd_image_data_z <= 0)%Z
   | 28 => (-1 * s V_cmd_image_data_z <= 0 /\ 1 * s V_cmd_image_data_z <= 0)%Z
   | 29 => (1 * s V_cmd_image_data_z <= 0 /\ -1 * s V_cmd_image_data_z <= 0)%Z
   | 30 => (-1 * s V_cmd_image_data_z <= 0 /\ 1 * s V_cmd_image_data_z <= 0 /\ 1 * s V_cmd_image_data_i <= 0 /\ -1 * s V_cmd_image_data_i <= 0)%Z
   | 31 => (-1 * s V_cmd_image_data_i <= 0 /\ 1 * s V_cmd_image_data_i <= 0 /\ 1 * s V_cmd_image_data_z <= 0 /\ -1 * s V_cmd_image_data_z <= 0)%Z
   | 32 => (-1 * s V_cmd_image_data_z <= 0 /\ -1 * s V_cmd_image_data_i <= 0)%Z
   | 33 => (-1 * s V_cmd_image_data_i <= 0 /\ -1 * s V_cmd_image_data_z <= 0 /\ 1 * s V_cmd_image_data__tmp2+ -1 * s V_cmd_image_data_i <= 0)%Z
   | 34 => (1 * s V_cmd_image_data__tmp2+ -1 * s V_cmd_image_data_i <= 0 /\ -1 * s V_cmd_image_data_z <= 0 /\ -1 * s V_cmd_image_data_i <= 0)%Z
   | 35 => (-1 * s V_cmd_image_data_i <= 0 /\ -1 * s V_cmd_image_data_z <= 0 /\ 1 * s V_cmd_image_data__tmp2+ -1 * s V_cmd_image_data_i <= 0 /\ 1 * s V_cmd_image_data__tmp1 <= 0 /\ -1 * s V_cmd_image_data__tmp1 <= 0)%Z
   | 36 => (-1 * s V_cmd_image_data__tmp1 <= 0 /\ 1 * s V_cmd_image_data__tmp1 <= 0 /\ 1 * s V_cmd_image_data__tmp2+ -1 * s V_cmd_image_data_i <= 0 /\ -1 * s V_cmd_image_data_z <= 0 /\ -1 * s V_cmd_image_data_i <= 0)%Z
   | 37 => (-1 * s V_cmd_image_data_i <= 0 /\ -1 * s V_cmd_image_data_z <= 0 /\ -1 * s V_cmd_image_data__tmp2+ 1 * s V_cmd_image_data_i + 1 <= 0)%Z
   | 38 => (-1 * s V_cmd_image_data__tmp2+ 1 * s V_cmd_image_data_i + 1 <= 0 /\ -1 * s V_cmd_image_data_z <= 0 /\ -1 * s V_cmd_image_data_i <= 0)%Z
   | 39 => (-1 * s V_cmd_image_data_i <= 0 /\ -1 * s V_cmd_image_data_z <= 0 /\ -1 * s V_cmd_image_data__tmp2+ 1 * s V_cmd_image_data_i + 1 <= 0)%Z
   | 40 => (-1 * s V_cmd_image_data_z <= 0 /\ -1 * s V_cmd_image_data_i + 1 <= 0 /\ -1 * s V_cmd_image_data__tmp2+ 1 * s V_cmd_image_data_i <= 0)%Z
   | 41 => (-1 * s V_cmd_image_data__tmp2+ 1 * s V_cmd_image_data_i <= 0 /\ -1 * s V_cmd_image_data_i + 1 <= 0 /\ -1 * s V_cmd_image_data_z <= 0)%Z
   | 42 => (-1 * s V_cmd_image_data_z <= 0 /\ -1 * s V_cmd_image_data_i + 1 <= 0 /\ -1 * s V_cmd_image_data__tmp2+ 1 * s V_cmd_image_data_i <= 0)%Z
   | 43 => (-1 * s V_cmd_image_data__tmp2+ 1 * s V_cmd_image_data_i <= 0 /\ -1 * s V_cmd_image_data_i + 1 <= 0 /\ -1 * s V_cmd_image_data_z + 1 <= 0)%Z
   | 44 => (1 * s V_cmd_image_data_z <= 0 /\ -1 * s V_cmd_image_data_z <= 0)%Z
   | 45 => (-1 * s V_cmd_image_data_z <= 0 /\ 1 * s V_cmd_image_data_z <= 0)%Z
   | 46 => (1 * s V_cmd_image_data_z <= 0 /\ -1 * s V_cmd_image_data_z <= 0)%Z
   | 47 => (-1 * s V_cmd_image_data_z <= 0 /\ 1 * s V_cmd_image_data_z <= 0)%Z
   | 48 => (1 * s V_cmd_image_data_z <= 0 /\ -1 * s V_cmd_image_data_z <= 0)%Z
   | 49 => (-1 * s V_cmd_image_data_z <= 0 /\ 1 * s V_cmd_image_data_z <= 0)%Z
   | 50 => (-1 * s V_cmd_image_data_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_cmd_image_data (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_cmd_image_data_h) <= z)%Q
   | 2 => (max0(s V_cmd_image_data_h) + max0(s V_cmd_image_data_z) <= z)%Q
   | 3 => (max0(s V_cmd_image_data_h) + max0(s V_cmd_image_data_z) <= z)%Q
   | 4 => (max0(s V_cmd_image_data_h) + max0(s V_cmd_image_data_z) <= z)%Q
   | 5 => (max0(s V_cmd_image_data_h) + max0(s V_cmd_image_data_z) <= z)%Q
   | 6 => (max0(s V_cmd_image_data__tmp2) + max0(s V_cmd_image_data_z) <= z)%Q
   | 7 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_cmd_image_data_z)) (F_check_ge (s V_cmd_image_data_z) (0))]
     (max0(s V_cmd_image_data__tmp2) + max0(s V_cmd_image_data_z) <= z)%Q
   | 8 => (s V_cmd_image_data_z + max0(s V_cmd_image_data__tmp2) <= z)%Q
   | 9 => (s V_cmd_image_data_z + max0(s V_cmd_image_data__tmp2) <= z)%Q
   | 10 => (s V_cmd_image_data_z + max0(s V_cmd_image_data__tmp2) <= z)%Q
   | 11 => (s V_cmd_image_data_z + max0(s V_cmd_image_data__tmp2) <= z)%Q
   | 12 => (s V_cmd_image_data_z + max0(s V_cmd_image_data__tmp2) <= z)%Q
   | 13 => (s V_cmd_image_data_z + max0(s V_cmd_image_data__tmp2) <= z)%Q
   | 14 => (s V_cmd_image_data_z + max0(s V_cmd_image_data__tmp2) <= z)%Q
   | 15 => (s V_cmd_image_data_z + max0(s V_cmd_image_data__tmp2) <= z)%Q
   | 16 => (s V_cmd_image_data_z + max0(s V_cmd_image_data__tmp2) <= z)%Q
   | 17 => (s V_cmd_image_data_z + max0(s V_cmd_image_data__tmp2) <= z)%Q
   | 18 => (s V_cmd_image_data_z + max0(s V_cmd_image_data__tmp2) <= z)%Q
   | 19 => (s V_cmd_image_data_z + max0(s V_cmd_image_data__tmp2) <= z)%Q
   | 20 => (s V_cmd_image_data_z + max0(s V_cmd_image_data__tmp2) <= z)%Q
   | 21 => (s V_cmd_image_data_z + max0(s V_cmd_image_data__tmp2) <= z)%Q
   | 22 => (s V_cmd_image_data_z + max0(s V_cmd_image_data__tmp2) <= z)%Q
   | 23 => (s V_cmd_image_data_z + max0(s V_cmd_image_data__tmp2) <= z)%Q
   | 24 => (s V_cmd_image_data_z + max0(s V_cmd_image_data__tmp2) <= z)%Q
   | 25 => (s V_cmd_image_data_z + max0(s V_cmd_image_data__tmp2) <= z)%Q
   | 26 => (s V_cmd_image_data_z + max0(s V_cmd_image_data__tmp2) <= z)%Q
   | 27 => (s V_cmd_image_data_z + max0(s V_cmd_image_data__tmp2) <= z)%Q
   | 28 => (s V_cmd_image_data_z + max0(s V_cmd_image_data__tmp2) <= z)%Q
   | 29 => (s V_cmd_image_data_z + max0(s V_cmd_image_data__tmp2) <= z)%Q
   | 30 => (s V_cmd_image_data_z
            + max0(s V_cmd_image_data__tmp2 - s V_cmd_image_data_i) <= z)%Q
   | 31 => (s V_cmd_image_data_z
            + max0(s V_cmd_image_data__tmp2 - s V_cmd_image_data_i) <= z)%Q
   | 32 => (s V_cmd_image_data_z
            + max0(s V_cmd_image_data__tmp2 - s V_cmd_image_data_i) <= z)%Q
   | 33 => (s V_cmd_image_data_z
            + max0(s V_cmd_image_data__tmp2 - s V_cmd_image_data_i) <= z)%Q
   | 34 => (s V_cmd_image_data_z
            + max0(s V_cmd_image_data__tmp2 - s V_cmd_image_data_i) <= z)%Q
   | 35 => (s V_cmd_image_data_z
            + max0(s V_cmd_image_data__tmp2 - s V_cmd_image_data_i) <= z)%Q
   | 36 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_cmd_image_data__tmp2
                                             - s V_cmd_image_data_i) (-1
                                                                    + s V_cmd_image_data__tmp2
                                                                    - s V_cmd_image_data_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V_cmd_image_data__tmp2
                            - s V_cmd_image_data_i)]
     (s V_cmd_image_data_z
      + max0(s V_cmd_image_data__tmp2 - s V_cmd_image_data_i) <= z)%Q
   | 37 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_cmd_image_data__tmp2
                                       - s V_cmd_image_data_i) (1)]
     (s V_cmd_image_data_z
      + max0(s V_cmd_image_data__tmp2 - s V_cmd_image_data_i) <= z)%Q
   | 38 => ((1 # 1) + s V_cmd_image_data_z
            + max0(-1 + s V_cmd_image_data__tmp2 - s V_cmd_image_data_i) <= z)%Q
   | 39 => ((1 # 1) + s V_cmd_image_data_z
            + max0(-1 + s V_cmd_image_data__tmp2 - s V_cmd_image_data_i) <= z)%Q
   | 40 => ((1 # 1) + s V_cmd_image_data_z
            + max0(s V_cmd_image_data__tmp2 - s V_cmd_image_data_i) <= z)%Q
   | 41 => ((1 # 1) + s V_cmd_image_data_z
            + max0(s V_cmd_image_data__tmp2 - s V_cmd_image_data_i) <= z)%Q
   | 42 => ((1 # 1) + s V_cmd_image_data_z
            + max0(s V_cmd_image_data__tmp2 - s V_cmd_image_data_i) <= z)%Q
   | 43 => (s V_cmd_image_data_z
            + max0(s V_cmd_image_data__tmp2 - s V_cmd_image_data_i) <= z)%Q
   | 44 => (s V_cmd_image_data_z + max0(s V_cmd_image_data__tmp2) <= z)%Q
   | 45 => (s V_cmd_image_data_z + max0(s V_cmd_image_data__tmp2) <= z)%Q
   | 46 => hints
     [(*-1 0*) F_max0_ge_0 (s V_cmd_image_data__tmp2)]
     (s V_cmd_image_data_z + max0(s V_cmd_image_data__tmp2) <= z)%Q
   | 47 => (s V_cmd_image_data_z + max0(s V_cmd_image_data__tmp2) <= z)%Q
   | 48 => (s V_cmd_image_data_z + max0(s V_cmd_image_data__tmp2) <= z)%Q
   | 49 => hints
     [(*-1 0*) F_max0_ge_0 (s V_cmd_image_data__tmp2)]
     (s V_cmd_image_data_z + max0(s V_cmd_image_data__tmp2) <= z)%Q
   | 50 => (s V_cmd_image_data_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_cmd_image_data =>
    [mkPA Q (fun n z s => ai_cmd_image_data n s /\ annot0_cmd_image_data n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_cmd_image_data (proc_start P_cmd_image_data) s1 (proc_end P_cmd_image_data) s2 ->
    (s2 V_cmd_image_data_z <= max0(s1 V_cmd_image_data_h))%Q.
Proof.
  prove_bound ipa admissible_ipa P_cmd_image_data.
Qed.
