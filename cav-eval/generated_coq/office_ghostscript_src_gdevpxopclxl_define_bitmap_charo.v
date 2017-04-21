Require Import pasta.Pasta.

Inductive proc: Type :=
  P_pclxl_define_bitmap_char.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_pclxl_define_bitmap_char_z := 1%positive.
Notation V_pclxl_define_bitmap_char__tmp := 2%positive.
Notation V_pclxl_define_bitmap_char__tmp1 := 3%positive.
Notation V_pclxl_define_bitmap_char__tmp2 := 4%positive.
Notation V_pclxl_define_bitmap_char__tmp3 := 5%positive.
Notation V_pclxl_define_bitmap_char_i := 6%positive.
Notation V_pclxl_define_bitmap_char_size := 7%positive.
Notation V_pclxl_define_bitmap_char_width_bytes := 8%positive.
Notation V_pclxl_define_bitmap_char_ccode := 9%positive.
Notation V_pclxl_define_bitmap_char_data := 10%positive.
Notation V_pclxl_define_bitmap_char_height := 11%positive.
Notation V_pclxl_define_bitmap_char_raster := 12%positive.
Notation V_pclxl_define_bitmap_char_width_bits := 13%positive.
Notation V_pclxl_define_bitmap_char_xdev := 14%positive.
Definition Pedges_pclxl_define_bitmap_char: list (edge proc) :=
  (EA 1 (AAssign V_pclxl_define_bitmap_char_z (Some (ENum (0)))) 2)::
  (EA 2 (AGuard (fun s => ((eval (EVar V_pclxl_define_bitmap_char_size) s) >=
  (eval (ENum (0)) s))%Z)) 3)::(EA 3 (AGuard
  (fun s => ((eval (EVar V_pclxl_define_bitmap_char_i) s) >= (eval (ENum (0))
  s))%Z)) 4)::(EA 4 (AGuard
  (fun s => ((eval (EVar V_pclxl_define_bitmap_char__tmp) s) >=
  (eval (ENum (0)) s))%Z)) 5)::(EA 5 AWeaken 6)::(EA 6 (AAssign
  V_pclxl_define_bitmap_char__tmp3
  (Some (EVar V_pclxl_define_bitmap_char_ccode))) 7)::(EA 7 (AAssign
  V_pclxl_define_bitmap_char__tmp2
  (Some (EVar V_pclxl_define_bitmap_char_raster))) 8)::(EA 8 (AAssign
  V_pclxl_define_bitmap_char__tmp1
  (Some (EVar V_pclxl_define_bitmap_char_width_bits))) 9)::(EA 9 (AAssign
  V_pclxl_define_bitmap_char__tmp
  (Some (EVar V_pclxl_define_bitmap_char_height))) 10)::(EA 10 (AAssign
  V_pclxl_define_bitmap_char_width_bytes None) 11)::(EA 11 (AAssign
  V_pclxl_define_bitmap_char_size (Some (EAdd (ENum (10))
  (EMul (EVar V_pclxl_define_bitmap_char_width_bytes)
  (EVar V_pclxl_define_bitmap_char__tmp))))) 12)::(EA 12 ANone 13)::
  (EA 13 ANone 14)::(EA 14 AWeaken 15)::(EA 15 ANone 17)::(EA 15 ANone 16)::
  (EA 16 ANone 18)::(EA 17 ANone 18)::(EA 18 ANone 19)::(EA 19 ANone 20)::
  (EA 20 (AAssign V_pclxl_define_bitmap_char_i (Some (ENum (0)))) 21)::
  (EA 21 ANone 22)::(EA 22 AWeaken 23)::(EA 23 (AGuard
  (fun s => ((eval (EVar V_pclxl_define_bitmap_char_i) s) <
  (eval (EVar V_pclxl_define_bitmap_char__tmp) s))%Z)) 26)::(EA 23 (AGuard
  (fun s => ((eval (EVar V_pclxl_define_bitmap_char_i) s) >=
  (eval (EVar V_pclxl_define_bitmap_char__tmp) s))%Z)) 24)::
  (EA 24 AWeaken 25)::(EA 26 AWeaken 27)::(EA 27 ANone 28)::(EA 28 (AAssign
  V_pclxl_define_bitmap_char_i
  (Some (EAdd (EVar V_pclxl_define_bitmap_char_i) (ENum (1))))) 29)::
  (EA 29 ANone 30)::(EA 30 ANone 31)::(EA 31 (AAssign
  V_pclxl_define_bitmap_char_z (Some (EAdd (ENum (1))
  (EVar V_pclxl_define_bitmap_char_z)))) 32)::(EA 32 AWeaken 23)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_pclxl_define_bitmap_char => Pedges_pclxl_define_bitmap_char
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_pclxl_define_bitmap_char => 25
     end)%positive;
  var_global := var_global
}.

Definition ai_pclxl_define_bitmap_char (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_pclxl_define_bitmap_char_z <= 0 /\ -1 * s V_pclxl_define_bitmap_char_z <= 0)%Z
   | 3 => (-1 * s V_pclxl_define_bitmap_char_z <= 0 /\ 1 * s V_pclxl_define_bitmap_char_z <= 0 /\ -1 * s V_pclxl_define_bitmap_char_size <= 0)%Z
   | 4 => (-1 * s V_pclxl_define_bitmap_char_size <= 0 /\ 1 * s V_pclxl_define_bitmap_char_z <= 0 /\ -1 * s V_pclxl_define_bitmap_char_z <= 0 /\ -1 * s V_pclxl_define_bitmap_char_i <= 0)%Z
   | 5 => (-1 * s V_pclxl_define_bitmap_char_i <= 0 /\ -1 * s V_pclxl_define_bitmap_char_z <= 0 /\ 1 * s V_pclxl_define_bitmap_char_z <= 0 /\ -1 * s V_pclxl_define_bitmap_char_size <= 0 /\ -1 * s V_pclxl_define_bitmap_char__tmp <= 0)%Z
   | 6 => (-1 * s V_pclxl_define_bitmap_char__tmp <= 0 /\ -1 * s V_pclxl_define_bitmap_char_size <= 0 /\ 1 * s V_pclxl_define_bitmap_char_z <= 0 /\ -1 * s V_pclxl_define_bitmap_char_z <= 0 /\ -1 * s V_pclxl_define_bitmap_char_i <= 0)%Z
   | 7 => (-1 * s V_pclxl_define_bitmap_char_i <= 0 /\ -1 * s V_pclxl_define_bitmap_char_z <= 0 /\ 1 * s V_pclxl_define_bitmap_char_z <= 0 /\ -1 * s V_pclxl_define_bitmap_char_size <= 0 /\ -1 * s V_pclxl_define_bitmap_char__tmp <= 0)%Z
   | 8 => (-1 * s V_pclxl_define_bitmap_char__tmp <= 0 /\ -1 * s V_pclxl_define_bitmap_char_size <= 0 /\ 1 * s V_pclxl_define_bitmap_char_z <= 0 /\ -1 * s V_pclxl_define_bitmap_char_z <= 0 /\ -1 * s V_pclxl_define_bitmap_char_i <= 0)%Z
   | 9 => (-1 * s V_pclxl_define_bitmap_char_i <= 0 /\ -1 * s V_pclxl_define_bitmap_char_z <= 0 /\ 1 * s V_pclxl_define_bitmap_char_z <= 0 /\ -1 * s V_pclxl_define_bitmap_char_size <= 0 /\ -1 * s V_pclxl_define_bitmap_char__tmp <= 0)%Z
   | 10 => (-1 * s V_pclxl_define_bitmap_char_size <= 0 /\ 1 * s V_pclxl_define_bitmap_char_z <= 0 /\ -1 * s V_pclxl_define_bitmap_char_z <= 0 /\ -1 * s V_pclxl_define_bitmap_char_i <= 0)%Z
   | 11 => (-1 * s V_pclxl_define_bitmap_char_i <= 0 /\ -1 * s V_pclxl_define_bitmap_char_z <= 0 /\ 1 * s V_pclxl_define_bitmap_char_z <= 0 /\ -1 * s V_pclxl_define_bitmap_char_size <= 0)%Z
   | 12 => (1 * s V_pclxl_define_bitmap_char_z <= 0 /\ -1 * s V_pclxl_define_bitmap_char_z <= 0 /\ -1 * s V_pclxl_define_bitmap_char_i <= 0)%Z
   | 13 => (-1 * s V_pclxl_define_bitmap_char_i <= 0 /\ -1 * s V_pclxl_define_bitmap_char_z <= 0 /\ 1 * s V_pclxl_define_bitmap_char_z <= 0)%Z
   | 14 => (1 * s V_pclxl_define_bitmap_char_z <= 0 /\ -1 * s V_pclxl_define_bitmap_char_z <= 0 /\ -1 * s V_pclxl_define_bitmap_char_i <= 0)%Z
   | 15 => (-1 * s V_pclxl_define_bitmap_char_i <= 0 /\ -1 * s V_pclxl_define_bitmap_char_z <= 0 /\ 1 * s V_pclxl_define_bitmap_char_z <= 0)%Z
   | 16 => (1 * s V_pclxl_define_bitmap_char_z <= 0 /\ -1 * s V_pclxl_define_bitmap_char_z <= 0 /\ -1 * s V_pclxl_define_bitmap_char_i <= 0)%Z
   | 17 => (1 * s V_pclxl_define_bitmap_char_z <= 0 /\ -1 * s V_pclxl_define_bitmap_char_z <= 0 /\ -1 * s V_pclxl_define_bitmap_char_i <= 0)%Z
   | 18 => (-1 * s V_pclxl_define_bitmap_char_i <= 0 /\ -1 * s V_pclxl_define_bitmap_char_z <= 0 /\ 1 * s V_pclxl_define_bitmap_char_z <= 0)%Z
   | 19 => (1 * s V_pclxl_define_bitmap_char_z <= 0 /\ -1 * s V_pclxl_define_bitmap_char_z <= 0 /\ -1 * s V_pclxl_define_bitmap_char_i <= 0)%Z
   | 20 => (-1 * s V_pclxl_define_bitmap_char_i <= 0 /\ -1 * s V_pclxl_define_bitmap_char_z <= 0 /\ 1 * s V_pclxl_define_bitmap_char_z <= 0)%Z
   | 21 => (1 * s V_pclxl_define_bitmap_char_z <= 0 /\ -1 * s V_pclxl_define_bitmap_char_z <= 0 /\ 1 * s V_pclxl_define_bitmap_char_i <= 0 /\ -1 * s V_pclxl_define_bitmap_char_i <= 0)%Z
   | 22 => (-1 * s V_pclxl_define_bitmap_char_i <= 0 /\ 1 * s V_pclxl_define_bitmap_char_i <= 0 /\ -1 * s V_pclxl_define_bitmap_char_z <= 0 /\ 1 * s V_pclxl_define_bitmap_char_z <= 0)%Z
   | 23 => (-1 * s V_pclxl_define_bitmap_char_z <= 0 /\ -1 * s V_pclxl_define_bitmap_char_i <= 0)%Z
   | 24 => (-1 * s V_pclxl_define_bitmap_char_i <= 0 /\ -1 * s V_pclxl_define_bitmap_char_z <= 0 /\ 1 * s V_pclxl_define_bitmap_char__tmp+ -1 * s V_pclxl_define_bitmap_char_i <= 0)%Z
   | 25 => (1 * s V_pclxl_define_bitmap_char__tmp+ -1 * s V_pclxl_define_bitmap_char_i <= 0 /\ -1 * s V_pclxl_define_bitmap_char_z <= 0 /\ -1 * s V_pclxl_define_bitmap_char_i <= 0)%Z
   | 26 => (-1 * s V_pclxl_define_bitmap_char_i <= 0 /\ -1 * s V_pclxl_define_bitmap_char_z <= 0 /\ -1 * s V_pclxl_define_bitmap_char__tmp+ 1 * s V_pclxl_define_bitmap_char_i + 1 <= 0)%Z
   | 27 => (-1 * s V_pclxl_define_bitmap_char__tmp+ 1 * s V_pclxl_define_bitmap_char_i + 1 <= 0 /\ -1 * s V_pclxl_define_bitmap_char_z <= 0 /\ -1 * s V_pclxl_define_bitmap_char_i <= 0)%Z
   | 28 => (-1 * s V_pclxl_define_bitmap_char_i <= 0 /\ -1 * s V_pclxl_define_bitmap_char_z <= 0 /\ -1 * s V_pclxl_define_bitmap_char__tmp+ 1 * s V_pclxl_define_bitmap_char_i + 1 <= 0)%Z
   | 29 => (-1 * s V_pclxl_define_bitmap_char_z <= 0 /\ -1 * s V_pclxl_define_bitmap_char_i + 1 <= 0 /\ -1 * s V_pclxl_define_bitmap_char__tmp+ 1 * s V_pclxl_define_bitmap_char_i <= 0)%Z
   | 30 => (-1 * s V_pclxl_define_bitmap_char__tmp+ 1 * s V_pclxl_define_bitmap_char_i <= 0 /\ -1 * s V_pclxl_define_bitmap_char_i + 1 <= 0 /\ -1 * s V_pclxl_define_bitmap_char_z <= 0)%Z
   | 31 => (-1 * s V_pclxl_define_bitmap_char_z <= 0 /\ -1 * s V_pclxl_define_bitmap_char_i + 1 <= 0 /\ -1 * s V_pclxl_define_bitmap_char__tmp+ 1 * s V_pclxl_define_bitmap_char_i <= 0)%Z
   | 32 => (-1 * s V_pclxl_define_bitmap_char__tmp+ 1 * s V_pclxl_define_bitmap_char_i <= 0 /\ -1 * s V_pclxl_define_bitmap_char_i + 1 <= 0 /\ -1 * s V_pclxl_define_bitmap_char_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_pclxl_define_bitmap_char (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_pclxl_define_bitmap_char_height) <= z)%Q
   | 2 => (s V_pclxl_define_bitmap_char_z
           + max0(s V_pclxl_define_bitmap_char_height) <= z)%Q
   | 3 => (s V_pclxl_define_bitmap_char_z
           + max0(s V_pclxl_define_bitmap_char_height) <= z)%Q
   | 4 => (s V_pclxl_define_bitmap_char_z
           + max0(s V_pclxl_define_bitmap_char_height) <= z)%Q
   | 5 => (s V_pclxl_define_bitmap_char_z
           + max0(s V_pclxl_define_bitmap_char_height) <= z)%Q
   | 6 => (s V_pclxl_define_bitmap_char_z
           + max0(s V_pclxl_define_bitmap_char_height) <= z)%Q
   | 7 => (s V_pclxl_define_bitmap_char_z
           + max0(s V_pclxl_define_bitmap_char_height) <= z)%Q
   | 8 => (s V_pclxl_define_bitmap_char_z
           + max0(s V_pclxl_define_bitmap_char_height) <= z)%Q
   | 9 => (s V_pclxl_define_bitmap_char_z
           + max0(s V_pclxl_define_bitmap_char_height) <= z)%Q
   | 10 => (s V_pclxl_define_bitmap_char_z
            + max0(s V_pclxl_define_bitmap_char__tmp) <= z)%Q
   | 11 => (s V_pclxl_define_bitmap_char_z
            + max0(s V_pclxl_define_bitmap_char__tmp) <= z)%Q
   | 12 => (s V_pclxl_define_bitmap_char_z
            + max0(s V_pclxl_define_bitmap_char__tmp) <= z)%Q
   | 13 => (s V_pclxl_define_bitmap_char_z
            + max0(s V_pclxl_define_bitmap_char__tmp) <= z)%Q
   | 14 => (s V_pclxl_define_bitmap_char_z
            + max0(s V_pclxl_define_bitmap_char__tmp) <= z)%Q
   | 15 => (s V_pclxl_define_bitmap_char_z
            + max0(s V_pclxl_define_bitmap_char__tmp) <= z)%Q
   | 16 => (s V_pclxl_define_bitmap_char_z
            + max0(s V_pclxl_define_bitmap_char__tmp) <= z)%Q
   | 17 => (s V_pclxl_define_bitmap_char_z
            + max0(s V_pclxl_define_bitmap_char__tmp) <= z)%Q
   | 18 => (s V_pclxl_define_bitmap_char_z
            + max0(s V_pclxl_define_bitmap_char__tmp) <= z)%Q
   | 19 => (s V_pclxl_define_bitmap_char_z
            + max0(s V_pclxl_define_bitmap_char__tmp) <= z)%Q
   | 20 => (s V_pclxl_define_bitmap_char_z
            + max0(s V_pclxl_define_bitmap_char__tmp) <= z)%Q
   | 21 => (s V_pclxl_define_bitmap_char_z
            + max0(s V_pclxl_define_bitmap_char__tmp
                   - s V_pclxl_define_bitmap_char_i) <= z)%Q
   | 22 => (s V_pclxl_define_bitmap_char_z
            + max0(s V_pclxl_define_bitmap_char__tmp
                   - s V_pclxl_define_bitmap_char_i) <= z)%Q
   | 23 => (s V_pclxl_define_bitmap_char_z
            + max0(s V_pclxl_define_bitmap_char__tmp
                   - s V_pclxl_define_bitmap_char_i) <= z)%Q
   | 24 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_pclxl_define_bitmap_char__tmp
                                             - s V_pclxl_define_bitmap_char_i) (-1
                                                                    + s V_pclxl_define_bitmap_char__tmp
                                                                    - s V_pclxl_define_bitmap_char_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V_pclxl_define_bitmap_char__tmp
                            - s V_pclxl_define_bitmap_char_i)]
     (s V_pclxl_define_bitmap_char_z
      + max0(s V_pclxl_define_bitmap_char__tmp
             - s V_pclxl_define_bitmap_char_i) <= z)%Q
   | 25 => (s V_pclxl_define_bitmap_char_z <= z)%Q
   | 26 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_pclxl_define_bitmap_char__tmp
                                       - s V_pclxl_define_bitmap_char_i) (1)]
     (s V_pclxl_define_bitmap_char_z
      + max0(s V_pclxl_define_bitmap_char__tmp
             - s V_pclxl_define_bitmap_char_i) <= z)%Q
   | 27 => ((1 # 1) + s V_pclxl_define_bitmap_char_z
            + max0(-1 + s V_pclxl_define_bitmap_char__tmp
                   - s V_pclxl_define_bitmap_char_i) <= z)%Q
   | 28 => ((1 # 1) + s V_pclxl_define_bitmap_char_z
            + max0(-1 + s V_pclxl_define_bitmap_char__tmp
                   - s V_pclxl_define_bitmap_char_i) <= z)%Q
   | 29 => ((1 # 1) + s V_pclxl_define_bitmap_char_z
            + max0(s V_pclxl_define_bitmap_char__tmp
                   - s V_pclxl_define_bitmap_char_i) <= z)%Q
   | 30 => ((1 # 1) + s V_pclxl_define_bitmap_char_z
            + max0(s V_pclxl_define_bitmap_char__tmp
                   - s V_pclxl_define_bitmap_char_i) <= z)%Q
   | 31 => ((1 # 1) + s V_pclxl_define_bitmap_char_z
            + max0(s V_pclxl_define_bitmap_char__tmp
                   - s V_pclxl_define_bitmap_char_i) <= z)%Q
   | 32 => (s V_pclxl_define_bitmap_char_z
            + max0(s V_pclxl_define_bitmap_char__tmp
                   - s V_pclxl_define_bitmap_char_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_pclxl_define_bitmap_char =>
    [mkPA Q (fun n z s => ai_pclxl_define_bitmap_char n s /\ annot0_pclxl_define_bitmap_char n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_pclxl_define_bitmap_char (proc_start P_pclxl_define_bitmap_char) s1 (proc_end P_pclxl_define_bitmap_char) s2 ->
    (s2 V_pclxl_define_bitmap_char_z <= max0(s1 V_pclxl_define_bitmap_char_height))%Q.
Proof.
  prove_bound ipa admissible_ipa P_pclxl_define_bitmap_char.
Qed.
