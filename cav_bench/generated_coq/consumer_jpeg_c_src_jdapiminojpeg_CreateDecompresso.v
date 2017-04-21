Require Import pasta.Pasta.

Inductive proc: Type :=
  P_jpeg_CreateDecompress.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_jpeg_CreateDecompress_z := 1%positive.
Notation V_jpeg_CreateDecompress__tmp := 2%positive.
Notation V_jpeg_CreateDecompress__tmp1 := 3%positive.
Notation V_jpeg_CreateDecompress_i := 4%positive.
Notation V_jpeg_CreateDecompress_cinfo := 5%positive.
Notation V_jpeg_CreateDecompress_structsize := 6%positive.
Notation V_jpeg_CreateDecompress_version := 7%positive.
Definition Pedges_jpeg_CreateDecompress: list (edge proc) :=
  (EA 1 (AAssign V_jpeg_CreateDecompress_z (Some (ENum (0)))) 2)::
  (EA 2 (AAssign V_jpeg_CreateDecompress__tmp
  (Some (EVar V_jpeg_CreateDecompress_version))) 3)::(EA 3 (AAssign
  V_jpeg_CreateDecompress__tmp1
  (Some (EVar V_jpeg_CreateDecompress_structsize))) 4)::(EA 4 AWeaken 5)::
  (EA 5 (AGuard (fun s => ((eval (EVar V_jpeg_CreateDecompress__tmp) s) <>
  (eval (ENum (61)) s))%Z)) 7)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_jpeg_CreateDecompress__tmp) s) = (eval (ENum (61))
  s))%Z)) 6)::(EA 6 AWeaken 10)::(EA 7 AWeaken 8)::(EA 8 ANone 9)::
  (EA 9 AWeaken 10)::(EA 10 (AGuard
  (fun s => ((eval (EVar V_jpeg_CreateDecompress__tmp1) s) <>
  (eval (ENum (616)) s))%Z)) 12)::(EA 10 (AGuard
  (fun s => ((eval (EVar V_jpeg_CreateDecompress__tmp1) s) =
  (eval (ENum (616)) s))%Z)) 11)::(EA 11 AWeaken 14)::(EA 12 AWeaken 13)::
  (EA 13 ANone 14)::(EA 14 (AAssign V_jpeg_CreateDecompress_i
  (Some (ENum (0)))) 15)::(EA 15 ANone 16)::(EA 16 AWeaken 17)::
  (EA 17 (AGuard (fun s => ((eval (EVar V_jpeg_CreateDecompress_i) s) <
  (eval (ENum (4)) s))%Z)) 32)::(EA 17 (AGuard
  (fun s => ((eval (EVar V_jpeg_CreateDecompress_i) s) >= (eval (ENum (4))
  s))%Z)) 18)::(EA 18 AWeaken 19)::(EA 19 (AAssign V_jpeg_CreateDecompress_i
  (Some (ENum (0)))) 20)::(EA 20 ANone 21)::(EA 21 AWeaken 22)::
  (EA 22 (AGuard (fun s => ((eval (EVar V_jpeg_CreateDecompress_i) s) <
  (eval (ENum (4)) s))%Z)) 25)::(EA 22 (AGuard
  (fun s => ((eval (EVar V_jpeg_CreateDecompress_i) s) >= (eval (ENum (4))
  s))%Z)) 23)::(EA 23 AWeaken 24)::(EA 25 AWeaken 26)::(EA 26 ANone 27)::
  (EA 27 (AAssign V_jpeg_CreateDecompress_i
  (Some (EAdd (EVar V_jpeg_CreateDecompress_i) (ENum (1))))) 28)::
  (EA 28 ANone 29)::(EA 29 ANone 30)::(EA 30 (AAssign
  V_jpeg_CreateDecompress_z (Some (EAdd (ENum (1))
  (EVar V_jpeg_CreateDecompress_z)))) 31)::(EA 31 AWeaken 22)::
  (EA 32 AWeaken 33)::(EA 33 ANone 34)::(EA 34 (AAssign
  V_jpeg_CreateDecompress_i (Some (EAdd (EVar V_jpeg_CreateDecompress_i)
  (ENum (1))))) 35)::(EA 35 ANone 36)::(EA 36 ANone 37)::(EA 37 (AAssign
  V_jpeg_CreateDecompress_z (Some (EAdd (ENum (1))
  (EVar V_jpeg_CreateDecompress_z)))) 38)::(EA 38 AWeaken 17)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_jpeg_CreateDecompress => Pedges_jpeg_CreateDecompress
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_jpeg_CreateDecompress => 24
     end)%positive;
  var_global := var_global
}.

Definition ai_jpeg_CreateDecompress (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_jpeg_CreateDecompress_z <= 0 /\ -1 * s V_jpeg_CreateDecompress_z <= 0)%Z
   | 3 => (-1 * s V_jpeg_CreateDecompress_z <= 0 /\ 1 * s V_jpeg_CreateDecompress_z <= 0)%Z
   | 4 => (1 * s V_jpeg_CreateDecompress_z <= 0 /\ -1 * s V_jpeg_CreateDecompress_z <= 0)%Z
   | 5 => (-1 * s V_jpeg_CreateDecompress_z <= 0 /\ 1 * s V_jpeg_CreateDecompress_z <= 0)%Z
   | 6 => (1 * s V_jpeg_CreateDecompress_z <= 0 /\ -1 * s V_jpeg_CreateDecompress_z <= 0 /\ 1 * s V_jpeg_CreateDecompress__tmp + -61 <= 0 /\ -1 * s V_jpeg_CreateDecompress__tmp + 61 <= 0)%Z
   | 7 => (1 * s V_jpeg_CreateDecompress_z <= 0 /\ -1 * s V_jpeg_CreateDecompress_z <= 0)%Z
   | 8 => (-1 * s V_jpeg_CreateDecompress_z <= 0 /\ 1 * s V_jpeg_CreateDecompress_z <= 0)%Z
   | 9 => (1 * s V_jpeg_CreateDecompress_z <= 0 /\ -1 * s V_jpeg_CreateDecompress_z <= 0)%Z
   | 10 => (-1 * s V_jpeg_CreateDecompress_z <= 0 /\ 1 * s V_jpeg_CreateDecompress_z <= 0)%Z
   | 11 => (1 * s V_jpeg_CreateDecompress_z <= 0 /\ -1 * s V_jpeg_CreateDecompress_z <= 0 /\ 1 * s V_jpeg_CreateDecompress__tmp1 + -616 <= 0 /\ -1 * s V_jpeg_CreateDecompress__tmp1 + 616 <= 0)%Z
   | 12 => (1 * s V_jpeg_CreateDecompress_z <= 0 /\ -1 * s V_jpeg_CreateDecompress_z <= 0)%Z
   | 13 => (-1 * s V_jpeg_CreateDecompress_z <= 0 /\ 1 * s V_jpeg_CreateDecompress_z <= 0)%Z
   | 14 => (1 * s V_jpeg_CreateDecompress_z <= 0 /\ -1 * s V_jpeg_CreateDecompress_z <= 0)%Z
   | 15 => (-1 * s V_jpeg_CreateDecompress_z <= 0 /\ 1 * s V_jpeg_CreateDecompress_z <= 0 /\ 1 * s V_jpeg_CreateDecompress_i <= 0 /\ -1 * s V_jpeg_CreateDecompress_i <= 0)%Z
   | 16 => (-1 * s V_jpeg_CreateDecompress_i <= 0 /\ 1 * s V_jpeg_CreateDecompress_i <= 0 /\ 1 * s V_jpeg_CreateDecompress_z <= 0 /\ -1 * s V_jpeg_CreateDecompress_z <= 0)%Z
   | 17 => (-1 * s V_jpeg_CreateDecompress_z <= 0 /\ -1 * s V_jpeg_CreateDecompress_i <= 0 /\ 1 * s V_jpeg_CreateDecompress_i + -4 <= 0)%Z
   | 18 => (1 * s V_jpeg_CreateDecompress_i + -4 <= 0 /\ -1 * s V_jpeg_CreateDecompress_z <= 0 /\ -1 * s V_jpeg_CreateDecompress_i + 4 <= 0)%Z
   | 19 => (-1 * s V_jpeg_CreateDecompress_i + 4 <= 0 /\ -1 * s V_jpeg_CreateDecompress_z <= 0 /\ 1 * s V_jpeg_CreateDecompress_i + -4 <= 0)%Z
   | 20 => (-1 * s V_jpeg_CreateDecompress_z <= 0 /\ 1 * s V_jpeg_CreateDecompress_i <= 0 /\ -1 * s V_jpeg_CreateDecompress_i <= 0)%Z
   | 21 => (-1 * s V_jpeg_CreateDecompress_i <= 0 /\ 1 * s V_jpeg_CreateDecompress_i <= 0 /\ -1 * s V_jpeg_CreateDecompress_z <= 0)%Z
   | 22 => (-1 * s V_jpeg_CreateDecompress_z <= 0 /\ -1 * s V_jpeg_CreateDecompress_i <= 0 /\ 1 * s V_jpeg_CreateDecompress_i + -4 <= 0)%Z
   | 23 => (1 * s V_jpeg_CreateDecompress_i + -4 <= 0 /\ -1 * s V_jpeg_CreateDecompress_z <= 0 /\ -1 * s V_jpeg_CreateDecompress_i + 4 <= 0)%Z
   | 24 => (-1 * s V_jpeg_CreateDecompress_i + 4 <= 0 /\ -1 * s V_jpeg_CreateDecompress_z <= 0 /\ 1 * s V_jpeg_CreateDecompress_i + -4 <= 0)%Z
   | 25 => (-1 * s V_jpeg_CreateDecompress_i <= 0 /\ -1 * s V_jpeg_CreateDecompress_z <= 0 /\ 1 * s V_jpeg_CreateDecompress_i + -3 <= 0)%Z
   | 26 => (1 * s V_jpeg_CreateDecompress_i + -3 <= 0 /\ -1 * s V_jpeg_CreateDecompress_z <= 0 /\ -1 * s V_jpeg_CreateDecompress_i <= 0)%Z
   | 27 => (-1 * s V_jpeg_CreateDecompress_i <= 0 /\ -1 * s V_jpeg_CreateDecompress_z <= 0 /\ 1 * s V_jpeg_CreateDecompress_i + -3 <= 0)%Z
   | 28 => (-1 * s V_jpeg_CreateDecompress_z <= 0 /\ -1 * s V_jpeg_CreateDecompress_i + 1 <= 0 /\ 1 * s V_jpeg_CreateDecompress_i + -4 <= 0)%Z
   | 29 => (1 * s V_jpeg_CreateDecompress_i + -4 <= 0 /\ -1 * s V_jpeg_CreateDecompress_i + 1 <= 0 /\ -1 * s V_jpeg_CreateDecompress_z <= 0)%Z
   | 30 => (-1 * s V_jpeg_CreateDecompress_z <= 0 /\ -1 * s V_jpeg_CreateDecompress_i + 1 <= 0 /\ 1 * s V_jpeg_CreateDecompress_i + -4 <= 0)%Z
   | 31 => (1 * s V_jpeg_CreateDecompress_i + -4 <= 0 /\ -1 * s V_jpeg_CreateDecompress_i + 1 <= 0 /\ -1 * s V_jpeg_CreateDecompress_z + 1 <= 0)%Z
   | 32 => (-1 * s V_jpeg_CreateDecompress_i <= 0 /\ -1 * s V_jpeg_CreateDecompress_z <= 0 /\ 1 * s V_jpeg_CreateDecompress_i + -3 <= 0)%Z
   | 33 => (1 * s V_jpeg_CreateDecompress_i + -3 <= 0 /\ -1 * s V_jpeg_CreateDecompress_z <= 0 /\ -1 * s V_jpeg_CreateDecompress_i <= 0)%Z
   | 34 => (-1 * s V_jpeg_CreateDecompress_i <= 0 /\ -1 * s V_jpeg_CreateDecompress_z <= 0 /\ 1 * s V_jpeg_CreateDecompress_i + -3 <= 0)%Z
   | 35 => (-1 * s V_jpeg_CreateDecompress_z <= 0 /\ -1 * s V_jpeg_CreateDecompress_i + 1 <= 0 /\ 1 * s V_jpeg_CreateDecompress_i + -4 <= 0)%Z
   | 36 => (1 * s V_jpeg_CreateDecompress_i + -4 <= 0 /\ -1 * s V_jpeg_CreateDecompress_i + 1 <= 0 /\ -1 * s V_jpeg_CreateDecompress_z <= 0)%Z
   | 37 => (-1 * s V_jpeg_CreateDecompress_z <= 0 /\ -1 * s V_jpeg_CreateDecompress_i + 1 <= 0 /\ 1 * s V_jpeg_CreateDecompress_i + -4 <= 0)%Z
   | 38 => (1 * s V_jpeg_CreateDecompress_i + -4 <= 0 /\ -1 * s V_jpeg_CreateDecompress_i + 1 <= 0 /\ -1 * s V_jpeg_CreateDecompress_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_jpeg_CreateDecompress (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((8 # 1) <= z)%Q
   | 2 => ((8 # 1) + s V_jpeg_CreateDecompress_z <= z)%Q
   | 3 => ((8 # 1) + s V_jpeg_CreateDecompress_z <= z)%Q
   | 4 => ((8 # 1) + s V_jpeg_CreateDecompress_z <= z)%Q
   | 5 => ((8 # 1) + s V_jpeg_CreateDecompress_z <= z)%Q
   | 6 => ((8 # 1) + s V_jpeg_CreateDecompress_z <= z)%Q
   | 7 => ((8 # 1) + s V_jpeg_CreateDecompress_z <= z)%Q
   | 8 => ((8 # 1) + s V_jpeg_CreateDecompress_z <= z)%Q
   | 9 => ((8 # 1) + s V_jpeg_CreateDecompress_z <= z)%Q
   | 10 => ((8 # 1) + s V_jpeg_CreateDecompress_z <= z)%Q
   | 11 => ((8 # 1) + s V_jpeg_CreateDecompress_z <= z)%Q
   | 12 => ((8 # 1) + s V_jpeg_CreateDecompress_z <= z)%Q
   | 13 => ((8 # 1) + s V_jpeg_CreateDecompress_z <= z)%Q
   | 14 => ((8 # 1) + s V_jpeg_CreateDecompress_z <= z)%Q
   | 15 => ((4 # 1) + s V_jpeg_CreateDecompress_z
            + max0(4 - s V_jpeg_CreateDecompress_i) <= z)%Q
   | 16 => ((4 # 1) + s V_jpeg_CreateDecompress_z
            + max0(4 - s V_jpeg_CreateDecompress_i) <= z)%Q
   | 17 => ((4 # 1) + s V_jpeg_CreateDecompress_z
            + max0(4 - s V_jpeg_CreateDecompress_i) <= z)%Q
   | 18 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (4 - s V_jpeg_CreateDecompress_i) (3
                                                                    - s V_jpeg_CreateDecompress_i));
      (*-1 0*) F_max0_ge_0 (3 - s V_jpeg_CreateDecompress_i)]
     ((4 # 1) + s V_jpeg_CreateDecompress_z
      + max0(4 - s V_jpeg_CreateDecompress_i) <= z)%Q
   | 19 => ((4 # 1) + s V_jpeg_CreateDecompress_z <= z)%Q
   | 20 => (s V_jpeg_CreateDecompress_z
            + max0(4 - s V_jpeg_CreateDecompress_i) <= z)%Q
   | 21 => (s V_jpeg_CreateDecompress_z
            + max0(4 - s V_jpeg_CreateDecompress_i) <= z)%Q
   | 22 => (s V_jpeg_CreateDecompress_z
            + max0(4 - s V_jpeg_CreateDecompress_i) <= z)%Q
   | 23 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (4 - s V_jpeg_CreateDecompress_i) (3
                                                                    - s V_jpeg_CreateDecompress_i));
      (*-1 0*) F_max0_ge_0 (3 - s V_jpeg_CreateDecompress_i)]
     (s V_jpeg_CreateDecompress_z + max0(4 - s V_jpeg_CreateDecompress_i) <= z)%Q
   | 24 => (s V_jpeg_CreateDecompress_z <= z)%Q
   | 25 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (4 - s V_jpeg_CreateDecompress_i) (1)]
     (s V_jpeg_CreateDecompress_z + max0(4 - s V_jpeg_CreateDecompress_i) <= z)%Q
   | 26 => ((1 # 1) + s V_jpeg_CreateDecompress_z
            + max0(3 - s V_jpeg_CreateDecompress_i) <= z)%Q
   | 27 => ((1 # 1) + s V_jpeg_CreateDecompress_z
            + max0(3 - s V_jpeg_CreateDecompress_i) <= z)%Q
   | 28 => ((1 # 1) + s V_jpeg_CreateDecompress_z
            + max0(4 - s V_jpeg_CreateDecompress_i) <= z)%Q
   | 29 => ((1 # 1) + s V_jpeg_CreateDecompress_z
            + max0(4 - s V_jpeg_CreateDecompress_i) <= z)%Q
   | 30 => ((1 # 1) + s V_jpeg_CreateDecompress_z
            + max0(4 - s V_jpeg_CreateDecompress_i) <= z)%Q
   | 31 => (s V_jpeg_CreateDecompress_z
            + max0(4 - s V_jpeg_CreateDecompress_i) <= z)%Q
   | 32 => hints
     [(*0 1*) F_max0_pre_decrement 1 (4 - s V_jpeg_CreateDecompress_i) (1)]
     ((4 # 1) + s V_jpeg_CreateDecompress_z
      + max0(4 - s V_jpeg_CreateDecompress_i) <= z)%Q
   | 33 => ((5 # 1) + s V_jpeg_CreateDecompress_z
            + max0(3 - s V_jpeg_CreateDecompress_i) <= z)%Q
   | 34 => ((5 # 1) + s V_jpeg_CreateDecompress_z
            + max0(3 - s V_jpeg_CreateDecompress_i) <= z)%Q
   | 35 => ((5 # 1) + s V_jpeg_CreateDecompress_z
            + max0(4 - s V_jpeg_CreateDecompress_i) <= z)%Q
   | 36 => ((5 # 1) + s V_jpeg_CreateDecompress_z
            + max0(4 - s V_jpeg_CreateDecompress_i) <= z)%Q
   | 37 => ((5 # 1) + s V_jpeg_CreateDecompress_z
            + max0(4 - s V_jpeg_CreateDecompress_i) <= z)%Q
   | 38 => ((4 # 1) + s V_jpeg_CreateDecompress_z
            + max0(4 - s V_jpeg_CreateDecompress_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_jpeg_CreateDecompress =>
    [mkPA Q (fun n z s => ai_jpeg_CreateDecompress n s /\ annot0_jpeg_CreateDecompress n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_jpeg_CreateDecompress (proc_start P_jpeg_CreateDecompress) s1 (proc_end P_jpeg_CreateDecompress) s2 ->
    (s2 V_jpeg_CreateDecompress_z <= (8 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_jpeg_CreateDecompress.
Qed.
