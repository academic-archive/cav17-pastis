Require Import pasta.Pasta.

Inductive proc: Type :=
  P_debug_dump_bitmap.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_debug_dump_bitmap_z := 1%positive.
Notation V_debug_dump_bitmap__tmp := 2%positive.
Notation V_debug_dump_bitmap__tmp1 := 3%positive.
Notation V_debug_dump_bitmap_y := 4%positive.
Notation V_debug_dump_bitmap_bits := 5%positive.
Notation V_debug_dump_bitmap_height := 6%positive.
Notation V_debug_dump_bitmap_msg := 7%positive.
Notation V_debug_dump_bitmap_raster := 8%positive.
Definition Pedges_debug_dump_bitmap: list (edge proc) :=
  (EA 1 (AAssign V_debug_dump_bitmap_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_debug_dump_bitmap_y) s) >= (eval (ENum (0))
  s))%Z)) 3)::(EA 3 (AGuard (fun s => ((eval (EVar V_debug_dump_bitmap__tmp)
  s) >= (eval (ENum (0)) s))%Z)) 4)::(EA 4 AWeaken 5)::(EA 5 (AAssign
  V_debug_dump_bitmap__tmp1 (Some (EVar V_debug_dump_bitmap_raster))) 6)::
  (EA 6 (AAssign V_debug_dump_bitmap__tmp
  (Some (EVar V_debug_dump_bitmap_height))) 7)::(EA 7 (AAssign
  V_debug_dump_bitmap_y (Some (ENum (0)))) 8)::(EA 8 ANone 9)::
  (EA 9 AWeaken 10)::(EA 10 (AGuard
  (fun s => ((eval (EVar V_debug_dump_bitmap_y) s) <
  (eval (EVar V_debug_dump_bitmap__tmp) s))%Z)) 13)::(EA 10 (AGuard
  (fun s => ((eval (EVar V_debug_dump_bitmap_y) s) >=
  (eval (EVar V_debug_dump_bitmap__tmp) s))%Z)) 11)::(EA 11 AWeaken 12)::
  (EA 13 AWeaken 14)::(EA 14 (AGuard
  (fun s => ((eval (EVar V_debug_dump_bitmap_y) s) = (eval (ENum (0))
  s))%Z)) 17)::(EA 14 (AGuard (fun s => ((eval (EVar V_debug_dump_bitmap_y)
  s) <> (eval (ENum (0)) s))%Z)) 15)::(EA 15 AWeaken 16)::(EA 16 ANone 19)::
  (EA 17 AWeaken 18)::(EA 18 ANone 19)::(EA 19 ANone 20)::(EA 20 (AAssign
  V_debug_dump_bitmap_y (Some (EAdd (EVar V_debug_dump_bitmap_y)
  (ENum (1))))) 21)::(EA 21 ANone 22)::(EA 22 ANone 23)::(EA 23 (AAssign
  V_debug_dump_bitmap_z (Some (EAdd (ENum (1))
  (EVar V_debug_dump_bitmap_z)))) 24)::(EA 24 AWeaken 10)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_debug_dump_bitmap => Pedges_debug_dump_bitmap
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_debug_dump_bitmap => 12
     end)%positive;
  var_global := var_global
}.

Definition ai_debug_dump_bitmap (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_debug_dump_bitmap_z <= 0 /\ -1 * s V_debug_dump_bitmap_z <= 0)%Z
   | 3 => (-1 * s V_debug_dump_bitmap_z <= 0 /\ 1 * s V_debug_dump_bitmap_z <= 0 /\ -1 * s V_debug_dump_bitmap_y <= 0)%Z
   | 4 => (-1 * s V_debug_dump_bitmap_y <= 0 /\ 1 * s V_debug_dump_bitmap_z <= 0 /\ -1 * s V_debug_dump_bitmap_z <= 0 /\ -1 * s V_debug_dump_bitmap__tmp <= 0)%Z
   | 5 => (-1 * s V_debug_dump_bitmap__tmp <= 0 /\ -1 * s V_debug_dump_bitmap_z <= 0 /\ 1 * s V_debug_dump_bitmap_z <= 0 /\ -1 * s V_debug_dump_bitmap_y <= 0)%Z
   | 6 => (-1 * s V_debug_dump_bitmap_y <= 0 /\ 1 * s V_debug_dump_bitmap_z <= 0 /\ -1 * s V_debug_dump_bitmap_z <= 0 /\ -1 * s V_debug_dump_bitmap__tmp <= 0)%Z
   | 7 => (-1 * s V_debug_dump_bitmap_z <= 0 /\ 1 * s V_debug_dump_bitmap_z <= 0 /\ -1 * s V_debug_dump_bitmap_y <= 0)%Z
   | 8 => (1 * s V_debug_dump_bitmap_z <= 0 /\ -1 * s V_debug_dump_bitmap_z <= 0 /\ 1 * s V_debug_dump_bitmap_y <= 0 /\ -1 * s V_debug_dump_bitmap_y <= 0)%Z
   | 9 => (-1 * s V_debug_dump_bitmap_y <= 0 /\ 1 * s V_debug_dump_bitmap_y <= 0 /\ -1 * s V_debug_dump_bitmap_z <= 0 /\ 1 * s V_debug_dump_bitmap_z <= 0)%Z
   | 10 => (-1 * s V_debug_dump_bitmap_z <= 0 /\ -1 * s V_debug_dump_bitmap_y <= 0)%Z
   | 11 => (-1 * s V_debug_dump_bitmap_y <= 0 /\ -1 * s V_debug_dump_bitmap_z <= 0 /\ 1 * s V_debug_dump_bitmap__tmp+ -1 * s V_debug_dump_bitmap_y <= 0)%Z
   | 12 => (1 * s V_debug_dump_bitmap__tmp+ -1 * s V_debug_dump_bitmap_y <= 0 /\ -1 * s V_debug_dump_bitmap_z <= 0 /\ -1 * s V_debug_dump_bitmap_y <= 0)%Z
   | 13 => (-1 * s V_debug_dump_bitmap_y <= 0 /\ -1 * s V_debug_dump_bitmap_z <= 0 /\ -1 * s V_debug_dump_bitmap__tmp+ 1 * s V_debug_dump_bitmap_y + 1 <= 0)%Z
   | 14 => (-1 * s V_debug_dump_bitmap__tmp+ 1 * s V_debug_dump_bitmap_y + 1 <= 0 /\ -1 * s V_debug_dump_bitmap_z <= 0 /\ -1 * s V_debug_dump_bitmap_y <= 0)%Z
   | 15 => (-1 * s V_debug_dump_bitmap_z <= 0 /\ -1 * s V_debug_dump_bitmap__tmp+ 1 * s V_debug_dump_bitmap_y + 1 <= 0 /\ -1 * s V_debug_dump_bitmap_y + 1 <= 0)%Z
   | 16 => (-1 * s V_debug_dump_bitmap_y + 1 <= 0 /\ -1 * s V_debug_dump_bitmap__tmp+ 1 * s V_debug_dump_bitmap_y + 1 <= 0 /\ -1 * s V_debug_dump_bitmap_z <= 0)%Z
   | 17 => (-1 * s V_debug_dump_bitmap_y <= 0 /\ -1 * s V_debug_dump_bitmap_z <= 0 /\ -1 * s V_debug_dump_bitmap__tmp+ 1 * s V_debug_dump_bitmap_y + 1 <= 0 /\ 1 * s V_debug_dump_bitmap_y <= 0)%Z
   | 18 => (1 * s V_debug_dump_bitmap_y <= 0 /\ -1 * s V_debug_dump_bitmap__tmp+ 1 * s V_debug_dump_bitmap_y + 1 <= 0 /\ -1 * s V_debug_dump_bitmap_z <= 0 /\ -1 * s V_debug_dump_bitmap_y <= 0)%Z
   | 19 => (-1 * s V_debug_dump_bitmap_y <= 0 /\ -1 * s V_debug_dump_bitmap_z <= 0 /\ -1 * s V_debug_dump_bitmap__tmp+ 1 * s V_debug_dump_bitmap_y + 1 <= 0)%Z
   | 20 => (-1 * s V_debug_dump_bitmap__tmp+ 1 * s V_debug_dump_bitmap_y + 1 <= 0 /\ -1 * s V_debug_dump_bitmap_z <= 0 /\ -1 * s V_debug_dump_bitmap_y <= 0)%Z
   | 21 => (-1 * s V_debug_dump_bitmap_z <= 0 /\ -1 * s V_debug_dump_bitmap__tmp+ 1 * s V_debug_dump_bitmap_y <= 0 /\ -1 * s V_debug_dump_bitmap_y + 1 <= 0)%Z
   | 22 => (-1 * s V_debug_dump_bitmap_y + 1 <= 0 /\ -1 * s V_debug_dump_bitmap__tmp+ 1 * s V_debug_dump_bitmap_y <= 0 /\ -1 * s V_debug_dump_bitmap_z <= 0)%Z
   | 23 => (-1 * s V_debug_dump_bitmap_z <= 0 /\ -1 * s V_debug_dump_bitmap__tmp+ 1 * s V_debug_dump_bitmap_y <= 0 /\ -1 * s V_debug_dump_bitmap_y + 1 <= 0)%Z
   | 24 => (-1 * s V_debug_dump_bitmap_y + 1 <= 0 /\ -1 * s V_debug_dump_bitmap__tmp+ 1 * s V_debug_dump_bitmap_y <= 0 /\ -1 * s V_debug_dump_bitmap_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_debug_dump_bitmap (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_debug_dump_bitmap_height) <= z)%Q
   | 2 => (s V_debug_dump_bitmap_z + max0(s V_debug_dump_bitmap_height) <= z)%Q
   | 3 => (s V_debug_dump_bitmap_z + max0(s V_debug_dump_bitmap_height) <= z)%Q
   | 4 => (s V_debug_dump_bitmap_z + max0(s V_debug_dump_bitmap_height) <= z)%Q
   | 5 => (s V_debug_dump_bitmap_z + max0(s V_debug_dump_bitmap_height) <= z)%Q
   | 6 => (s V_debug_dump_bitmap_z + max0(s V_debug_dump_bitmap_height) <= z)%Q
   | 7 => (s V_debug_dump_bitmap_z + max0(s V_debug_dump_bitmap__tmp) <= z)%Q
   | 8 => (s V_debug_dump_bitmap_z
           + max0(s V_debug_dump_bitmap__tmp - s V_debug_dump_bitmap_y) <= z)%Q
   | 9 => (s V_debug_dump_bitmap_z
           + max0(s V_debug_dump_bitmap__tmp - s V_debug_dump_bitmap_y) <= z)%Q
   | 10 => (s V_debug_dump_bitmap_z
            + max0(s V_debug_dump_bitmap__tmp - s V_debug_dump_bitmap_y) <= z)%Q
   | 11 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_debug_dump_bitmap__tmp
                                             - s V_debug_dump_bitmap_y) (-1
                                                                    + s V_debug_dump_bitmap__tmp
                                                                    - s V_debug_dump_bitmap_y));
      (*-1 0*) F_max0_ge_0 (-1 + s V_debug_dump_bitmap__tmp
                            - s V_debug_dump_bitmap_y)]
     (s V_debug_dump_bitmap_z
      + max0(s V_debug_dump_bitmap__tmp - s V_debug_dump_bitmap_y) <= z)%Q
   | 12 => (s V_debug_dump_bitmap_z <= z)%Q
   | 13 => (s V_debug_dump_bitmap_z
            + max0(s V_debug_dump_bitmap__tmp - s V_debug_dump_bitmap_y) <= z)%Q
   | 14 => (s V_debug_dump_bitmap_z
            + max0(s V_debug_dump_bitmap__tmp - s V_debug_dump_bitmap_y) <= z)%Q
   | 15 => hints
     [(*0 1*) F_max0_pre_decrement 1 (s V_debug_dump_bitmap__tmp
                                      - s V_debug_dump_bitmap_y) (1)]
     (s V_debug_dump_bitmap_z
      + max0(s V_debug_dump_bitmap__tmp - s V_debug_dump_bitmap_y) <= z)%Q
   | 16 => ((1 # 1) + s V_debug_dump_bitmap_z
            + max0(-1 + s V_debug_dump_bitmap__tmp - s V_debug_dump_bitmap_y) <= z)%Q
   | 17 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_debug_dump_bitmap__tmp
                                       - s V_debug_dump_bitmap_y) (1)]
     (s V_debug_dump_bitmap_z
      + max0(s V_debug_dump_bitmap__tmp - s V_debug_dump_bitmap_y) <= z)%Q
   | 18 => ((1 # 1) + s V_debug_dump_bitmap_z
            + max0(-1 + s V_debug_dump_bitmap__tmp - s V_debug_dump_bitmap_y) <= z)%Q
   | 19 => ((1 # 1) + s V_debug_dump_bitmap_z
            + max0(-1 + s V_debug_dump_bitmap__tmp - s V_debug_dump_bitmap_y) <= z)%Q
   | 20 => ((1 # 1) + s V_debug_dump_bitmap_z
            + max0(-1 + s V_debug_dump_bitmap__tmp - s V_debug_dump_bitmap_y) <= z)%Q
   | 21 => ((1 # 1) + s V_debug_dump_bitmap_z
            + max0(s V_debug_dump_bitmap__tmp - s V_debug_dump_bitmap_y) <= z)%Q
   | 22 => ((1 # 1) + s V_debug_dump_bitmap_z
            + max0(s V_debug_dump_bitmap__tmp - s V_debug_dump_bitmap_y) <= z)%Q
   | 23 => ((1 # 1) + s V_debug_dump_bitmap_z
            + max0(s V_debug_dump_bitmap__tmp - s V_debug_dump_bitmap_y) <= z)%Q
   | 24 => (s V_debug_dump_bitmap_z
            + max0(s V_debug_dump_bitmap__tmp - s V_debug_dump_bitmap_y) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_debug_dump_bitmap =>
    [mkPA Q (fun n z s => ai_debug_dump_bitmap n s /\ annot0_debug_dump_bitmap n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_debug_dump_bitmap (proc_start P_debug_dump_bitmap) s1 (proc_end P_debug_dump_bitmap) s2 ->
    (s2 V_debug_dump_bitmap_z <= max0(s1 V_debug_dump_bitmap_height))%Q.
Proof.
  prove_bound ipa admissible_ipa P_debug_dump_bitmap.
Qed.
