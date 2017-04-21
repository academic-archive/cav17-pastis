Require Import pasta.Pasta.

Inductive proc: Type :=
  P_setdump.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_setdump_z := 1%positive.
Notation V_setdump__tmp := 2%positive.
Notation V_setdump_cnum := 3%positive.
Notation V_setdump_firstnz := 4%positive.
Notation V_setdump_numnz := 5%positive.
Notation V_setdump_mask := 6%positive.
Notation V_setdump_setp := 7%positive.
Definition Pedges_setdump: list (edge proc) :=
  (EA 1 (AAssign V_setdump_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_setdump__tmp (Some (EVar V_setdump_mask))) 3)::(EA 3 (AAssign
  V_setdump_numnz (Some (ENum (0)))) 4)::(EA 4 (AAssign V_setdump_firstnz
  (Some (ENum (0)))) 5)::(EA 5 (AAssign V_setdump_cnum
  (Some (ENum (128)))) 6)::(EA 6 ANone 7)::(EA 7 (AAssign V_setdump_cnum
  (Some (EAdd (EVar V_setdump_cnum) (ENum (-1))))) 8)::(EA 8 AWeaken 9)::
  (EA 9 (AGuard (fun s => ((eval (EAdd (EVar V_setdump_cnum) (ENum (-1)))
  s) >= (eval (ENum (0)) s))%Z)) 29)::(EA 9 (AGuard
  (fun s => ((eval (EAdd (EVar V_setdump_cnum) (ENum (-1))) s) <
  (eval (ENum (0)) s))%Z)) 10)::(EA 10 AWeaken 11)::(EA 11 (AGuard
  (fun s => ((eval (EVar V_setdump_numnz) s) = (eval (ENum (1)) s))%Z)) 25)::
  (EA 11 (AGuard (fun s => ((eval (EVar V_setdump_numnz) s) <>
  (eval (ENum (1)) s))%Z)) 12)::(EA 12 AWeaken 13)::(EA 13 (AGuard
  (fun s => ((eval (EVar V_setdump_numnz) s) = (eval (ENum (128))
  s))%Z)) 21)::(EA 13 (AGuard (fun s => ((eval (EVar V_setdump_numnz) s) <>
  (eval (ENum (128)) s))%Z)) 14)::(EA 14 AWeaken 15)::(EA 15 (AGuard
  (fun s => ((eval (EVar V_setdump_numnz) s) > (eval (ENum (64))
  s))%Z)) 18)::(EA 15 (AGuard (fun s => ((eval (EVar V_setdump_numnz) s) <=
  (eval (ENum (64)) s))%Z)) 16)::(EA 16 AWeaken 17)::(EA 17 ANone 20)::
  (EA 18 AWeaken 19)::(EA 19 ANone 20)::(EA 20 ANone 23)::
  (EA 21 AWeaken 22)::(EA 22 ANone 23)::(EA 23 ANone 24)::
  (EA 24 AWeaken 28)::(EA 25 AWeaken 26)::(EA 26 ANone 27)::
  (EA 27 AWeaken 28)::(EA 29 AWeaken 30)::(EA 30 ANone 31)::
  (EA 30 ANone 34)::(EA 31 (AAssign V_setdump_numnz
  (Some (EAdd (EVar V_setdump_numnz) (ENum (1))))) 32)::(EA 32 (AAssign
  V_setdump_firstnz (Some (EVar V_setdump_cnum))) 33)::(EA 33 ANone 34)::
  (EA 34 ANone 35)::(EA 35 ANone 36)::(EA 36 (AAssign V_setdump_z
  (Some (EAdd (ENum (1)) (EVar V_setdump_z)))) 7)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_setdump => Pedges_setdump
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_setdump => 28
     end)%positive;
  var_global := var_global
}.

Definition ai_setdump (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_setdump_z <= 0 /\ -1 * s V_setdump_z <= 0)%Z
   | 3 => (-1 * s V_setdump_z <= 0 /\ 1 * s V_setdump_z <= 0)%Z
   | 4 => (1 * s V_setdump_z <= 0 /\ -1 * s V_setdump_z <= 0 /\ 1 * s V_setdump_numnz <= 0 /\ -1 * s V_setdump_numnz <= 0)%Z
   | 5 => (-1 * s V_setdump_numnz <= 0 /\ 1 * s V_setdump_numnz <= 0 /\ -1 * s V_setdump_z <= 0 /\ 1 * s V_setdump_z <= 0 /\ 1 * s V_setdump_firstnz <= 0 /\ -1 * s V_setdump_firstnz <= 0)%Z
   | 6 => (-1 * s V_setdump_firstnz <= 0 /\ 1 * s V_setdump_firstnz <= 0 /\ 1 * s V_setdump_z <= 0 /\ -1 * s V_setdump_z <= 0 /\ 1 * s V_setdump_numnz <= 0 /\ -1 * s V_setdump_numnz <= 0 /\ 1 * s V_setdump_cnum + -128 <= 0 /\ -1 * s V_setdump_cnum + 128 <= 0)%Z
   | 7 => (1 * s V_setdump_cnum + -128 <= 0 /\ -1 * s V_setdump_z <= 0 /\ -1 * s V_setdump_cnum + 1 <= 0 /\ -1 * s V_setdump_numnz <= 0 /\ -1 * s V_setdump_firstnz <= 0)%Z
   | 8 => (-1 * s V_setdump_firstnz <= 0 /\ -1 * s V_setdump_numnz <= 0 /\ -1 * s V_setdump_z <= 0 /\ 1 * s V_setdump_cnum + -127 <= 0 /\ -1 * s V_setdump_cnum <= 0)%Z
   | 9 => (-1 * s V_setdump_cnum <= 0 /\ 1 * s V_setdump_cnum + -127 <= 0 /\ -1 * s V_setdump_z <= 0 /\ -1 * s V_setdump_numnz <= 0 /\ -1 * s V_setdump_firstnz <= 0)%Z
   | 10 => (-1 * s V_setdump_firstnz <= 0 /\ -1 * s V_setdump_numnz <= 0 /\ -1 * s V_setdump_z <= 0 /\ -1 * s V_setdump_cnum <= 0 /\ 1 * s V_setdump_cnum <= 0)%Z
   | 11 => (1 * s V_setdump_cnum <= 0 /\ -1 * s V_setdump_cnum <= 0 /\ -1 * s V_setdump_z <= 0 /\ -1 * s V_setdump_numnz <= 0 /\ -1 * s V_setdump_firstnz <= 0)%Z
   | 12 => (-1 * s V_setdump_firstnz <= 0 /\ -1 * s V_setdump_numnz <= 0 /\ -1 * s V_setdump_z <= 0 /\ -1 * s V_setdump_cnum <= 0 /\ 1 * s V_setdump_cnum <= 0)%Z
   | 13 => (1 * s V_setdump_cnum <= 0 /\ -1 * s V_setdump_cnum <= 0 /\ -1 * s V_setdump_z <= 0 /\ -1 * s V_setdump_numnz <= 0 /\ -1 * s V_setdump_firstnz <= 0)%Z
   | 14 => (-1 * s V_setdump_firstnz <= 0 /\ -1 * s V_setdump_numnz <= 0 /\ -1 * s V_setdump_z <= 0 /\ -1 * s V_setdump_cnum <= 0 /\ 1 * s V_setdump_cnum <= 0)%Z
   | 15 => (1 * s V_setdump_cnum <= 0 /\ -1 * s V_setdump_cnum <= 0 /\ -1 * s V_setdump_z <= 0 /\ -1 * s V_setdump_numnz <= 0 /\ -1 * s V_setdump_firstnz <= 0)%Z
   | 16 => (-1 * s V_setdump_firstnz <= 0 /\ -1 * s V_setdump_numnz <= 0 /\ -1 * s V_setdump_z <= 0 /\ -1 * s V_setdump_cnum <= 0 /\ 1 * s V_setdump_cnum <= 0 /\ 1 * s V_setdump_numnz + -64 <= 0)%Z
   | 17 => (1 * s V_setdump_numnz + -64 <= 0 /\ 1 * s V_setdump_cnum <= 0 /\ -1 * s V_setdump_cnum <= 0 /\ -1 * s V_setdump_z <= 0 /\ -1 * s V_setdump_numnz <= 0 /\ -1 * s V_setdump_firstnz <= 0)%Z
   | 18 => (-1 * s V_setdump_firstnz <= 0 /\ -1 * s V_setdump_z <= 0 /\ -1 * s V_setdump_cnum <= 0 /\ 1 * s V_setdump_cnum <= 0 /\ -1 * s V_setdump_numnz + 65 <= 0)%Z
   | 19 => (-1 * s V_setdump_numnz + 65 <= 0 /\ 1 * s V_setdump_cnum <= 0 /\ -1 * s V_setdump_cnum <= 0 /\ -1 * s V_setdump_z <= 0 /\ -1 * s V_setdump_firstnz <= 0)%Z
   | 20 => (-1 * s V_setdump_numnz <= 0 /\ -1 * s V_setdump_firstnz <= 0 /\ -1 * s V_setdump_z <= 0 /\ -1 * s V_setdump_cnum <= 0 /\ 1 * s V_setdump_cnum <= 0)%Z
   | 21 => (-1 * s V_setdump_firstnz <= 0 /\ -1 * s V_setdump_z <= 0 /\ -1 * s V_setdump_cnum <= 0 /\ 1 * s V_setdump_cnum <= 0 /\ 1 * s V_setdump_numnz + -128 <= 0 /\ -1 * s V_setdump_numnz + 128 <= 0)%Z
   | 22 => (-1 * s V_setdump_numnz + 128 <= 0 /\ 1 * s V_setdump_numnz + -128 <= 0 /\ 1 * s V_setdump_cnum <= 0 /\ -1 * s V_setdump_cnum <= 0 /\ -1 * s V_setdump_z <= 0 /\ -1 * s V_setdump_firstnz <= 0)%Z
   | 23 => (-1 * s V_setdump_numnz <= 0 /\ -1 * s V_setdump_firstnz <= 0 /\ -1 * s V_setdump_z <= 0 /\ -1 * s V_setdump_cnum <= 0 /\ 1 * s V_setdump_cnum <= 0)%Z
   | 24 => (1 * s V_setdump_cnum <= 0 /\ -1 * s V_setdump_cnum <= 0 /\ -1 * s V_setdump_z <= 0 /\ -1 * s V_setdump_firstnz <= 0 /\ -1 * s V_setdump_numnz <= 0)%Z
   | 25 => (-1 * s V_setdump_firstnz <= 0 /\ -1 * s V_setdump_z <= 0 /\ -1 * s V_setdump_cnum <= 0 /\ 1 * s V_setdump_cnum <= 0 /\ 1 * s V_setdump_numnz + -1 <= 0 /\ -1 * s V_setdump_numnz + 1 <= 0)%Z
   | 26 => (-1 * s V_setdump_numnz + 1 <= 0 /\ 1 * s V_setdump_numnz + -1 <= 0 /\ 1 * s V_setdump_cnum <= 0 /\ -1 * s V_setdump_cnum <= 0 /\ -1 * s V_setdump_z <= 0 /\ -1 * s V_setdump_firstnz <= 0)%Z
   | 27 => (-1 * s V_setdump_firstnz <= 0 /\ -1 * s V_setdump_z <= 0 /\ -1 * s V_setdump_cnum <= 0 /\ 1 * s V_setdump_cnum <= 0 /\ 1 * s V_setdump_numnz + -1 <= 0 /\ -1 * s V_setdump_numnz + 1 <= 0)%Z
   | 28 => (-1 * s V_setdump_numnz <= 0 /\ 1 * s V_setdump_cnum <= 0 /\ -1 * s V_setdump_cnum <= 0 /\ -1 * s V_setdump_z <= 0 /\ -1 * s V_setdump_firstnz <= 0)%Z
   | 29 => (-1 * s V_setdump_firstnz <= 0 /\ -1 * s V_setdump_numnz <= 0 /\ -1 * s V_setdump_z <= 0 /\ 1 * s V_setdump_cnum + -127 <= 0 /\ -1 * s V_setdump_cnum + 1 <= 0)%Z
   | 30 => (-1 * s V_setdump_cnum + 1 <= 0 /\ 1 * s V_setdump_cnum + -127 <= 0 /\ -1 * s V_setdump_z <= 0 /\ -1 * s V_setdump_numnz <= 0 /\ -1 * s V_setdump_firstnz <= 0)%Z
   | 31 => (-1 * s V_setdump_firstnz <= 0 /\ -1 * s V_setdump_numnz <= 0 /\ -1 * s V_setdump_z <= 0 /\ 1 * s V_setdump_cnum + -127 <= 0 /\ -1 * s V_setdump_cnum + 1 <= 0)%Z
   | 32 => (-1 * s V_setdump_cnum + 1 <= 0 /\ 1 * s V_setdump_cnum + -127 <= 0 /\ -1 * s V_setdump_z <= 0 /\ -1 * s V_setdump_firstnz <= 0 /\ -1 * s V_setdump_numnz + 1 <= 0)%Z
   | 33 => (-1 * s V_setdump_numnz + 1 <= 0 /\ -1 * s V_setdump_z <= 0 /\ 1 * s V_setdump_cnum + -127 <= 0 /\ -1 * s V_setdump_cnum + 1 <= 0 /\ 1 * s V_setdump_firstnz + -127 <= 0 /\ -1 * s V_setdump_firstnz + 1 <= 0)%Z
   | 34 => (-1 * s V_setdump_firstnz <= 0 /\ -1 * s V_setdump_numnz <= 0 /\ -1 * s V_setdump_cnum + 1 <= 0 /\ 1 * s V_setdump_cnum + -127 <= 0 /\ -1 * s V_setdump_z <= 0)%Z
   | 35 => (-1 * s V_setdump_z <= 0 /\ 1 * s V_setdump_cnum + -127 <= 0 /\ -1 * s V_setdump_cnum + 1 <= 0 /\ -1 * s V_setdump_numnz <= 0 /\ -1 * s V_setdump_firstnz <= 0)%Z
   | 36 => (-1 * s V_setdump_firstnz <= 0 /\ -1 * s V_setdump_numnz <= 0 /\ -1 * s V_setdump_cnum + 1 <= 0 /\ 1 * s V_setdump_cnum + -127 <= 0 /\ -1 * s V_setdump_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_setdump (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((127 # 1) <= z)%Q
   | 2 => ((127 # 1) + s V_setdump_z <= z)%Q
   | 3 => ((127 # 1) + s V_setdump_z <= z)%Q
   | 4 => ((127 # 1) + s V_setdump_z <= z)%Q
   | 5 => ((127 # 1) + s V_setdump_z <= z)%Q
   | 6 => (s V_setdump_z + max0(-1 + s V_setdump_cnum) <= z)%Q
   | 7 => (s V_setdump_z + max0(-1 + s V_setdump_cnum) <= z)%Q
   | 8 => (s V_setdump_z + max0(s V_setdump_cnum) <= z)%Q
   | 9 => (s V_setdump_z + max0(s V_setdump_cnum) <= z)%Q
   | 10 => (s V_setdump_z + max0(s V_setdump_cnum) <= z)%Q
   | 11 => (s V_setdump_z + max0(s V_setdump_cnum) <= z)%Q
   | 12 => (s V_setdump_z + max0(s V_setdump_cnum) <= z)%Q
   | 13 => (s V_setdump_z + max0(s V_setdump_cnum) <= z)%Q
   | 14 => (s V_setdump_z + max0(s V_setdump_cnum) <= z)%Q
   | 15 => (s V_setdump_z + max0(s V_setdump_cnum) <= z)%Q
   | 16 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_setdump_cnum) (-1
                                                                + s V_setdump_cnum));
      (*-1 0*) F_max0_ge_0 (-1 + s V_setdump_cnum)]
     (s V_setdump_z + max0(s V_setdump_cnum) <= z)%Q
   | 17 => (s V_setdump_z <= z)%Q
   | 18 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_setdump_cnum) (-1
                                                                + s V_setdump_cnum));
      (*-1 0*) F_max0_ge_0 (-1 + s V_setdump_cnum)]
     (s V_setdump_z + max0(s V_setdump_cnum) <= z)%Q
   | 19 => (s V_setdump_z <= z)%Q
   | 20 => (s V_setdump_z <= z)%Q
   | 21 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_setdump_cnum) (-1
                                                                + s V_setdump_cnum));
      (*-1 0*) F_max0_ge_0 (-1 + s V_setdump_cnum)]
     (s V_setdump_z + max0(s V_setdump_cnum) <= z)%Q
   | 22 => (s V_setdump_z <= z)%Q
   | 23 => (s V_setdump_z <= z)%Q
   | 24 => (s V_setdump_z <= z)%Q
   | 25 => (s V_setdump_z + max0(s V_setdump_cnum) <= z)%Q
   | 26 => (s V_setdump_z + max0(s V_setdump_cnum) <= z)%Q
   | 27 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_setdump_cnum) (-1
                                                                + s V_setdump_cnum));
      (*-1 0*) F_max0_ge_0 (-1 + s V_setdump_cnum)]
     (s V_setdump_z + max0(s V_setdump_cnum) <= z)%Q
   | 28 => (s V_setdump_z <= z)%Q
   | 29 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_setdump_cnum) (1)]
     (s V_setdump_z + max0(s V_setdump_cnum) <= z)%Q
   | 30 => ((1 # 1) + s V_setdump_z + max0(-1 + s V_setdump_cnum) <= z)%Q
   | 31 => ((1 # 1) + s V_setdump_z + max0(-1 + s V_setdump_cnum) <= z)%Q
   | 32 => ((1 # 1) + s V_setdump_z + max0(-1 + s V_setdump_cnum) <= z)%Q
   | 33 => ((1 # 1) + s V_setdump_z + max0(-1 + s V_setdump_cnum) <= z)%Q
   | 34 => ((1 # 1) + s V_setdump_z + max0(-1 + s V_setdump_cnum) <= z)%Q
   | 35 => ((1 # 1) + s V_setdump_z + max0(-1 + s V_setdump_cnum) <= z)%Q
   | 36 => ((1 # 1) + s V_setdump_z + max0(-1 + s V_setdump_cnum) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_setdump =>
    [mkPA Q (fun n z s => ai_setdump n s /\ annot0_setdump n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_setdump (proc_start P_setdump) s1 (proc_end P_setdump) s2 ->
    (s2 V_setdump_z <= (127 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_setdump.
Qed.
