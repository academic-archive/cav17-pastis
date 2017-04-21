Require Import pasta.Pasta.

Inductive proc: Type :=
  P_TIFFReverseBits.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_TIFFReverseBits_z := 1%positive.
Notation V_TIFFReverseBits__tmp := 2%positive.
Notation V_TIFFReverseBits_cp := 3%positive.
Notation V_TIFFReverseBits_n := 4%positive.
Definition Pedges_TIFFReverseBits: list (edge proc) :=
  (EA 1 (AAssign V_TIFFReverseBits_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_TIFFReverseBits__tmp) s) >= (eval (ENum (0))
  s))%Z)) 3)::(EA 3 AWeaken 4)::(EA 4 (AAssign V_TIFFReverseBits__tmp
  (Some (EVar V_TIFFReverseBits_n))) 5)::(EA 5 ANone 6)::(EA 6 AWeaken 7)::
  (EA 7 (AGuard (fun s => ((eval (EVar V_TIFFReverseBits__tmp) s) >
  (eval (ENum (8)) s))%Z)) 19)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_TIFFReverseBits__tmp) s) <= (eval (ENum (8))
  s))%Z)) 8)::(EA 8 AWeaken 9)::(EA 9 ANone 10)::(EA 10 (AAssign
  V_TIFFReverseBits__tmp (Some (EAdd (EVar V_TIFFReverseBits__tmp)
  (ENum (-1))))) 11)::(EA 11 AWeaken 12)::(EA 12 (AGuard
  (fun s => ((eval (EVar V_TIFFReverseBits__tmp) s) > (eval (ENum (0))
  s))%Z)) 15)::(EA 12 (AGuard (fun s => ((eval (EVar V_TIFFReverseBits__tmp)
  s) <= (eval (ENum (0)) s))%Z)) 13)::(EA 13 AWeaken 14)::
  (EA 15 AWeaken 16)::(EA 16 ANone 17)::(EA 17 ANone 18)::(EA 18 (AAssign
  V_TIFFReverseBits_z (Some (EAdd (ENum (1))
  (EVar V_TIFFReverseBits_z)))) 10)::(EA 19 AWeaken 20)::(EA 20 ANone 21)::
  (EA 21 (AAssign V_TIFFReverseBits__tmp
  (Some (ESub (EVar V_TIFFReverseBits__tmp) (ENum (8))))) 22)::
  (EA 22 ANone 23)::(EA 23 ANone 24)::(EA 24 (AAssign V_TIFFReverseBits_z
  (Some (EAdd (ENum (1)) (EVar V_TIFFReverseBits_z)))) 25)::
  (EA 25 AWeaken 7)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_TIFFReverseBits => Pedges_TIFFReverseBits
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_TIFFReverseBits => 14
     end)%positive;
  var_global := var_global
}.

Definition ai_TIFFReverseBits (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_TIFFReverseBits_z <= 0 /\ -1 * s V_TIFFReverseBits_z <= 0)%Z
   | 3 => (-1 * s V_TIFFReverseBits_z <= 0 /\ 1 * s V_TIFFReverseBits_z <= 0 /\ -1 * s V_TIFFReverseBits__tmp <= 0)%Z
   | 4 => (-1 * s V_TIFFReverseBits__tmp <= 0 /\ 1 * s V_TIFFReverseBits_z <= 0 /\ -1 * s V_TIFFReverseBits_z <= 0)%Z
   | 5 => (-1 * s V_TIFFReverseBits_z <= 0 /\ 1 * s V_TIFFReverseBits_z <= 0)%Z
   | 6 => (1 * s V_TIFFReverseBits_z <= 0 /\ -1 * s V_TIFFReverseBits_z <= 0)%Z
   | 7 => (-1 * s V_TIFFReverseBits_z <= 0)%Z
   | 8 => (-1 * s V_TIFFReverseBits_z <= 0 /\ 1 * s V_TIFFReverseBits__tmp + -8 <= 0)%Z
   | 9 => (1 * s V_TIFFReverseBits__tmp + -8 <= 0 /\ -1 * s V_TIFFReverseBits_z <= 0)%Z
   | 10 => (-1 * s V_TIFFReverseBits_z <= 0 /\ 1 * s V_TIFFReverseBits__tmp + -8 <= 0)%Z
   | 11 => (-1 * s V_TIFFReverseBits_z <= 0 /\ 1 * s V_TIFFReverseBits__tmp + -7 <= 0)%Z
   | 12 => (1 * s V_TIFFReverseBits__tmp + -7 <= 0 /\ -1 * s V_TIFFReverseBits_z <= 0)%Z
   | 13 => (-1 * s V_TIFFReverseBits_z <= 0 /\ 1 * s V_TIFFReverseBits__tmp <= 0)%Z
   | 14 => (1 * s V_TIFFReverseBits__tmp <= 0 /\ -1 * s V_TIFFReverseBits_z <= 0)%Z
   | 15 => (-1 * s V_TIFFReverseBits_z <= 0 /\ 1 * s V_TIFFReverseBits__tmp + -7 <= 0 /\ -1 * s V_TIFFReverseBits__tmp + 1 <= 0)%Z
   | 16 => (-1 * s V_TIFFReverseBits__tmp + 1 <= 0 /\ 1 * s V_TIFFReverseBits__tmp + -7 <= 0 /\ -1 * s V_TIFFReverseBits_z <= 0)%Z
   | 17 => (-1 * s V_TIFFReverseBits_z <= 0 /\ 1 * s V_TIFFReverseBits__tmp + -7 <= 0 /\ -1 * s V_TIFFReverseBits__tmp + 1 <= 0)%Z
   | 18 => (-1 * s V_TIFFReverseBits__tmp + 1 <= 0 /\ 1 * s V_TIFFReverseBits__tmp + -7 <= 0 /\ -1 * s V_TIFFReverseBits_z <= 0)%Z
   | 19 => (-1 * s V_TIFFReverseBits_z <= 0 /\ -1 * s V_TIFFReverseBits__tmp + 9 <= 0)%Z
   | 20 => (-1 * s V_TIFFReverseBits__tmp + 9 <= 0 /\ -1 * s V_TIFFReverseBits_z <= 0)%Z
   | 21 => (-1 * s V_TIFFReverseBits_z <= 0 /\ -1 * s V_TIFFReverseBits__tmp + 9 <= 0)%Z
   | 22 => (-1 * s V_TIFFReverseBits_z <= 0 /\ -1 * s V_TIFFReverseBits__tmp + 1 <= 0)%Z
   | 23 => (-1 * s V_TIFFReverseBits__tmp + 1 <= 0 /\ -1 * s V_TIFFReverseBits_z <= 0)%Z
   | 24 => (-1 * s V_TIFFReverseBits_z <= 0 /\ -1 * s V_TIFFReverseBits__tmp + 1 <= 0)%Z
   | 25 => (-1 * s V_TIFFReverseBits__tmp + 1 <= 0 /\ -1 * s V_TIFFReverseBits_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_TIFFReverseBits (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((7 # 8) * max0(-1 + s V_TIFFReverseBits_n)
           + (1 # 8) * max0(s V_TIFFReverseBits_n) <= z)%Q
   | 2 => (s V_TIFFReverseBits_z + (7 # 8) * max0(-1 + s V_TIFFReverseBits_n)
           + (1 # 8) * max0(s V_TIFFReverseBits_n) <= z)%Q
   | 3 => (s V_TIFFReverseBits_z + (7 # 8) * max0(-1 + s V_TIFFReverseBits_n)
           + (1 # 8) * max0(s V_TIFFReverseBits_n) <= z)%Q
   | 4 => (s V_TIFFReverseBits_z + (7 # 8) * max0(-1 + s V_TIFFReverseBits_n)
           + (1 # 8) * max0(s V_TIFFReverseBits_n) <= z)%Q
   | 5 => (s V_TIFFReverseBits_z
           + (7 # 8) * max0(-1 + s V_TIFFReverseBits__tmp)
           + (1 # 8) * max0(s V_TIFFReverseBits__tmp) <= z)%Q
   | 6 => (s V_TIFFReverseBits_z
           + (7 # 8) * max0(-1 + s V_TIFFReverseBits__tmp)
           + (1 # 8) * max0(s V_TIFFReverseBits__tmp) <= z)%Q
   | 7 => (s V_TIFFReverseBits_z
           + (7 # 8) * max0(-1 + s V_TIFFReverseBits__tmp)
           + (1 # 8) * max0(s V_TIFFReverseBits__tmp) <= z)%Q
   | 8 => hints
     [(*0 0.125*) F_max0_monotonic (F_check_ge (s V_TIFFReverseBits__tmp) (-1
                                                                    + s V_TIFFReverseBits__tmp))]
     (s V_TIFFReverseBits_z + (7 # 8) * max0(-1 + s V_TIFFReverseBits__tmp)
      + (1 # 8) * max0(s V_TIFFReverseBits__tmp) <= z)%Q
   | 9 => (s V_TIFFReverseBits_z + max0(-1 + s V_TIFFReverseBits__tmp) <= z)%Q
   | 10 => (s V_TIFFReverseBits_z + max0(-1 + s V_TIFFReverseBits__tmp) <= z)%Q
   | 11 => (s V_TIFFReverseBits_z + max0(s V_TIFFReverseBits__tmp) <= z)%Q
   | 12 => (s V_TIFFReverseBits_z + max0(s V_TIFFReverseBits__tmp) <= z)%Q
   | 13 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-1 + s V_TIFFReverseBits__tmp) (-9
                                                                    + s V_TIFFReverseBits__tmp));
      (*-1 0*) F_max0_ge_0 (-9 + s V_TIFFReverseBits__tmp);
      (*-1 0*) F_max0_monotonic (F_check_ge (s V_TIFFReverseBits__tmp) (-1
                                                                    + s V_TIFFReverseBits__tmp))]
     (s V_TIFFReverseBits_z + max0(s V_TIFFReverseBits__tmp) <= z)%Q
   | 14 => (s V_TIFFReverseBits_z <= z)%Q
   | 15 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_TIFFReverseBits__tmp) (1)]
     (s V_TIFFReverseBits_z + max0(s V_TIFFReverseBits__tmp) <= z)%Q
   | 16 => ((1 # 1) + s V_TIFFReverseBits_z
            + max0(-1 + s V_TIFFReverseBits__tmp) <= z)%Q
   | 17 => ((1 # 1) + s V_TIFFReverseBits_z
            + max0(-1 + s V_TIFFReverseBits__tmp) <= z)%Q
   | 18 => ((1 # 1) + s V_TIFFReverseBits_z
            + max0(-1 + s V_TIFFReverseBits__tmp) <= z)%Q
   | 19 => hints
     [(*-0.125 0*) F_max0_pre_decrement 1 (s V_TIFFReverseBits__tmp) (8);
      (*-0.875 0*) F_max0_monotonic (F_check_ge (-1
                                                 + s V_TIFFReverseBits__tmp) (-9
                                                                    + s V_TIFFReverseBits__tmp))]
     (s V_TIFFReverseBits_z + (7 # 8) * max0(-1 + s V_TIFFReverseBits__tmp)
      + (1 # 8) * max0(s V_TIFFReverseBits__tmp) <= z)%Q
   | 20 => ((1 # 1) + s V_TIFFReverseBits_z
            + (7 # 8) * max0(-9 + s V_TIFFReverseBits__tmp)
            + (1 # 8) * max0(-8 + s V_TIFFReverseBits__tmp) <= z)%Q
   | 21 => ((1 # 1) + s V_TIFFReverseBits_z
            + (7 # 8) * max0(-9 + s V_TIFFReverseBits__tmp)
            + (1 # 8) * max0(-8 + s V_TIFFReverseBits__tmp) <= z)%Q
   | 22 => ((1 # 1) + s V_TIFFReverseBits_z
            + (7 # 8) * max0(-1 + s V_TIFFReverseBits__tmp)
            + (1 # 8) * max0(s V_TIFFReverseBits__tmp) <= z)%Q
   | 23 => ((1 # 1) + s V_TIFFReverseBits_z
            + (7 # 8) * max0(-1 + s V_TIFFReverseBits__tmp)
            + (1 # 8) * max0(s V_TIFFReverseBits__tmp) <= z)%Q
   | 24 => ((1 # 1) + s V_TIFFReverseBits_z
            + (7 # 8) * max0(-1 + s V_TIFFReverseBits__tmp)
            + (1 # 8) * max0(s V_TIFFReverseBits__tmp) <= z)%Q
   | 25 => (s V_TIFFReverseBits_z
            + (7 # 8) * max0(-1 + s V_TIFFReverseBits__tmp)
            + (1 # 8) * max0(s V_TIFFReverseBits__tmp) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_TIFFReverseBits =>
    [mkPA Q (fun n z s => ai_TIFFReverseBits n s /\ annot0_TIFFReverseBits n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_TIFFReverseBits (proc_start P_TIFFReverseBits) s1 (proc_end P_TIFFReverseBits) s2 ->
    (s2 V_TIFFReverseBits_z <= (7 # 8) * max0(-1 + s1 V_TIFFReverseBits_n)
                               + (1 # 8) * max0(s1 V_TIFFReverseBits_n))%Q.
Proof.
  prove_bound ipa admissible_ipa P_TIFFReverseBits.
Qed.
