Require Import pasta.Pasta.

Inductive proc: Type :=
  P_checkcmap.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_checkcmap_z := 1%positive.
Notation V_checkcmap__tmp := 2%positive.
Notation V_checkcmap__tmp1 := 3%positive.
Notation V_checkcmap_b := 4%positive.
Notation V_checkcmap_g := 5%positive.
Notation V_checkcmap_n := 6%positive.
Notation V_checkcmap_r := 7%positive.
Notation V_checkcmap_tif := 8%positive.
Definition Pedges_checkcmap: list (edge proc) :=
  (EA 1 (AAssign V_checkcmap_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_checkcmap__tmp (Some (EVar V_checkcmap_n))) 3)::(EA 3 ANone 4)::
  (EA 4 (AAssign V_checkcmap__tmp (Some (EAdd (EVar V_checkcmap__tmp)
  (ENum (-1))))) 5)::(EA 5 AWeaken 6)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_checkcmap__tmp) s) > (eval (ENum (0))
  s))%Z)) 11)::(EA 6 (AGuard (fun s => ((eval (EVar V_checkcmap__tmp) s) <=
  (eval (ENum (0)) s))%Z)) 7)::(EA 7 AWeaken 8)::(EA 8 (AAssign
  V_checkcmap__tmp1 (Some (ENum (8)))) 9)::(EA 9 ANone 10)::
  (EA 10 AWeaken 23)::(EA 11 AWeaken 12)::(EA 12 ANone 20)::
  (EA 12 ANone 13)::(EA 13 AWeaken 14)::(EA 14 ANone 20)::(EA 14 ANone 15)::
  (EA 15 AWeaken 16)::(EA 16 ANone 20)::(EA 16 ANone 17)::(EA 17 ANone 18)::
  (EA 18 ANone 19)::(EA 19 (AAssign V_checkcmap_z (Some (EAdd (ENum (1))
  (EVar V_checkcmap_z)))) 4)::(EA 20 (AAssign V_checkcmap__tmp1
  (Some (ENum (16)))) 21)::(EA 21 ANone 22)::(EA 22 AWeaken 23)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_checkcmap => Pedges_checkcmap
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_checkcmap => 23
     end)%positive;
  var_global := var_global
}.

Definition ai_checkcmap (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_checkcmap_z <= 0 /\ -1 * s V_checkcmap_z <= 0)%Z
   | 3 => (-1 * s V_checkcmap_z <= 0 /\ 1 * s V_checkcmap_z <= 0)%Z
   | 4 => (-1 * s V_checkcmap_z <= 0)%Z
   | 5 => (-1 * s V_checkcmap_z <= 0)%Z
   | 6 => (-1 * s V_checkcmap_z <= 0)%Z
   | 7 => (-1 * s V_checkcmap_z <= 0 /\ 1 * s V_checkcmap__tmp <= 0)%Z
   | 8 => (1 * s V_checkcmap__tmp <= 0 /\ -1 * s V_checkcmap_z <= 0)%Z
   | 9 => (-1 * s V_checkcmap_z <= 0 /\ 1 * s V_checkcmap__tmp <= 0 /\ 1 * s V_checkcmap__tmp1 + -8 <= 0 /\ -1 * s V_checkcmap__tmp1 + 8 <= 0)%Z
   | 10 => (-1 * s V_checkcmap__tmp1 + 8 <= 0 /\ 1 * s V_checkcmap__tmp1 + -8 <= 0 /\ 1 * s V_checkcmap__tmp <= 0 /\ -1 * s V_checkcmap_z <= 0)%Z
   | 11 => (-1 * s V_checkcmap_z <= 0 /\ -1 * s V_checkcmap__tmp + 1 <= 0)%Z
   | 12 => (-1 * s V_checkcmap__tmp + 1 <= 0 /\ -1 * s V_checkcmap_z <= 0)%Z
   | 13 => (-1 * s V_checkcmap_z <= 0 /\ -1 * s V_checkcmap__tmp + 1 <= 0)%Z
   | 14 => (-1 * s V_checkcmap__tmp + 1 <= 0 /\ -1 * s V_checkcmap_z <= 0)%Z
   | 15 => (-1 * s V_checkcmap_z <= 0 /\ -1 * s V_checkcmap__tmp + 1 <= 0)%Z
   | 16 => (-1 * s V_checkcmap__tmp + 1 <= 0 /\ -1 * s V_checkcmap_z <= 0)%Z
   | 17 => (-1 * s V_checkcmap_z <= 0 /\ -1 * s V_checkcmap__tmp + 1 <= 0)%Z
   | 18 => (-1 * s V_checkcmap__tmp + 1 <= 0 /\ -1 * s V_checkcmap_z <= 0)%Z
   | 19 => (-1 * s V_checkcmap_z <= 0 /\ -1 * s V_checkcmap__tmp + 1 <= 0)%Z
   | 20 => (-1 * s V_checkcmap_z <= 0 /\ -1 * s V_checkcmap__tmp + 1 <= 0)%Z
   | 21 => (-1 * s V_checkcmap__tmp + 1 <= 0 /\ -1 * s V_checkcmap_z <= 0 /\ 1 * s V_checkcmap__tmp1 + -16 <= 0 /\ -1 * s V_checkcmap__tmp1 + 16 <= 0)%Z
   | 22 => (-1 * s V_checkcmap__tmp1 + 16 <= 0 /\ 1 * s V_checkcmap__tmp1 + -16 <= 0 /\ -1 * s V_checkcmap_z <= 0 /\ -1 * s V_checkcmap__tmp + 1 <= 0)%Z
   | 23 => (-1 * s V_checkcmap__tmp1 + 8 <= 0 /\ -1 * s V_checkcmap_z <= 0 /\ 1 * s V_checkcmap__tmp1 + -16 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_checkcmap (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(-1 + s V_checkcmap_n) <= z)%Q
   | 2 => (s V_checkcmap_z + max0(-1 + s V_checkcmap_n) <= z)%Q
   | 3 => (s V_checkcmap_z + max0(-1 + s V_checkcmap__tmp) <= z)%Q
   | 4 => (s V_checkcmap_z + max0(-1 + s V_checkcmap__tmp) <= z)%Q
   | 5 => (s V_checkcmap_z + max0(s V_checkcmap__tmp) <= z)%Q
   | 6 => (s V_checkcmap_z + max0(s V_checkcmap__tmp) <= z)%Q
   | 7 => (s V_checkcmap_z + max0(s V_checkcmap__tmp) <= z)%Q
   | 8 => (s V_checkcmap_z + max0(s V_checkcmap__tmp) <= z)%Q
   | 9 => (s V_checkcmap_z + max0(s V_checkcmap__tmp) <= z)%Q
   | 10 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_checkcmap__tmp) (-1
                                                                  + s V_checkcmap__tmp));
      (*-1 0*) F_max0_ge_0 (-1 + s V_checkcmap__tmp)]
     (s V_checkcmap_z + max0(s V_checkcmap__tmp) <= z)%Q
   | 11 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_checkcmap__tmp) (1)]
     (s V_checkcmap_z + max0(s V_checkcmap__tmp) <= z)%Q
   | 12 => ((1 # 1) + s V_checkcmap_z + max0(-1 + s V_checkcmap__tmp) <= z)%Q
   | 13 => ((1 # 1) + s V_checkcmap_z + max0(-1 + s V_checkcmap__tmp) <= z)%Q
   | 14 => ((1 # 1) + s V_checkcmap_z + max0(-1 + s V_checkcmap__tmp) <= z)%Q
   | 15 => ((1 # 1) + s V_checkcmap_z + max0(-1 + s V_checkcmap__tmp) <= z)%Q
   | 16 => ((1 # 1) + s V_checkcmap_z + max0(-1 + s V_checkcmap__tmp) <= z)%Q
   | 17 => ((1 # 1) + s V_checkcmap_z + max0(-1 + s V_checkcmap__tmp) <= z)%Q
   | 18 => ((1 # 1) + s V_checkcmap_z + max0(-1 + s V_checkcmap__tmp) <= z)%Q
   | 19 => ((1 # 1) + s V_checkcmap_z + max0(-1 + s V_checkcmap__tmp) <= z)%Q
   | 20 => ((1 # 1) + s V_checkcmap_z + max0(-1 + s V_checkcmap__tmp) <= z)%Q
   | 21 => (s V_checkcmap_z + (1 # 8) * max0(-8 + s V_checkcmap__tmp1)
            + max0(-1 + s V_checkcmap__tmp) <= z)%Q
   | 22 => hints
     [(*-1 0*) F_max0_ge_0 (-1 + s V_checkcmap__tmp);
      (*-0.125 0*) F_binom_monotonic 1 (F_max0_ge_0 (-8 + s V_checkcmap__tmp1)) (F_check_ge (0) (0))]
     (s V_checkcmap_z + (1 # 8) * max0(-8 + s V_checkcmap__tmp1)
      + max0(-1 + s V_checkcmap__tmp) <= z)%Q
   | 23 => (s V_checkcmap_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_checkcmap =>
    [mkPA Q (fun n z s => ai_checkcmap n s /\ annot0_checkcmap n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_checkcmap (proc_start P_checkcmap) s1 (proc_end P_checkcmap) s2 ->
    (s2 V_checkcmap_z <= max0(-1 + s1 V_checkcmap_n))%Q.
Proof.
  prove_bound ipa admissible_ipa P_checkcmap.
Qed.
