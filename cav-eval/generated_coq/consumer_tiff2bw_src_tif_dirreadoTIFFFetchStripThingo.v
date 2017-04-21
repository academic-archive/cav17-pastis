Require Import pasta.Pasta.

Inductive proc: Type :=
  P_TIFFFetchStripThing.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_TIFFFetchStripThing_z := 1%positive.
Notation V_TIFFFetchStripThing__tmp := 2%positive.
Notation V_TIFFFetchStripThing__tmp1 := 3%positive.
Notation V_TIFFFetchStripThing_status := 4%positive.
Notation V_TIFFFetchStripThing_dir := 5%positive.
Notation V_TIFFFetchStripThing_lpp := 6%positive.
Notation V_TIFFFetchStripThing_nstrips := 7%positive.
Notation V_TIFFFetchStripThing_tif := 8%positive.
Definition Pedges_TIFFFetchStripThing: list (edge proc) :=
  (EA 1 (AAssign V_TIFFFetchStripThing_z (Some (ENum (0)))) 2)::
  (EA 2 (AAssign V_TIFFFetchStripThing__tmp1
  (Some (EVar V_TIFFFetchStripThing_nstrips))) 3)::(EA 3 AWeaken 4)::
  (EA 4 ANone 8)::(EA 4 ANone 5)::(EA 5 (AAssign V_TIFFFetchStripThing__tmp
  (Some (ENum (0)))) 6)::(EA 6 ANone 7)::(EA 7 AWeaken 42)::
  (EA 8 AWeaken 9)::(EA 9 ANone 11)::(EA 9 ANone 10)::(EA 10 AWeaken 14)::
  (EA 11 AWeaken 12)::(EA 12 ANone 39)::(EA 12 ANone 13)::
  (EA 13 AWeaken 14)::(EA 14 ANone 17)::(EA 14 ANone 15)::(EA 15 (AAssign
  V_TIFFFetchStripThing_status None) 16)::(EA 16 ANone 29)::
  (EA 17 AWeaken 18)::(EA 18 ANone 36)::(EA 18 ANone 19)::(EA 19 (AAssign
  V_TIFFFetchStripThing_status None) 20)::(EA 20 AWeaken 21)::
  (EA 21 ANone 22)::(EA 21 ANone 28)::(EA 22 ANone 23)::(EA 23 (AAssign
  V_TIFFFetchStripThing__tmp1 (Some (EAdd (EVar V_TIFFFetchStripThing__tmp1)
  (ENum (-1))))) 24)::(EA 24 AWeaken 25)::(EA 25 (AGuard
  (fun s => ((eval (EVar V_TIFFFetchStripThing__tmp1) s) > (eval (ENum (0))
  s))%Z)) 32)::(EA 25 (AGuard
  (fun s => ((eval (EVar V_TIFFFetchStripThing__tmp1) s) <= (eval (ENum (0))
  s))%Z)) 26)::(EA 26 AWeaken 27)::(EA 27 ANone 28)::(EA 28 ANone 29)::
  (EA 29 (AAssign V_TIFFFetchStripThing__tmp
  (Some (EVar V_TIFFFetchStripThing_status))) 30)::(EA 30 ANone 31)::
  (EA 31 AWeaken 42)::(EA 32 AWeaken 33)::(EA 33 ANone 34)::
  (EA 34 ANone 35)::(EA 35 (AAssign V_TIFFFetchStripThing_z
  (Some (EAdd (ENum (1)) (EVar V_TIFFFetchStripThing_z)))) 23)::
  (EA 36 (AAssign V_TIFFFetchStripThing__tmp (Some (ENum (0)))) 37)::
  (EA 37 ANone 38)::(EA 38 AWeaken 42)::(EA 39 (AAssign
  V_TIFFFetchStripThing__tmp (Some (ENum (0)))) 40)::(EA 40 ANone 41)::
  (EA 41 AWeaken 42)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_TIFFFetchStripThing => Pedges_TIFFFetchStripThing
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_TIFFFetchStripThing => 42
     end)%positive;
  var_global := var_global
}.

Definition ai_TIFFFetchStripThing (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_TIFFFetchStripThing_z <= 0 /\ -1 * s V_TIFFFetchStripThing_z <= 0)%Z
   | 3 => (-1 * s V_TIFFFetchStripThing_z <= 0 /\ 1 * s V_TIFFFetchStripThing_z <= 0)%Z
   | 4 => (1 * s V_TIFFFetchStripThing_z <= 0 /\ -1 * s V_TIFFFetchStripThing_z <= 0)%Z
   | 5 => (-1 * s V_TIFFFetchStripThing_z <= 0 /\ 1 * s V_TIFFFetchStripThing_z <= 0)%Z
   | 6 => (1 * s V_TIFFFetchStripThing_z <= 0 /\ -1 * s V_TIFFFetchStripThing_z <= 0 /\ 1 * s V_TIFFFetchStripThing__tmp <= 0 /\ -1 * s V_TIFFFetchStripThing__tmp <= 0)%Z
   | 7 => (-1 * s V_TIFFFetchStripThing__tmp <= 0 /\ 1 * s V_TIFFFetchStripThing__tmp <= 0 /\ -1 * s V_TIFFFetchStripThing_z <= 0 /\ 1 * s V_TIFFFetchStripThing_z <= 0)%Z
   | 8 => (-1 * s V_TIFFFetchStripThing_z <= 0 /\ 1 * s V_TIFFFetchStripThing_z <= 0)%Z
   | 9 => (1 * s V_TIFFFetchStripThing_z <= 0 /\ -1 * s V_TIFFFetchStripThing_z <= 0)%Z
   | 10 => (-1 * s V_TIFFFetchStripThing_z <= 0 /\ 1 * s V_TIFFFetchStripThing_z <= 0)%Z
   | 11 => (-1 * s V_TIFFFetchStripThing_z <= 0 /\ 1 * s V_TIFFFetchStripThing_z <= 0)%Z
   | 12 => (1 * s V_TIFFFetchStripThing_z <= 0 /\ -1 * s V_TIFFFetchStripThing_z <= 0)%Z
   | 13 => (-1 * s V_TIFFFetchStripThing_z <= 0 /\ 1 * s V_TIFFFetchStripThing_z <= 0)%Z
   | 14 => (1 * s V_TIFFFetchStripThing_z <= 0 /\ -1 * s V_TIFFFetchStripThing_z <= 0)%Z
   | 15 => (-1 * s V_TIFFFetchStripThing_z <= 0 /\ 1 * s V_TIFFFetchStripThing_z <= 0)%Z
   | 16 => (1 * s V_TIFFFetchStripThing_z <= 0 /\ -1 * s V_TIFFFetchStripThing_z <= 0)%Z
   | 17 => (-1 * s V_TIFFFetchStripThing_z <= 0 /\ 1 * s V_TIFFFetchStripThing_z <= 0)%Z
   | 18 => (1 * s V_TIFFFetchStripThing_z <= 0 /\ -1 * s V_TIFFFetchStripThing_z <= 0)%Z
   | 19 => (-1 * s V_TIFFFetchStripThing_z <= 0 /\ 1 * s V_TIFFFetchStripThing_z <= 0)%Z
   | 20 => (1 * s V_TIFFFetchStripThing_z <= 0 /\ -1 * s V_TIFFFetchStripThing_z <= 0)%Z
   | 21 => (-1 * s V_TIFFFetchStripThing_z <= 0 /\ 1 * s V_TIFFFetchStripThing_z <= 0)%Z
   | 22 => (1 * s V_TIFFFetchStripThing_z <= 0 /\ -1 * s V_TIFFFetchStripThing_z <= 0)%Z
   | 23 => (-1 * s V_TIFFFetchStripThing_z <= 0)%Z
   | 24 => (-1 * s V_TIFFFetchStripThing_z <= 0)%Z
   | 25 => (-1 * s V_TIFFFetchStripThing_z <= 0)%Z
   | 26 => (-1 * s V_TIFFFetchStripThing_z <= 0 /\ 1 * s V_TIFFFetchStripThing__tmp1 <= 0)%Z
   | 27 => (1 * s V_TIFFFetchStripThing__tmp1 <= 0 /\ -1 * s V_TIFFFetchStripThing_z <= 0)%Z
   | 28 => (-1 * s V_TIFFFetchStripThing_z <= 0)%Z
   | 29 => (-1 * s V_TIFFFetchStripThing_z <= 0)%Z
   | 30 => (-1 * s V_TIFFFetchStripThing_z <= 0)%Z
   | 31 => (-1 * s V_TIFFFetchStripThing_z <= 0)%Z
   | 32 => (-1 * s V_TIFFFetchStripThing_z <= 0 /\ -1 * s V_TIFFFetchStripThing__tmp1 + 1 <= 0)%Z
   | 33 => (-1 * s V_TIFFFetchStripThing__tmp1 + 1 <= 0 /\ -1 * s V_TIFFFetchStripThing_z <= 0)%Z
   | 34 => (-1 * s V_TIFFFetchStripThing_z <= 0 /\ -1 * s V_TIFFFetchStripThing__tmp1 + 1 <= 0)%Z
   | 35 => (-1 * s V_TIFFFetchStripThing__tmp1 + 1 <= 0 /\ -1 * s V_TIFFFetchStripThing_z <= 0)%Z
   | 36 => (-1 * s V_TIFFFetchStripThing_z <= 0 /\ 1 * s V_TIFFFetchStripThing_z <= 0)%Z
   | 37 => (1 * s V_TIFFFetchStripThing_z <= 0 /\ -1 * s V_TIFFFetchStripThing_z <= 0 /\ 1 * s V_TIFFFetchStripThing__tmp <= 0 /\ -1 * s V_TIFFFetchStripThing__tmp <= 0)%Z
   | 38 => (-1 * s V_TIFFFetchStripThing__tmp <= 0 /\ 1 * s V_TIFFFetchStripThing__tmp <= 0 /\ -1 * s V_TIFFFetchStripThing_z <= 0 /\ 1 * s V_TIFFFetchStripThing_z <= 0)%Z
   | 39 => (-1 * s V_TIFFFetchStripThing_z <= 0 /\ 1 * s V_TIFFFetchStripThing_z <= 0)%Z
   | 40 => (1 * s V_TIFFFetchStripThing_z <= 0 /\ -1 * s V_TIFFFetchStripThing_z <= 0 /\ 1 * s V_TIFFFetchStripThing__tmp <= 0 /\ -1 * s V_TIFFFetchStripThing__tmp <= 0)%Z
   | 41 => (-1 * s V_TIFFFetchStripThing__tmp <= 0 /\ 1 * s V_TIFFFetchStripThing__tmp <= 0 /\ -1 * s V_TIFFFetchStripThing_z <= 0 /\ 1 * s V_TIFFFetchStripThing_z <= 0)%Z
   | 42 => (-1 * s V_TIFFFetchStripThing_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_TIFFFetchStripThing (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_TIFFFetchStripThing_nstrips) <= z)%Q
   | 2 => (s V_TIFFFetchStripThing_z + max0(s V_TIFFFetchStripThing_nstrips) <= z)%Q
   | 3 => (s V_TIFFFetchStripThing_z + max0(s V_TIFFFetchStripThing__tmp1) <= z)%Q
   | 4 => (s V_TIFFFetchStripThing_z + max0(s V_TIFFFetchStripThing__tmp1) <= z)%Q
   | 5 => (s V_TIFFFetchStripThing_z + max0(s V_TIFFFetchStripThing__tmp1) <= z)%Q
   | 6 => (s V_TIFFFetchStripThing_z + max0(s V_TIFFFetchStripThing__tmp1) <= z)%Q
   | 7 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_TIFFFetchStripThing__tmp1) (-1
                                                                    + s V_TIFFFetchStripThing__tmp1));
      (*-1 0*) F_max0_ge_0 (-1 + s V_TIFFFetchStripThing__tmp1)]
     (s V_TIFFFetchStripThing_z + max0(s V_TIFFFetchStripThing__tmp1) <= z)%Q
   | 8 => (s V_TIFFFetchStripThing_z + max0(s V_TIFFFetchStripThing__tmp1) <= z)%Q
   | 9 => (s V_TIFFFetchStripThing_z + max0(s V_TIFFFetchStripThing__tmp1) <= z)%Q
   | 10 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_TIFFFetchStripThing__tmp1) (-1
                                                                    + s V_TIFFFetchStripThing__tmp1))]
     (s V_TIFFFetchStripThing_z + max0(s V_TIFFFetchStripThing__tmp1) <= z)%Q
   | 11 => (s V_TIFFFetchStripThing_z + max0(s V_TIFFFetchStripThing__tmp1) <= z)%Q
   | 12 => (s V_TIFFFetchStripThing_z + max0(s V_TIFFFetchStripThing__tmp1) <= z)%Q
   | 13 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_TIFFFetchStripThing__tmp1) (-1
                                                                    + s V_TIFFFetchStripThing__tmp1))]
     (s V_TIFFFetchStripThing_z + max0(s V_TIFFFetchStripThing__tmp1) <= z)%Q
   | 14 => (s V_TIFFFetchStripThing_z
            + max0(-1 + s V_TIFFFetchStripThing__tmp1) <= z)%Q
   | 15 => (s V_TIFFFetchStripThing_z
            + max0(-1 + s V_TIFFFetchStripThing__tmp1) <= z)%Q
   | 16 => (s V_TIFFFetchStripThing_z
            + max0(-1 + s V_TIFFFetchStripThing__tmp1) <= z)%Q
   | 17 => (s V_TIFFFetchStripThing_z
            + max0(-1 + s V_TIFFFetchStripThing__tmp1) <= z)%Q
   | 18 => (s V_TIFFFetchStripThing_z
            + max0(-1 + s V_TIFFFetchStripThing__tmp1) <= z)%Q
   | 19 => (s V_TIFFFetchStripThing_z
            + max0(-1 + s V_TIFFFetchStripThing__tmp1) <= z)%Q
   | 20 => (s V_TIFFFetchStripThing_z
            + max0(-1 + s V_TIFFFetchStripThing__tmp1) <= z)%Q
   | 21 => (s V_TIFFFetchStripThing_z
            + max0(-1 + s V_TIFFFetchStripThing__tmp1) <= z)%Q
   | 22 => (s V_TIFFFetchStripThing_z
            + max0(-1 + s V_TIFFFetchStripThing__tmp1) <= z)%Q
   | 23 => (s V_TIFFFetchStripThing_z
            + max0(-1 + s V_TIFFFetchStripThing__tmp1) <= z)%Q
   | 24 => (s V_TIFFFetchStripThing_z + max0(s V_TIFFFetchStripThing__tmp1) <= z)%Q
   | 25 => (s V_TIFFFetchStripThing_z + max0(s V_TIFFFetchStripThing__tmp1) <= z)%Q
   | 26 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_TIFFFetchStripThing__tmp1) (-1
                                                                    + s V_TIFFFetchStripThing__tmp1))]
     (s V_TIFFFetchStripThing_z + max0(s V_TIFFFetchStripThing__tmp1) <= z)%Q
   | 27 => (s V_TIFFFetchStripThing_z
            + max0(-1 + s V_TIFFFetchStripThing__tmp1) <= z)%Q
   | 28 => (s V_TIFFFetchStripThing_z
            + max0(-1 + s V_TIFFFetchStripThing__tmp1) <= z)%Q
   | 29 => (s V_TIFFFetchStripThing_z
            + max0(-1 + s V_TIFFFetchStripThing__tmp1) <= z)%Q
   | 30 => (s V_TIFFFetchStripThing_z
            + max0(-1 + s V_TIFFFetchStripThing__tmp1) <= z)%Q
   | 31 => hints
     [(*-1 0*) F_max0_ge_0 (-1 + s V_TIFFFetchStripThing__tmp1)]
     (s V_TIFFFetchStripThing_z + max0(-1 + s V_TIFFFetchStripThing__tmp1) <= z)%Q
   | 32 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_TIFFFetchStripThing__tmp1) (1)]
     (s V_TIFFFetchStripThing_z + max0(s V_TIFFFetchStripThing__tmp1) <= z)%Q
   | 33 => ((1 # 1) + s V_TIFFFetchStripThing_z
            + max0(-1 + s V_TIFFFetchStripThing__tmp1) <= z)%Q
   | 34 => ((1 # 1) + s V_TIFFFetchStripThing_z
            + max0(-1 + s V_TIFFFetchStripThing__tmp1) <= z)%Q
   | 35 => ((1 # 1) + s V_TIFFFetchStripThing_z
            + max0(-1 + s V_TIFFFetchStripThing__tmp1) <= z)%Q
   | 36 => (s V_TIFFFetchStripThing_z
            + max0(-1 + s V_TIFFFetchStripThing__tmp1) <= z)%Q
   | 37 => (s V_TIFFFetchStripThing_z
            + max0(-1 + s V_TIFFFetchStripThing__tmp1) <= z)%Q
   | 38 => hints
     [(*-1 0*) F_max0_ge_0 (-1 + s V_TIFFFetchStripThing__tmp1)]
     (s V_TIFFFetchStripThing_z + max0(-1 + s V_TIFFFetchStripThing__tmp1) <= z)%Q
   | 39 => (s V_TIFFFetchStripThing_z + max0(s V_TIFFFetchStripThing__tmp1) <= z)%Q
   | 40 => (s V_TIFFFetchStripThing_z + max0(s V_TIFFFetchStripThing__tmp1) <= z)%Q
   | 41 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_TIFFFetchStripThing__tmp1) (-1
                                                                    + s V_TIFFFetchStripThing__tmp1));
      (*-1 0*) F_max0_ge_0 (-1 + s V_TIFFFetchStripThing__tmp1)]
     (s V_TIFFFetchStripThing_z + max0(s V_TIFFFetchStripThing__tmp1) <= z)%Q
   | 42 => (s V_TIFFFetchStripThing_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_TIFFFetchStripThing =>
    [mkPA Q (fun n z s => ai_TIFFFetchStripThing n s /\ annot0_TIFFFetchStripThing n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_TIFFFetchStripThing (proc_start P_TIFFFetchStripThing) s1 (proc_end P_TIFFFetchStripThing) s2 ->
    (s2 V_TIFFFetchStripThing_z <= max0(s1 V_TIFFFetchStripThing_nstrips))%Q.
Proof.
  prove_bound ipa admissible_ipa P_TIFFFetchStripThing.
Qed.
