Require Import pasta.Pasta.

Inductive proc: Type :=
  P_Luv24fromLuv48.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_Luv24fromLuv48_z := 1%positive.
Notation V_Luv24fromLuv48_Ce := 2%positive.
Notation V_Luv24fromLuv48_Le := 3%positive.
Notation V_Luv24fromLuv48__tmp := 4%positive.
Notation V_Luv24fromLuv48_n := 5%positive.
Notation V_Luv24fromLuv48_op := 6%positive.
Notation V_Luv24fromLuv48_sp := 7%positive.
Definition Pedges_Luv24fromLuv48: list (edge proc) :=
  (EA 1 (AAssign V_Luv24fromLuv48_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_Luv24fromLuv48__tmp (Some (EVar V_Luv24fromLuv48_n))) 3)::
  (EA 3 ANone 4)::(EA 4 (AAssign V_Luv24fromLuv48__tmp
  (Some (EAdd (EVar V_Luv24fromLuv48__tmp) (ENum (-1))))) 5)::
  (EA 5 AWeaken 6)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_Luv24fromLuv48__tmp) s) > (eval (ENum (0))
  s))%Z)) 9)::(EA 6 (AGuard (fun s => ((eval (EVar V_Luv24fromLuv48__tmp)
  s) <= (eval (ENum (0)) s))%Z)) 7)::(EA 7 AWeaken 8)::(EA 9 AWeaken 10)::
  (EA 10 ANone 18)::(EA 10 ANone 11)::(EA 11 AWeaken 12)::(EA 12 ANone 15)::
  (EA 12 ANone 13)::(EA 13 (AAssign V_Luv24fromLuv48_Le None) 14)::
  (EA 14 ANone 17)::(EA 15 (AAssign V_Luv24fromLuv48_Le
  (Some (ENum (1023)))) 16)::(EA 16 ANone 17)::(EA 17 ANone 20)::
  (EA 18 (AAssign V_Luv24fromLuv48_Le (Some (ENum (0)))) 19)::
  (EA 19 ANone 20)::(EA 20 (AAssign V_Luv24fromLuv48_Ce None) 21)::
  (EA 21 AWeaken 22)::(EA 22 (AGuard
  (fun s => ((eval (EVar V_Luv24fromLuv48_Ce) s) < (eval (ENum (0))
  s))%Z)) 24)::(EA 22 (AGuard (fun s => ((eval (EVar V_Luv24fromLuv48_Ce)
  s) >= (eval (ENum (0)) s))%Z)) 23)::(EA 23 AWeaken 27)::
  (EA 24 AWeaken 25)::(EA 25 (AAssign V_Luv24fromLuv48_Ce None) 26)::
  (EA 26 ANone 27)::(EA 27 ANone 28)::(EA 28 ANone 29)::(EA 29 (AAssign
  V_Luv24fromLuv48_z (Some (EAdd (ENum (1)) (EVar V_Luv24fromLuv48_z)))) 4)::
  nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_Luv24fromLuv48 => Pedges_Luv24fromLuv48
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_Luv24fromLuv48 => 8
     end)%positive;
  var_global := var_global
}.

Definition ai_Luv24fromLuv48 (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_Luv24fromLuv48_z <= 0 /\ -1 * s V_Luv24fromLuv48_z <= 0)%Z
   | 3 => (-1 * s V_Luv24fromLuv48_z <= 0 /\ 1 * s V_Luv24fromLuv48_z <= 0)%Z
   | 4 => (-1 * s V_Luv24fromLuv48_z <= 0)%Z
   | 5 => (-1 * s V_Luv24fromLuv48_z <= 0)%Z
   | 6 => (-1 * s V_Luv24fromLuv48_z <= 0)%Z
   | 7 => (-1 * s V_Luv24fromLuv48_z <= 0 /\ 1 * s V_Luv24fromLuv48__tmp <= 0)%Z
   | 8 => (1 * s V_Luv24fromLuv48__tmp <= 0 /\ -1 * s V_Luv24fromLuv48_z <= 0)%Z
   | 9 => (-1 * s V_Luv24fromLuv48_z <= 0 /\ -1 * s V_Luv24fromLuv48__tmp + 1 <= 0)%Z
   | 10 => (-1 * s V_Luv24fromLuv48__tmp + 1 <= 0 /\ -1 * s V_Luv24fromLuv48_z <= 0)%Z
   | 11 => (-1 * s V_Luv24fromLuv48_z <= 0 /\ -1 * s V_Luv24fromLuv48__tmp + 1 <= 0)%Z
   | 12 => (-1 * s V_Luv24fromLuv48__tmp + 1 <= 0 /\ -1 * s V_Luv24fromLuv48_z <= 0)%Z
   | 13 => (-1 * s V_Luv24fromLuv48_z <= 0 /\ -1 * s V_Luv24fromLuv48__tmp + 1 <= 0)%Z
   | 14 => (-1 * s V_Luv24fromLuv48__tmp + 1 <= 0 /\ -1 * s V_Luv24fromLuv48_z <= 0)%Z
   | 15 => (-1 * s V_Luv24fromLuv48_z <= 0 /\ -1 * s V_Luv24fromLuv48__tmp + 1 <= 0)%Z
   | 16 => (-1 * s V_Luv24fromLuv48__tmp + 1 <= 0 /\ -1 * s V_Luv24fromLuv48_z <= 0 /\ 1 * s V_Luv24fromLuv48_Le + -1023 <= 0 /\ -1 * s V_Luv24fromLuv48_Le + 1023 <= 0)%Z
   | 17 => (-1 * s V_Luv24fromLuv48_z <= 0 /\ -1 * s V_Luv24fromLuv48__tmp + 1 <= 0)%Z
   | 18 => (-1 * s V_Luv24fromLuv48_z <= 0 /\ -1 * s V_Luv24fromLuv48__tmp + 1 <= 0)%Z
   | 19 => (-1 * s V_Luv24fromLuv48__tmp + 1 <= 0 /\ -1 * s V_Luv24fromLuv48_z <= 0 /\ 1 * s V_Luv24fromLuv48_Le <= 0 /\ -1 * s V_Luv24fromLuv48_Le <= 0)%Z
   | 20 => (-1 * s V_Luv24fromLuv48_z <= 0 /\ -1 * s V_Luv24fromLuv48__tmp + 1 <= 0)%Z
   | 21 => (-1 * s V_Luv24fromLuv48__tmp + 1 <= 0 /\ -1 * s V_Luv24fromLuv48_z <= 0)%Z
   | 22 => (-1 * s V_Luv24fromLuv48_z <= 0 /\ -1 * s V_Luv24fromLuv48__tmp + 1 <= 0)%Z
   | 23 => (-1 * s V_Luv24fromLuv48__tmp + 1 <= 0 /\ -1 * s V_Luv24fromLuv48_z <= 0 /\ -1 * s V_Luv24fromLuv48_Ce <= 0)%Z
   | 24 => (-1 * s V_Luv24fromLuv48__tmp + 1 <= 0 /\ -1 * s V_Luv24fromLuv48_z <= 0 /\ 1 * s V_Luv24fromLuv48_Ce + 1 <= 0)%Z
   | 25 => (1 * s V_Luv24fromLuv48_Ce + 1 <= 0 /\ -1 * s V_Luv24fromLuv48_z <= 0 /\ -1 * s V_Luv24fromLuv48__tmp + 1 <= 0)%Z
   | 26 => (-1 * s V_Luv24fromLuv48__tmp + 1 <= 0 /\ -1 * s V_Luv24fromLuv48_z <= 0)%Z
   | 27 => (-1 * s V_Luv24fromLuv48_z <= 0 /\ -1 * s V_Luv24fromLuv48__tmp + 1 <= 0)%Z
   | 28 => (-1 * s V_Luv24fromLuv48__tmp + 1 <= 0 /\ -1 * s V_Luv24fromLuv48_z <= 0)%Z
   | 29 => (-1 * s V_Luv24fromLuv48_z <= 0 /\ -1 * s V_Luv24fromLuv48__tmp + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_Luv24fromLuv48 (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(-1 + s V_Luv24fromLuv48_n) <= z)%Q
   | 2 => (s V_Luv24fromLuv48_z + max0(-1 + s V_Luv24fromLuv48_n) <= z)%Q
   | 3 => (s V_Luv24fromLuv48_z + max0(-1 + s V_Luv24fromLuv48__tmp) <= z)%Q
   | 4 => (s V_Luv24fromLuv48_z + max0(-1 + s V_Luv24fromLuv48__tmp) <= z)%Q
   | 5 => (s V_Luv24fromLuv48_z + max0(s V_Luv24fromLuv48__tmp) <= z)%Q
   | 6 => (s V_Luv24fromLuv48_z + max0(s V_Luv24fromLuv48__tmp) <= z)%Q
   | 7 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_Luv24fromLuv48__tmp) (-1
                                                                    + s V_Luv24fromLuv48__tmp));
      (*-1 0*) F_max0_ge_0 (-1 + s V_Luv24fromLuv48__tmp)]
     (s V_Luv24fromLuv48_z + max0(s V_Luv24fromLuv48__tmp) <= z)%Q
   | 8 => (s V_Luv24fromLuv48_z <= z)%Q
   | 9 => (s V_Luv24fromLuv48_z + max0(s V_Luv24fromLuv48__tmp) <= z)%Q
   | 10 => (s V_Luv24fromLuv48_z + max0(s V_Luv24fromLuv48__tmp) <= z)%Q
   | 11 => (s V_Luv24fromLuv48_z + max0(s V_Luv24fromLuv48__tmp) <= z)%Q
   | 12 => (s V_Luv24fromLuv48_z + max0(s V_Luv24fromLuv48__tmp) <= z)%Q
   | 13 => (s V_Luv24fromLuv48_z + max0(s V_Luv24fromLuv48__tmp) <= z)%Q
   | 14 => (s V_Luv24fromLuv48_z + max0(s V_Luv24fromLuv48__tmp) <= z)%Q
   | 15 => (s V_Luv24fromLuv48_z + max0(s V_Luv24fromLuv48__tmp) <= z)%Q
   | 16 => (s V_Luv24fromLuv48_z + max0(s V_Luv24fromLuv48__tmp) <= z)%Q
   | 17 => (s V_Luv24fromLuv48_z + max0(s V_Luv24fromLuv48__tmp) <= z)%Q
   | 18 => (s V_Luv24fromLuv48_z + max0(s V_Luv24fromLuv48__tmp) <= z)%Q
   | 19 => (s V_Luv24fromLuv48_z + max0(s V_Luv24fromLuv48__tmp) <= z)%Q
   | 20 => (s V_Luv24fromLuv48_z + max0(s V_Luv24fromLuv48__tmp) <= z)%Q
   | 21 => (s V_Luv24fromLuv48_z + max0(s V_Luv24fromLuv48__tmp) <= z)%Q
   | 22 => (s V_Luv24fromLuv48_z + max0(s V_Luv24fromLuv48__tmp) <= z)%Q
   | 23 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_Luv24fromLuv48__tmp) (1)]
     (s V_Luv24fromLuv48_z + max0(s V_Luv24fromLuv48__tmp) <= z)%Q
   | 24 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_Luv24fromLuv48__tmp) (1)]
     (s V_Luv24fromLuv48_z + max0(s V_Luv24fromLuv48__tmp) <= z)%Q
   | 25 => ((1 # 1) + s V_Luv24fromLuv48_z
            + max0(-1 + s V_Luv24fromLuv48__tmp) <= z)%Q
   | 26 => ((1 # 1) + s V_Luv24fromLuv48_z
            + max0(-1 + s V_Luv24fromLuv48__tmp) <= z)%Q
   | 27 => ((1 # 1) + s V_Luv24fromLuv48_z
            + max0(-1 + s V_Luv24fromLuv48__tmp) <= z)%Q
   | 28 => ((1 # 1) + s V_Luv24fromLuv48_z
            + max0(-1 + s V_Luv24fromLuv48__tmp) <= z)%Q
   | 29 => ((1 # 1) + s V_Luv24fromLuv48_z
            + max0(-1 + s V_Luv24fromLuv48__tmp) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_Luv24fromLuv48 =>
    [mkPA Q (fun n z s => ai_Luv24fromLuv48 n s /\ annot0_Luv24fromLuv48 n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_Luv24fromLuv48 (proc_start P_Luv24fromLuv48) s1 (proc_end P_Luv24fromLuv48) s2 ->
    (s2 V_Luv24fromLuv48_z <= max0(-1 + s1 V_Luv24fromLuv48_n))%Q.
Proof.
  prove_bound ipa admissible_ipa P_Luv24fromLuv48.
Qed.
