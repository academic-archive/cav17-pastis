Require Import pasta.Pasta.

Inductive proc: Type :=
  P_jinit_2pass_quantizer.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_jinit_2pass_quantizer_z := 1%positive.
Notation V_jinit_2pass_quantizer_desired := 2%positive.
Notation V_jinit_2pass_quantizer_i := 3%positive.
Notation V_jinit_2pass_quantizer_cinfo := 4%positive.
Definition Pedges_jinit_2pass_quantizer: list (edge proc) :=
  (EA 1 (AAssign V_jinit_2pass_quantizer_z (Some (ENum (0)))) 2)::
  (EA 2 AWeaken 3)::(EA 3 ANone 4)::(EA 3 ANone 5)::(EA 4 ANone 5)::
  (EA 5 (AAssign V_jinit_2pass_quantizer_i (Some (ENum (0)))) 6)::
  (EA 6 ANone 7)::(EA 7 AWeaken 8)::(EA 8 (AGuard
  (fun s => ((eval (EVar V_jinit_2pass_quantizer_i) s) < (eval (ENum (32))
  s))%Z)) 35)::(EA 8 (AGuard
  (fun s => ((eval (EVar V_jinit_2pass_quantizer_i) s) >= (eval (ENum (32))
  s))%Z)) 9)::(EA 9 AWeaken 10)::(EA 10 ANone 13)::(EA 10 ANone 11)::
  (EA 11 ANone 12)::(EA 12 AWeaken 26)::(EA 13 (AAssign
  V_jinit_2pass_quantizer_desired None) 14)::(EA 14 AWeaken 15)::
  (EA 15 (AGuard (fun s => ((eval (EVar V_jinit_2pass_quantizer_desired) s) <
  (eval (ENum (8)) s))%Z)) 17)::(EA 15 (AGuard
  (fun s => ((eval (EVar V_jinit_2pass_quantizer_desired) s) >=
  (eval (ENum (8)) s))%Z)) 16)::(EA 16 AWeaken 20)::(EA 17 AWeaken 18)::
  (EA 18 ANone 19)::(EA 19 AWeaken 20)::(EA 20 (AGuard
  (fun s => ((eval (EVar V_jinit_2pass_quantizer_desired) s) >
  (eval (ENum (256)) s))%Z)) 22)::(EA 20 (AGuard
  (fun s => ((eval (EVar V_jinit_2pass_quantizer_desired) s) <=
  (eval (ENum (256)) s))%Z)) 21)::(EA 21 AWeaken 24)::(EA 22 AWeaken 23)::
  (EA 23 ANone 24)::(EA 24 ANone 25)::(EA 25 AWeaken 26)::(EA 26 ANone 28)::
  (EA 26 ANone 27)::(EA 27 AWeaken 30)::(EA 28 ANone 29)::
  (EA 29 AWeaken 30)::(EA 30 ANone 32)::(EA 30 ANone 31)::
  (EA 31 AWeaken 34)::(EA 32 ANone 33)::(EA 33 AWeaken 34)::
  (EA 35 AWeaken 36)::(EA 36 ANone 37)::(EA 37 (AAssign
  V_jinit_2pass_quantizer_i (Some (EAdd (EVar V_jinit_2pass_quantizer_i)
  (ENum (1))))) 38)::(EA 38 ANone 39)::(EA 39 ANone 40)::(EA 40 (AAssign
  V_jinit_2pass_quantizer_z (Some (EAdd (ENum (1))
  (EVar V_jinit_2pass_quantizer_z)))) 41)::(EA 41 AWeaken 8)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_jinit_2pass_quantizer => Pedges_jinit_2pass_quantizer
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_jinit_2pass_quantizer => 34
     end)%positive;
  var_global := var_global
}.

Definition ai_jinit_2pass_quantizer (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_jinit_2pass_quantizer_z <= 0 /\ -1 * s V_jinit_2pass_quantizer_z <= 0)%Z
   | 3 => (-1 * s V_jinit_2pass_quantizer_z <= 0 /\ 1 * s V_jinit_2pass_quantizer_z <= 0)%Z
   | 4 => (1 * s V_jinit_2pass_quantizer_z <= 0 /\ -1 * s V_jinit_2pass_quantizer_z <= 0)%Z
   | 5 => (-1 * s V_jinit_2pass_quantizer_z <= 0 /\ 1 * s V_jinit_2pass_quantizer_z <= 0)%Z
   | 6 => (1 * s V_jinit_2pass_quantizer_z <= 0 /\ -1 * s V_jinit_2pass_quantizer_z <= 0 /\ 1 * s V_jinit_2pass_quantizer_i <= 0 /\ -1 * s V_jinit_2pass_quantizer_i <= 0)%Z
   | 7 => (-1 * s V_jinit_2pass_quantizer_i <= 0 /\ 1 * s V_jinit_2pass_quantizer_i <= 0 /\ -1 * s V_jinit_2pass_quantizer_z <= 0 /\ 1 * s V_jinit_2pass_quantizer_z <= 0)%Z
   | 8 => (-1 * s V_jinit_2pass_quantizer_z <= 0 /\ -1 * s V_jinit_2pass_quantizer_i <= 0 /\ 1 * s V_jinit_2pass_quantizer_i + -32 <= 0)%Z
   | 9 => (1 * s V_jinit_2pass_quantizer_i + -32 <= 0 /\ -1 * s V_jinit_2pass_quantizer_z <= 0 /\ -1 * s V_jinit_2pass_quantizer_i + 32 <= 0)%Z
   | 10 => (-1 * s V_jinit_2pass_quantizer_i + 32 <= 0 /\ -1 * s V_jinit_2pass_quantizer_z <= 0 /\ 1 * s V_jinit_2pass_quantizer_i + -32 <= 0)%Z
   | 11 => (1 * s V_jinit_2pass_quantizer_i + -32 <= 0 /\ -1 * s V_jinit_2pass_quantizer_z <= 0 /\ -1 * s V_jinit_2pass_quantizer_i + 32 <= 0)%Z
   | 12 => (-1 * s V_jinit_2pass_quantizer_i + 32 <= 0 /\ -1 * s V_jinit_2pass_quantizer_z <= 0 /\ 1 * s V_jinit_2pass_quantizer_i + -32 <= 0)%Z
   | 13 => (1 * s V_jinit_2pass_quantizer_i + -32 <= 0 /\ -1 * s V_jinit_2pass_quantizer_z <= 0 /\ -1 * s V_jinit_2pass_quantizer_i + 32 <= 0)%Z
   | 14 => (-1 * s V_jinit_2pass_quantizer_i + 32 <= 0 /\ -1 * s V_jinit_2pass_quantizer_z <= 0 /\ 1 * s V_jinit_2pass_quantizer_i + -32 <= 0)%Z
   | 15 => (1 * s V_jinit_2pass_quantizer_i + -32 <= 0 /\ -1 * s V_jinit_2pass_quantizer_z <= 0 /\ -1 * s V_jinit_2pass_quantizer_i + 32 <= 0)%Z
   | 16 => (-1 * s V_jinit_2pass_quantizer_i + 32 <= 0 /\ -1 * s V_jinit_2pass_quantizer_z <= 0 /\ 1 * s V_jinit_2pass_quantizer_i + -32 <= 0 /\ -1 * s V_jinit_2pass_quantizer_desired + 8 <= 0)%Z
   | 17 => (-1 * s V_jinit_2pass_quantizer_i + 32 <= 0 /\ -1 * s V_jinit_2pass_quantizer_z <= 0 /\ 1 * s V_jinit_2pass_quantizer_i + -32 <= 0 /\ 1 * s V_jinit_2pass_quantizer_desired + -7 <= 0)%Z
   | 18 => (1 * s V_jinit_2pass_quantizer_desired + -7 <= 0 /\ 1 * s V_jinit_2pass_quantizer_i + -32 <= 0 /\ -1 * s V_jinit_2pass_quantizer_z <= 0 /\ -1 * s V_jinit_2pass_quantizer_i + 32 <= 0)%Z
   | 19 => (-1 * s V_jinit_2pass_quantizer_i + 32 <= 0 /\ -1 * s V_jinit_2pass_quantizer_z <= 0 /\ 1 * s V_jinit_2pass_quantizer_i + -32 <= 0 /\ 1 * s V_jinit_2pass_quantizer_desired + -7 <= 0)%Z
   | 20 => (1 * s V_jinit_2pass_quantizer_i + -32 <= 0 /\ -1 * s V_jinit_2pass_quantizer_z <= 0 /\ -1 * s V_jinit_2pass_quantizer_i + 32 <= 0)%Z
   | 21 => (-1 * s V_jinit_2pass_quantizer_i + 32 <= 0 /\ -1 * s V_jinit_2pass_quantizer_z <= 0 /\ 1 * s V_jinit_2pass_quantizer_i + -32 <= 0 /\ 1 * s V_jinit_2pass_quantizer_desired + -256 <= 0)%Z
   | 22 => (-1 * s V_jinit_2pass_quantizer_i + 32 <= 0 /\ -1 * s V_jinit_2pass_quantizer_z <= 0 /\ 1 * s V_jinit_2pass_quantizer_i + -32 <= 0 /\ -1 * s V_jinit_2pass_quantizer_desired + 257 <= 0)%Z
   | 23 => (-1 * s V_jinit_2pass_quantizer_desired + 257 <= 0 /\ 1 * s V_jinit_2pass_quantizer_i + -32 <= 0 /\ -1 * s V_jinit_2pass_quantizer_z <= 0 /\ -1 * s V_jinit_2pass_quantizer_i + 32 <= 0)%Z
   | 24 => (-1 * s V_jinit_2pass_quantizer_i + 32 <= 0 /\ -1 * s V_jinit_2pass_quantizer_z <= 0 /\ 1 * s V_jinit_2pass_quantizer_i + -32 <= 0)%Z
   | 25 => (1 * s V_jinit_2pass_quantizer_i + -32 <= 0 /\ -1 * s V_jinit_2pass_quantizer_z <= 0 /\ -1 * s V_jinit_2pass_quantizer_i + 32 <= 0)%Z
   | 26 => (-1 * s V_jinit_2pass_quantizer_i + 32 <= 0 /\ -1 * s V_jinit_2pass_quantizer_z <= 0 /\ 1 * s V_jinit_2pass_quantizer_i + -32 <= 0)%Z
   | 27 => (1 * s V_jinit_2pass_quantizer_i + -32 <= 0 /\ -1 * s V_jinit_2pass_quantizer_z <= 0 /\ -1 * s V_jinit_2pass_quantizer_i + 32 <= 0)%Z
   | 28 => (1 * s V_jinit_2pass_quantizer_i + -32 <= 0 /\ -1 * s V_jinit_2pass_quantizer_z <= 0 /\ -1 * s V_jinit_2pass_quantizer_i + 32 <= 0)%Z
   | 29 => (-1 * s V_jinit_2pass_quantizer_i + 32 <= 0 /\ -1 * s V_jinit_2pass_quantizer_z <= 0 /\ 1 * s V_jinit_2pass_quantizer_i + -32 <= 0)%Z
   | 30 => (1 * s V_jinit_2pass_quantizer_i + -32 <= 0 /\ -1 * s V_jinit_2pass_quantizer_z <= 0 /\ -1 * s V_jinit_2pass_quantizer_i + 32 <= 0)%Z
   | 31 => (-1 * s V_jinit_2pass_quantizer_i + 32 <= 0 /\ -1 * s V_jinit_2pass_quantizer_z <= 0 /\ 1 * s V_jinit_2pass_quantizer_i + -32 <= 0)%Z
   | 32 => (-1 * s V_jinit_2pass_quantizer_i + 32 <= 0 /\ -1 * s V_jinit_2pass_quantizer_z <= 0 /\ 1 * s V_jinit_2pass_quantizer_i + -32 <= 0)%Z
   | 33 => (1 * s V_jinit_2pass_quantizer_i + -32 <= 0 /\ -1 * s V_jinit_2pass_quantizer_z <= 0 /\ -1 * s V_jinit_2pass_quantizer_i + 32 <= 0)%Z
   | 34 => (-1 * s V_jinit_2pass_quantizer_i + 32 <= 0 /\ -1 * s V_jinit_2pass_quantizer_z <= 0 /\ 1 * s V_jinit_2pass_quantizer_i + -32 <= 0)%Z
   | 35 => (-1 * s V_jinit_2pass_quantizer_i <= 0 /\ -1 * s V_jinit_2pass_quantizer_z <= 0 /\ 1 * s V_jinit_2pass_quantizer_i + -31 <= 0)%Z
   | 36 => (1 * s V_jinit_2pass_quantizer_i + -31 <= 0 /\ -1 * s V_jinit_2pass_quantizer_z <= 0 /\ -1 * s V_jinit_2pass_quantizer_i <= 0)%Z
   | 37 => (-1 * s V_jinit_2pass_quantizer_i <= 0 /\ -1 * s V_jinit_2pass_quantizer_z <= 0 /\ 1 * s V_jinit_2pass_quantizer_i + -31 <= 0)%Z
   | 38 => (-1 * s V_jinit_2pass_quantizer_z <= 0 /\ -1 * s V_jinit_2pass_quantizer_i + 1 <= 0 /\ 1 * s V_jinit_2pass_quantizer_i + -32 <= 0)%Z
   | 39 => (1 * s V_jinit_2pass_quantizer_i + -32 <= 0 /\ -1 * s V_jinit_2pass_quantizer_i + 1 <= 0 /\ -1 * s V_jinit_2pass_quantizer_z <= 0)%Z
   | 40 => (-1 * s V_jinit_2pass_quantizer_z <= 0 /\ -1 * s V_jinit_2pass_quantizer_i + 1 <= 0 /\ 1 * s V_jinit_2pass_quantizer_i + -32 <= 0)%Z
   | 41 => (1 * s V_jinit_2pass_quantizer_i + -32 <= 0 /\ -1 * s V_jinit_2pass_quantizer_i + 1 <= 0 /\ -1 * s V_jinit_2pass_quantizer_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_jinit_2pass_quantizer (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((32 # 1) <= z)%Q
   | 2 => ((32 # 1) + s V_jinit_2pass_quantizer_z <= z)%Q
   | 3 => ((32 # 1) + s V_jinit_2pass_quantizer_z <= z)%Q
   | 4 => ((32 # 1) + s V_jinit_2pass_quantizer_z <= z)%Q
   | 5 => ((32 # 1) + s V_jinit_2pass_quantizer_z <= z)%Q
   | 6 => (s V_jinit_2pass_quantizer_z
           + max0(32 - s V_jinit_2pass_quantizer_i) <= z)%Q
   | 7 => (s V_jinit_2pass_quantizer_z
           + max0(32 - s V_jinit_2pass_quantizer_i) <= z)%Q
   | 8 => (s V_jinit_2pass_quantizer_z
           + max0(32 - s V_jinit_2pass_quantizer_i) <= z)%Q
   | 9 => hints
     [(*-1 0*) F_max0_ge_0 (32 - s V_jinit_2pass_quantizer_i)]
     (s V_jinit_2pass_quantizer_z + max0(32 - s V_jinit_2pass_quantizer_i) <= z)%Q
   | 10 => (s V_jinit_2pass_quantizer_z <= z)%Q
   | 11 => (s V_jinit_2pass_quantizer_z <= z)%Q
   | 12 => (s V_jinit_2pass_quantizer_z <= z)%Q
   | 13 => (s V_jinit_2pass_quantizer_z <= z)%Q
   | 14 => (s V_jinit_2pass_quantizer_z <= z)%Q
   | 15 => (s V_jinit_2pass_quantizer_z <= z)%Q
   | 16 => (s V_jinit_2pass_quantizer_z <= z)%Q
   | 17 => (s V_jinit_2pass_quantizer_z <= z)%Q
   | 18 => (s V_jinit_2pass_quantizer_z <= z)%Q
   | 19 => (s V_jinit_2pass_quantizer_z <= z)%Q
   | 20 => (s V_jinit_2pass_quantizer_z <= z)%Q
   | 21 => (s V_jinit_2pass_quantizer_z <= z)%Q
   | 22 => (s V_jinit_2pass_quantizer_z <= z)%Q
   | 23 => (s V_jinit_2pass_quantizer_z <= z)%Q
   | 24 => (s V_jinit_2pass_quantizer_z <= z)%Q
   | 25 => (s V_jinit_2pass_quantizer_z <= z)%Q
   | 26 => (s V_jinit_2pass_quantizer_z <= z)%Q
   | 27 => (s V_jinit_2pass_quantizer_z <= z)%Q
   | 28 => (s V_jinit_2pass_quantizer_z <= z)%Q
   | 29 => (s V_jinit_2pass_quantizer_z <= z)%Q
   | 30 => (s V_jinit_2pass_quantizer_z <= z)%Q
   | 31 => (s V_jinit_2pass_quantizer_z <= z)%Q
   | 32 => (s V_jinit_2pass_quantizer_z <= z)%Q
   | 33 => (s V_jinit_2pass_quantizer_z <= z)%Q
   | 34 => (s V_jinit_2pass_quantizer_z <= z)%Q
   | 35 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (32 - s V_jinit_2pass_quantizer_i) (1)]
     (s V_jinit_2pass_quantizer_z + max0(32 - s V_jinit_2pass_quantizer_i) <= z)%Q
   | 36 => ((1 # 1) + s V_jinit_2pass_quantizer_z
            + max0(31 - s V_jinit_2pass_quantizer_i) <= z)%Q
   | 37 => ((1 # 1) + s V_jinit_2pass_quantizer_z
            + max0(31 - s V_jinit_2pass_quantizer_i) <= z)%Q
   | 38 => ((1 # 1) + s V_jinit_2pass_quantizer_z
            + max0(32 - s V_jinit_2pass_quantizer_i) <= z)%Q
   | 39 => ((1 # 1) + s V_jinit_2pass_quantizer_z
            + max0(32 - s V_jinit_2pass_quantizer_i) <= z)%Q
   | 40 => ((1 # 1) + s V_jinit_2pass_quantizer_z
            + max0(32 - s V_jinit_2pass_quantizer_i) <= z)%Q
   | 41 => (s V_jinit_2pass_quantizer_z
            + max0(32 - s V_jinit_2pass_quantizer_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_jinit_2pass_quantizer =>
    [mkPA Q (fun n z s => ai_jinit_2pass_quantizer n s /\ annot0_jinit_2pass_quantizer n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_jinit_2pass_quantizer (proc_start P_jinit_2pass_quantizer) s1 (proc_end P_jinit_2pass_quantizer) s2 ->
    (s2 V_jinit_2pass_quantizer_z <= (32 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_jinit_2pass_quantizer.
Qed.
