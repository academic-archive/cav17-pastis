Require Import pasta.Pasta.

Inductive proc: Type :=
  P_jpeg_fdct_float.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_jpeg_fdct_float_z := 1%positive.
Notation V_jpeg_fdct_float_ctr := 2%positive.
Notation V_jpeg_fdct_float_data := 3%positive.
Definition Pedges_jpeg_fdct_float: list (edge proc) :=
  (EA 1 (AAssign V_jpeg_fdct_float_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_jpeg_fdct_float_ctr (Some (ENum (7)))) 3)::(EA 3 ANone 4)::
  (EA 4 AWeaken 5)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_jpeg_fdct_float_ctr) s) >= (eval (ENum (0))
  s))%Z)) 20)::(EA 5 (AGuard (fun s => ((eval (EVar V_jpeg_fdct_float_ctr)
  s) < (eval (ENum (0)) s))%Z)) 6)::(EA 6 AWeaken 7)::(EA 7 (AAssign
  V_jpeg_fdct_float_ctr (Some (ENum (7)))) 8)::(EA 8 ANone 9)::
  (EA 9 AWeaken 10)::(EA 10 (AGuard
  (fun s => ((eval (EVar V_jpeg_fdct_float_ctr) s) >= (eval (ENum (0))
  s))%Z)) 13)::(EA 10 (AGuard (fun s => ((eval (EVar V_jpeg_fdct_float_ctr)
  s) < (eval (ENum (0)) s))%Z)) 11)::(EA 11 AWeaken 12)::(EA 13 AWeaken 14)::
  (EA 14 ANone 15)::(EA 15 (AAssign V_jpeg_fdct_float_ctr
  (Some (EAdd (EVar V_jpeg_fdct_float_ctr) (ENum (-1))))) 16)::
  (EA 16 ANone 17)::(EA 17 ANone 18)::(EA 18 (AAssign V_jpeg_fdct_float_z
  (Some (EAdd (ENum (1)) (EVar V_jpeg_fdct_float_z)))) 19)::
  (EA 19 AWeaken 10)::(EA 20 AWeaken 21)::(EA 21 ANone 22)::(EA 22 (AAssign
  V_jpeg_fdct_float_ctr (Some (EAdd (EVar V_jpeg_fdct_float_ctr)
  (ENum (-1))))) 23)::(EA 23 ANone 24)::(EA 24 ANone 25)::(EA 25 (AAssign
  V_jpeg_fdct_float_z (Some (EAdd (ENum (1))
  (EVar V_jpeg_fdct_float_z)))) 26)::(EA 26 AWeaken 5)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_jpeg_fdct_float => Pedges_jpeg_fdct_float
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_jpeg_fdct_float => 12
     end)%positive;
  var_global := var_global
}.

Definition ai_jpeg_fdct_float (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_jpeg_fdct_float_z <= 0 /\ -1 * s V_jpeg_fdct_float_z <= 0)%Z
   | 3 => (-1 * s V_jpeg_fdct_float_z <= 0 /\ 1 * s V_jpeg_fdct_float_z <= 0 /\ 1 * s V_jpeg_fdct_float_ctr + -7 <= 0 /\ -1 * s V_jpeg_fdct_float_ctr + 7 <= 0)%Z
   | 4 => (-1 * s V_jpeg_fdct_float_ctr + 7 <= 0 /\ 1 * s V_jpeg_fdct_float_ctr + -7 <= 0 /\ 1 * s V_jpeg_fdct_float_z <= 0 /\ -1 * s V_jpeg_fdct_float_z <= 0)%Z
   | 5 => (-1 * s V_jpeg_fdct_float_z <= 0 /\ 1 * s V_jpeg_fdct_float_ctr + -7 <= 0 /\ -1 * s V_jpeg_fdct_float_ctr + -1 <= 0)%Z
   | 6 => (-1 * s V_jpeg_fdct_float_ctr + -1 <= 0 /\ -1 * s V_jpeg_fdct_float_z <= 0 /\ 1 * s V_jpeg_fdct_float_ctr + 1 <= 0)%Z
   | 7 => (1 * s V_jpeg_fdct_float_ctr + 1 <= 0 /\ -1 * s V_jpeg_fdct_float_z <= 0 /\ -1 * s V_jpeg_fdct_float_ctr + -1 <= 0)%Z
   | 8 => (-1 * s V_jpeg_fdct_float_z <= 0 /\ 1 * s V_jpeg_fdct_float_ctr + -7 <= 0 /\ -1 * s V_jpeg_fdct_float_ctr + 7 <= 0)%Z
   | 9 => (-1 * s V_jpeg_fdct_float_ctr + 7 <= 0 /\ 1 * s V_jpeg_fdct_float_ctr + -7 <= 0 /\ -1 * s V_jpeg_fdct_float_z <= 0)%Z
   | 10 => (-1 * s V_jpeg_fdct_float_z <= 0 /\ 1 * s V_jpeg_fdct_float_ctr + -7 <= 0 /\ -1 * s V_jpeg_fdct_float_ctr + -1 <= 0)%Z
   | 11 => (-1 * s V_jpeg_fdct_float_ctr + -1 <= 0 /\ -1 * s V_jpeg_fdct_float_z <= 0 /\ 1 * s V_jpeg_fdct_float_ctr + 1 <= 0)%Z
   | 12 => (1 * s V_jpeg_fdct_float_ctr + 1 <= 0 /\ -1 * s V_jpeg_fdct_float_z <= 0 /\ -1 * s V_jpeg_fdct_float_ctr + -1 <= 0)%Z
   | 13 => (1 * s V_jpeg_fdct_float_ctr + -7 <= 0 /\ -1 * s V_jpeg_fdct_float_z <= 0 /\ -1 * s V_jpeg_fdct_float_ctr <= 0)%Z
   | 14 => (-1 * s V_jpeg_fdct_float_ctr <= 0 /\ -1 * s V_jpeg_fdct_float_z <= 0 /\ 1 * s V_jpeg_fdct_float_ctr + -7 <= 0)%Z
   | 15 => (1 * s V_jpeg_fdct_float_ctr + -7 <= 0 /\ -1 * s V_jpeg_fdct_float_z <= 0 /\ -1 * s V_jpeg_fdct_float_ctr <= 0)%Z
   | 16 => (-1 * s V_jpeg_fdct_float_z <= 0 /\ 1 * s V_jpeg_fdct_float_ctr + -6 <= 0 /\ -1 * s V_jpeg_fdct_float_ctr + -1 <= 0)%Z
   | 17 => (-1 * s V_jpeg_fdct_float_ctr + -1 <= 0 /\ 1 * s V_jpeg_fdct_float_ctr + -6 <= 0 /\ -1 * s V_jpeg_fdct_float_z <= 0)%Z
   | 18 => (-1 * s V_jpeg_fdct_float_z <= 0 /\ 1 * s V_jpeg_fdct_float_ctr + -6 <= 0 /\ -1 * s V_jpeg_fdct_float_ctr + -1 <= 0)%Z
   | 19 => (-1 * s V_jpeg_fdct_float_ctr + -1 <= 0 /\ 1 * s V_jpeg_fdct_float_ctr + -6 <= 0 /\ -1 * s V_jpeg_fdct_float_z + 1 <= 0)%Z
   | 20 => (1 * s V_jpeg_fdct_float_ctr + -7 <= 0 /\ -1 * s V_jpeg_fdct_float_z <= 0 /\ -1 * s V_jpeg_fdct_float_ctr <= 0)%Z
   | 21 => (-1 * s V_jpeg_fdct_float_ctr <= 0 /\ -1 * s V_jpeg_fdct_float_z <= 0 /\ 1 * s V_jpeg_fdct_float_ctr + -7 <= 0)%Z
   | 22 => (1 * s V_jpeg_fdct_float_ctr + -7 <= 0 /\ -1 * s V_jpeg_fdct_float_z <= 0 /\ -1 * s V_jpeg_fdct_float_ctr <= 0)%Z
   | 23 => (-1 * s V_jpeg_fdct_float_z <= 0 /\ 1 * s V_jpeg_fdct_float_ctr + -6 <= 0 /\ -1 * s V_jpeg_fdct_float_ctr + -1 <= 0)%Z
   | 24 => (-1 * s V_jpeg_fdct_float_ctr + -1 <= 0 /\ 1 * s V_jpeg_fdct_float_ctr + -6 <= 0 /\ -1 * s V_jpeg_fdct_float_z <= 0)%Z
   | 25 => (-1 * s V_jpeg_fdct_float_z <= 0 /\ 1 * s V_jpeg_fdct_float_ctr + -6 <= 0 /\ -1 * s V_jpeg_fdct_float_ctr + -1 <= 0)%Z
   | 26 => (-1 * s V_jpeg_fdct_float_ctr + -1 <= 0 /\ 1 * s V_jpeg_fdct_float_ctr + -6 <= 0 /\ -1 * s V_jpeg_fdct_float_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_jpeg_fdct_float (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((16 # 1) <= z)%Q
   | 2 => ((16 # 1) + s V_jpeg_fdct_float_z <= z)%Q
   | 3 => ((8 # 1) + s V_jpeg_fdct_float_z
           + max0(1 + s V_jpeg_fdct_float_ctr) <= z)%Q
   | 4 => ((8 # 1) + s V_jpeg_fdct_float_z
           + max0(1 + s V_jpeg_fdct_float_ctr) <= z)%Q
   | 5 => ((8 # 1) + s V_jpeg_fdct_float_z
           + max0(1 + s V_jpeg_fdct_float_ctr) <= z)%Q
   | 6 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (1 + s V_jpeg_fdct_float_ctr) (s V_jpeg_fdct_float_ctr));
      (*-1 0*) F_max0_ge_0 (s V_jpeg_fdct_float_ctr)]
     ((8 # 1) + s V_jpeg_fdct_float_z + max0(1 + s V_jpeg_fdct_float_ctr) <= z)%Q
   | 7 => ((8 # 1) + s V_jpeg_fdct_float_z <= z)%Q
   | 8 => (s V_jpeg_fdct_float_z + max0(1 + s V_jpeg_fdct_float_ctr) <= z)%Q
   | 9 => (s V_jpeg_fdct_float_z + max0(1 + s V_jpeg_fdct_float_ctr) <= z)%Q
   | 10 => (s V_jpeg_fdct_float_z + max0(1 + s V_jpeg_fdct_float_ctr) <= z)%Q
   | 11 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (1 + s V_jpeg_fdct_float_ctr) (s V_jpeg_fdct_float_ctr));
      (*-1 0*) F_max0_ge_0 (s V_jpeg_fdct_float_ctr)]
     (s V_jpeg_fdct_float_z + max0(1 + s V_jpeg_fdct_float_ctr) <= z)%Q
   | 12 => (s V_jpeg_fdct_float_z <= z)%Q
   | 13 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (1 + s V_jpeg_fdct_float_ctr) (1)]
     (s V_jpeg_fdct_float_z + max0(1 + s V_jpeg_fdct_float_ctr) <= z)%Q
   | 14 => ((1 # 1) + s V_jpeg_fdct_float_z + max0(s V_jpeg_fdct_float_ctr) <= z)%Q
   | 15 => ((1 # 1) + s V_jpeg_fdct_float_z + max0(s V_jpeg_fdct_float_ctr) <= z)%Q
   | 16 => ((1 # 1) + s V_jpeg_fdct_float_z
            + max0(1 + s V_jpeg_fdct_float_ctr) <= z)%Q
   | 17 => ((1 # 1) + s V_jpeg_fdct_float_z
            + max0(1 + s V_jpeg_fdct_float_ctr) <= z)%Q
   | 18 => ((1 # 1) + s V_jpeg_fdct_float_z
            + max0(1 + s V_jpeg_fdct_float_ctr) <= z)%Q
   | 19 => (s V_jpeg_fdct_float_z + max0(1 + s V_jpeg_fdct_float_ctr) <= z)%Q
   | 20 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (1 + s V_jpeg_fdct_float_ctr) (1)]
     ((8 # 1) + s V_jpeg_fdct_float_z + max0(1 + s V_jpeg_fdct_float_ctr) <= z)%Q
   | 21 => ((9 # 1) + s V_jpeg_fdct_float_z + max0(s V_jpeg_fdct_float_ctr) <= z)%Q
   | 22 => ((9 # 1) + s V_jpeg_fdct_float_z + max0(s V_jpeg_fdct_float_ctr) <= z)%Q
   | 23 => ((9 # 1) + s V_jpeg_fdct_float_z
            + max0(1 + s V_jpeg_fdct_float_ctr) <= z)%Q
   | 24 => ((9 # 1) + s V_jpeg_fdct_float_z
            + max0(1 + s V_jpeg_fdct_float_ctr) <= z)%Q
   | 25 => ((9 # 1) + s V_jpeg_fdct_float_z
            + max0(1 + s V_jpeg_fdct_float_ctr) <= z)%Q
   | 26 => ((8 # 1) + s V_jpeg_fdct_float_z
            + max0(1 + s V_jpeg_fdct_float_ctr) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_jpeg_fdct_float =>
    [mkPA Q (fun n z s => ai_jpeg_fdct_float n s /\ annot0_jpeg_fdct_float n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_jpeg_fdct_float (proc_start P_jpeg_fdct_float) s1 (proc_end P_jpeg_fdct_float) s2 ->
    (s2 V_jpeg_fdct_float_z <= (16 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_jpeg_fdct_float.
Qed.
