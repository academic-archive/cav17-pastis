Require Import pasta.Pasta.

Inductive proc: Type :=
  P_fillrand.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_fillrand_z := 1%positive.
Notation V_fillrand__tmp := 2%positive.
Notation V_fillrand_fillrand_dot_count := 3%positive.
Notation V_fillrand_fillrand_dot_mt := 4%positive.
Notation V_fillrand_i := 5%positive.
Notation V_fillrand_buf := 6%positive.
Notation V_fillrand_len := 7%positive.
Definition Pedges_fillrand: list (edge proc) :=
  (EA 1 (AAssign V_fillrand_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_fillrand__tmp (Some (EVar V_fillrand_len))) 3)::(EA 3 AWeaken 4)::
  (EA 4 (AGuard (fun s => ((eval (EVar V_fillrand_fillrand_dot_mt) s) <>
  (eval (ENum (0)) s))%Z)) 6)::(EA 4 (AGuard
  (fun s => ((eval (EVar V_fillrand_fillrand_dot_mt) s) = (eval (ENum (0))
  s))%Z)) 5)::(EA 5 AWeaken 9)::(EA 6 AWeaken 7)::(EA 7 (AAssign
  V_fillrand_fillrand_dot_mt (Some (ENum (0)))) 8)::(EA 8 ANone 9)::
  (EA 9 (AAssign V_fillrand_i (Some (ENum (0)))) 10)::(EA 10 ANone 11)::
  (EA 11 AWeaken 12)::(EA 12 (AGuard (fun s => ((eval (EVar V_fillrand_i)
  s) < (eval (EVar V_fillrand__tmp) s))%Z)) 15)::(EA 12 (AGuard
  (fun s => ((eval (EVar V_fillrand_i) s) >= (eval (EVar V_fillrand__tmp)
  s))%Z)) 13)::(EA 13 AWeaken 14)::(EA 15 AWeaken 16)::(EA 16 (AGuard
  (fun s => ((eval (EVar V_fillrand_fillrand_dot_count) s) = (eval (ENum (4))
  s))%Z)) 18)::(EA 16 (AGuard
  (fun s => ((eval (EVar V_fillrand_fillrand_dot_count) s) <>
  (eval (ENum (4)) s))%Z)) 17)::(EA 17 AWeaken 21)::(EA 18 AWeaken 19)::
  (EA 19 (AAssign V_fillrand_fillrand_dot_count (Some (ENum (0)))) 20)::
  (EA 20 ANone 21)::(EA 21 (AAssign V_fillrand_fillrand_dot_count
  (Some (EAdd (EVar V_fillrand_fillrand_dot_count) (ENum (1))))) 22)::
  (EA 22 ANone 23)::(EA 23 (AAssign V_fillrand_i
  (Some (EAdd (EVar V_fillrand_i) (ENum (1))))) 24)::(EA 24 ANone 25)::
  (EA 25 ANone 26)::(EA 26 (AAssign V_fillrand_z (Some (EAdd (ENum (1))
  (EVar V_fillrand_z)))) 27)::(EA 27 AWeaken 12)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_fillrand => Pedges_fillrand
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_fillrand => 14
     end)%positive;
  var_global := var_global
}.

Definition ai_fillrand (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_fillrand_z <= 0 /\ -1 * s V_fillrand_z <= 0)%Z
   | 3 => (-1 * s V_fillrand_z <= 0 /\ 1 * s V_fillrand_z <= 0)%Z
   | 4 => (1 * s V_fillrand_z <= 0 /\ -1 * s V_fillrand_z <= 0)%Z
   | 5 => (-1 * s V_fillrand_z <= 0 /\ 1 * s V_fillrand_z <= 0 /\ 1 * s V_fillrand_fillrand_dot_mt <= 0 /\ -1 * s V_fillrand_fillrand_dot_mt <= 0)%Z
   | 6 => (-1 * s V_fillrand_z <= 0 /\ 1 * s V_fillrand_z <= 0)%Z
   | 7 => (1 * s V_fillrand_z <= 0 /\ -1 * s V_fillrand_z <= 0)%Z
   | 8 => (-1 * s V_fillrand_z <= 0 /\ 1 * s V_fillrand_z <= 0 /\ 1 * s V_fillrand_fillrand_dot_mt <= 0 /\ -1 * s V_fillrand_fillrand_dot_mt <= 0)%Z
   | 9 => (-1 * s V_fillrand_fillrand_dot_mt <= 0 /\ 1 * s V_fillrand_fillrand_dot_mt <= 0 /\ 1 * s V_fillrand_z <= 0 /\ -1 * s V_fillrand_z <= 0)%Z
   | 10 => (-1 * s V_fillrand_z <= 0 /\ 1 * s V_fillrand_z <= 0 /\ 1 * s V_fillrand_fillrand_dot_mt <= 0 /\ -1 * s V_fillrand_fillrand_dot_mt <= 0 /\ 1 * s V_fillrand_i <= 0 /\ -1 * s V_fillrand_i <= 0)%Z
   | 11 => (-1 * s V_fillrand_i <= 0 /\ 1 * s V_fillrand_i <= 0 /\ -1 * s V_fillrand_fillrand_dot_mt <= 0 /\ 1 * s V_fillrand_fillrand_dot_mt <= 0 /\ 1 * s V_fillrand_z <= 0 /\ -1 * s V_fillrand_z <= 0)%Z
   | 12 => (-1 * s V_fillrand_z <= 0 /\ -1 * s V_fillrand_i <= 0 /\ -1 * s V_fillrand_fillrand_dot_mt <= 0 /\ 1 * s V_fillrand_fillrand_dot_mt <= 0)%Z
   | 13 => (1 * s V_fillrand_fillrand_dot_mt <= 0 /\ -1 * s V_fillrand_fillrand_dot_mt <= 0 /\ -1 * s V_fillrand_i <= 0 /\ -1 * s V_fillrand_z <= 0 /\ 1 * s V_fillrand__tmp+ -1 * s V_fillrand_i <= 0)%Z
   | 14 => (1 * s V_fillrand__tmp+ -1 * s V_fillrand_i <= 0 /\ -1 * s V_fillrand_z <= 0 /\ -1 * s V_fillrand_i <= 0 /\ -1 * s V_fillrand_fillrand_dot_mt <= 0 /\ 1 * s V_fillrand_fillrand_dot_mt <= 0)%Z
   | 15 => (1 * s V_fillrand_fillrand_dot_mt <= 0 /\ -1 * s V_fillrand_fillrand_dot_mt <= 0 /\ -1 * s V_fillrand_i <= 0 /\ -1 * s V_fillrand_z <= 0 /\ -1 * s V_fillrand__tmp+ 1 * s V_fillrand_i + 1 <= 0)%Z
   | 16 => (-1 * s V_fillrand__tmp+ 1 * s V_fillrand_i + 1 <= 0 /\ -1 * s V_fillrand_z <= 0 /\ -1 * s V_fillrand_i <= 0 /\ -1 * s V_fillrand_fillrand_dot_mt <= 0 /\ 1 * s V_fillrand_fillrand_dot_mt <= 0)%Z
   | 17 => (1 * s V_fillrand_fillrand_dot_mt <= 0 /\ -1 * s V_fillrand_fillrand_dot_mt <= 0 /\ -1 * s V_fillrand_i <= 0 /\ -1 * s V_fillrand_z <= 0 /\ -1 * s V_fillrand__tmp+ 1 * s V_fillrand_i + 1 <= 0)%Z
   | 18 => (1 * s V_fillrand_fillrand_dot_mt <= 0 /\ -1 * s V_fillrand_fillrand_dot_mt <= 0 /\ -1 * s V_fillrand_i <= 0 /\ -1 * s V_fillrand_z <= 0 /\ -1 * s V_fillrand__tmp+ 1 * s V_fillrand_i + 1 <= 0 /\ 1 * s V_fillrand_fillrand_dot_count + -4 <= 0 /\ -1 * s V_fillrand_fillrand_dot_count + 4 <= 0)%Z
   | 19 => (-1 * s V_fillrand_fillrand_dot_count + 4 <= 0 /\ 1 * s V_fillrand_fillrand_dot_count + -4 <= 0 /\ -1 * s V_fillrand__tmp+ 1 * s V_fillrand_i + 1 <= 0 /\ -1 * s V_fillrand_z <= 0 /\ -1 * s V_fillrand_i <= 0 /\ -1 * s V_fillrand_fillrand_dot_mt <= 0 /\ 1 * s V_fillrand_fillrand_dot_mt <= 0)%Z
   | 20 => (1 * s V_fillrand_fillrand_dot_mt <= 0 /\ -1 * s V_fillrand_fillrand_dot_mt <= 0 /\ -1 * s V_fillrand_i <= 0 /\ -1 * s V_fillrand_z <= 0 /\ -1 * s V_fillrand__tmp+ 1 * s V_fillrand_i + 1 <= 0 /\ 1 * s V_fillrand_fillrand_dot_count <= 0 /\ -1 * s V_fillrand_fillrand_dot_count <= 0)%Z
   | 21 => (-1 * s V_fillrand__tmp+ 1 * s V_fillrand_i + 1 <= 0 /\ -1 * s V_fillrand_z <= 0 /\ -1 * s V_fillrand_i <= 0 /\ -1 * s V_fillrand_fillrand_dot_mt <= 0 /\ 1 * s V_fillrand_fillrand_dot_mt <= 0)%Z
   | 22 => (1 * s V_fillrand_fillrand_dot_mt <= 0 /\ -1 * s V_fillrand_fillrand_dot_mt <= 0 /\ -1 * s V_fillrand_i <= 0 /\ -1 * s V_fillrand_z <= 0 /\ -1 * s V_fillrand__tmp+ 1 * s V_fillrand_i + 1 <= 0)%Z
   | 23 => (-1 * s V_fillrand__tmp+ 1 * s V_fillrand_i + 1 <= 0 /\ -1 * s V_fillrand_z <= 0 /\ -1 * s V_fillrand_i <= 0 /\ -1 * s V_fillrand_fillrand_dot_mt <= 0 /\ 1 * s V_fillrand_fillrand_dot_mt <= 0)%Z
   | 24 => (1 * s V_fillrand_fillrand_dot_mt <= 0 /\ -1 * s V_fillrand_fillrand_dot_mt <= 0 /\ -1 * s V_fillrand_z <= 0 /\ -1 * s V_fillrand__tmp+ 1 * s V_fillrand_i <= 0 /\ -1 * s V_fillrand_i + 1 <= 0)%Z
   | 25 => (-1 * s V_fillrand_i + 1 <= 0 /\ -1 * s V_fillrand__tmp+ 1 * s V_fillrand_i <= 0 /\ -1 * s V_fillrand_z <= 0 /\ -1 * s V_fillrand_fillrand_dot_mt <= 0 /\ 1 * s V_fillrand_fillrand_dot_mt <= 0)%Z
   | 26 => (1 * s V_fillrand_fillrand_dot_mt <= 0 /\ -1 * s V_fillrand_fillrand_dot_mt <= 0 /\ -1 * s V_fillrand_z <= 0 /\ -1 * s V_fillrand__tmp+ 1 * s V_fillrand_i <= 0 /\ -1 * s V_fillrand_i + 1 <= 0)%Z
   | 27 => (-1 * s V_fillrand_i + 1 <= 0 /\ -1 * s V_fillrand__tmp+ 1 * s V_fillrand_i <= 0 /\ -1 * s V_fillrand_fillrand_dot_mt <= 0 /\ 1 * s V_fillrand_fillrand_dot_mt <= 0 /\ -1 * s V_fillrand_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_fillrand (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_fillrand_len) <= z)%Q
   | 2 => (s V_fillrand_z + max0(s V_fillrand_len) <= z)%Q
   | 3 => (s V_fillrand_z + max0(s V_fillrand__tmp) <= z)%Q
   | 4 => (s V_fillrand_z + max0(s V_fillrand__tmp) <= z)%Q
   | 5 => (s V_fillrand_z + max0(s V_fillrand__tmp) <= z)%Q
   | 6 => (s V_fillrand_z + max0(s V_fillrand__tmp) <= z)%Q
   | 7 => (s V_fillrand_z + max0(s V_fillrand__tmp) <= z)%Q
   | 8 => (s V_fillrand_z + max0(s V_fillrand__tmp) <= z)%Q
   | 9 => (s V_fillrand_z + max0(s V_fillrand__tmp) <= z)%Q
   | 10 => (s V_fillrand_z + max0(s V_fillrand__tmp - s V_fillrand_i) <= z)%Q
   | 11 => (s V_fillrand_z + max0(s V_fillrand__tmp - s V_fillrand_i) <= z)%Q
   | 12 => (s V_fillrand_z + max0(s V_fillrand__tmp - s V_fillrand_i) <= z)%Q
   | 13 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_fillrand__tmp
                                             - s V_fillrand_i) (-1
                                                                + s V_fillrand__tmp
                                                                - s V_fillrand_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V_fillrand__tmp - s V_fillrand_i)]
     (s V_fillrand_z + max0(s V_fillrand__tmp - s V_fillrand_i) <= z)%Q
   | 14 => (s V_fillrand_z <= z)%Q
   | 15 => (s V_fillrand_z + max0(s V_fillrand__tmp - s V_fillrand_i) <= z)%Q
   | 16 => (s V_fillrand_z + max0(s V_fillrand__tmp - s V_fillrand_i) <= z)%Q
   | 17 => hints
     [(*0 1*) F_max0_pre_decrement 1 (s V_fillrand__tmp - s V_fillrand_i) (1)]
     (s V_fillrand_z + max0(s V_fillrand__tmp - s V_fillrand_i) <= z)%Q
   | 18 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_fillrand__tmp - s V_fillrand_i) (1)]
     (s V_fillrand_z + max0(s V_fillrand__tmp - s V_fillrand_i) <= z)%Q
   | 19 => ((1 # 1) + s V_fillrand_z
            + max0(-1 + s V_fillrand__tmp - s V_fillrand_i) <= z)%Q
   | 20 => ((1 # 1) + s V_fillrand_z
            + max0(-1 + s V_fillrand__tmp - s V_fillrand_i) <= z)%Q
   | 21 => ((1 # 1) + s V_fillrand_z
            + max0(-1 + s V_fillrand__tmp - s V_fillrand_i) <= z)%Q
   | 22 => ((1 # 1) + s V_fillrand_z
            + max0(-1 + s V_fillrand__tmp - s V_fillrand_i) <= z)%Q
   | 23 => ((1 # 1) + s V_fillrand_z
            + max0(-1 + s V_fillrand__tmp - s V_fillrand_i) <= z)%Q
   | 24 => ((1 # 1) + s V_fillrand_z
            + max0(s V_fillrand__tmp - s V_fillrand_i) <= z)%Q
   | 25 => ((1 # 1) + s V_fillrand_z
            + max0(s V_fillrand__tmp - s V_fillrand_i) <= z)%Q
   | 26 => ((1 # 1) + s V_fillrand_z
            + max0(s V_fillrand__tmp - s V_fillrand_i) <= z)%Q
   | 27 => (s V_fillrand_z + max0(s V_fillrand__tmp - s V_fillrand_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_fillrand =>
    [mkPA Q (fun n z s => ai_fillrand n s /\ annot0_fillrand n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_fillrand (proc_start P_fillrand) s1 (proc_end P_fillrand) s2 ->
    (s2 V_fillrand_z <= max0(s1 V_fillrand_len))%Q.
Proof.
  prove_bound ipa admissible_ipa P_fillrand.
Qed.
