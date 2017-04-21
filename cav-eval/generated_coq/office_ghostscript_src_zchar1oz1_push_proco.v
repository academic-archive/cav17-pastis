Require Import pasta.Pasta.

Inductive proc: Type :=
  P_z1_push_proc.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_z1_push_proc_z := 1%positive.
Notation V_z1_push_proc__tmp := 2%positive.
Notation V_z1_push_proc__tmp1 := 3%positive.
Notation V_z1_push_proc_i := 4%positive.
Notation V_z1_push_proc_count := 5%positive.
Notation V_z1_push_proc_ignore := 6%positive.
Notation V_z1_push_proc_pf := 7%positive.
Definition Pedges_z1_push_proc: list (edge proc) :=
  (EA 1 (AAssign V_z1_push_proc_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_z1_push_proc__tmp (Some (EVar V_z1_push_proc_count))) 3)::
  (EA 3 AWeaken 4)::(EA 4 ANone 20)::(EA 4 ANone 5)::(EA 5 (AAssign
  V_z1_push_proc_i (Some (ENum (0)))) 6)::(EA 6 ANone 7)::(EA 7 AWeaken 8)::
  (EA 8 (AGuard (fun s => ((eval (EVar V_z1_push_proc_i) s) <
  (eval (EVar V_z1_push_proc__tmp) s))%Z)) 13)::(EA 8 (AGuard
  (fun s => ((eval (EVar V_z1_push_proc_i) s) >=
  (eval (EVar V_z1_push_proc__tmp) s))%Z)) 9)::(EA 9 AWeaken 10)::
  (EA 10 (AAssign V_z1_push_proc__tmp1 (Some (ENum (0)))) 11)::
  (EA 11 ANone 12)::(EA 12 AWeaken 23)::(EA 13 AWeaken 14)::
  (EA 14 ANone 15)::(EA 15 (AAssign V_z1_push_proc_i
  (Some (EAdd (EVar V_z1_push_proc_i) (ENum (1))))) 16)::(EA 16 ANone 17)::
  (EA 17 ANone 18)::(EA 18 (AAssign V_z1_push_proc_z (Some (EAdd (ENum (1))
  (EVar V_z1_push_proc_z)))) 19)::(EA 19 AWeaken 8)::(EA 20 (AAssign
  V_z1_push_proc__tmp1 (Some (ENum (-16)))) 21)::(EA 21 ANone 22)::
  (EA 22 AWeaken 23)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_z1_push_proc => Pedges_z1_push_proc
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_z1_push_proc => 23
     end)%positive;
  var_global := var_global
}.

Definition ai_z1_push_proc (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_z1_push_proc_z <= 0 /\ -1 * s V_z1_push_proc_z <= 0)%Z
   | 3 => (-1 * s V_z1_push_proc_z <= 0 /\ 1 * s V_z1_push_proc_z <= 0)%Z
   | 4 => (1 * s V_z1_push_proc_z <= 0 /\ -1 * s V_z1_push_proc_z <= 0)%Z
   | 5 => (-1 * s V_z1_push_proc_z <= 0 /\ 1 * s V_z1_push_proc_z <= 0)%Z
   | 6 => (1 * s V_z1_push_proc_z <= 0 /\ -1 * s V_z1_push_proc_z <= 0 /\ 1 * s V_z1_push_proc_i <= 0 /\ -1 * s V_z1_push_proc_i <= 0)%Z
   | 7 => (-1 * s V_z1_push_proc_i <= 0 /\ 1 * s V_z1_push_proc_i <= 0 /\ -1 * s V_z1_push_proc_z <= 0 /\ 1 * s V_z1_push_proc_z <= 0)%Z
   | 8 => (-1 * s V_z1_push_proc_z <= 0 /\ -1 * s V_z1_push_proc_i <= 0)%Z
   | 9 => (-1 * s V_z1_push_proc_i <= 0 /\ -1 * s V_z1_push_proc_z <= 0 /\ 1 * s V_z1_push_proc__tmp+ -1 * s V_z1_push_proc_i <= 0)%Z
   | 10 => (1 * s V_z1_push_proc__tmp+ -1 * s V_z1_push_proc_i <= 0 /\ -1 * s V_z1_push_proc_z <= 0 /\ -1 * s V_z1_push_proc_i <= 0)%Z
   | 11 => (-1 * s V_z1_push_proc_i <= 0 /\ -1 * s V_z1_push_proc_z <= 0 /\ 1 * s V_z1_push_proc__tmp+ -1 * s V_z1_push_proc_i <= 0 /\ 1 * s V_z1_push_proc__tmp1 <= 0 /\ -1 * s V_z1_push_proc__tmp1 <= 0)%Z
   | 12 => (-1 * s V_z1_push_proc__tmp1 <= 0 /\ 1 * s V_z1_push_proc__tmp1 <= 0 /\ 1 * s V_z1_push_proc__tmp+ -1 * s V_z1_push_proc_i <= 0 /\ -1 * s V_z1_push_proc_z <= 0 /\ -1 * s V_z1_push_proc_i <= 0)%Z
   | 13 => (-1 * s V_z1_push_proc_i <= 0 /\ -1 * s V_z1_push_proc_z <= 0 /\ -1 * s V_z1_push_proc__tmp+ 1 * s V_z1_push_proc_i + 1 <= 0)%Z
   | 14 => (-1 * s V_z1_push_proc__tmp+ 1 * s V_z1_push_proc_i + 1 <= 0 /\ -1 * s V_z1_push_proc_z <= 0 /\ -1 * s V_z1_push_proc_i <= 0)%Z
   | 15 => (-1 * s V_z1_push_proc_i <= 0 /\ -1 * s V_z1_push_proc_z <= 0 /\ -1 * s V_z1_push_proc__tmp+ 1 * s V_z1_push_proc_i + 1 <= 0)%Z
   | 16 => (-1 * s V_z1_push_proc_z <= 0 /\ -1 * s V_z1_push_proc_i + 1 <= 0 /\ -1 * s V_z1_push_proc__tmp+ 1 * s V_z1_push_proc_i <= 0)%Z
   | 17 => (-1 * s V_z1_push_proc__tmp+ 1 * s V_z1_push_proc_i <= 0 /\ -1 * s V_z1_push_proc_i + 1 <= 0 /\ -1 * s V_z1_push_proc_z <= 0)%Z
   | 18 => (-1 * s V_z1_push_proc_z <= 0 /\ -1 * s V_z1_push_proc_i + 1 <= 0 /\ -1 * s V_z1_push_proc__tmp+ 1 * s V_z1_push_proc_i <= 0)%Z
   | 19 => (-1 * s V_z1_push_proc__tmp+ 1 * s V_z1_push_proc_i <= 0 /\ -1 * s V_z1_push_proc_i + 1 <= 0 /\ -1 * s V_z1_push_proc_z + 1 <= 0)%Z
   | 20 => (-1 * s V_z1_push_proc_z <= 0 /\ 1 * s V_z1_push_proc_z <= 0)%Z
   | 21 => (1 * s V_z1_push_proc_z <= 0 /\ -1 * s V_z1_push_proc_z <= 0 /\ 1 * s V_z1_push_proc__tmp1 + 16 <= 0 /\ -1 * s V_z1_push_proc__tmp1 + -16 <= 0)%Z
   | 22 => (-1 * s V_z1_push_proc__tmp1 + -16 <= 0 /\ 1 * s V_z1_push_proc__tmp1 + 16 <= 0 /\ -1 * s V_z1_push_proc_z <= 0 /\ 1 * s V_z1_push_proc_z <= 0)%Z
   | 23 => (1 * s V_z1_push_proc__tmp1 <= 0 /\ -1 * s V_z1_push_proc_z <= 0 /\ -1 * s V_z1_push_proc__tmp1 + -16 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_z1_push_proc (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_z1_push_proc_count) <= z)%Q
   | 2 => (s V_z1_push_proc_z + max0(s V_z1_push_proc_count) <= z)%Q
   | 3 => (s V_z1_push_proc_z + max0(s V_z1_push_proc__tmp) <= z)%Q
   | 4 => (s V_z1_push_proc_z + max0(s V_z1_push_proc__tmp) <= z)%Q
   | 5 => (s V_z1_push_proc_z + max0(s V_z1_push_proc__tmp) <= z)%Q
   | 6 => (s V_z1_push_proc_z
           + max0(s V_z1_push_proc__tmp - s V_z1_push_proc_i) <= z)%Q
   | 7 => (s V_z1_push_proc_z
           + max0(s V_z1_push_proc__tmp - s V_z1_push_proc_i) <= z)%Q
   | 8 => (s V_z1_push_proc_z
           + max0(s V_z1_push_proc__tmp - s V_z1_push_proc_i) <= z)%Q
   | 9 => (s V_z1_push_proc_z
           + max0(s V_z1_push_proc__tmp - s V_z1_push_proc_i) <= z)%Q
   | 10 => (s V_z1_push_proc_z
            + max0(s V_z1_push_proc__tmp - s V_z1_push_proc_i) <= z)%Q
   | 11 => (s V_z1_push_proc_z
            + max0(s V_z1_push_proc__tmp - s V_z1_push_proc_i) <= z)%Q
   | 12 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_z1_push_proc__tmp
                                             - s V_z1_push_proc_i) (-1
                                                                    + 
                                                                    s V_z1_push_proc__tmp
                                                                    - 
                                                                    s V_z1_push_proc_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V_z1_push_proc__tmp - s V_z1_push_proc_i)]
     (s V_z1_push_proc_z + max0(s V_z1_push_proc__tmp - s V_z1_push_proc_i) <= z)%Q
   | 13 => hints
     [(*0 1*) F_max0_pre_decrement 1 (s V_z1_push_proc__tmp
                                      - s V_z1_push_proc_i) (1)]
     (s V_z1_push_proc_z + max0(s V_z1_push_proc__tmp - s V_z1_push_proc_i) <= z)%Q
   | 14 => ((1 # 1) + s V_z1_push_proc_z
            + max0(-1 + s V_z1_push_proc__tmp - s V_z1_push_proc_i) <= z)%Q
   | 15 => ((1 # 1) + s V_z1_push_proc_z
            + max0(-1 + s V_z1_push_proc__tmp - s V_z1_push_proc_i) <= z)%Q
   | 16 => ((1 # 1) + s V_z1_push_proc_z
            + max0(s V_z1_push_proc__tmp - s V_z1_push_proc_i) <= z)%Q
   | 17 => ((1 # 1) + s V_z1_push_proc_z
            + max0(s V_z1_push_proc__tmp - s V_z1_push_proc_i) <= z)%Q
   | 18 => ((1 # 1) + s V_z1_push_proc_z
            + max0(s V_z1_push_proc__tmp - s V_z1_push_proc_i) <= z)%Q
   | 19 => (s V_z1_push_proc_z
            + max0(s V_z1_push_proc__tmp - s V_z1_push_proc_i) <= z)%Q
   | 20 => (s V_z1_push_proc_z + max0(s V_z1_push_proc__tmp) <= z)%Q
   | 21 => (s V_z1_push_proc_z + max0(s V_z1_push_proc__tmp) <= z)%Q
   | 22 => hints
     [(*-1 0*) F_max0_ge_0 (s V_z1_push_proc__tmp)]
     (s V_z1_push_proc_z + max0(s V_z1_push_proc__tmp) <= z)%Q
   | 23 => (s V_z1_push_proc_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_z1_push_proc =>
    [mkPA Q (fun n z s => ai_z1_push_proc n s /\ annot0_z1_push_proc n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_z1_push_proc (proc_start P_z1_push_proc) s1 (proc_end P_z1_push_proc) s2 ->
    (s2 V_z1_push_proc_z <= max0(s1 V_z1_push_proc_count))%Q.
Proof.
  prove_bound ipa admissible_ipa P_z1_push_proc.
Qed.
