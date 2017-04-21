Require Import pasta.Pasta.

Inductive proc: Type :=
  P_ialloc_init.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_ialloc_init_z := 1%positive.
Notation V_ialloc_init__tmp := 2%positive.
Notation V_ialloc_init__tmp1 := 3%positive.
Notation V_ialloc_init_i := 4%positive.
Notation V_ialloc_init_chunk_size := 5%positive.
Notation V_ialloc_init_level2 := 6%positive.
Notation V_ialloc_init_mem := 7%positive.
Definition Pedges_ialloc_init: list (edge proc) :=
  (EA 1 (AAssign V_ialloc_init_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_ialloc_init_i) s) >= (eval (ENum (0)) s))%Z)) 3)::
  (EA 3 AWeaken 4)::(EA 4 (AAssign V_ialloc_init__tmp1
  (Some (EVar V_ialloc_init_chunk_size))) 5)::(EA 5 (AAssign
  V_ialloc_init__tmp (Some (EVar V_ialloc_init_level2))) 6)::
  (EA 6 AWeaken 7)::(EA 7 (AGuard (fun s => ((eval (EVar V_ialloc_init__tmp)
  s) <> (eval (ENum (0)) s))%Z)) 10)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_ialloc_init__tmp) s) = (eval (ENum (0))
  s))%Z)) 8)::(EA 8 AWeaken 9)::(EA 9 ANone 12)::(EA 10 AWeaken 11)::
  (EA 11 ANone 12)::(EA 12 (AAssign V_ialloc_init_i (Some (ENum (0)))) 13)::
  (EA 13 ANone 14)::(EA 14 AWeaken 15)::(EA 15 (AGuard
  (fun s => ((eval (EVar V_ialloc_init_i) s) < (eval (ENum (4)) s))%Z)) 18)::
  (EA 15 (AGuard (fun s => ((eval (EVar V_ialloc_init_i) s) >=
  (eval (ENum (4)) s))%Z)) 16)::(EA 16 AWeaken 17)::(EA 18 AWeaken 19)::
  (EA 19 ANone 20)::(EA 20 (AAssign V_ialloc_init_i
  (Some (EAdd (EVar V_ialloc_init_i) (ENum (1))))) 21)::(EA 21 ANone 22)::
  (EA 22 ANone 23)::(EA 23 (AAssign V_ialloc_init_z (Some (EAdd (ENum (1))
  (EVar V_ialloc_init_z)))) 24)::(EA 24 AWeaken 15)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_ialloc_init => Pedges_ialloc_init
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_ialloc_init => 17
     end)%positive;
  var_global := var_global
}.

Definition ai_ialloc_init (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_ialloc_init_z <= 0 /\ -1 * s V_ialloc_init_z <= 0)%Z
   | 3 => (-1 * s V_ialloc_init_z <= 0 /\ 1 * s V_ialloc_init_z <= 0 /\ -1 * s V_ialloc_init_i <= 0)%Z
   | 4 => (-1 * s V_ialloc_init_i <= 0 /\ 1 * s V_ialloc_init_z <= 0 /\ -1 * s V_ialloc_init_z <= 0)%Z
   | 5 => (-1 * s V_ialloc_init_z <= 0 /\ 1 * s V_ialloc_init_z <= 0 /\ -1 * s V_ialloc_init_i <= 0)%Z
   | 6 => (-1 * s V_ialloc_init_i <= 0 /\ 1 * s V_ialloc_init_z <= 0 /\ -1 * s V_ialloc_init_z <= 0)%Z
   | 7 => (-1 * s V_ialloc_init_z <= 0 /\ 1 * s V_ialloc_init_z <= 0 /\ -1 * s V_ialloc_init_i <= 0)%Z
   | 8 => (-1 * s V_ialloc_init_i <= 0 /\ 1 * s V_ialloc_init_z <= 0 /\ -1 * s V_ialloc_init_z <= 0 /\ 1 * s V_ialloc_init__tmp <= 0 /\ -1 * s V_ialloc_init__tmp <= 0)%Z
   | 9 => (-1 * s V_ialloc_init__tmp <= 0 /\ 1 * s V_ialloc_init__tmp <= 0 /\ -1 * s V_ialloc_init_z <= 0 /\ 1 * s V_ialloc_init_z <= 0 /\ -1 * s V_ialloc_init_i <= 0)%Z
   | 10 => (-1 * s V_ialloc_init_i <= 0 /\ 1 * s V_ialloc_init_z <= 0 /\ -1 * s V_ialloc_init_z <= 0)%Z
   | 11 => (-1 * s V_ialloc_init_z <= 0 /\ 1 * s V_ialloc_init_z <= 0 /\ -1 * s V_ialloc_init_i <= 0)%Z
   | 12 => (-1 * s V_ialloc_init_i <= 0 /\ 1 * s V_ialloc_init_z <= 0 /\ -1 * s V_ialloc_init_z <= 0)%Z
   | 13 => (-1 * s V_ialloc_init_z <= 0 /\ 1 * s V_ialloc_init_z <= 0 /\ 1 * s V_ialloc_init_i <= 0 /\ -1 * s V_ialloc_init_i <= 0)%Z
   | 14 => (-1 * s V_ialloc_init_i <= 0 /\ 1 * s V_ialloc_init_i <= 0 /\ 1 * s V_ialloc_init_z <= 0 /\ -1 * s V_ialloc_init_z <= 0)%Z
   | 15 => (-1 * s V_ialloc_init_z <= 0 /\ -1 * s V_ialloc_init_i <= 0 /\ 1 * s V_ialloc_init_i + -4 <= 0)%Z
   | 16 => (1 * s V_ialloc_init_i + -4 <= 0 /\ -1 * s V_ialloc_init_z <= 0 /\ -1 * s V_ialloc_init_i + 4 <= 0)%Z
   | 17 => (-1 * s V_ialloc_init_i + 4 <= 0 /\ -1 * s V_ialloc_init_z <= 0 /\ 1 * s V_ialloc_init_i + -4 <= 0)%Z
   | 18 => (-1 * s V_ialloc_init_i <= 0 /\ -1 * s V_ialloc_init_z <= 0 /\ 1 * s V_ialloc_init_i + -3 <= 0)%Z
   | 19 => (1 * s V_ialloc_init_i + -3 <= 0 /\ -1 * s V_ialloc_init_z <= 0 /\ -1 * s V_ialloc_init_i <= 0)%Z
   | 20 => (-1 * s V_ialloc_init_i <= 0 /\ -1 * s V_ialloc_init_z <= 0 /\ 1 * s V_ialloc_init_i + -3 <= 0)%Z
   | 21 => (-1 * s V_ialloc_init_z <= 0 /\ -1 * s V_ialloc_init_i + 1 <= 0 /\ 1 * s V_ialloc_init_i + -4 <= 0)%Z
   | 22 => (1 * s V_ialloc_init_i + -4 <= 0 /\ -1 * s V_ialloc_init_i + 1 <= 0 /\ -1 * s V_ialloc_init_z <= 0)%Z
   | 23 => (-1 * s V_ialloc_init_z <= 0 /\ -1 * s V_ialloc_init_i + 1 <= 0 /\ 1 * s V_ialloc_init_i + -4 <= 0)%Z
   | 24 => (1 * s V_ialloc_init_i + -4 <= 0 /\ -1 * s V_ialloc_init_i + 1 <= 0 /\ -1 * s V_ialloc_init_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_ialloc_init (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((4 # 1) <= z)%Q
   | 2 => ((4 # 1) + s V_ialloc_init_z <= z)%Q
   | 3 => ((4 # 1) + s V_ialloc_init_z <= z)%Q
   | 4 => ((4 # 1) + s V_ialloc_init_z <= z)%Q
   | 5 => ((4 # 1) + s V_ialloc_init_z <= z)%Q
   | 6 => ((4 # 1) + s V_ialloc_init_z <= z)%Q
   | 7 => ((4 # 1) + s V_ialloc_init_z <= z)%Q
   | 8 => ((4 # 1) + s V_ialloc_init_z <= z)%Q
   | 9 => ((4 # 1) + s V_ialloc_init_z <= z)%Q
   | 10 => ((4 # 1) + s V_ialloc_init_z <= z)%Q
   | 11 => ((4 # 1) + s V_ialloc_init_z <= z)%Q
   | 12 => ((4 # 1) + s V_ialloc_init_z <= z)%Q
   | 13 => (s V_ialloc_init_z + max0(4 - s V_ialloc_init_i) <= z)%Q
   | 14 => (s V_ialloc_init_z + max0(4 - s V_ialloc_init_i) <= z)%Q
   | 15 => (s V_ialloc_init_z + max0(4 - s V_ialloc_init_i) <= z)%Q
   | 16 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (4 - s V_ialloc_init_i) (3
                                                                    - 
                                                                    s V_ialloc_init_i));
      (*-1 0*) F_max0_ge_0 (3 - s V_ialloc_init_i)]
     (s V_ialloc_init_z + max0(4 - s V_ialloc_init_i) <= z)%Q
   | 17 => (s V_ialloc_init_z <= z)%Q
   | 18 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (4 - s V_ialloc_init_i) (1)]
     (s V_ialloc_init_z + max0(4 - s V_ialloc_init_i) <= z)%Q
   | 19 => ((1 # 1) + s V_ialloc_init_z + max0(3 - s V_ialloc_init_i) <= z)%Q
   | 20 => ((1 # 1) + s V_ialloc_init_z + max0(3 - s V_ialloc_init_i) <= z)%Q
   | 21 => ((1 # 1) + s V_ialloc_init_z + max0(4 - s V_ialloc_init_i) <= z)%Q
   | 22 => ((1 # 1) + s V_ialloc_init_z + max0(4 - s V_ialloc_init_i) <= z)%Q
   | 23 => ((1 # 1) + s V_ialloc_init_z + max0(4 - s V_ialloc_init_i) <= z)%Q
   | 24 => (s V_ialloc_init_z + max0(4 - s V_ialloc_init_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_ialloc_init =>
    [mkPA Q (fun n z s => ai_ialloc_init n s /\ annot0_ialloc_init n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_ialloc_init (proc_start P_ialloc_init) s1 (proc_end P_ialloc_init) s2 ->
    (s2 V_ialloc_init_z <= (4 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_ialloc_init.
Qed.
