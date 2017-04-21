Require Import pasta.Pasta.

Inductive proc: Type :=
  P_alloc_add_chunk.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_alloc_add_chunk_z := 1%positive.
Notation V_alloc_add_chunk__tmp := 2%positive.
Notation V_alloc_add_chunk__tmp1 := 3%positive.
Notation V_alloc_add_chunk_elt_size := 4%positive.
Notation V_alloc_add_chunk_num_elts := 5%positive.
Notation V_alloc_add_chunk_cname := 6%positive.
Notation V_alloc_add_chunk_csize := 7%positive.
Notation V_alloc_add_chunk_has_strings := 8%positive.
Notation V_alloc_add_chunk_mem := 9%positive.
Definition Pedges_alloc_add_chunk: list (edge proc) :=
  (EA 1 (AAssign V_alloc_add_chunk_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_alloc_add_chunk__tmp (Some (EVar V_alloc_add_chunk_csize))) 3)::
  (EA 3 (AAssign V_alloc_add_chunk__tmp1
  (Some (EVar V_alloc_add_chunk_has_strings))) 4)::(EA 4 (AAssign
  V_alloc_add_chunk_elt_size (Some (EVar V_alloc_add_chunk__tmp))) 5)::
  (EA 5 (AAssign V_alloc_add_chunk_num_elts (Some (ENum (1)))) 6)::
  (EA 6 AWeaken 7)::(EA 7 ANone 8)::(EA 7 ANone 15)::(EA 8 AWeaken 9)::
  (EA 9 ANone 33)::(EA 9 ANone 10)::(EA 10 AWeaken 11)::(EA 11 ANone 33)::
  (EA 11 ANone 12)::(EA 12 ANone 13)::(EA 13 ANone 14)::(EA 14 ANone 15)::
  (EA 15 ANone 16)::(EA 16 AWeaken 17)::(EA 17 (AGuard
  (fun s => ((eval (EVar V_alloc_add_chunk_elt_size) s) <>
  (eval (EVar V_alloc_add_chunk_elt_size) s))%Z)) 26)::(EA 17 (AGuard
  (fun s => ((eval (EVar V_alloc_add_chunk_elt_size) s) =
  (eval (EVar V_alloc_add_chunk_elt_size) s))%Z)) 18)::(EA 18 AWeaken 19)::
  (EA 19 ANone 24)::(EA 19 ANone 20)::(EA 20 AWeaken 21)::(EA 21 ANone 24)::
  (EA 21 ANone 22)::(EA 22 ANone 23)::(EA 23 AWeaken 35)::(EA 24 ANone 25)::
  (EA 25 AWeaken 35)::(EA 26 AWeaken 27)::(EA 27 (AAssign
  V_alloc_add_chunk_elt_size None) 28)::(EA 28 (AAssign
  V_alloc_add_chunk_num_elts None) 29)::(EA 29 ANone 30)::(EA 30 ANone 31)::
  (EA 31 (AAssign V_alloc_add_chunk_z (Some (EAdd (ENum (1))
  (EVar V_alloc_add_chunk_z)))) 32)::(EA 32 AWeaken 17)::(EA 33 ANone 34)::
  (EA 34 AWeaken 35)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_alloc_add_chunk => Pedges_alloc_add_chunk
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_alloc_add_chunk => 35
     end)%positive;
  var_global := var_global
}.

Definition ai_alloc_add_chunk (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_alloc_add_chunk_z <= 0 /\ -1 * s V_alloc_add_chunk_z <= 0)%Z
   | 3 => (-1 * s V_alloc_add_chunk_z <= 0 /\ 1 * s V_alloc_add_chunk_z <= 0)%Z
   | 4 => (1 * s V_alloc_add_chunk_z <= 0 /\ -1 * s V_alloc_add_chunk_z <= 0)%Z
   | 5 => (-1 * s V_alloc_add_chunk_z <= 0 /\ 1 * s V_alloc_add_chunk_z <= 0)%Z
   | 6 => (1 * s V_alloc_add_chunk_z <= 0 /\ -1 * s V_alloc_add_chunk_z <= 0 /\ 1 * s V_alloc_add_chunk_num_elts + -1 <= 0 /\ -1 * s V_alloc_add_chunk_num_elts + 1 <= 0)%Z
   | 7 => (-1 * s V_alloc_add_chunk_num_elts + 1 <= 0 /\ 1 * s V_alloc_add_chunk_num_elts + -1 <= 0 /\ -1 * s V_alloc_add_chunk_z <= 0 /\ 1 * s V_alloc_add_chunk_z <= 0)%Z
   | 8 => (1 * s V_alloc_add_chunk_z <= 0 /\ -1 * s V_alloc_add_chunk_z <= 0 /\ 1 * s V_alloc_add_chunk_num_elts + -1 <= 0 /\ -1 * s V_alloc_add_chunk_num_elts + 1 <= 0)%Z
   | 9 => (-1 * s V_alloc_add_chunk_num_elts + 1 <= 0 /\ 1 * s V_alloc_add_chunk_num_elts + -1 <= 0 /\ -1 * s V_alloc_add_chunk_z <= 0 /\ 1 * s V_alloc_add_chunk_z <= 0)%Z
   | 10 => (1 * s V_alloc_add_chunk_z <= 0 /\ -1 * s V_alloc_add_chunk_z <= 0 /\ 1 * s V_alloc_add_chunk_num_elts + -1 <= 0 /\ -1 * s V_alloc_add_chunk_num_elts + 1 <= 0)%Z
   | 11 => (-1 * s V_alloc_add_chunk_num_elts + 1 <= 0 /\ 1 * s V_alloc_add_chunk_num_elts + -1 <= 0 /\ -1 * s V_alloc_add_chunk_z <= 0 /\ 1 * s V_alloc_add_chunk_z <= 0)%Z
   | 12 => (1 * s V_alloc_add_chunk_z <= 0 /\ -1 * s V_alloc_add_chunk_z <= 0 /\ 1 * s V_alloc_add_chunk_num_elts + -1 <= 0 /\ -1 * s V_alloc_add_chunk_num_elts + 1 <= 0)%Z
   | 13 => (-1 * s V_alloc_add_chunk_num_elts + 1 <= 0 /\ 1 * s V_alloc_add_chunk_num_elts + -1 <= 0 /\ -1 * s V_alloc_add_chunk_z <= 0 /\ 1 * s V_alloc_add_chunk_z <= 0)%Z
   | 14 => (1 * s V_alloc_add_chunk_z <= 0 /\ -1 * s V_alloc_add_chunk_z <= 0 /\ 1 * s V_alloc_add_chunk_num_elts + -1 <= 0 /\ -1 * s V_alloc_add_chunk_num_elts + 1 <= 0)%Z
   | 15 => (-1 * s V_alloc_add_chunk_num_elts + 1 <= 0 /\ 1 * s V_alloc_add_chunk_num_elts + -1 <= 0 /\ -1 * s V_alloc_add_chunk_z <= 0 /\ 1 * s V_alloc_add_chunk_z <= 0)%Z
   | 16 => (1 * s V_alloc_add_chunk_z <= 0 /\ -1 * s V_alloc_add_chunk_z <= 0 /\ 1 * s V_alloc_add_chunk_num_elts + -1 <= 0 /\ -1 * s V_alloc_add_chunk_num_elts + 1 <= 0)%Z
   | 17 => (-1 * s V_alloc_add_chunk_num_elts + 1 <= 0 /\ 1 * s V_alloc_add_chunk_num_elts + -1 <= 0 /\ -1 * s V_alloc_add_chunk_z <= 0 /\ 1 * s V_alloc_add_chunk_z <= 0)%Z
   | 18 => (1 * s V_alloc_add_chunk_z <= 0 /\ -1 * s V_alloc_add_chunk_z <= 0 /\ 1 * s V_alloc_add_chunk_num_elts + -1 <= 0 /\ -1 * s V_alloc_add_chunk_num_elts + 1 <= 0)%Z
   | 19 => (-1 * s V_alloc_add_chunk_num_elts + 1 <= 0 /\ 1 * s V_alloc_add_chunk_num_elts + -1 <= 0 /\ -1 * s V_alloc_add_chunk_z <= 0 /\ 1 * s V_alloc_add_chunk_z <= 0)%Z
   | 20 => (1 * s V_alloc_add_chunk_z <= 0 /\ -1 * s V_alloc_add_chunk_z <= 0 /\ 1 * s V_alloc_add_chunk_num_elts + -1 <= 0 /\ -1 * s V_alloc_add_chunk_num_elts + 1 <= 0)%Z
   | 21 => (-1 * s V_alloc_add_chunk_num_elts + 1 <= 0 /\ 1 * s V_alloc_add_chunk_num_elts + -1 <= 0 /\ -1 * s V_alloc_add_chunk_z <= 0 /\ 1 * s V_alloc_add_chunk_z <= 0)%Z
   | 22 => (1 * s V_alloc_add_chunk_z <= 0 /\ -1 * s V_alloc_add_chunk_z <= 0 /\ 1 * s V_alloc_add_chunk_num_elts + -1 <= 0 /\ -1 * s V_alloc_add_chunk_num_elts + 1 <= 0)%Z
   | 23 => (-1 * s V_alloc_add_chunk_num_elts + 1 <= 0 /\ 1 * s V_alloc_add_chunk_num_elts + -1 <= 0 /\ -1 * s V_alloc_add_chunk_z <= 0 /\ 1 * s V_alloc_add_chunk_z <= 0)%Z
   | 24 => (1 * s V_alloc_add_chunk_z <= 0 /\ -1 * s V_alloc_add_chunk_z <= 0 /\ 1 * s V_alloc_add_chunk_num_elts + -1 <= 0 /\ -1 * s V_alloc_add_chunk_num_elts + 1 <= 0)%Z
   | 25 => (-1 * s V_alloc_add_chunk_num_elts + 1 <= 0 /\ 1 * s V_alloc_add_chunk_num_elts + -1 <= 0 /\ -1 * s V_alloc_add_chunk_z <= 0 /\ 1 * s V_alloc_add_chunk_z <= 0)%Z
   | 26 => (False)%Z
   | 27 => (False)%Z
   | 28 => (False)%Z
   | 29 => (False)%Z
   | 30 => (False)%Z
   | 31 => (False)%Z
   | 32 => (False)%Z
   | 33 => (1 * s V_alloc_add_chunk_z <= 0 /\ -1 * s V_alloc_add_chunk_z <= 0 /\ 1 * s V_alloc_add_chunk_num_elts + -1 <= 0 /\ -1 * s V_alloc_add_chunk_num_elts + 1 <= 0)%Z
   | 34 => (-1 * s V_alloc_add_chunk_num_elts + 1 <= 0 /\ 1 * s V_alloc_add_chunk_num_elts + -1 <= 0 /\ -1 * s V_alloc_add_chunk_z <= 0 /\ 1 * s V_alloc_add_chunk_z <= 0)%Z
   | 35 => (1 * s V_alloc_add_chunk_z <= 0 /\ -1 * s V_alloc_add_chunk_z <= 0 /\ 1 * s V_alloc_add_chunk_num_elts + -1 <= 0 /\ -1 * s V_alloc_add_chunk_num_elts + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_alloc_add_chunk (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (0 <= z)%Q
   | 2 => (s V_alloc_add_chunk_z <= z)%Q
   | 3 => (s V_alloc_add_chunk_z <= z)%Q
   | 4 => (s V_alloc_add_chunk_z <= z)%Q
   | 5 => (s V_alloc_add_chunk_z <= z)%Q
   | 6 => (s V_alloc_add_chunk_z <= z)%Q
   | 7 => (s V_alloc_add_chunk_z <= z)%Q
   | 8 => (s V_alloc_add_chunk_z <= z)%Q
   | 9 => (s V_alloc_add_chunk_z <= z)%Q
   | 10 => (s V_alloc_add_chunk_z <= z)%Q
   | 11 => (s V_alloc_add_chunk_z <= z)%Q
   | 12 => (s V_alloc_add_chunk_z <= z)%Q
   | 13 => (s V_alloc_add_chunk_z <= z)%Q
   | 14 => (s V_alloc_add_chunk_z <= z)%Q
   | 15 => (s V_alloc_add_chunk_z <= z)%Q
   | 16 => (s V_alloc_add_chunk_z <= z)%Q
   | 17 => (s V_alloc_add_chunk_z <= z)%Q
   | 18 => (s V_alloc_add_chunk_z <= z)%Q
   | 19 => (s V_alloc_add_chunk_z <= z)%Q
   | 20 => (s V_alloc_add_chunk_z <= z)%Q
   | 21 => (s V_alloc_add_chunk_z <= z)%Q
   | 22 => (s V_alloc_add_chunk_z <= z)%Q
   | 23 => (s V_alloc_add_chunk_z <= z)%Q
   | 24 => (s V_alloc_add_chunk_z <= z)%Q
   | 25 => (s V_alloc_add_chunk_z <= z)%Q
   | 26 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_alloc_add_chunk_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_alloc_add_chunk_z) (0))) (F_max0_ge_0 (s V_alloc_add_chunk_z))]
     (s V_alloc_add_chunk_z <= z)%Q
   | 27 => (0 <= z)%Q
   | 28 => (0 <= z)%Q
   | 29 => (0 <= z)%Q
   | 30 => (0 <= z)%Q
   | 31 => (0 <= z)%Q
   | 32 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_alloc_add_chunk_z)) (F_check_ge (0) (0));
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_alloc_add_chunk_z) (0))) (F_max0_ge_0 (-
                                                                    s V_alloc_add_chunk_z))]
     (0 <= z)%Q
   | 33 => (s V_alloc_add_chunk_z <= z)%Q
   | 34 => (s V_alloc_add_chunk_z <= z)%Q
   | 35 => (s V_alloc_add_chunk_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_alloc_add_chunk =>
    [mkPA Q (fun n z s => ai_alloc_add_chunk n s /\ annot0_alloc_add_chunk n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_alloc_add_chunk (proc_start P_alloc_add_chunk) s1 (proc_end P_alloc_add_chunk) s2 ->
    (s2 V_alloc_add_chunk_z <= 0)%Q.
Proof.
  prove_bound ipa admissible_ipa P_alloc_add_chunk.
Qed.
