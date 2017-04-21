Require Import pasta.Pasta.

Inductive proc: Type :=
  P_gen_codes.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_gen_codes_z := 1%positive.
Notation V_gen_codes__tmp := 2%positive.
Notation V_gen_codes_bits := 3%positive.
Notation V_gen_codes_code := 4%positive.
Notation V_gen_codes_len := 5%positive.
Notation V_gen_codes_n := 6%positive.
Notation V_gen_codes_max_code := 7%positive.
Notation V_gen_codes_tree := 8%positive.
Definition Pedges_gen_codes: list (edge proc) :=
  (EA 1 (AAssign V_gen_codes_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_gen_codes__tmp (Some (EVar V_gen_codes_max_code))) 3)::(EA 3 (AAssign
  V_gen_codes_code (Some (ENum (0)))) 4)::(EA 4 (AAssign V_gen_codes_bits
  (Some (ENum (1)))) 5)::(EA 5 ANone 6)::(EA 6 AWeaken 7)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_gen_codes_bits) s) <= (eval (ENum (15))
  s))%Z)) 28)::(EA 7 (AGuard (fun s => ((eval (EVar V_gen_codes_bits) s) >
  (eval (ENum (15)) s))%Z)) 8)::(EA 8 AWeaken 9)::(EA 9 (AAssign
  V_gen_codes_n (Some (ENum (0)))) 10)::(EA 10 ANone 11)::
  (EA 11 AWeaken 12)::(EA 12 (AGuard (fun s => ((eval (EVar V_gen_codes_n)
  s) <= (eval (EVar V_gen_codes__tmp) s))%Z)) 15)::(EA 12 (AGuard
  (fun s => ((eval (EVar V_gen_codes_n) s) > (eval (EVar V_gen_codes__tmp)
  s))%Z)) 13)::(EA 13 AWeaken 14)::(EA 15 AWeaken 16)::(EA 16 (AAssign
  V_gen_codes_len None) 17)::(EA 17 AWeaken 18)::(EA 18 (AGuard
  (fun s => ((eval (EVar V_gen_codes_len) s) = (eval (ENum (0)) s))%Z)) 21)::
  (EA 18 (AGuard (fun s => ((eval (EVar V_gen_codes_len) s) <>
  (eval (ENum (0)) s))%Z)) 19)::(EA 19 AWeaken 20)::(EA 20 ANone 23)::
  (EA 21 AWeaken 22)::(EA 22 ANone 23)::(EA 23 (AAssign V_gen_codes_n
  (Some (EAdd (EVar V_gen_codes_n) (ENum (1))))) 24)::(EA 24 ANone 25)::
  (EA 25 ANone 26)::(EA 26 (AAssign V_gen_codes_z (Some (EAdd (ENum (1))
  (EVar V_gen_codes_z)))) 27)::(EA 27 AWeaken 12)::(EA 28 AWeaken 29)::
  (EA 29 (AAssign V_gen_codes_code None) 30)::(EA 30 ANone 31)::
  (EA 31 (AAssign V_gen_codes_bits (Some (EAdd (EVar V_gen_codes_bits)
  (ENum (1))))) 32)::(EA 32 ANone 33)::(EA 33 ANone 34)::(EA 34 (AAssign
  V_gen_codes_z (Some (EAdd (ENum (1)) (EVar V_gen_codes_z)))) 35)::
  (EA 35 AWeaken 7)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_gen_codes => Pedges_gen_codes
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_gen_codes => 14
     end)%positive;
  var_global := var_global
}.

Definition ai_gen_codes (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_gen_codes_z <= 0 /\ -1 * s V_gen_codes_z <= 0)%Z
   | 3 => (-1 * s V_gen_codes_z <= 0 /\ 1 * s V_gen_codes_z <= 0)%Z
   | 4 => (1 * s V_gen_codes_z <= 0 /\ -1 * s V_gen_codes_z <= 0 /\ 1 * s V_gen_codes_code <= 0 /\ -1 * s V_gen_codes_code <= 0)%Z
   | 5 => (-1 * s V_gen_codes_code <= 0 /\ 1 * s V_gen_codes_code <= 0 /\ -1 * s V_gen_codes_z <= 0 /\ 1 * s V_gen_codes_z <= 0 /\ 1 * s V_gen_codes_bits + -1 <= 0 /\ -1 * s V_gen_codes_bits + 1 <= 0)%Z
   | 6 => (-1 * s V_gen_codes_bits + 1 <= 0 /\ 1 * s V_gen_codes_bits + -1 <= 0 /\ 1 * s V_gen_codes_z <= 0 /\ -1 * s V_gen_codes_z <= 0 /\ 1 * s V_gen_codes_code <= 0 /\ -1 * s V_gen_codes_code <= 0)%Z
   | 7 => (-1 * s V_gen_codes_z <= 0 /\ -1 * s V_gen_codes_bits + 1 <= 0 /\ 1 * s V_gen_codes_bits + -16 <= 0)%Z
   | 8 => (1 * s V_gen_codes_bits + -16 <= 0 /\ -1 * s V_gen_codes_z <= 0 /\ -1 * s V_gen_codes_bits + 16 <= 0)%Z
   | 9 => (-1 * s V_gen_codes_bits + 16 <= 0 /\ -1 * s V_gen_codes_z <= 0 /\ 1 * s V_gen_codes_bits + -16 <= 0)%Z
   | 10 => (1 * s V_gen_codes_bits + -16 <= 0 /\ -1 * s V_gen_codes_z <= 0 /\ -1 * s V_gen_codes_bits + 16 <= 0 /\ 1 * s V_gen_codes_n <= 0 /\ -1 * s V_gen_codes_n <= 0)%Z
   | 11 => (-1 * s V_gen_codes_n <= 0 /\ 1 * s V_gen_codes_n <= 0 /\ -1 * s V_gen_codes_bits + 16 <= 0 /\ -1 * s V_gen_codes_z <= 0 /\ 1 * s V_gen_codes_bits + -16 <= 0)%Z
   | 12 => (-1 * s V_gen_codes_z <= 0 /\ -1 * s V_gen_codes_n <= 0 /\ 1 * s V_gen_codes_bits + -16 <= 0 /\ -1 * s V_gen_codes_bits + 16 <= 0)%Z
   | 13 => (-1 * s V_gen_codes_bits + 16 <= 0 /\ 1 * s V_gen_codes_bits + -16 <= 0 /\ -1 * s V_gen_codes_n <= 0 /\ -1 * s V_gen_codes_z <= 0 /\ 1 * s V_gen_codes__tmp+ -1 * s V_gen_codes_n + 1 <= 0)%Z
   | 14 => (1 * s V_gen_codes__tmp+ -1 * s V_gen_codes_n + 1 <= 0 /\ -1 * s V_gen_codes_z <= 0 /\ -1 * s V_gen_codes_n <= 0 /\ 1 * s V_gen_codes_bits + -16 <= 0 /\ -1 * s V_gen_codes_bits + 16 <= 0)%Z
   | 15 => (-1 * s V_gen_codes_bits + 16 <= 0 /\ 1 * s V_gen_codes_bits + -16 <= 0 /\ -1 * s V_gen_codes_n <= 0 /\ -1 * s V_gen_codes_z <= 0 /\ -1 * s V_gen_codes__tmp+ 1 * s V_gen_codes_n <= 0)%Z
   | 16 => (-1 * s V_gen_codes__tmp+ 1 * s V_gen_codes_n <= 0 /\ -1 * s V_gen_codes_z <= 0 /\ -1 * s V_gen_codes_n <= 0 /\ 1 * s V_gen_codes_bits + -16 <= 0 /\ -1 * s V_gen_codes_bits + 16 <= 0)%Z
   | 17 => (-1 * s V_gen_codes_bits + 16 <= 0 /\ 1 * s V_gen_codes_bits + -16 <= 0 /\ -1 * s V_gen_codes_n <= 0 /\ -1 * s V_gen_codes_z <= 0 /\ -1 * s V_gen_codes__tmp+ 1 * s V_gen_codes_n <= 0)%Z
   | 18 => (-1 * s V_gen_codes__tmp+ 1 * s V_gen_codes_n <= 0 /\ -1 * s V_gen_codes_z <= 0 /\ -1 * s V_gen_codes_n <= 0 /\ 1 * s V_gen_codes_bits + -16 <= 0 /\ -1 * s V_gen_codes_bits + 16 <= 0)%Z
   | 19 => (-1 * s V_gen_codes_bits + 16 <= 0 /\ 1 * s V_gen_codes_bits + -16 <= 0 /\ -1 * s V_gen_codes_n <= 0 /\ -1 * s V_gen_codes_z <= 0 /\ -1 * s V_gen_codes__tmp+ 1 * s V_gen_codes_n <= 0)%Z
   | 20 => (-1 * s V_gen_codes__tmp+ 1 * s V_gen_codes_n <= 0 /\ -1 * s V_gen_codes_z <= 0 /\ -1 * s V_gen_codes_n <= 0 /\ 1 * s V_gen_codes_bits + -16 <= 0 /\ -1 * s V_gen_codes_bits + 16 <= 0)%Z
   | 21 => (-1 * s V_gen_codes_bits + 16 <= 0 /\ 1 * s V_gen_codes_bits + -16 <= 0 /\ -1 * s V_gen_codes_n <= 0 /\ -1 * s V_gen_codes_z <= 0 /\ -1 * s V_gen_codes__tmp+ 1 * s V_gen_codes_n <= 0 /\ 1 * s V_gen_codes_len <= 0 /\ -1 * s V_gen_codes_len <= 0)%Z
   | 22 => (-1 * s V_gen_codes_len <= 0 /\ 1 * s V_gen_codes_len <= 0 /\ -1 * s V_gen_codes__tmp+ 1 * s V_gen_codes_n <= 0 /\ -1 * s V_gen_codes_z <= 0 /\ -1 * s V_gen_codes_n <= 0 /\ 1 * s V_gen_codes_bits + -16 <= 0 /\ -1 * s V_gen_codes_bits + 16 <= 0)%Z
   | 23 => (-1 * s V_gen_codes_bits + 16 <= 0 /\ 1 * s V_gen_codes_bits + -16 <= 0 /\ -1 * s V_gen_codes_n <= 0 /\ -1 * s V_gen_codes_z <= 0 /\ -1 * s V_gen_codes__tmp+ 1 * s V_gen_codes_n <= 0)%Z
   | 24 => (-1 * s V_gen_codes_z <= 0 /\ 1 * s V_gen_codes_bits + -16 <= 0 /\ -1 * s V_gen_codes_bits + 16 <= 0 /\ -1 * s V_gen_codes_n + 1 <= 0 /\ -1 * s V_gen_codes__tmp+ 1 * s V_gen_codes_n + -1 <= 0)%Z
   | 25 => (-1 * s V_gen_codes__tmp+ 1 * s V_gen_codes_n + -1 <= 0 /\ -1 * s V_gen_codes_n + 1 <= 0 /\ -1 * s V_gen_codes_bits + 16 <= 0 /\ 1 * s V_gen_codes_bits + -16 <= 0 /\ -1 * s V_gen_codes_z <= 0)%Z
   | 26 => (-1 * s V_gen_codes_z <= 0 /\ 1 * s V_gen_codes_bits + -16 <= 0 /\ -1 * s V_gen_codes_bits + 16 <= 0 /\ -1 * s V_gen_codes_n + 1 <= 0 /\ -1 * s V_gen_codes__tmp+ 1 * s V_gen_codes_n + -1 <= 0)%Z
   | 27 => (-1 * s V_gen_codes__tmp+ 1 * s V_gen_codes_n + -1 <= 0 /\ -1 * s V_gen_codes_n + 1 <= 0 /\ -1 * s V_gen_codes_bits + 16 <= 0 /\ 1 * s V_gen_codes_bits + -16 <= 0 /\ -1 * s V_gen_codes_z + 1 <= 0)%Z
   | 28 => (-1 * s V_gen_codes_bits + 1 <= 0 /\ -1 * s V_gen_codes_z <= 0 /\ 1 * s V_gen_codes_bits + -15 <= 0)%Z
   | 29 => (1 * s V_gen_codes_bits + -15 <= 0 /\ -1 * s V_gen_codes_z <= 0 /\ -1 * s V_gen_codes_bits + 1 <= 0)%Z
   | 30 => (-1 * s V_gen_codes_bits + 1 <= 0 /\ -1 * s V_gen_codes_z <= 0 /\ 1 * s V_gen_codes_bits + -15 <= 0)%Z
   | 31 => (1 * s V_gen_codes_bits + -15 <= 0 /\ -1 * s V_gen_codes_z <= 0 /\ -1 * s V_gen_codes_bits + 1 <= 0)%Z
   | 32 => (-1 * s V_gen_codes_z <= 0 /\ 1 * s V_gen_codes_bits + -16 <= 0 /\ -1 * s V_gen_codes_bits + 2 <= 0)%Z
   | 33 => (-1 * s V_gen_codes_bits + 2 <= 0 /\ 1 * s V_gen_codes_bits + -16 <= 0 /\ -1 * s V_gen_codes_z <= 0)%Z
   | 34 => (-1 * s V_gen_codes_z <= 0 /\ 1 * s V_gen_codes_bits + -16 <= 0 /\ -1 * s V_gen_codes_bits + 2 <= 0)%Z
   | 35 => (-1 * s V_gen_codes_bits + 2 <= 0 /\ 1 * s V_gen_codes_bits + -16 <= 0 /\ -1 * s V_gen_codes_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_gen_codes (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((15 # 1) + max0(1 + s V_gen_codes_max_code) <= z)%Q
   | 2 => ((15 # 1) + s V_gen_codes_z + max0(1 + s V_gen_codes_max_code) <= z)%Q
   | 3 => ((15 # 1) + s V_gen_codes_z + max0(1 + s V_gen_codes__tmp) <= z)%Q
   | 4 => ((15 # 1) + s V_gen_codes_z + max0(1 + s V_gen_codes__tmp) <= z)%Q
   | 5 => (s V_gen_codes_z + max0(1 + s V_gen_codes__tmp)
           + max0(16 - s V_gen_codes_bits) <= z)%Q
   | 6 => (s V_gen_codes_z + max0(1 + s V_gen_codes__tmp)
           + max0(16 - s V_gen_codes_bits) <= z)%Q
   | 7 => (s V_gen_codes_z + max0(1 + s V_gen_codes__tmp)
           + max0(16 - s V_gen_codes_bits) <= z)%Q
   | 8 => (s V_gen_codes_z + max0(1 + s V_gen_codes__tmp)
           + max0(16 - s V_gen_codes_bits) <= z)%Q
   | 9 => (s V_gen_codes_z + max0(1 + s V_gen_codes__tmp)
           + max0(16 - s V_gen_codes_bits) <= z)%Q
   | 10 => (s V_gen_codes_z + max0(1 + s V_gen_codes__tmp - s V_gen_codes_n)
            + max0(16 - s V_gen_codes_bits) <= z)%Q
   | 11 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (16 - s V_gen_codes_bits) (15
                                                                    - s V_gen_codes_bits));
      (*-1 0*) F_max0_ge_0 (15 - s V_gen_codes_bits)]
     (s V_gen_codes_z + max0(1 + s V_gen_codes__tmp - s V_gen_codes_n)
      + max0(16 - s V_gen_codes_bits) <= z)%Q
   | 12 => (s V_gen_codes_z + max0(1 + s V_gen_codes__tmp - s V_gen_codes_n) <= z)%Q
   | 13 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (1 + s V_gen_codes__tmp
                                             - s V_gen_codes_n) (s V_gen_codes__tmp
                                                                 - s V_gen_codes_n));
      (*-1 0*) F_max0_ge_0 (s V_gen_codes__tmp - s V_gen_codes_n)]
     (s V_gen_codes_z + max0(1 + s V_gen_codes__tmp - s V_gen_codes_n) <= z)%Q
   | 14 => (s V_gen_codes_z <= z)%Q
   | 15 => (s V_gen_codes_z + max0(1 + s V_gen_codes__tmp - s V_gen_codes_n) <= z)%Q
   | 16 => (s V_gen_codes_z + max0(1 + s V_gen_codes__tmp - s V_gen_codes_n) <= z)%Q
   | 17 => hints
     [(*0 1*) F_max0_pre_decrement 1 (1 + s V_gen_codes__tmp
                                      - s V_gen_codes_n) (1)]
     (s V_gen_codes_z + max0(1 + s V_gen_codes__tmp - s V_gen_codes_n) <= z)%Q
   | 18 => ((1 # 1) + s V_gen_codes_z
            + max0(s V_gen_codes__tmp - s V_gen_codes_n) <= z)%Q
   | 19 => ((1 # 1) + s V_gen_codes_z
            + max0(s V_gen_codes__tmp - s V_gen_codes_n) <= z)%Q
   | 20 => ((1 # 1) + s V_gen_codes_z
            + max0(s V_gen_codes__tmp - s V_gen_codes_n) <= z)%Q
   | 21 => ((1 # 1) + s V_gen_codes_z
            + max0(s V_gen_codes__tmp - s V_gen_codes_n) <= z)%Q
   | 22 => ((1 # 1) + s V_gen_codes_z
            + max0(s V_gen_codes__tmp - s V_gen_codes_n) <= z)%Q
   | 23 => ((1 # 1) + s V_gen_codes_z
            + max0(s V_gen_codes__tmp - s V_gen_codes_n) <= z)%Q
   | 24 => ((1 # 1) + s V_gen_codes_z
            + max0(1 + s V_gen_codes__tmp - s V_gen_codes_n) <= z)%Q
   | 25 => ((1 # 1) + s V_gen_codes_z
            + max0(1 + s V_gen_codes__tmp - s V_gen_codes_n) <= z)%Q
   | 26 => ((1 # 1) + s V_gen_codes_z
            + max0(1 + s V_gen_codes__tmp - s V_gen_codes_n) <= z)%Q
   | 27 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_gen_codes_z)) (F_check_ge (s V_gen_codes_z) (0));
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_gen_codes_z) (0))) (F_max0_ge_0 (s V_gen_codes_z))]
     (s V_gen_codes_z + max0(1 + s V_gen_codes__tmp - s V_gen_codes_n) <= z)%Q
   | 28 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (16 - s V_gen_codes_bits) (1)]
     (s V_gen_codes_z + max0(1 + s V_gen_codes__tmp)
      + max0(16 - s V_gen_codes_bits) <= z)%Q
   | 29 => ((1 # 1) + s V_gen_codes_z + max0(1 + s V_gen_codes__tmp)
            + max0(15 - s V_gen_codes_bits) <= z)%Q
   | 30 => ((1 # 1) + s V_gen_codes_z + max0(1 + s V_gen_codes__tmp)
            + max0(15 - s V_gen_codes_bits) <= z)%Q
   | 31 => ((1 # 1) + s V_gen_codes_z + max0(1 + s V_gen_codes__tmp)
            + max0(15 - s V_gen_codes_bits) <= z)%Q
   | 32 => ((1 # 1) + s V_gen_codes_z + max0(1 + s V_gen_codes__tmp)
            + max0(16 - s V_gen_codes_bits) <= z)%Q
   | 33 => ((1 # 1) + s V_gen_codes_z + max0(1 + s V_gen_codes__tmp)
            + max0(16 - s V_gen_codes_bits) <= z)%Q
   | 34 => ((1 # 1) + s V_gen_codes_z + max0(1 + s V_gen_codes__tmp)
            + max0(16 - s V_gen_codes_bits) <= z)%Q
   | 35 => (s V_gen_codes_z + max0(1 + s V_gen_codes__tmp)
            + max0(16 - s V_gen_codes_bits) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_gen_codes =>
    [mkPA Q (fun n z s => ai_gen_codes n s /\ annot0_gen_codes n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_gen_codes (proc_start P_gen_codes) s1 (proc_end P_gen_codes) s2 ->
    (s2 V_gen_codes_z <= (15 # 1) + max0(1 + s1 V_gen_codes_max_code))%Q.
Proof.
  prove_bound ipa admissible_ipa P_gen_codes.
Qed.
