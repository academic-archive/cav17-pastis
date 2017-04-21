Require Import pasta.Pasta.

Inductive proc: Type :=
  P_name_init.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_name_init_z := 1%positive.
Notation V_name_init__tmp := 2%positive.
Notation V_name_init_i := 3%positive.
Notation V_name_init_ncnt := 4%positive.
Notation V_name_init_nidx := 5%positive.
Notation V_name_init_count := 6%positive.
Notation V_name_init_mem := 7%positive.
Definition Pedges_name_init: list (edge proc) :=
  (EA 1 (AAssign V_name_init_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_name_init__tmp (Some (EVar V_name_init_count))) 3)::(EA 3 AWeaken 4)::
  (EA 4 (AGuard (fun s => ((eval (EVar V_name_init__tmp) s) =
  (eval (ENum (0)) s))%Z)) 10)::(EA 4 (AGuard
  (fun s => ((eval (EVar V_name_init__tmp) s) <> (eval (ENum (0))
  s))%Z)) 5)::(EA 5 AWeaken 6)::(EA 6 ANone 8)::(EA 6 ANone 7)::
  (EA 7 ANone 13)::(EA 8 ANone 9)::(EA 9 AWeaken 25)::(EA 10 AWeaken 11)::
  (EA 11 (AAssign V_name_init__tmp None) 12)::(EA 12 ANone 13)::
  (EA 13 (AAssign V_name_init_i (Some (ENum (0)))) 14)::(EA 14 ANone 15)::
  (EA 15 AWeaken 16)::(EA 16 (AGuard (fun s => ((eval (EVar V_name_init_i)
  s) < (eval (ENum (130)) s))%Z)) 41)::(EA 16 (AGuard
  (fun s => ((eval (EVar V_name_init_i) s) >= (eval (ENum (130))
  s))%Z)) 17)::(EA 17 AWeaken 18)::(EA 18 (AAssign V_name_init_i
  (Some (ENum (-1)))) 19)::(EA 19 ANone 20)::(EA 20 AWeaken 21)::
  (EA 21 (AGuard (fun s => ((eval (EVar V_name_init_i) s) <
  (eval (ENum (128)) s))%Z)) 26)::(EA 21 (AGuard
  (fun s => ((eval (EVar V_name_init_i) s) >= (eval (ENum (128))
  s))%Z)) 22)::(EA 22 AWeaken 23)::(EA 23 ANone 24)::(EA 24 AWeaken 25)::
  (EA 26 AWeaken 27)::(EA 27 (AAssign V_name_init_ncnt (Some (EAdd (ENum (2))
  (EVar V_name_init_i)))) 28)::(EA 28 (AAssign V_name_init_nidx None) 29)::
  (EA 29 AWeaken 30)::(EA 30 (AGuard (fun s => ((eval (EVar V_name_init_i)
  s) < (eval (ENum (0)) s))%Z)) 33)::(EA 30 (AGuard
  (fun s => ((eval (EVar V_name_init_i) s) >= (eval (ENum (0)) s))%Z)) 31)::
  (EA 31 AWeaken 32)::(EA 32 ANone 35)::(EA 33 AWeaken 34)::
  (EA 34 ANone 35)::(EA 35 ANone 36)::(EA 36 (AAssign V_name_init_i
  (Some (EAdd (EVar V_name_init_i) (ENum (1))))) 37)::(EA 37 ANone 38)::
  (EA 38 ANone 39)::(EA 39 (AAssign V_name_init_z (Some (EAdd (ENum (1))
  (EVar V_name_init_z)))) 40)::(EA 40 AWeaken 21)::(EA 41 AWeaken 42)::
  (EA 42 ANone 43)::(EA 43 (AAssign V_name_init_i
  (Some (EAdd (EVar V_name_init_i) (ENum (128))))) 44)::(EA 44 ANone 45)::
  (EA 45 ANone 46)::(EA 46 (AAssign V_name_init_z (Some (EAdd (ENum (1))
  (EVar V_name_init_z)))) 47)::(EA 47 AWeaken 16)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_name_init => Pedges_name_init
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_name_init => 25
     end)%positive;
  var_global := var_global
}.

Definition ai_name_init (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_name_init_z <= 0 /\ -1 * s V_name_init_z <= 0)%Z
   | 3 => (-1 * s V_name_init_z <= 0 /\ 1 * s V_name_init_z <= 0)%Z
   | 4 => (1 * s V_name_init_z <= 0 /\ -1 * s V_name_init_z <= 0)%Z
   | 5 => (-1 * s V_name_init_z <= 0 /\ 1 * s V_name_init_z <= 0)%Z
   | 6 => (1 * s V_name_init_z <= 0 /\ -1 * s V_name_init_z <= 0)%Z
   | 7 => (-1 * s V_name_init_z <= 0 /\ 1 * s V_name_init_z <= 0)%Z
   | 8 => (-1 * s V_name_init_z <= 0 /\ 1 * s V_name_init_z <= 0)%Z
   | 9 => (1 * s V_name_init_z <= 0 /\ -1 * s V_name_init_z <= 0)%Z
   | 10 => (-1 * s V_name_init_z <= 0 /\ 1 * s V_name_init_z <= 0 /\ 1 * s V_name_init__tmp <= 0 /\ -1 * s V_name_init__tmp <= 0)%Z
   | 11 => (-1 * s V_name_init__tmp <= 0 /\ 1 * s V_name_init__tmp <= 0 /\ 1 * s V_name_init_z <= 0 /\ -1 * s V_name_init_z <= 0)%Z
   | 12 => (-1 * s V_name_init_z <= 0 /\ 1 * s V_name_init_z <= 0)%Z
   | 13 => (1 * s V_name_init_z <= 0 /\ -1 * s V_name_init_z <= 0)%Z
   | 14 => (-1 * s V_name_init_z <= 0 /\ 1 * s V_name_init_z <= 0 /\ 1 * s V_name_init_i <= 0 /\ -1 * s V_name_init_i <= 0)%Z
   | 15 => (-1 * s V_name_init_i <= 0 /\ 1 * s V_name_init_i <= 0 /\ 1 * s V_name_init_z <= 0 /\ -1 * s V_name_init_z <= 0)%Z
   | 16 => (-1 * s V_name_init_z <= 0 /\ -1 * s V_name_init_i <= 0 /\ 1 * s V_name_init_i + -257 <= 0)%Z
   | 17 => (1 * s V_name_init_i + -257 <= 0 /\ -1 * s V_name_init_z <= 0 /\ -1 * s V_name_init_i + 130 <= 0)%Z
   | 18 => (-1 * s V_name_init_i + 130 <= 0 /\ -1 * s V_name_init_z <= 0 /\ 1 * s V_name_init_i + -257 <= 0)%Z
   | 19 => (-1 * s V_name_init_z <= 0 /\ 1 * s V_name_init_i + 1 <= 0 /\ -1 * s V_name_init_i + -1 <= 0)%Z
   | 20 => (-1 * s V_name_init_i + -1 <= 0 /\ 1 * s V_name_init_i + 1 <= 0 /\ -1 * s V_name_init_z <= 0)%Z
   | 21 => (-1 * s V_name_init_z <= 0 /\ -1 * s V_name_init_i + -1 <= 0 /\ 1 * s V_name_init_i + -128 <= 0)%Z
   | 22 => (1 * s V_name_init_i + -128 <= 0 /\ -1 * s V_name_init_z <= 0 /\ -1 * s V_name_init_i + 128 <= 0)%Z
   | 23 => (-1 * s V_name_init_i + 128 <= 0 /\ -1 * s V_name_init_z <= 0 /\ 1 * s V_name_init_i + -128 <= 0)%Z
   | 24 => (1 * s V_name_init_i + -128 <= 0 /\ -1 * s V_name_init_z <= 0 /\ -1 * s V_name_init_i + 128 <= 0)%Z
   | 25 => (-1 * s V_name_init_z <= 0)%Z
   | 26 => (-1 * s V_name_init_i + -1 <= 0 /\ -1 * s V_name_init_z <= 0 /\ 1 * s V_name_init_i + -127 <= 0)%Z
   | 27 => (1 * s V_name_init_i + -127 <= 0 /\ -1 * s V_name_init_z <= 0 /\ -1 * s V_name_init_i + -1 <= 0)%Z
   | 28 => (-1 * s V_name_init_i + -1 <= 0 /\ -1 * s V_name_init_z <= 0 /\ 1 * s V_name_init_i + -127 <= 0 /\ 1 * s V_name_init_ncnt + -129 <= 0 /\ -1 * s V_name_init_ncnt + 1 <= 0)%Z
   | 29 => (-1 * s V_name_init_ncnt + 1 <= 0 /\ 1 * s V_name_init_ncnt + -129 <= 0 /\ 1 * s V_name_init_i + -127 <= 0 /\ -1 * s V_name_init_z <= 0 /\ -1 * s V_name_init_i + -1 <= 0)%Z
   | 30 => (-1 * s V_name_init_i + -1 <= 0 /\ -1 * s V_name_init_z <= 0 /\ 1 * s V_name_init_i + -127 <= 0 /\ 1 * s V_name_init_ncnt + -129 <= 0 /\ -1 * s V_name_init_ncnt + 1 <= 0)%Z
   | 31 => (-1 * s V_name_init_ncnt + 1 <= 0 /\ 1 * s V_name_init_ncnt + -129 <= 0 /\ 1 * s V_name_init_i + -127 <= 0 /\ -1 * s V_name_init_z <= 0 /\ -1 * s V_name_init_i <= 0)%Z
   | 32 => (-1 * s V_name_init_i <= 0 /\ -1 * s V_name_init_z <= 0 /\ 1 * s V_name_init_i + -127 <= 0 /\ 1 * s V_name_init_ncnt + -129 <= 0 /\ -1 * s V_name_init_ncnt + 1 <= 0)%Z
   | 33 => (-1 * s V_name_init_ncnt + 1 <= 0 /\ 1 * s V_name_init_ncnt + -129 <= 0 /\ -1 * s V_name_init_z <= 0 /\ -1 * s V_name_init_i + -1 <= 0 /\ 1 * s V_name_init_i + 1 <= 0)%Z
   | 34 => (1 * s V_name_init_i + 1 <= 0 /\ -1 * s V_name_init_i + -1 <= 0 /\ -1 * s V_name_init_z <= 0 /\ 1 * s V_name_init_ncnt + -129 <= 0 /\ -1 * s V_name_init_ncnt + 1 <= 0)%Z
   | 35 => (1 * s V_name_init_i + -127 <= 0 /\ -1 * s V_name_init_ncnt + 1 <= 0 /\ 1 * s V_name_init_ncnt + -129 <= 0 /\ -1 * s V_name_init_z <= 0 /\ -1 * s V_name_init_i + -1 <= 0)%Z
   | 36 => (-1 * s V_name_init_i + -1 <= 0 /\ -1 * s V_name_init_z <= 0 /\ 1 * s V_name_init_ncnt + -129 <= 0 /\ -1 * s V_name_init_ncnt + 1 <= 0 /\ 1 * s V_name_init_i + -127 <= 0)%Z
   | 37 => (-1 * s V_name_init_ncnt + 1 <= 0 /\ 1 * s V_name_init_ncnt + -129 <= 0 /\ -1 * s V_name_init_z <= 0 /\ -1 * s V_name_init_i <= 0 /\ 1 * s V_name_init_i + -128 <= 0)%Z
   | 38 => (1 * s V_name_init_i + -128 <= 0 /\ -1 * s V_name_init_i <= 0 /\ -1 * s V_name_init_z <= 0 /\ 1 * s V_name_init_ncnt + -129 <= 0 /\ -1 * s V_name_init_ncnt + 1 <= 0)%Z
   | 39 => (-1 * s V_name_init_ncnt + 1 <= 0 /\ 1 * s V_name_init_ncnt + -129 <= 0 /\ -1 * s V_name_init_z <= 0 /\ -1 * s V_name_init_i <= 0 /\ 1 * s V_name_init_i + -128 <= 0)%Z
   | 40 => (1 * s V_name_init_i + -128 <= 0 /\ -1 * s V_name_init_i <= 0 /\ 1 * s V_name_init_ncnt + -129 <= 0 /\ -1 * s V_name_init_ncnt + 1 <= 0 /\ -1 * s V_name_init_z + 1 <= 0)%Z
   | 41 => (-1 * s V_name_init_i <= 0 /\ -1 * s V_name_init_z <= 0 /\ 1 * s V_name_init_i + -129 <= 0)%Z
   | 42 => (1 * s V_name_init_i + -129 <= 0 /\ -1 * s V_name_init_z <= 0 /\ -1 * s V_name_init_i <= 0)%Z
   | 43 => (-1 * s V_name_init_i <= 0 /\ -1 * s V_name_init_z <= 0 /\ 1 * s V_name_init_i + -129 <= 0)%Z
   | 44 => (-1 * s V_name_init_z <= 0 /\ -1 * s V_name_init_i + 128 <= 0 /\ 1 * s V_name_init_i + -257 <= 0)%Z
   | 45 => (1 * s V_name_init_i + -257 <= 0 /\ -1 * s V_name_init_i + 128 <= 0 /\ -1 * s V_name_init_z <= 0)%Z
   | 46 => (-1 * s V_name_init_z <= 0 /\ -1 * s V_name_init_i + 128 <= 0 /\ 1 * s V_name_init_i + -257 <= 0)%Z
   | 47 => (1 * s V_name_init_i + -257 <= 0 /\ -1 * s V_name_init_i + 128 <= 0 /\ -1 * s V_name_init_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_name_init (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((16638 # 127) <= z)%Q
   | 2 => ((16638 # 127) + s V_name_init_z <= z)%Q
   | 3 => ((16638 # 127) + s V_name_init_z <= z)%Q
   | 4 => ((16638 # 127) + s V_name_init_z <= z)%Q
   | 5 => ((16638 # 127) + s V_name_init_z <= z)%Q
   | 6 => ((16638 # 127) + s V_name_init_z <= z)%Q
   | 7 => ((16638 # 127) + s V_name_init_z <= z)%Q
   | 8 => ((16638 # 127) + s V_name_init_z <= z)%Q
   | 9 => hints
     [(*-131.008 0*) F_one]
     ((16638 # 127) + s V_name_init_z <= z)%Q
   | 10 => ((16638 # 127) + s V_name_init_z <= z)%Q
   | 11 => ((16638 # 127) + s V_name_init_z <= z)%Q
   | 12 => ((16638 # 127) + s V_name_init_z <= z)%Q
   | 13 => ((16638 # 127) + s V_name_init_z <= z)%Q
   | 14 => ((129 # 1) + s V_name_init_z
            + (1 # 127) * max0(257 - s V_name_init_i) <= z)%Q
   | 15 => ((129 # 1) + s V_name_init_z
            + (1 # 127) * max0(257 - s V_name_init_i) <= z)%Q
   | 16 => ((129 # 1) + s V_name_init_z
            + (1 # 127) * max0(257 - s V_name_init_i) <= z)%Q
   | 17 => hints
     [(*-0.0078125 0*) F_binom_monotonic 1 (F_max0_ge_0 (257
                                                         - s V_name_init_i)) (F_check_ge (0) (0))]
     ((129 # 1) + s V_name_init_z + (1 # 127) * max0(257 - s V_name_init_i) <= z)%Q
   | 18 => ((129 # 1) + s V_name_init_z <= z)%Q
   | 19 => (s V_name_init_z + max0(128 - s V_name_init_i) <= z)%Q
   | 20 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_name_init_z) (0))) (F_max0_ge_0 (s V_name_init_z))]
     (s V_name_init_z + max0(128 - s V_name_init_i) <= z)%Q
   | 21 => (max0(128 - s V_name_init_i) + max0(s V_name_init_z) <= z)%Q
   | 22 => (max0(128 - s V_name_init_i) + max0(s V_name_init_z) <= z)%Q
   | 23 => (max0(128 - s V_name_init_i) + max0(s V_name_init_z) <= z)%Q
   | 24 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (128 - s V_name_init_i) (127
                                                                    - 
                                                                    s V_name_init_i));
      (*-1 0*) F_max0_ge_0 (127 - s V_name_init_i);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_name_init_z)) (F_check_ge (s V_name_init_z) (0))]
     (max0(128 - s V_name_init_i) + max0(s V_name_init_z) <= z)%Q
   | 25 => (s V_name_init_z <= z)%Q
   | 26 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_name_init_z)) (F_check_ge (s V_name_init_z) (0))]
     (max0(128 - s V_name_init_i) + max0(s V_name_init_z) <= z)%Q
   | 27 => (s V_name_init_z + max0(128 - s V_name_init_i) <= z)%Q
   | 28 => (s V_name_init_z + max0(128 - s V_name_init_i) <= z)%Q
   | 29 => (s V_name_init_z + max0(128 - s V_name_init_i) <= z)%Q
   | 30 => (s V_name_init_z + max0(128 - s V_name_init_i) <= z)%Q
   | 31 => hints
     [(*0 1*) F_max0_pre_decrement 1 (128 - s V_name_init_i) (1)]
     (s V_name_init_z + max0(128 - s V_name_init_i) <= z)%Q
   | 32 => ((1 # 1) + s V_name_init_z + max0(127 - s V_name_init_i) <= z)%Q
   | 33 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (128 - s V_name_init_i) (1)]
     (s V_name_init_z + max0(128 - s V_name_init_i) <= z)%Q
   | 34 => ((1 # 1) + s V_name_init_z + max0(127 - s V_name_init_i) <= z)%Q
   | 35 => ((1 # 1) + s V_name_init_z + max0(127 - s V_name_init_i) <= z)%Q
   | 36 => ((1 # 1) + s V_name_init_z + max0(127 - s V_name_init_i) <= z)%Q
   | 37 => ((1 # 1) + s V_name_init_z + max0(128 - s V_name_init_i) <= z)%Q
   | 38 => ((1 # 1) + s V_name_init_z + max0(128 - s V_name_init_i) <= z)%Q
   | 39 => ((1 # 1) + s V_name_init_z + max0(128 - s V_name_init_i) <= z)%Q
   | 40 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_name_init_z) (0))) (F_max0_ge_0 (s V_name_init_z))]
     (s V_name_init_z + max0(128 - s V_name_init_i) <= z)%Q
   | 41 => hints
     [(*-0.0078125 0*) F_max0_pre_decrement 1 (257 - s V_name_init_i) (128)]
     ((129 # 1) + s V_name_init_z + (1 # 127) * max0(257 - s V_name_init_i) <= z)%Q
   | 42 => ((130 # 1) + s V_name_init_z
            + (1 # 127) * max0(129 - s V_name_init_i) <= z)%Q
   | 43 => ((130 # 1) + s V_name_init_z
            + (1 # 127) * max0(129 - s V_name_init_i) <= z)%Q
   | 44 => ((130 # 1) + s V_name_init_z
            + (1 # 127) * max0(257 - s V_name_init_i) <= z)%Q
   | 45 => ((130 # 1) + s V_name_init_z
            + (1 # 127) * max0(257 - s V_name_init_i) <= z)%Q
   | 46 => ((130 # 1) + s V_name_init_z
            + (1 # 127) * max0(257 - s V_name_init_i) <= z)%Q
   | 47 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_name_init_z)) (F_check_ge (s V_name_init_z) (0));
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_name_init_z) (0))) (F_max0_ge_0 (s V_name_init_z))]
     ((129 # 1) + s V_name_init_z + (1 # 127) * max0(257 - s V_name_init_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_name_init =>
    [mkPA Q (fun n z s => ai_name_init n s /\ annot0_name_init n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_name_init (proc_start P_name_init) s1 (proc_end P_name_init) s2 ->
    (s2 V_name_init_z <= (16638 # 127))%Q.
Proof.
  prove_bound ipa admissible_ipa P_name_init.
Qed.
