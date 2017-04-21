Require Import pasta.Pasta.

Inductive proc: Type :=
  P_init_fft.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_init_fft_z := 1%positive.
Notation V_init_fft_i := 2%positive.
Definition Pedges_init_fft: list (edge proc) :=
  (EA 1 (AAssign V_init_fft_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_init_fft_i (Some (ENum (0)))) 3)::(EA 3 ANone 4)::(EA 4 AWeaken 5)::
  (EA 5 (AGuard (fun s => ((eval (EVar V_init_fft_i) s) < (eval (ENum (4))
  s))%Z)) 32)::(EA 5 (AGuard (fun s => ((eval (EVar V_init_fft_i) s) >=
  (eval (ENum (4)) s))%Z)) 6)::(EA 6 AWeaken 7)::(EA 7 (AAssign V_init_fft_i
  (Some (ENum (0)))) 8)::(EA 8 ANone 9)::(EA 9 AWeaken 10)::(EA 10 (AGuard
  (fun s => ((eval (EVar V_init_fft_i) s) < (eval (ENum (512)) s))%Z)) 25)::
  (EA 10 (AGuard (fun s => ((eval (EVar V_init_fft_i) s) >=
  (eval (ENum (512)) s))%Z)) 11)::(EA 11 AWeaken 12)::(EA 12 (AAssign
  V_init_fft_i (Some (ENum (0)))) 13)::(EA 13 ANone 14)::(EA 14 AWeaken 15)::
  (EA 15 (AGuard (fun s => ((eval (EVar V_init_fft_i) s) < (eval (ENum (128))
  s))%Z)) 18)::(EA 15 (AGuard (fun s => ((eval (EVar V_init_fft_i) s) >=
  (eval (ENum (128)) s))%Z)) 16)::(EA 16 AWeaken 17)::(EA 18 AWeaken 19)::
  (EA 19 ANone 20)::(EA 20 (AAssign V_init_fft_i
  (Some (EAdd (EVar V_init_fft_i) (ENum (1))))) 21)::(EA 21 ANone 22)::
  (EA 22 ANone 23)::(EA 23 (AAssign V_init_fft_z (Some (EAdd (ENum (1))
  (EVar V_init_fft_z)))) 24)::(EA 24 AWeaken 15)::(EA 25 AWeaken 26)::
  (EA 26 ANone 27)::(EA 27 (AAssign V_init_fft_i
  (Some (EAdd (EVar V_init_fft_i) (ENum (1))))) 28)::(EA 28 ANone 29)::
  (EA 29 ANone 30)::(EA 30 (AAssign V_init_fft_z (Some (EAdd (ENum (1))
  (EVar V_init_fft_z)))) 31)::(EA 31 AWeaken 10)::(EA 32 AWeaken 33)::
  (EA 33 ANone 34)::(EA 34 (AAssign V_init_fft_i
  (Some (EAdd (EVar V_init_fft_i) (ENum (1))))) 35)::(EA 35 ANone 36)::
  (EA 36 ANone 37)::(EA 37 (AAssign V_init_fft_z (Some (EAdd (ENum (1))
  (EVar V_init_fft_z)))) 38)::(EA 38 AWeaken 5)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_init_fft => Pedges_init_fft
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_init_fft => 17
     end)%positive;
  var_global := var_global
}.

Definition ai_init_fft (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_init_fft_z <= 0 /\ -1 * s V_init_fft_z <= 0)%Z
   | 3 => (-1 * s V_init_fft_z <= 0 /\ 1 * s V_init_fft_z <= 0 /\ 1 * s V_init_fft_i <= 0 /\ -1 * s V_init_fft_i <= 0)%Z
   | 4 => (-1 * s V_init_fft_i <= 0 /\ 1 * s V_init_fft_i <= 0 /\ 1 * s V_init_fft_z <= 0 /\ -1 * s V_init_fft_z <= 0)%Z
   | 5 => (-1 * s V_init_fft_z <= 0 /\ -1 * s V_init_fft_i <= 0 /\ 1 * s V_init_fft_i + -4 <= 0)%Z
   | 6 => (1 * s V_init_fft_i + -4 <= 0 /\ -1 * s V_init_fft_z <= 0 /\ -1 * s V_init_fft_i + 4 <= 0)%Z
   | 7 => (-1 * s V_init_fft_i + 4 <= 0 /\ -1 * s V_init_fft_z <= 0 /\ 1 * s V_init_fft_i + -4 <= 0)%Z
   | 8 => (-1 * s V_init_fft_z <= 0 /\ 1 * s V_init_fft_i <= 0 /\ -1 * s V_init_fft_i <= 0)%Z
   | 9 => (-1 * s V_init_fft_i <= 0 /\ 1 * s V_init_fft_i <= 0 /\ -1 * s V_init_fft_z <= 0)%Z
   | 10 => (-1 * s V_init_fft_z <= 0 /\ -1 * s V_init_fft_i <= 0 /\ 1 * s V_init_fft_i + -512 <= 0)%Z
   | 11 => (1 * s V_init_fft_i + -512 <= 0 /\ -1 * s V_init_fft_z <= 0 /\ -1 * s V_init_fft_i + 512 <= 0)%Z
   | 12 => (-1 * s V_init_fft_i + 512 <= 0 /\ -1 * s V_init_fft_z <= 0 /\ 1 * s V_init_fft_i + -512 <= 0)%Z
   | 13 => (-1 * s V_init_fft_z <= 0 /\ 1 * s V_init_fft_i <= 0 /\ -1 * s V_init_fft_i <= 0)%Z
   | 14 => (-1 * s V_init_fft_i <= 0 /\ 1 * s V_init_fft_i <= 0 /\ -1 * s V_init_fft_z <= 0)%Z
   | 15 => (-1 * s V_init_fft_z <= 0 /\ -1 * s V_init_fft_i <= 0 /\ 1 * s V_init_fft_i + -128 <= 0)%Z
   | 16 => (1 * s V_init_fft_i + -128 <= 0 /\ -1 * s V_init_fft_z <= 0 /\ -1 * s V_init_fft_i + 128 <= 0)%Z
   | 17 => (-1 * s V_init_fft_i + 128 <= 0 /\ -1 * s V_init_fft_z <= 0 /\ 1 * s V_init_fft_i + -128 <= 0)%Z
   | 18 => (-1 * s V_init_fft_i <= 0 /\ -1 * s V_init_fft_z <= 0 /\ 1 * s V_init_fft_i + -127 <= 0)%Z
   | 19 => (1 * s V_init_fft_i + -127 <= 0 /\ -1 * s V_init_fft_z <= 0 /\ -1 * s V_init_fft_i <= 0)%Z
   | 20 => (-1 * s V_init_fft_i <= 0 /\ -1 * s V_init_fft_z <= 0 /\ 1 * s V_init_fft_i + -127 <= 0)%Z
   | 21 => (-1 * s V_init_fft_z <= 0 /\ -1 * s V_init_fft_i + 1 <= 0 /\ 1 * s V_init_fft_i + -128 <= 0)%Z
   | 22 => (1 * s V_init_fft_i + -128 <= 0 /\ -1 * s V_init_fft_i + 1 <= 0 /\ -1 * s V_init_fft_z <= 0)%Z
   | 23 => (-1 * s V_init_fft_z <= 0 /\ -1 * s V_init_fft_i + 1 <= 0 /\ 1 * s V_init_fft_i + -128 <= 0)%Z
   | 24 => (1 * s V_init_fft_i + -128 <= 0 /\ -1 * s V_init_fft_i + 1 <= 0 /\ -1 * s V_init_fft_z + 1 <= 0)%Z
   | 25 => (-1 * s V_init_fft_i <= 0 /\ -1 * s V_init_fft_z <= 0 /\ 1 * s V_init_fft_i + -511 <= 0)%Z
   | 26 => (1 * s V_init_fft_i + -511 <= 0 /\ -1 * s V_init_fft_z <= 0 /\ -1 * s V_init_fft_i <= 0)%Z
   | 27 => (-1 * s V_init_fft_i <= 0 /\ -1 * s V_init_fft_z <= 0 /\ 1 * s V_init_fft_i + -511 <= 0)%Z
   | 28 => (-1 * s V_init_fft_z <= 0 /\ -1 * s V_init_fft_i + 1 <= 0 /\ 1 * s V_init_fft_i + -512 <= 0)%Z
   | 29 => (1 * s V_init_fft_i + -512 <= 0 /\ -1 * s V_init_fft_i + 1 <= 0 /\ -1 * s V_init_fft_z <= 0)%Z
   | 30 => (-1 * s V_init_fft_z <= 0 /\ -1 * s V_init_fft_i + 1 <= 0 /\ 1 * s V_init_fft_i + -512 <= 0)%Z
   | 31 => (1 * s V_init_fft_i + -512 <= 0 /\ -1 * s V_init_fft_i + 1 <= 0 /\ -1 * s V_init_fft_z + 1 <= 0)%Z
   | 32 => (-1 * s V_init_fft_i <= 0 /\ -1 * s V_init_fft_z <= 0 /\ 1 * s V_init_fft_i + -3 <= 0)%Z
   | 33 => (1 * s V_init_fft_i + -3 <= 0 /\ -1 * s V_init_fft_z <= 0 /\ -1 * s V_init_fft_i <= 0)%Z
   | 34 => (-1 * s V_init_fft_i <= 0 /\ -1 * s V_init_fft_z <= 0 /\ 1 * s V_init_fft_i + -3 <= 0)%Z
   | 35 => (-1 * s V_init_fft_z <= 0 /\ -1 * s V_init_fft_i + 1 <= 0 /\ 1 * s V_init_fft_i + -4 <= 0)%Z
   | 36 => (1 * s V_init_fft_i + -4 <= 0 /\ -1 * s V_init_fft_i + 1 <= 0 /\ -1 * s V_init_fft_z <= 0)%Z
   | 37 => (-1 * s V_init_fft_z <= 0 /\ -1 * s V_init_fft_i + 1 <= 0 /\ 1 * s V_init_fft_i + -4 <= 0)%Z
   | 38 => (1 * s V_init_fft_i + -4 <= 0 /\ -1 * s V_init_fft_i + 1 <= 0 /\ -1 * s V_init_fft_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_init_fft (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((644 # 1) <= z)%Q
   | 2 => ((644 # 1) + s V_init_fft_z <= z)%Q
   | 3 => ((640 # 1) + s V_init_fft_z + max0(4 - s V_init_fft_i) <= z)%Q
   | 4 => ((640 # 1) + s V_init_fft_z + max0(4 - s V_init_fft_i) <= z)%Q
   | 5 => ((640 # 1) + s V_init_fft_z + max0(4 - s V_init_fft_i) <= z)%Q
   | 6 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (4 - s V_init_fft_i) (3
                                                                  - s V_init_fft_i));
      (*-1 0*) F_max0_ge_0 (3 - s V_init_fft_i)]
     ((640 # 1) + s V_init_fft_z + max0(4 - s V_init_fft_i) <= z)%Q
   | 7 => ((640 # 1) + s V_init_fft_z <= z)%Q
   | 8 => ((128 # 1) + s V_init_fft_z + max0(512 - s V_init_fft_i) <= z)%Q
   | 9 => ((128 # 1) + s V_init_fft_z + max0(512 - s V_init_fft_i) <= z)%Q
   | 10 => ((128 # 1) + s V_init_fft_z + max0(512 - s V_init_fft_i) <= z)%Q
   | 11 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (512 - s V_init_fft_i) (511
                                                                    - 
                                                                    s V_init_fft_i));
      (*-1 0*) F_max0_ge_0 (511 - s V_init_fft_i)]
     ((128 # 1) + s V_init_fft_z + max0(512 - s V_init_fft_i) <= z)%Q
   | 12 => ((128 # 1) + s V_init_fft_z <= z)%Q
   | 13 => (s V_init_fft_z + max0(128 - s V_init_fft_i) <= z)%Q
   | 14 => (s V_init_fft_z + max0(128 - s V_init_fft_i) <= z)%Q
   | 15 => (s V_init_fft_z + max0(128 - s V_init_fft_i) <= z)%Q
   | 16 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (128 - s V_init_fft_i) (127
                                                                    - 
                                                                    s V_init_fft_i));
      (*-1 0*) F_max0_ge_0 (127 - s V_init_fft_i)]
     (s V_init_fft_z + max0(128 - s V_init_fft_i) <= z)%Q
   | 17 => (s V_init_fft_z <= z)%Q
   | 18 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (128 - s V_init_fft_i) (1)]
     (s V_init_fft_z + max0(128 - s V_init_fft_i) <= z)%Q
   | 19 => ((1 # 1) + s V_init_fft_z + max0(127 - s V_init_fft_i) <= z)%Q
   | 20 => ((1 # 1) + s V_init_fft_z + max0(127 - s V_init_fft_i) <= z)%Q
   | 21 => ((1 # 1) + s V_init_fft_z + max0(128 - s V_init_fft_i) <= z)%Q
   | 22 => ((1 # 1) + s V_init_fft_z + max0(128 - s V_init_fft_i) <= z)%Q
   | 23 => ((1 # 1) + s V_init_fft_z + max0(128 - s V_init_fft_i) <= z)%Q
   | 24 => (s V_init_fft_z + max0(128 - s V_init_fft_i) <= z)%Q
   | 25 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (512 - s V_init_fft_i) (1)]
     ((128 # 1) + s V_init_fft_z + max0(512 - s V_init_fft_i) <= z)%Q
   | 26 => ((129 # 1) + s V_init_fft_z + max0(511 - s V_init_fft_i) <= z)%Q
   | 27 => ((129 # 1) + s V_init_fft_z + max0(511 - s V_init_fft_i) <= z)%Q
   | 28 => ((129 # 1) + s V_init_fft_z + max0(512 - s V_init_fft_i) <= z)%Q
   | 29 => ((129 # 1) + s V_init_fft_z + max0(512 - s V_init_fft_i) <= z)%Q
   | 30 => ((129 # 1) + s V_init_fft_z + max0(512 - s V_init_fft_i) <= z)%Q
   | 31 => ((128 # 1) + s V_init_fft_z + max0(512 - s V_init_fft_i) <= z)%Q
   | 32 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (4 - s V_init_fft_i) (1)]
     ((640 # 1) + s V_init_fft_z + max0(4 - s V_init_fft_i) <= z)%Q
   | 33 => ((641 # 1) + s V_init_fft_z + max0(3 - s V_init_fft_i) <= z)%Q
   | 34 => ((641 # 1) + s V_init_fft_z + max0(3 - s V_init_fft_i) <= z)%Q
   | 35 => ((641 # 1) + s V_init_fft_z + max0(4 - s V_init_fft_i) <= z)%Q
   | 36 => ((641 # 1) + s V_init_fft_z + max0(4 - s V_init_fft_i) <= z)%Q
   | 37 => ((641 # 1) + s V_init_fft_z + max0(4 - s V_init_fft_i) <= z)%Q
   | 38 => ((640 # 1) + s V_init_fft_z + max0(4 - s V_init_fft_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_init_fft =>
    [mkPA Q (fun n z s => ai_init_fft n s /\ annot0_init_fft n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_init_fft (proc_start P_init_fft) s1 (proc_end P_init_fft) s2 ->
    (s2 V_init_fft_z <= (644 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_init_fft.
Qed.
