Require Import pasta.Pasta.

Inductive proc: Type :=
  P_audio_play.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_audio_play_z := 1%positive.
Notation V_audio_play__tmp := 2%positive.
Notation V_audio_play_dev_fd := 3%positive.
Notation V_audio_play_i := 4%positive.
Notation V_audio_play_linear_fd := 5%positive.
Notation V_audio_play_data := 6%positive.
Notation V_audio_play_n := 7%positive.
Definition Pedges_audio_play: list (edge proc) :=
  (EA 1 (AAssign V_audio_play_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_audio_play__tmp (Some (EVar V_audio_play_n))) 3)::(EA 3 AWeaken 4)::
  (EA 4 (AGuard (fun s => ((eval (EVar V_audio_play__tmp) s) >
  (eval (ENum (0)) s))%Z)) 6)::(EA 4 (AGuard
  (fun s => ((eval (EVar V_audio_play__tmp) s) <= (eval (ENum (0))
  s))%Z)) 5)::(EA 5 AWeaken 27)::(EA 6 AWeaken 7)::(EA 7 ANone 36)::
  (EA 7 ANone 8)::(EA 8 (AAssign V_audio_play_i (Some (ENum (0)))) 9)::
  (EA 9 ANone 10)::(EA 10 AWeaken 11)::(EA 11 (AGuard
  (fun s => ((eval (EVar V_audio_play_i) s) < (eval (EVar V_audio_play__tmp)
  s))%Z)) 29)::(EA 11 (AGuard (fun s => ((eval (EVar V_audio_play_i) s) >=
  (eval (EVar V_audio_play__tmp) s))%Z)) 12)::(EA 12 AWeaken 13)::
  (EA 13 (AGuard (fun s => ((eval (EVar V_audio_play_linear_fd) s) >=
  (eval (ENum (0)) s))%Z)) 15)::(EA 13 (AGuard
  (fun s => ((eval (EVar V_audio_play_linear_fd) s) < (eval (ENum (0))
  s))%Z)) 14)::(EA 14 AWeaken 20)::(EA 15 AWeaken 16)::(EA 16 ANone 17)::
  (EA 16 ANone 18)::(EA 17 ANone 18)::(EA 18 ANone 19)::(EA 19 AWeaken 20)::
  (EA 20 (AGuard (fun s => ((eval (EVar V_audio_play_dev_fd) s) >=
  (eval (ENum (0)) s))%Z)) 22)::(EA 20 (AGuard
  (fun s => ((eval (EVar V_audio_play_dev_fd) s) < (eval (ENum (0))
  s))%Z)) 21)::(EA 21 AWeaken 26)::(EA 22 AWeaken 23)::(EA 23 ANone 24)::
  (EA 23 ANone 25)::(EA 24 ANone 25)::(EA 25 ANone 26)::(EA 26 ANone 27)::
  (EA 27 ANone 28)::(EA 28 AWeaken 37)::(EA 29 AWeaken 30)::
  (EA 30 ANone 31)::(EA 31 (AAssign V_audio_play_i
  (Some (EAdd (EVar V_audio_play_i) (ENum (1))))) 32)::(EA 32 ANone 33)::
  (EA 33 ANone 34)::(EA 34 (AAssign V_audio_play_z (Some (EAdd (ENum (1))
  (EVar V_audio_play_z)))) 35)::(EA 35 AWeaken 11)::(EA 36 AWeaken 37)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_audio_play => Pedges_audio_play
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_audio_play => 37
     end)%positive;
  var_global := var_global
}.

Definition ai_audio_play (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_audio_play_z <= 0 /\ -1 * s V_audio_play_z <= 0)%Z
   | 3 => (-1 * s V_audio_play_z <= 0 /\ 1 * s V_audio_play_z <= 0)%Z
   | 4 => (1 * s V_audio_play_z <= 0 /\ -1 * s V_audio_play_z <= 0)%Z
   | 5 => (-1 * s V_audio_play_z <= 0 /\ 1 * s V_audio_play_z <= 0 /\ 1 * s V_audio_play__tmp <= 0)%Z
   | 6 => (-1 * s V_audio_play_z <= 0 /\ 1 * s V_audio_play_z <= 0 /\ -1 * s V_audio_play__tmp + 1 <= 0)%Z
   | 7 => (-1 * s V_audio_play__tmp + 1 <= 0 /\ 1 * s V_audio_play_z <= 0 /\ -1 * s V_audio_play_z <= 0)%Z
   | 8 => (-1 * s V_audio_play_z <= 0 /\ 1 * s V_audio_play_z <= 0 /\ -1 * s V_audio_play__tmp + 1 <= 0)%Z
   | 9 => (-1 * s V_audio_play__tmp + 1 <= 0 /\ 1 * s V_audio_play_z <= 0 /\ -1 * s V_audio_play_z <= 0 /\ 1 * s V_audio_play_i <= 0 /\ -1 * s V_audio_play_i <= 0)%Z
   | 10 => (-1 * s V_audio_play_i <= 0 /\ 1 * s V_audio_play_i <= 0 /\ -1 * s V_audio_play_z <= 0 /\ 1 * s V_audio_play_z <= 0 /\ -1 * s V_audio_play__tmp + 1 <= 0)%Z
   | 11 => (-1 * s V_audio_play__tmp + 1 <= 0 /\ -1 * s V_audio_play_z <= 0 /\ -1 * s V_audio_play_i <= 0 /\ -1 * s V_audio_play__tmp+ 1 * s V_audio_play_i <= 0)%Z
   | 12 => (-1 * s V_audio_play__tmp+ 1 * s V_audio_play_i <= 0 /\ -1 * s V_audio_play_z <= 0 /\ -1 * s V_audio_play__tmp + 1 <= 0 /\ 1 * s V_audio_play__tmp+ -1 * s V_audio_play_i <= 0)%Z
   | 13 => (1 * s V_audio_play__tmp+ -1 * s V_audio_play_i <= 0 /\ -1 * s V_audio_play__tmp + 1 <= 0 /\ -1 * s V_audio_play_z <= 0 /\ -1 * s V_audio_play__tmp+ 1 * s V_audio_play_i <= 0)%Z
   | 14 => (-1 * s V_audio_play__tmp+ 1 * s V_audio_play_i <= 0 /\ -1 * s V_audio_play_z <= 0 /\ -1 * s V_audio_play__tmp + 1 <= 0 /\ 1 * s V_audio_play__tmp+ -1 * s V_audio_play_i <= 0 /\ 1 * s V_audio_play_linear_fd + 1 <= 0)%Z
   | 15 => (-1 * s V_audio_play__tmp+ 1 * s V_audio_play_i <= 0 /\ -1 * s V_audio_play_z <= 0 /\ -1 * s V_audio_play__tmp + 1 <= 0 /\ 1 * s V_audio_play__tmp+ -1 * s V_audio_play_i <= 0 /\ -1 * s V_audio_play_linear_fd <= 0)%Z
   | 16 => (-1 * s V_audio_play_linear_fd <= 0 /\ 1 * s V_audio_play__tmp+ -1 * s V_audio_play_i <= 0 /\ -1 * s V_audio_play__tmp + 1 <= 0 /\ -1 * s V_audio_play_z <= 0 /\ -1 * s V_audio_play__tmp+ 1 * s V_audio_play_i <= 0)%Z
   | 17 => (-1 * s V_audio_play__tmp+ 1 * s V_audio_play_i <= 0 /\ -1 * s V_audio_play_z <= 0 /\ -1 * s V_audio_play__tmp + 1 <= 0 /\ 1 * s V_audio_play__tmp+ -1 * s V_audio_play_i <= 0 /\ -1 * s V_audio_play_linear_fd <= 0)%Z
   | 18 => (-1 * s V_audio_play_linear_fd <= 0 /\ 1 * s V_audio_play__tmp+ -1 * s V_audio_play_i <= 0 /\ -1 * s V_audio_play__tmp + 1 <= 0 /\ -1 * s V_audio_play_z <= 0 /\ -1 * s V_audio_play__tmp+ 1 * s V_audio_play_i <= 0)%Z
   | 19 => (-1 * s V_audio_play__tmp+ 1 * s V_audio_play_i <= 0 /\ -1 * s V_audio_play_z <= 0 /\ -1 * s V_audio_play__tmp + 1 <= 0 /\ 1 * s V_audio_play__tmp+ -1 * s V_audio_play_i <= 0 /\ -1 * s V_audio_play_linear_fd <= 0)%Z
   | 20 => (1 * s V_audio_play__tmp+ -1 * s V_audio_play_i <= 0 /\ -1 * s V_audio_play__tmp + 1 <= 0 /\ -1 * s V_audio_play_z <= 0 /\ -1 * s V_audio_play__tmp+ 1 * s V_audio_play_i <= 0)%Z
   | 21 => (-1 * s V_audio_play__tmp+ 1 * s V_audio_play_i <= 0 /\ -1 * s V_audio_play_z <= 0 /\ -1 * s V_audio_play__tmp + 1 <= 0 /\ 1 * s V_audio_play__tmp+ -1 * s V_audio_play_i <= 0 /\ 1 * s V_audio_play_dev_fd + 1 <= 0)%Z
   | 22 => (-1 * s V_audio_play__tmp+ 1 * s V_audio_play_i <= 0 /\ -1 * s V_audio_play_z <= 0 /\ -1 * s V_audio_play__tmp + 1 <= 0 /\ 1 * s V_audio_play__tmp+ -1 * s V_audio_play_i <= 0 /\ -1 * s V_audio_play_dev_fd <= 0)%Z
   | 23 => (-1 * s V_audio_play_dev_fd <= 0 /\ 1 * s V_audio_play__tmp+ -1 * s V_audio_play_i <= 0 /\ -1 * s V_audio_play__tmp + 1 <= 0 /\ -1 * s V_audio_play_z <= 0 /\ -1 * s V_audio_play__tmp+ 1 * s V_audio_play_i <= 0)%Z
   | 24 => (-1 * s V_audio_play__tmp+ 1 * s V_audio_play_i <= 0 /\ -1 * s V_audio_play_z <= 0 /\ -1 * s V_audio_play__tmp + 1 <= 0 /\ 1 * s V_audio_play__tmp+ -1 * s V_audio_play_i <= 0 /\ -1 * s V_audio_play_dev_fd <= 0)%Z
   | 25 => (-1 * s V_audio_play_dev_fd <= 0 /\ 1 * s V_audio_play__tmp+ -1 * s V_audio_play_i <= 0 /\ -1 * s V_audio_play__tmp + 1 <= 0 /\ -1 * s V_audio_play_z <= 0 /\ -1 * s V_audio_play__tmp+ 1 * s V_audio_play_i <= 0)%Z
   | 26 => (-1 * s V_audio_play__tmp+ 1 * s V_audio_play_i <= 0 /\ -1 * s V_audio_play_z <= 0 /\ -1 * s V_audio_play__tmp + 1 <= 0 /\ 1 * s V_audio_play__tmp+ -1 * s V_audio_play_i <= 0)%Z
   | 27 => (-1 * s V_audio_play_z <= 0)%Z
   | 28 => (-1 * s V_audio_play_z <= 0)%Z
   | 29 => (-1 * s V_audio_play_i <= 0 /\ -1 * s V_audio_play_z <= 0 /\ -1 * s V_audio_play__tmp + 1 <= 0 /\ -1 * s V_audio_play__tmp+ 1 * s V_audio_play_i + 1 <= 0)%Z
   | 30 => (-1 * s V_audio_play__tmp+ 1 * s V_audio_play_i + 1 <= 0 /\ -1 * s V_audio_play__tmp + 1 <= 0 /\ -1 * s V_audio_play_z <= 0 /\ -1 * s V_audio_play_i <= 0)%Z
   | 31 => (-1 * s V_audio_play_i <= 0 /\ -1 * s V_audio_play_z <= 0 /\ -1 * s V_audio_play__tmp + 1 <= 0 /\ -1 * s V_audio_play__tmp+ 1 * s V_audio_play_i + 1 <= 0)%Z
   | 32 => (-1 * s V_audio_play_z <= 0 /\ -1 * s V_audio_play_i + 1 <= 0 /\ -1 * s V_audio_play__tmp+ 1 * s V_audio_play_i <= 0)%Z
   | 33 => (-1 * s V_audio_play__tmp+ 1 * s V_audio_play_i <= 0 /\ -1 * s V_audio_play_i + 1 <= 0 /\ -1 * s V_audio_play_z <= 0)%Z
   | 34 => (-1 * s V_audio_play_z <= 0 /\ -1 * s V_audio_play_i + 1 <= 0 /\ -1 * s V_audio_play__tmp+ 1 * s V_audio_play_i <= 0)%Z
   | 35 => (-1 * s V_audio_play__tmp+ 1 * s V_audio_play_i <= 0 /\ -1 * s V_audio_play_i + 1 <= 0 /\ -1 * s V_audio_play_z + 1 <= 0)%Z
   | 36 => (-1 * s V_audio_play_z <= 0 /\ 1 * s V_audio_play_z <= 0 /\ -1 * s V_audio_play__tmp + 1 <= 0)%Z
   | 37 => (-1 * s V_audio_play_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_audio_play (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_audio_play_n) <= z)%Q
   | 2 => (s V_audio_play_z + max0(s V_audio_play_n) <= z)%Q
   | 3 => (s V_audio_play_z + max0(s V_audio_play__tmp) <= z)%Q
   | 4 => (s V_audio_play_z + max0(s V_audio_play__tmp) <= z)%Q
   | 5 => hints
     [(*-1 0*) F_max0_ge_0 (s V_audio_play__tmp)]
     (s V_audio_play_z + max0(s V_audio_play__tmp) <= z)%Q
   | 6 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_audio_play__tmp)) (F_check_ge (s V_audio_play__tmp) (0))]
     (s V_audio_play_z + max0(s V_audio_play__tmp) <= z)%Q
   | 7 => (s V_audio_play__tmp + s V_audio_play_z <= z)%Q
   | 8 => (s V_audio_play__tmp + s V_audio_play_z <= z)%Q
   | 9 => (s V_audio_play__tmp + s V_audio_play_z - max0(s V_audio_play__tmp)
           + max0(s V_audio_play__tmp - s V_audio_play_i) <= z)%Q
   | 10 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_audio_play__tmp) (0))) (F_max0_ge_0 (s V_audio_play__tmp))]
     (s V_audio_play__tmp + s V_audio_play_z - max0(s V_audio_play__tmp)
      + max0(s V_audio_play__tmp - s V_audio_play_i) <= z)%Q
   | 11 => (s V_audio_play_z + max0(s V_audio_play__tmp - s V_audio_play_i) <= z)%Q
   | 12 => (s V_audio_play_z + max0(s V_audio_play__tmp - s V_audio_play_i) <= z)%Q
   | 13 => (s V_audio_play_z + max0(s V_audio_play__tmp - s V_audio_play_i) <= z)%Q
   | 14 => (s V_audio_play_z + max0(s V_audio_play__tmp - s V_audio_play_i) <= z)%Q
   | 15 => (s V_audio_play_z + max0(s V_audio_play__tmp - s V_audio_play_i) <= z)%Q
   | 16 => (s V_audio_play_z + max0(s V_audio_play__tmp - s V_audio_play_i) <= z)%Q
   | 17 => (s V_audio_play_z + max0(s V_audio_play__tmp - s V_audio_play_i) <= z)%Q
   | 18 => (s V_audio_play_z + max0(s V_audio_play__tmp - s V_audio_play_i) <= z)%Q
   | 19 => (s V_audio_play_z + max0(s V_audio_play__tmp - s V_audio_play_i) <= z)%Q
   | 20 => (s V_audio_play_z + max0(s V_audio_play__tmp - s V_audio_play_i) <= z)%Q
   | 21 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_audio_play__tmp
                                             - s V_audio_play_i) (-1
                                                                  + s V_audio_play__tmp
                                                                  - s V_audio_play_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V_audio_play__tmp - s V_audio_play_i)]
     (s V_audio_play_z + max0(s V_audio_play__tmp - s V_audio_play_i) <= z)%Q
   | 22 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_audio_play__tmp
                                             - s V_audio_play_i) (-1
                                                                  + s V_audio_play__tmp
                                                                  - s V_audio_play_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V_audio_play__tmp - s V_audio_play_i)]
     (s V_audio_play_z + max0(s V_audio_play__tmp - s V_audio_play_i) <= z)%Q
   | 23 => (s V_audio_play_z <= z)%Q
   | 24 => (s V_audio_play_z <= z)%Q
   | 25 => (s V_audio_play_z <= z)%Q
   | 26 => (s V_audio_play_z <= z)%Q
   | 27 => (s V_audio_play_z <= z)%Q
   | 28 => (s V_audio_play_z <= z)%Q
   | 29 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_audio_play__tmp - s V_audio_play_i) (1)]
     (s V_audio_play_z + max0(s V_audio_play__tmp - s V_audio_play_i) <= z)%Q
   | 30 => ((1 # 1) + s V_audio_play_z
            + max0(-1 + s V_audio_play__tmp - s V_audio_play_i) <= z)%Q
   | 31 => ((1 # 1) + s V_audio_play_z
            + max0(-1 + s V_audio_play__tmp - s V_audio_play_i) <= z)%Q
   | 32 => ((1 # 1) + s V_audio_play_z
            + max0(s V_audio_play__tmp - s V_audio_play_i) <= z)%Q
   | 33 => ((1 # 1) + s V_audio_play_z
            + max0(s V_audio_play__tmp - s V_audio_play_i) <= z)%Q
   | 34 => ((1 # 1) + s V_audio_play_z
            + max0(s V_audio_play__tmp - s V_audio_play_i) <= z)%Q
   | 35 => (s V_audio_play_z + max0(s V_audio_play__tmp - s V_audio_play_i) <= z)%Q
   | 36 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_audio_play__tmp)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_audio_play__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_audio_play__tmp))]
     (s V_audio_play__tmp + s V_audio_play_z <= z)%Q
   | 37 => (s V_audio_play_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_audio_play =>
    [mkPA Q (fun n z s => ai_audio_play n s /\ annot0_audio_play n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_audio_play (proc_start P_audio_play) s1 (proc_end P_audio_play) s2 ->
    (s2 V_audio_play_z <= max0(s1 V_audio_play_n))%Q.
Proof.
  prove_bound ipa admissible_ipa P_audio_play.
Qed.
