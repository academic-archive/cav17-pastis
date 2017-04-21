Require Import pasta.Pasta.

Inductive proc: Type :=
  P_read_samples_mp3.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_read_samples_mp3_z := 1%positive.
Notation V_read_samples_mp3__tmp := 2%positive.
Notation V_read_samples_mp3__tmp1 := 3%positive.
Notation V_read_samples_mp3_j := 4%positive.
Notation V_read_samples_mp3_out := 5%positive.
Notation V_read_samples_mp3_gfp := 6%positive.
Notation V_read_samples_mp3_mpg123pcm := 7%positive.
Notation V_read_samples_mp3_musicin := 8%positive.
Notation V_read_samples_mp3_stereo := 9%positive.
Definition Pedges_read_samples_mp3: list (edge proc) :=
  (EA 1 (AAssign V_read_samples_mp3_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_read_samples_mp3__tmp1 (Some (EVar V_read_samples_mp3_stereo))) 3)::
  (EA 3 (AAssign V_read_samples_mp3_out (Some (ENum (0)))) 4)::(EA 4 (AAssign
  V_read_samples_mp3_out None) 5)::(EA 5 AWeaken 6)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_read_samples_mp3_out) s) = (eval (ENum (-1))
  s))%Z)) 8)::(EA 6 (AGuard (fun s => ((eval (EVar V_read_samples_mp3_out)
  s) <> (eval (ENum (-1)) s))%Z)) 7)::(EA 7 AWeaken 16)::(EA 8 AWeaken 9)::
  (EA 9 (AAssign V_read_samples_mp3_j (Some (ENum (0)))) 10)::
  (EA 10 ANone 11)::(EA 11 AWeaken 12)::(EA 12 (AGuard
  (fun s => ((eval (EVar V_read_samples_mp3_j) s) < (eval (ENum (1152))
  s))%Z)) 26)::(EA 12 (AGuard (fun s => ((eval (EVar V_read_samples_mp3_j)
  s) >= (eval (ENum (1152)) s))%Z)) 13)::(EA 13 AWeaken 14)::
  (EA 14 ANone 15)::(EA 15 AWeaken 16)::(EA 16 (AGuard
  (fun s => ((eval (EVar V_read_samples_mp3_out) s) = (eval (ENum (-1))
  s))%Z)) 21)::(EA 16 (AGuard (fun s => ((eval (EVar V_read_samples_mp3_out)
  s) <> (eval (ENum (-1)) s))%Z)) 17)::(EA 17 AWeaken 18)::(EA 18 (AAssign
  V_read_samples_mp3__tmp (Some (EVar V_read_samples_mp3_out))) 19)::
  (EA 19 ANone 20)::(EA 20 AWeaken 25)::(EA 21 AWeaken 22)::(EA 22 (AAssign
  V_read_samples_mp3__tmp (Some (ENum (0)))) 23)::(EA 23 ANone 24)::
  (EA 24 AWeaken 25)::(EA 26 AWeaken 27)::(EA 27 ANone 28)::(EA 28 (AAssign
  V_read_samples_mp3_j (Some (EAdd (EVar V_read_samples_mp3_j)
  (ENum (1))))) 29)::(EA 29 ANone 30)::(EA 30 ANone 31)::(EA 31 (AAssign
  V_read_samples_mp3_z (Some (EAdd (ENum (1))
  (EVar V_read_samples_mp3_z)))) 32)::(EA 32 AWeaken 12)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_read_samples_mp3 => Pedges_read_samples_mp3
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_read_samples_mp3 => 25
     end)%positive;
  var_global := var_global
}.

Definition ai_read_samples_mp3 (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_read_samples_mp3_z <= 0 /\ -1 * s V_read_samples_mp3_z <= 0)%Z
   | 3 => (-1 * s V_read_samples_mp3_z <= 0 /\ 1 * s V_read_samples_mp3_z <= 0)%Z
   | 4 => (1 * s V_read_samples_mp3_z <= 0 /\ -1 * s V_read_samples_mp3_z <= 0 /\ 1 * s V_read_samples_mp3_out <= 0 /\ -1 * s V_read_samples_mp3_out <= 0)%Z
   | 5 => (-1 * s V_read_samples_mp3_z <= 0 /\ 1 * s V_read_samples_mp3_z <= 0)%Z
   | 6 => (1 * s V_read_samples_mp3_z <= 0 /\ -1 * s V_read_samples_mp3_z <= 0)%Z
   | 7 => (-1 * s V_read_samples_mp3_z <= 0 /\ 1 * s V_read_samples_mp3_z <= 0)%Z
   | 8 => (-1 * s V_read_samples_mp3_z <= 0 /\ 1 * s V_read_samples_mp3_z <= 0 /\ 1 * s V_read_samples_mp3_out + 1 <= 0 /\ -1 * s V_read_samples_mp3_out + -1 <= 0)%Z
   | 9 => (-1 * s V_read_samples_mp3_out + -1 <= 0 /\ 1 * s V_read_samples_mp3_out + 1 <= 0 /\ 1 * s V_read_samples_mp3_z <= 0 /\ -1 * s V_read_samples_mp3_z <= 0)%Z
   | 10 => (-1 * s V_read_samples_mp3_z <= 0 /\ 1 * s V_read_samples_mp3_z <= 0 /\ 1 * s V_read_samples_mp3_out + 1 <= 0 /\ -1 * s V_read_samples_mp3_out + -1 <= 0 /\ 1 * s V_read_samples_mp3_j <= 0 /\ -1 * s V_read_samples_mp3_j <= 0)%Z
   | 11 => (-1 * s V_read_samples_mp3_j <= 0 /\ 1 * s V_read_samples_mp3_j <= 0 /\ -1 * s V_read_samples_mp3_out + -1 <= 0 /\ 1 * s V_read_samples_mp3_out + 1 <= 0 /\ 1 * s V_read_samples_mp3_z <= 0 /\ -1 * s V_read_samples_mp3_z <= 0)%Z
   | 12 => (-1 * s V_read_samples_mp3_z <= 0 /\ -1 * s V_read_samples_mp3_j <= 0 /\ 1 * s V_read_samples_mp3_out + 1 <= 0 /\ -1 * s V_read_samples_mp3_out + -1 <= 0 /\ 1 * s V_read_samples_mp3_j + -1152 <= 0)%Z
   | 13 => (1 * s V_read_samples_mp3_j + -1152 <= 0 /\ -1 * s V_read_samples_mp3_out + -1 <= 0 /\ 1 * s V_read_samples_mp3_out + 1 <= 0 /\ -1 * s V_read_samples_mp3_z <= 0 /\ -1 * s V_read_samples_mp3_j + 1152 <= 0)%Z
   | 14 => (-1 * s V_read_samples_mp3_j + 1152 <= 0 /\ -1 * s V_read_samples_mp3_z <= 0 /\ 1 * s V_read_samples_mp3_out + 1 <= 0 /\ -1 * s V_read_samples_mp3_out + -1 <= 0 /\ 1 * s V_read_samples_mp3_j + -1152 <= 0)%Z
   | 15 => (1 * s V_read_samples_mp3_j + -1152 <= 0 /\ -1 * s V_read_samples_mp3_out + -1 <= 0 /\ 1 * s V_read_samples_mp3_out + 1 <= 0 /\ -1 * s V_read_samples_mp3_z <= 0 /\ -1 * s V_read_samples_mp3_j + 1152 <= 0)%Z
   | 16 => (-1 * s V_read_samples_mp3_z <= 0)%Z
   | 17 => (-1 * s V_read_samples_mp3_z <= 0)%Z
   | 18 => (-1 * s V_read_samples_mp3_z <= 0)%Z
   | 19 => (-1 * s V_read_samples_mp3_z <= 0)%Z
   | 20 => (-1 * s V_read_samples_mp3_z <= 0)%Z
   | 21 => (-1 * s V_read_samples_mp3_z <= 0 /\ 1 * s V_read_samples_mp3_out + 1 <= 0 /\ -1 * s V_read_samples_mp3_out + -1 <= 0)%Z
   | 22 => (-1 * s V_read_samples_mp3_out + -1 <= 0 /\ 1 * s V_read_samples_mp3_out + 1 <= 0 /\ -1 * s V_read_samples_mp3_z <= 0)%Z
   | 23 => (-1 * s V_read_samples_mp3_z <= 0 /\ 1 * s V_read_samples_mp3_out + 1 <= 0 /\ -1 * s V_read_samples_mp3_out + -1 <= 0 /\ 1 * s V_read_samples_mp3__tmp <= 0 /\ -1 * s V_read_samples_mp3__tmp <= 0)%Z
   | 24 => (-1 * s V_read_samples_mp3__tmp <= 0 /\ 1 * s V_read_samples_mp3__tmp <= 0 /\ -1 * s V_read_samples_mp3_out + -1 <= 0 /\ 1 * s V_read_samples_mp3_out + 1 <= 0 /\ -1 * s V_read_samples_mp3_z <= 0)%Z
   | 25 => (-1 * s V_read_samples_mp3_z <= 0)%Z
   | 26 => (-1 * s V_read_samples_mp3_out + -1 <= 0 /\ 1 * s V_read_samples_mp3_out + 1 <= 0 /\ -1 * s V_read_samples_mp3_j <= 0 /\ -1 * s V_read_samples_mp3_z <= 0 /\ 1 * s V_read_samples_mp3_j + -1151 <= 0)%Z
   | 27 => (1 * s V_read_samples_mp3_j + -1151 <= 0 /\ -1 * s V_read_samples_mp3_z <= 0 /\ -1 * s V_read_samples_mp3_j <= 0 /\ 1 * s V_read_samples_mp3_out + 1 <= 0 /\ -1 * s V_read_samples_mp3_out + -1 <= 0)%Z
   | 28 => (-1 * s V_read_samples_mp3_out + -1 <= 0 /\ 1 * s V_read_samples_mp3_out + 1 <= 0 /\ -1 * s V_read_samples_mp3_j <= 0 /\ -1 * s V_read_samples_mp3_z <= 0 /\ 1 * s V_read_samples_mp3_j + -1151 <= 0)%Z
   | 29 => (-1 * s V_read_samples_mp3_z <= 0 /\ 1 * s V_read_samples_mp3_out + 1 <= 0 /\ -1 * s V_read_samples_mp3_out + -1 <= 0 /\ -1 * s V_read_samples_mp3_j + 1 <= 0 /\ 1 * s V_read_samples_mp3_j + -1152 <= 0)%Z
   | 30 => (1 * s V_read_samples_mp3_j + -1152 <= 0 /\ -1 * s V_read_samples_mp3_j + 1 <= 0 /\ -1 * s V_read_samples_mp3_out + -1 <= 0 /\ 1 * s V_read_samples_mp3_out + 1 <= 0 /\ -1 * s V_read_samples_mp3_z <= 0)%Z
   | 31 => (-1 * s V_read_samples_mp3_z <= 0 /\ 1 * s V_read_samples_mp3_out + 1 <= 0 /\ -1 * s V_read_samples_mp3_out + -1 <= 0 /\ -1 * s V_read_samples_mp3_j + 1 <= 0 /\ 1 * s V_read_samples_mp3_j + -1152 <= 0)%Z
   | 32 => (1 * s V_read_samples_mp3_j + -1152 <= 0 /\ -1 * s V_read_samples_mp3_j + 1 <= 0 /\ -1 * s V_read_samples_mp3_out + -1 <= 0 /\ 1 * s V_read_samples_mp3_out + 1 <= 0 /\ -1 * s V_read_samples_mp3_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_read_samples_mp3 (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((1152 # 1) <= z)%Q
   | 2 => ((1152 # 1) <= z)%Q
   | 3 => ((1152 # 1) <= z)%Q
   | 4 => ((1152 # 1) <= z)%Q
   | 5 => ((1152 # 1) <= z)%Q
   | 6 => ((1152 # 1) <= z)%Q
   | 7 => hints
     [(*-1152 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_read_samples_mp3_z)) (F_check_ge (0) (0));
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_read_samples_mp3_z) (0))) (F_max0_ge_0 (-
                                                                    s V_read_samples_mp3_z))]
     ((1152 # 1) <= z)%Q
   | 8 => ((1152 # 1) <= z)%Q
   | 9 => ((1152 # 1) <= z)%Q
   | 10 => ((1152 # 1) - s V_read_samples_mp3_j <= z)%Q
   | 11 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_read_samples_mp3_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_read_samples_mp3_z) (0))) (F_max0_ge_0 (-
                                                                    s V_read_samples_mp3_z))]
     ((1152 # 1) - s V_read_samples_mp3_j <= z)%Q
   | 12 => ((1152 # 1) - s V_read_samples_mp3_j + s V_read_samples_mp3_z <= z)%Q
   | 13 => ((1152 # 1) - s V_read_samples_mp3_j + s V_read_samples_mp3_z <= z)%Q
   | 14 => ((1152 # 1) - s V_read_samples_mp3_j + s V_read_samples_mp3_z <= z)%Q
   | 15 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (1152 - s V_read_samples_mp3_j) (1151
                                                                    - s V_read_samples_mp3_j));
      (*-1 0*) F_max0_ge_0 (1151 - s V_read_samples_mp3_j);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1152
                                                               - s V_read_samples_mp3_j) (0))) (F_max0_ge_0 (1152
                                                                    - s V_read_samples_mp3_j))]
     ((1152 # 1) - s V_read_samples_mp3_j + s V_read_samples_mp3_z <= z)%Q
   | 16 => (s V_read_samples_mp3_z <= z)%Q
   | 17 => (s V_read_samples_mp3_z <= z)%Q
   | 18 => (s V_read_samples_mp3_z <= z)%Q
   | 19 => (s V_read_samples_mp3_z <= z)%Q
   | 20 => (s V_read_samples_mp3_z <= z)%Q
   | 21 => (s V_read_samples_mp3_z <= z)%Q
   | 22 => (s V_read_samples_mp3_z <= z)%Q
   | 23 => (s V_read_samples_mp3_z <= z)%Q
   | 24 => (s V_read_samples_mp3_z <= z)%Q
   | 25 => (s V_read_samples_mp3_z <= z)%Q
   | 26 => ((1152 # 1) - s V_read_samples_mp3_j + s V_read_samples_mp3_z <= z)%Q
   | 27 => ((1152 # 1) - s V_read_samples_mp3_j + s V_read_samples_mp3_z <= z)%Q
   | 28 => ((1152 # 1) - s V_read_samples_mp3_j + s V_read_samples_mp3_z <= z)%Q
   | 29 => ((1153 # 1) - s V_read_samples_mp3_j + s V_read_samples_mp3_z <= z)%Q
   | 30 => ((1153 # 1) - s V_read_samples_mp3_j + s V_read_samples_mp3_z <= z)%Q
   | 31 => ((1153 # 1) - s V_read_samples_mp3_j + s V_read_samples_mp3_z <= z)%Q
   | 32 => ((1152 # 1) - s V_read_samples_mp3_j + s V_read_samples_mp3_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_read_samples_mp3 =>
    [mkPA Q (fun n z s => ai_read_samples_mp3 n s /\ annot0_read_samples_mp3 n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_read_samples_mp3 (proc_start P_read_samples_mp3) s1 (proc_end P_read_samples_mp3) s2 ->
    (s2 V_read_samples_mp3_z <= (1152 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_read_samples_mp3.
Qed.
