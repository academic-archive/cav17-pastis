Require Import pasta.Pasta.

Inductive proc: Type :=
  P_init_error_limit.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_init_error_limit_z := 1%positive.
Notation V_init_error_limit_in := 2%positive.
Notation V_init_error_limit_out := 3%positive.
Notation V_init_error_limit_cinfo := 4%positive.
Definition Pedges_init_error_limit: list (edge proc) :=
  (EA 1 (AAssign V_init_error_limit_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_init_error_limit_out (Some (ENum (0)))) 3)::(EA 3 (AAssign
  V_init_error_limit_in (Some (ENum (0)))) 4)::(EA 4 ANone 5)::
  (EA 5 AWeaken 6)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_init_error_limit_in) s) < (eval (ENum (16))
  s))%Z)) 32)::(EA 6 (AGuard (fun s => ((eval (EVar V_init_error_limit_in)
  s) >= (eval (ENum (16)) s))%Z)) 7)::(EA 7 AWeaken 8)::(EA 8 ANone 9)::
  (EA 9 AWeaken 10)::(EA 10 (AGuard
  (fun s => ((eval (EVar V_init_error_limit_in) s) < (eval (ENum (48))
  s))%Z)) 24)::(EA 10 (AGuard (fun s => ((eval (EVar V_init_error_limit_in)
  s) >= (eval (ENum (48)) s))%Z)) 11)::(EA 11 AWeaken 12)::(EA 12 ANone 13)::
  (EA 13 AWeaken 14)::(EA 14 (AGuard
  (fun s => ((eval (EVar V_init_error_limit_in) s) <= (eval (ENum (255))
  s))%Z)) 17)::(EA 14 (AGuard (fun s => ((eval (EVar V_init_error_limit_in)
  s) > (eval (ENum (255)) s))%Z)) 15)::(EA 15 AWeaken 16)::
  (EA 17 AWeaken 18)::(EA 18 ANone 19)::(EA 19 (AAssign V_init_error_limit_in
  (Some (EAdd (EVar V_init_error_limit_in) (ENum (1))))) 20)::
  (EA 20 ANone 21)::(EA 21 ANone 22)::(EA 22 (AAssign V_init_error_limit_z
  (Some (EAdd (ENum (1)) (EVar V_init_error_limit_z)))) 23)::
  (EA 23 AWeaken 14)::(EA 24 AWeaken 25)::(EA 25 ANone 26)::(EA 26 (AAssign
  V_init_error_limit_in (Some (EAdd (EVar V_init_error_limit_in)
  (ENum (1))))) 27)::(EA 27 (AAssign V_init_error_limit_out None) 28)::
  (EA 28 ANone 29)::(EA 29 ANone 30)::(EA 30 (AAssign V_init_error_limit_z
  (Some (EAdd (ENum (1)) (EVar V_init_error_limit_z)))) 31)::
  (EA 31 AWeaken 10)::(EA 32 AWeaken 33)::(EA 33 ANone 34)::(EA 34 (AAssign
  V_init_error_limit_in (Some (EAdd (EVar V_init_error_limit_in)
  (ENum (1))))) 35)::(EA 35 (AAssign V_init_error_limit_out
  (Some (EAdd (EVar V_init_error_limit_out) (ENum (1))))) 36)::
  (EA 36 ANone 37)::(EA 37 ANone 38)::(EA 38 (AAssign V_init_error_limit_z
  (Some (EAdd (ENum (1)) (EVar V_init_error_limit_z)))) 39)::
  (EA 39 AWeaken 6)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_init_error_limit => Pedges_init_error_limit
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_init_error_limit => 16
     end)%positive;
  var_global := var_global
}.

Definition ai_init_error_limit (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_init_error_limit_z <= 0 /\ -1 * s V_init_error_limit_z <= 0)%Z
   | 3 => (-1 * s V_init_error_limit_z <= 0 /\ 1 * s V_init_error_limit_z <= 0 /\ 1 * s V_init_error_limit_out <= 0 /\ -1 * s V_init_error_limit_out <= 0)%Z
   | 4 => (-1 * s V_init_error_limit_out <= 0 /\ 1 * s V_init_error_limit_out <= 0 /\ 1 * s V_init_error_limit_z <= 0 /\ -1 * s V_init_error_limit_z <= 0 /\ 1 * s V_init_error_limit_in <= 0 /\ -1 * s V_init_error_limit_in <= 0)%Z
   | 5 => (-1 * s V_init_error_limit_in <= 0 /\ 1 * s V_init_error_limit_in <= 0 /\ -1 * s V_init_error_limit_z <= 0 /\ 1 * s V_init_error_limit_z <= 0 /\ 1 * s V_init_error_limit_out <= 0 /\ -1 * s V_init_error_limit_out <= 0)%Z
   | 6 => (-1 * s V_init_error_limit_out <= 0 /\ -1 * s V_init_error_limit_z <= 0 /\ -1 * s V_init_error_limit_in <= 0 /\ 1 * s V_init_error_limit_in + -16 <= 0)%Z
   | 7 => (1 * s V_init_error_limit_in + -16 <= 0 /\ -1 * s V_init_error_limit_z <= 0 /\ -1 * s V_init_error_limit_out <= 0 /\ -1 * s V_init_error_limit_in + 16 <= 0)%Z
   | 8 => (-1 * s V_init_error_limit_in + 16 <= 0 /\ -1 * s V_init_error_limit_out <= 0 /\ -1 * s V_init_error_limit_z <= 0 /\ 1 * s V_init_error_limit_in + -16 <= 0)%Z
   | 9 => (1 * s V_init_error_limit_in + -16 <= 0 /\ -1 * s V_init_error_limit_z <= 0 /\ -1 * s V_init_error_limit_out <= 0 /\ -1 * s V_init_error_limit_in + 16 <= 0)%Z
   | 10 => (-1 * s V_init_error_limit_in + 16 <= 0 /\ -1 * s V_init_error_limit_z <= 0 /\ 1 * s V_init_error_limit_in + -48 <= 0)%Z
   | 11 => (1 * s V_init_error_limit_in + -48 <= 0 /\ -1 * s V_init_error_limit_z <= 0 /\ -1 * s V_init_error_limit_in + 48 <= 0)%Z
   | 12 => (-1 * s V_init_error_limit_in + 48 <= 0 /\ -1 * s V_init_error_limit_z <= 0 /\ 1 * s V_init_error_limit_in + -48 <= 0)%Z
   | 13 => (1 * s V_init_error_limit_in + -48 <= 0 /\ -1 * s V_init_error_limit_z <= 0 /\ -1 * s V_init_error_limit_in + 48 <= 0)%Z
   | 14 => (-1 * s V_init_error_limit_in + 48 <= 0 /\ -1 * s V_init_error_limit_z <= 0 /\ 1 * s V_init_error_limit_in + -256 <= 0)%Z
   | 15 => (1 * s V_init_error_limit_in + -256 <= 0 /\ -1 * s V_init_error_limit_z <= 0 /\ -1 * s V_init_error_limit_in + 256 <= 0)%Z
   | 16 => (-1 * s V_init_error_limit_in + 256 <= 0 /\ -1 * s V_init_error_limit_z <= 0 /\ 1 * s V_init_error_limit_in + -256 <= 0)%Z
   | 17 => (-1 * s V_init_error_limit_z <= 0 /\ -1 * s V_init_error_limit_in + 48 <= 0 /\ 1 * s V_init_error_limit_in + -255 <= 0)%Z
   | 18 => (1 * s V_init_error_limit_in + -255 <= 0 /\ -1 * s V_init_error_limit_in + 48 <= 0 /\ -1 * s V_init_error_limit_z <= 0)%Z
   | 19 => (-1 * s V_init_error_limit_z <= 0 /\ -1 * s V_init_error_limit_in + 48 <= 0 /\ 1 * s V_init_error_limit_in + -255 <= 0)%Z
   | 20 => (-1 * s V_init_error_limit_z <= 0 /\ -1 * s V_init_error_limit_in + 49 <= 0 /\ 1 * s V_init_error_limit_in + -256 <= 0)%Z
   | 21 => (1 * s V_init_error_limit_in + -256 <= 0 /\ -1 * s V_init_error_limit_in + 49 <= 0 /\ -1 * s V_init_error_limit_z <= 0)%Z
   | 22 => (-1 * s V_init_error_limit_z <= 0 /\ -1 * s V_init_error_limit_in + 49 <= 0 /\ 1 * s V_init_error_limit_in + -256 <= 0)%Z
   | 23 => (1 * s V_init_error_limit_in + -256 <= 0 /\ -1 * s V_init_error_limit_in + 49 <= 0 /\ -1 * s V_init_error_limit_z + 1 <= 0)%Z
   | 24 => (-1 * s V_init_error_limit_z <= 0 /\ -1 * s V_init_error_limit_in + 16 <= 0 /\ 1 * s V_init_error_limit_in + -47 <= 0)%Z
   | 25 => (1 * s V_init_error_limit_in + -47 <= 0 /\ -1 * s V_init_error_limit_in + 16 <= 0 /\ -1 * s V_init_error_limit_z <= 0)%Z
   | 26 => (-1 * s V_init_error_limit_z <= 0 /\ -1 * s V_init_error_limit_in + 16 <= 0 /\ 1 * s V_init_error_limit_in + -47 <= 0)%Z
   | 27 => (-1 * s V_init_error_limit_z <= 0 /\ -1 * s V_init_error_limit_in + 17 <= 0 /\ 1 * s V_init_error_limit_in + -48 <= 0)%Z
   | 28 => (1 * s V_init_error_limit_in + -48 <= 0 /\ -1 * s V_init_error_limit_in + 17 <= 0 /\ -1 * s V_init_error_limit_z <= 0)%Z
   | 29 => (-1 * s V_init_error_limit_z <= 0 /\ -1 * s V_init_error_limit_in + 17 <= 0 /\ 1 * s V_init_error_limit_in + -48 <= 0)%Z
   | 30 => (1 * s V_init_error_limit_in + -48 <= 0 /\ -1 * s V_init_error_limit_in + 17 <= 0 /\ -1 * s V_init_error_limit_z <= 0)%Z
   | 31 => (-1 * s V_init_error_limit_in + 17 <= 0 /\ 1 * s V_init_error_limit_in + -48 <= 0 /\ -1 * s V_init_error_limit_z + 1 <= 0)%Z
   | 32 => (-1 * s V_init_error_limit_in <= 0 /\ -1 * s V_init_error_limit_z <= 0 /\ -1 * s V_init_error_limit_out <= 0 /\ 1 * s V_init_error_limit_in + -15 <= 0)%Z
   | 33 => (1 * s V_init_error_limit_in + -15 <= 0 /\ -1 * s V_init_error_limit_out <= 0 /\ -1 * s V_init_error_limit_z <= 0 /\ -1 * s V_init_error_limit_in <= 0)%Z
   | 34 => (-1 * s V_init_error_limit_in <= 0 /\ -1 * s V_init_error_limit_z <= 0 /\ -1 * s V_init_error_limit_out <= 0 /\ 1 * s V_init_error_limit_in + -15 <= 0)%Z
   | 35 => (-1 * s V_init_error_limit_out <= 0 /\ -1 * s V_init_error_limit_z <= 0 /\ -1 * s V_init_error_limit_in + 1 <= 0 /\ 1 * s V_init_error_limit_in + -16 <= 0)%Z
   | 36 => (1 * s V_init_error_limit_in + -16 <= 0 /\ -1 * s V_init_error_limit_in + 1 <= 0 /\ -1 * s V_init_error_limit_z <= 0 /\ -1 * s V_init_error_limit_out + 1 <= 0)%Z
   | 37 => (-1 * s V_init_error_limit_out + 1 <= 0 /\ -1 * s V_init_error_limit_z <= 0 /\ -1 * s V_init_error_limit_in + 1 <= 0 /\ 1 * s V_init_error_limit_in + -16 <= 0)%Z
   | 38 => (1 * s V_init_error_limit_in + -16 <= 0 /\ -1 * s V_init_error_limit_in + 1 <= 0 /\ -1 * s V_init_error_limit_z <= 0 /\ -1 * s V_init_error_limit_out + 1 <= 0)%Z
   | 39 => (-1 * s V_init_error_limit_out + 1 <= 0 /\ -1 * s V_init_error_limit_in + 1 <= 0 /\ 1 * s V_init_error_limit_in + -16 <= 0 /\ -1 * s V_init_error_limit_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_init_error_limit (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((256 # 1) <= z)%Q
   | 2 => ((256 # 1) + s V_init_error_limit_z <= z)%Q
   | 3 => ((256 # 1) + s V_init_error_limit_z <= z)%Q
   | 4 => ((256 # 1) - s V_init_error_limit_in + s V_init_error_limit_z <= z)%Q
   | 5 => ((256 # 1) - s V_init_error_limit_in + s V_init_error_limit_z <= z)%Q
   | 6 => ((256 # 1) - s V_init_error_limit_in + s V_init_error_limit_z <= z)%Q
   | 7 => hints
     [(*-5.40211e-09 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (256
                                                                    - s V_init_error_limit_in) (0))) (F_max0_ge_0 (256
                                                                    - s V_init_error_limit_in))]
     ((256 # 1) - s V_init_error_limit_in + s V_init_error_limit_z <= z)%Q
   | 8 => (-(0 # 1) + s V_init_error_limit_z
           + max0(256 - s V_init_error_limit_in) <= z)%Q
   | 9 => (-(0 # 1) + s V_init_error_limit_z
           + max0(256 - s V_init_error_limit_in) <= z)%Q
   | 10 => (-(0 # 1) + s V_init_error_limit_z
            + max0(256 - s V_init_error_limit_in) <= z)%Q
   | 11 => (-(0 # 1) + s V_init_error_limit_z
            + max0(256 - s V_init_error_limit_in) <= z)%Q
   | 12 => (-(0 # 1) + s V_init_error_limit_z
            + max0(256 - s V_init_error_limit_in) <= z)%Q
   | 13 => (-(0 # 1) + s V_init_error_limit_z
            + max0(256 - s V_init_error_limit_in) <= z)%Q
   | 14 => (s V_init_error_limit_z + max0(256 - s V_init_error_limit_in) <= z)%Q
   | 15 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (256 - s V_init_error_limit_in) (255
                                                                    - s V_init_error_limit_in));
      (*-1 0*) F_max0_ge_0 (255 - s V_init_error_limit_in)]
     (s V_init_error_limit_z + max0(256 - s V_init_error_limit_in) <= z)%Q
   | 16 => (s V_init_error_limit_z <= z)%Q
   | 17 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (256 - s V_init_error_limit_in) (1)]
     (s V_init_error_limit_z + max0(256 - s V_init_error_limit_in) <= z)%Q
   | 18 => ((1 # 1) + s V_init_error_limit_z
            + max0(255 - s V_init_error_limit_in) <= z)%Q
   | 19 => ((1 # 1) + s V_init_error_limit_z
            + max0(255 - s V_init_error_limit_in) <= z)%Q
   | 20 => ((1 # 1) + s V_init_error_limit_z
            + max0(256 - s V_init_error_limit_in) <= z)%Q
   | 21 => ((1 # 1) + s V_init_error_limit_z
            + max0(256 - s V_init_error_limit_in) <= z)%Q
   | 22 => ((1 # 1) + s V_init_error_limit_z
            + max0(256 - s V_init_error_limit_in) <= z)%Q
   | 23 => (s V_init_error_limit_z + max0(256 - s V_init_error_limit_in) <= z)%Q
   | 24 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (256 - s V_init_error_limit_in) (1)]
     (-(0 # 1) + s V_init_error_limit_z + max0(256 - s V_init_error_limit_in) <= z)%Q
   | 25 => ((1 # 1) + s V_init_error_limit_z
            + max0(255 - s V_init_error_limit_in) <= z)%Q
   | 26 => ((1 # 1) + s V_init_error_limit_z
            + max0(255 - s V_init_error_limit_in) <= z)%Q
   | 27 => ((1 # 1) + s V_init_error_limit_z
            + max0(256 - s V_init_error_limit_in) <= z)%Q
   | 28 => ((1 # 1) + s V_init_error_limit_z
            + max0(256 - s V_init_error_limit_in) <= z)%Q
   | 29 => ((1 # 1) + s V_init_error_limit_z
            + max0(256 - s V_init_error_limit_in) <= z)%Q
   | 30 => ((1 # 1) + s V_init_error_limit_z
            + max0(256 - s V_init_error_limit_in) <= z)%Q
   | 31 => (-(0 # 1) + s V_init_error_limit_z
            + max0(256 - s V_init_error_limit_in) <= z)%Q
   | 32 => ((256 # 1) - s V_init_error_limit_in + s V_init_error_limit_z <= z)%Q
   | 33 => ((256 # 1) - s V_init_error_limit_in + s V_init_error_limit_z <= z)%Q
   | 34 => ((256 # 1) - s V_init_error_limit_in + s V_init_error_limit_z <= z)%Q
   | 35 => ((257 # 1) - s V_init_error_limit_in + s V_init_error_limit_z <= z)%Q
   | 36 => ((257 # 1) - s V_init_error_limit_in + s V_init_error_limit_z <= z)%Q
   | 37 => ((257 # 1) - s V_init_error_limit_in + s V_init_error_limit_z <= z)%Q
   | 38 => ((257 # 1) - s V_init_error_limit_in + s V_init_error_limit_z <= z)%Q
   | 39 => ((256 # 1) - s V_init_error_limit_in + s V_init_error_limit_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_init_error_limit =>
    [mkPA Q (fun n z s => ai_init_error_limit n s /\ annot0_init_error_limit n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_init_error_limit (proc_start P_init_error_limit) s1 (proc_end P_init_error_limit) s2 ->
    (s2 V_init_error_limit_z <= (256 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_init_error_limit.
Qed.
