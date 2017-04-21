Require Import pasta.Pasta.

Inductive proc: Type :=
  P_setup_brightness_lut.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_setup_brightness_lut_z := 1%positive.
Notation V_setup_brightness_lut__tmp := 2%positive.
Notation V_setup_brightness_lut__tmp1 := 3%positive.
Notation V_setup_brightness_lut_k := 4%positive.
Notation V_setup_brightness_lut_bp := 5%positive.
Notation V_setup_brightness_lut_form := 6%positive.
Notation V_setup_brightness_lut_thresh := 7%positive.
Definition Pedges_setup_brightness_lut: list (edge proc) :=
  (EA 1 (AAssign V_setup_brightness_lut_z (Some (ENum (0)))) 2)::
  (EA 2 (AAssign V_setup_brightness_lut__tmp1
  (Some (EVar V_setup_brightness_lut_thresh))) 3)::(EA 3 (AAssign
  V_setup_brightness_lut__tmp (Some (EVar V_setup_brightness_lut_form))) 4)::
  (EA 4 (AAssign V_setup_brightness_lut_k (Some (ENum (-256)))) 5)::
  (EA 5 ANone 6)::(EA 6 AWeaken 7)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_setup_brightness_lut_k) s) < (eval (ENum (257))
  s))%Z)) 10)::(EA 7 (AGuard (fun s => ((eval (EVar V_setup_brightness_lut_k)
  s) >= (eval (ENum (257)) s))%Z)) 8)::(EA 8 AWeaken 9)::(EA 10 AWeaken 11)::
  (EA 11 (AGuard (fun s => ((eval (EVar V_setup_brightness_lut__tmp) s) =
  (eval (ENum (6)) s))%Z)) 13)::(EA 11 (AGuard
  (fun s => ((eval (EVar V_setup_brightness_lut__tmp) s) <> (eval (ENum (6))
  s))%Z)) 12)::(EA 12 AWeaken 15)::(EA 13 AWeaken 14)::(EA 14 ANone 15)::
  (EA 15 ANone 16)::(EA 16 (AAssign V_setup_brightness_lut_k
  (Some (EAdd (EVar V_setup_brightness_lut_k) (ENum (1))))) 17)::
  (EA 17 ANone 18)::(EA 18 ANone 19)::(EA 19 (AAssign
  V_setup_brightness_lut_z (Some (EAdd (ENum (1))
  (EVar V_setup_brightness_lut_z)))) 20)::(EA 20 AWeaken 7)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_setup_brightness_lut => Pedges_setup_brightness_lut
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_setup_brightness_lut => 9
     end)%positive;
  var_global := var_global
}.

Definition ai_setup_brightness_lut (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_setup_brightness_lut_z <= 0 /\ -1 * s V_setup_brightness_lut_z <= 0)%Z
   | 3 => (-1 * s V_setup_brightness_lut_z <= 0 /\ 1 * s V_setup_brightness_lut_z <= 0)%Z
   | 4 => (1 * s V_setup_brightness_lut_z <= 0 /\ -1 * s V_setup_brightness_lut_z <= 0)%Z
   | 5 => (-1 * s V_setup_brightness_lut_z <= 0 /\ 1 * s V_setup_brightness_lut_z <= 0 /\ 1 * s V_setup_brightness_lut_k + 256 <= 0 /\ -1 * s V_setup_brightness_lut_k + -256 <= 0)%Z
   | 6 => (-1 * s V_setup_brightness_lut_k + -256 <= 0 /\ 1 * s V_setup_brightness_lut_k + 256 <= 0 /\ 1 * s V_setup_brightness_lut_z <= 0 /\ -1 * s V_setup_brightness_lut_z <= 0)%Z
   | 7 => (-1 * s V_setup_brightness_lut_z <= 0 /\ -1 * s V_setup_brightness_lut_k + -256 <= 0 /\ 1 * s V_setup_brightness_lut_k + -257 <= 0)%Z
   | 8 => (1 * s V_setup_brightness_lut_k + -257 <= 0 /\ -1 * s V_setup_brightness_lut_z <= 0 /\ -1 * s V_setup_brightness_lut_k + 257 <= 0)%Z
   | 9 => (-1 * s V_setup_brightness_lut_k + 257 <= 0 /\ -1 * s V_setup_brightness_lut_z <= 0 /\ 1 * s V_setup_brightness_lut_k + -257 <= 0)%Z
   | 10 => (-1 * s V_setup_brightness_lut_k + -256 <= 0 /\ -1 * s V_setup_brightness_lut_z <= 0 /\ 1 * s V_setup_brightness_lut_k + -256 <= 0)%Z
   | 11 => (1 * s V_setup_brightness_lut_k + -256 <= 0 /\ -1 * s V_setup_brightness_lut_z <= 0 /\ -1 * s V_setup_brightness_lut_k + -256 <= 0)%Z
   | 12 => (-1 * s V_setup_brightness_lut_k + -256 <= 0 /\ -1 * s V_setup_brightness_lut_z <= 0 /\ 1 * s V_setup_brightness_lut_k + -256 <= 0)%Z
   | 13 => (-1 * s V_setup_brightness_lut_k + -256 <= 0 /\ -1 * s V_setup_brightness_lut_z <= 0 /\ 1 * s V_setup_brightness_lut_k + -256 <= 0 /\ 1 * s V_setup_brightness_lut__tmp + -6 <= 0 /\ -1 * s V_setup_brightness_lut__tmp + 6 <= 0)%Z
   | 14 => (-1 * s V_setup_brightness_lut__tmp + 6 <= 0 /\ 1 * s V_setup_brightness_lut__tmp + -6 <= 0 /\ 1 * s V_setup_brightness_lut_k + -256 <= 0 /\ -1 * s V_setup_brightness_lut_z <= 0 /\ -1 * s V_setup_brightness_lut_k + -256 <= 0)%Z
   | 15 => (-1 * s V_setup_brightness_lut_k + -256 <= 0 /\ -1 * s V_setup_brightness_lut_z <= 0 /\ 1 * s V_setup_brightness_lut_k + -256 <= 0)%Z
   | 16 => (1 * s V_setup_brightness_lut_k + -256 <= 0 /\ -1 * s V_setup_brightness_lut_z <= 0 /\ -1 * s V_setup_brightness_lut_k + -256 <= 0)%Z
   | 17 => (-1 * s V_setup_brightness_lut_z <= 0 /\ 1 * s V_setup_brightness_lut_k + -257 <= 0 /\ -1 * s V_setup_brightness_lut_k + -255 <= 0)%Z
   | 18 => (-1 * s V_setup_brightness_lut_k + -255 <= 0 /\ 1 * s V_setup_brightness_lut_k + -257 <= 0 /\ -1 * s V_setup_brightness_lut_z <= 0)%Z
   | 19 => (-1 * s V_setup_brightness_lut_z <= 0 /\ 1 * s V_setup_brightness_lut_k + -257 <= 0 /\ -1 * s V_setup_brightness_lut_k + -255 <= 0)%Z
   | 20 => (-1 * s V_setup_brightness_lut_k + -255 <= 0 /\ 1 * s V_setup_brightness_lut_k + -257 <= 0 /\ -1 * s V_setup_brightness_lut_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_setup_brightness_lut (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((513 # 1) <= z)%Q
   | 2 => ((513 # 1) + s V_setup_brightness_lut_z <= z)%Q
   | 3 => ((513 # 1) + s V_setup_brightness_lut_z <= z)%Q
   | 4 => ((513 # 1) + s V_setup_brightness_lut_z <= z)%Q
   | 5 => (s V_setup_brightness_lut_z
           + max0(257 - s V_setup_brightness_lut_k) <= z)%Q
   | 6 => (s V_setup_brightness_lut_z
           + max0(257 - s V_setup_brightness_lut_k) <= z)%Q
   | 7 => (s V_setup_brightness_lut_z
           + max0(257 - s V_setup_brightness_lut_k) <= z)%Q
   | 8 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (257 - s V_setup_brightness_lut_k) (256
                                                                    - s V_setup_brightness_lut_k));
      (*-1 0*) F_max0_ge_0 (256 - s V_setup_brightness_lut_k)]
     (s V_setup_brightness_lut_z + max0(257 - s V_setup_brightness_lut_k) <= z)%Q
   | 9 => (s V_setup_brightness_lut_z <= z)%Q
   | 10 => (s V_setup_brightness_lut_z
            + max0(257 - s V_setup_brightness_lut_k) <= z)%Q
   | 11 => (s V_setup_brightness_lut_z
            + max0(257 - s V_setup_brightness_lut_k) <= z)%Q
   | 12 => hints
     [(*0 1*) F_max0_pre_decrement 1 (257 - s V_setup_brightness_lut_k) (1)]
     (s V_setup_brightness_lut_z + max0(257 - s V_setup_brightness_lut_k) <= z)%Q
   | 13 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (257 - s V_setup_brightness_lut_k) (1)]
     (s V_setup_brightness_lut_z + max0(257 - s V_setup_brightness_lut_k) <= z)%Q
   | 14 => ((1 # 1) + s V_setup_brightness_lut_z
            + max0(256 - s V_setup_brightness_lut_k) <= z)%Q
   | 15 => ((1 # 1) + s V_setup_brightness_lut_z
            + max0(256 - s V_setup_brightness_lut_k) <= z)%Q
   | 16 => ((1 # 1) + s V_setup_brightness_lut_z
            + max0(256 - s V_setup_brightness_lut_k) <= z)%Q
   | 17 => ((1 # 1) + s V_setup_brightness_lut_z
            + max0(257 - s V_setup_brightness_lut_k) <= z)%Q
   | 18 => ((1 # 1) + s V_setup_brightness_lut_z
            + max0(257 - s V_setup_brightness_lut_k) <= z)%Q
   | 19 => ((1 # 1) + s V_setup_brightness_lut_z
            + max0(257 - s V_setup_brightness_lut_k) <= z)%Q
   | 20 => (s V_setup_brightness_lut_z
            + max0(257 - s V_setup_brightness_lut_k) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_setup_brightness_lut =>
    [mkPA Q (fun n z s => ai_setup_brightness_lut n s /\ annot0_setup_brightness_lut n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_setup_brightness_lut (proc_start P_setup_brightness_lut) s1 (proc_end P_setup_brightness_lut) s2 ->
    (s2 V_setup_brightness_lut_z <= (513 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_setup_brightness_lut.
Qed.