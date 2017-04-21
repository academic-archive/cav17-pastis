Require Import pasta.Pasta.

Inductive proc: Type :=
  P_pclxl_beginpage.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_pclxl_beginpage_z := 1%positive.
Notation V_pclxl_beginpage_i := 2%positive.
Notation V_pclxl_beginpage_size := 3%positive.
Notation V_pclxl_beginpage_vdev_dref_off1248 := 4%positive.
Notation V_pclxl_beginpage_vdev := 5%positive.
Definition Pedges_pclxl_beginpage: list (edge proc) :=
  (EA 1 (AAssign V_pclxl_beginpage_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_pclxl_beginpage_i (Some (ENum (10)))) 3)::(EA 3 ANone 4)::
  (EA 4 AWeaken 5)::(EA 5 (AGuard (fun s => ((eval (EVar V_pclxl_beginpage_i)
  s) > (eval (ENum (0)) s))%Z)) 7)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_pclxl_beginpage_i) s) <= (eval (ENum (0))
  s))%Z)) 6)::(EA 6 AWeaken 18)::(EA 7 AWeaken 8)::(EA 8 ANone 9)::
  (EA 8 ANone 11)::(EA 9 AWeaken 10)::(EA 10 ANone 17)::(EA 10 ANone 11)::
  (EA 11 ANone 12)::(EA 12 (AAssign V_pclxl_beginpage_i
  (Some (EAdd (EVar V_pclxl_beginpage_i) (ENum (-1))))) 13)::
  (EA 13 ANone 14)::(EA 14 ANone 15)::(EA 15 (AAssign V_pclxl_beginpage_z
  (Some (EAdd (ENum (1)) (EVar V_pclxl_beginpage_z)))) 16)::
  (EA 16 AWeaken 5)::(EA 17 ANone 18)::(EA 18 (AAssign V_pclxl_beginpage_size
  None) 19)::(EA 19 AWeaken 20)::(EA 20 (AGuard
  (fun s => ((eval (EVar V_pclxl_beginpage_size) s) <>
  (eval (EVar V_pclxl_beginpage_vdev_dref_off1248) s))%Z)) 22)::
  (EA 20 (AGuard (fun s => ((eval (EVar V_pclxl_beginpage_size) s) =
  (eval (EVar V_pclxl_beginpage_vdev_dref_off1248) s))%Z)) 21)::
  (EA 21 AWeaken 26)::(EA 22 AWeaken 23)::(EA 23 (AAssign
  V_pclxl_beginpage_vdev_dref_off1248
  (Some (EVar V_pclxl_beginpage_size))) 24)::(EA 24 ANone 25)::
  (EA 25 AWeaken 26)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_pclxl_beginpage => Pedges_pclxl_beginpage
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_pclxl_beginpage => 26
     end)%positive;
  var_global := var_global
}.

Definition ai_pclxl_beginpage (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_pclxl_beginpage_z <= 0 /\ -1 * s V_pclxl_beginpage_z <= 0)%Z
   | 3 => (-1 * s V_pclxl_beginpage_z <= 0 /\ 1 * s V_pclxl_beginpage_z <= 0 /\ 1 * s V_pclxl_beginpage_i + -10 <= 0 /\ -1 * s V_pclxl_beginpage_i + 10 <= 0)%Z
   | 4 => (-1 * s V_pclxl_beginpage_i + 10 <= 0 /\ 1 * s V_pclxl_beginpage_i + -10 <= 0 /\ 1 * s V_pclxl_beginpage_z <= 0 /\ -1 * s V_pclxl_beginpage_z <= 0)%Z
   | 5 => (-1 * s V_pclxl_beginpage_z <= 0 /\ 1 * s V_pclxl_beginpage_i + -10 <= 0 /\ -1 * s V_pclxl_beginpage_i <= 0)%Z
   | 6 => (-1 * s V_pclxl_beginpage_i <= 0 /\ -1 * s V_pclxl_beginpage_z <= 0 /\ 1 * s V_pclxl_beginpage_i <= 0)%Z
   | 7 => (1 * s V_pclxl_beginpage_i + -10 <= 0 /\ -1 * s V_pclxl_beginpage_z <= 0 /\ -1 * s V_pclxl_beginpage_i + 1 <= 0)%Z
   | 8 => (-1 * s V_pclxl_beginpage_i + 1 <= 0 /\ -1 * s V_pclxl_beginpage_z <= 0 /\ 1 * s V_pclxl_beginpage_i + -10 <= 0)%Z
   | 9 => (1 * s V_pclxl_beginpage_i + -10 <= 0 /\ -1 * s V_pclxl_beginpage_z <= 0 /\ -1 * s V_pclxl_beginpage_i + 1 <= 0)%Z
   | 10 => (-1 * s V_pclxl_beginpage_i + 1 <= 0 /\ -1 * s V_pclxl_beginpage_z <= 0 /\ 1 * s V_pclxl_beginpage_i + -10 <= 0)%Z
   | 11 => (1 * s V_pclxl_beginpage_i + -10 <= 0 /\ -1 * s V_pclxl_beginpage_z <= 0 /\ -1 * s V_pclxl_beginpage_i + 1 <= 0)%Z
   | 12 => (-1 * s V_pclxl_beginpage_i + 1 <= 0 /\ -1 * s V_pclxl_beginpage_z <= 0 /\ 1 * s V_pclxl_beginpage_i + -10 <= 0)%Z
   | 13 => (-1 * s V_pclxl_beginpage_z <= 0 /\ -1 * s V_pclxl_beginpage_i <= 0 /\ 1 * s V_pclxl_beginpage_i + -9 <= 0)%Z
   | 14 => (1 * s V_pclxl_beginpage_i + -9 <= 0 /\ -1 * s V_pclxl_beginpage_i <= 0 /\ -1 * s V_pclxl_beginpage_z <= 0)%Z
   | 15 => (-1 * s V_pclxl_beginpage_z <= 0 /\ -1 * s V_pclxl_beginpage_i <= 0 /\ 1 * s V_pclxl_beginpage_i + -9 <= 0)%Z
   | 16 => (1 * s V_pclxl_beginpage_i + -9 <= 0 /\ -1 * s V_pclxl_beginpage_i <= 0 /\ -1 * s V_pclxl_beginpage_z + 1 <= 0)%Z
   | 17 => (1 * s V_pclxl_beginpage_i + -10 <= 0 /\ -1 * s V_pclxl_beginpage_z <= 0 /\ -1 * s V_pclxl_beginpage_i + 1 <= 0)%Z
   | 18 => (-1 * s V_pclxl_beginpage_i <= 0 /\ -1 * s V_pclxl_beginpage_z <= 0 /\ 1 * s V_pclxl_beginpage_i + -10 <= 0)%Z
   | 19 => (1 * s V_pclxl_beginpage_i + -10 <= 0 /\ -1 * s V_pclxl_beginpage_z <= 0 /\ -1 * s V_pclxl_beginpage_i <= 0)%Z
   | 20 => (-1 * s V_pclxl_beginpage_i <= 0 /\ -1 * s V_pclxl_beginpage_z <= 0 /\ 1 * s V_pclxl_beginpage_i + -10 <= 0)%Z
   | 21 => (1 * s V_pclxl_beginpage_i + -10 <= 0 /\ -1 * s V_pclxl_beginpage_z <= 0 /\ -1 * s V_pclxl_beginpage_i <= 0 /\ 1 * s V_pclxl_beginpage_size+ -1 * s V_pclxl_beginpage_vdev_dref_off1248 <= 0 /\ -1 * s V_pclxl_beginpage_size+ 1 * s V_pclxl_beginpage_vdev_dref_off1248 <= 0)%Z
   | 22 => (1 * s V_pclxl_beginpage_i + -10 <= 0 /\ -1 * s V_pclxl_beginpage_z <= 0 /\ -1 * s V_pclxl_beginpage_i <= 0)%Z
   | 23 => (-1 * s V_pclxl_beginpage_i <= 0 /\ -1 * s V_pclxl_beginpage_z <= 0 /\ 1 * s V_pclxl_beginpage_i + -10 <= 0)%Z
   | 24 => (1 * s V_pclxl_beginpage_i + -10 <= 0 /\ -1 * s V_pclxl_beginpage_z <= 0 /\ -1 * s V_pclxl_beginpage_i <= 0)%Z
   | 25 => (-1 * s V_pclxl_beginpage_i <= 0 /\ -1 * s V_pclxl_beginpage_z <= 0 /\ 1 * s V_pclxl_beginpage_i + -10 <= 0)%Z
   | 26 => (1 * s V_pclxl_beginpage_i + -10 <= 0 /\ -1 * s V_pclxl_beginpage_z <= 0 /\ -1 * s V_pclxl_beginpage_i <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_pclxl_beginpage (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((10 # 1) <= z)%Q
   | 2 => ((10 # 1) + s V_pclxl_beginpage_z <= z)%Q
   | 3 => (s V_pclxl_beginpage_i + s V_pclxl_beginpage_z <= z)%Q
   | 4 => (s V_pclxl_beginpage_i + s V_pclxl_beginpage_z <= z)%Q
   | 5 => (s V_pclxl_beginpage_i + s V_pclxl_beginpage_z <= z)%Q
   | 6 => (s V_pclxl_beginpage_i + s V_pclxl_beginpage_z <= z)%Q
   | 7 => (s V_pclxl_beginpage_i + s V_pclxl_beginpage_z <= z)%Q
   | 8 => (s V_pclxl_beginpage_i + s V_pclxl_beginpage_z <= z)%Q
   | 9 => (s V_pclxl_beginpage_i + s V_pclxl_beginpage_z <= z)%Q
   | 10 => (s V_pclxl_beginpage_i + s V_pclxl_beginpage_z <= z)%Q
   | 11 => (s V_pclxl_beginpage_i + s V_pclxl_beginpage_z <= z)%Q
   | 12 => (s V_pclxl_beginpage_i + s V_pclxl_beginpage_z <= z)%Q
   | 13 => ((1 # 1) + s V_pclxl_beginpage_i + s V_pclxl_beginpage_z <= z)%Q
   | 14 => ((1 # 1) + s V_pclxl_beginpage_i + s V_pclxl_beginpage_z <= z)%Q
   | 15 => ((1 # 1) + s V_pclxl_beginpage_i + s V_pclxl_beginpage_z <= z)%Q
   | 16 => (s V_pclxl_beginpage_i + s V_pclxl_beginpage_z <= z)%Q
   | 17 => (s V_pclxl_beginpage_i + s V_pclxl_beginpage_z <= z)%Q
   | 18 => (s V_pclxl_beginpage_i + s V_pclxl_beginpage_z <= z)%Q
   | 19 => (s V_pclxl_beginpage_i + s V_pclxl_beginpage_z <= z)%Q
   | 20 => (s V_pclxl_beginpage_i + s V_pclxl_beginpage_z <= z)%Q
   | 21 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_pclxl_beginpage_i) (-1
                                                                    + 
                                                                    s V_pclxl_beginpage_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V_pclxl_beginpage_i);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_pclxl_beginpage_i) (0))) (F_max0_ge_0 (s V_pclxl_beginpage_i))]
     (s V_pclxl_beginpage_i + s V_pclxl_beginpage_z <= z)%Q
   | 22 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_pclxl_beginpage_i) (0))) (F_max0_ge_0 (s V_pclxl_beginpage_i))]
     (s V_pclxl_beginpage_i + s V_pclxl_beginpage_z <= z)%Q
   | 23 => (s V_pclxl_beginpage_z + max0(s V_pclxl_beginpage_i) <= z)%Q
   | 24 => (s V_pclxl_beginpage_z + max0(s V_pclxl_beginpage_i) <= z)%Q
   | 25 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_pclxl_beginpage_i) (-1
                                                                    + 
                                                                    s V_pclxl_beginpage_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V_pclxl_beginpage_i)]
     (s V_pclxl_beginpage_z + max0(s V_pclxl_beginpage_i) <= z)%Q
   | 26 => (s V_pclxl_beginpage_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_pclxl_beginpage =>
    [mkPA Q (fun n z s => ai_pclxl_beginpage n s /\ annot0_pclxl_beginpage n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_pclxl_beginpage (proc_start P_pclxl_beginpage) s1 (proc_end P_pclxl_beginpage) s2 ->
    (s2 V_pclxl_beginpage_z <= (10 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_pclxl_beginpage.
Qed.
