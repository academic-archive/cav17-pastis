Require Import pasta.Pasta.

Inductive proc: Type :=
  P_makebwmap.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_makebwmap_z := 1%positive.
Notation V_makebwmap__tmp := 2%positive.
Notation V_makebwmap_bitspersample := 3%positive.
Notation V_makebwmap_c := 4%positive.
Notation V_makebwmap_i := 5%positive.
Notation V_makebwmap_img_dref_off28 := 6%positive.
Notation V_makebwmap_nsamples := 7%positive.
Notation V_makebwmap_img := 8%positive.
Definition Pedges_makebwmap: list (edge proc) :=
  (EA 1 (AAssign V_makebwmap_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_makebwmap_bitspersample (Some (EVar V_makebwmap_img_dref_off28))) 3)::
  (EA 3 (AAssign V_makebwmap_nsamples None) 4)::(EA 4 AWeaken 5)::
  (EA 5 ANone 41)::(EA 5 ANone 6)::(EA 6 (AAssign V_makebwmap_i
  (Some (ENum (0)))) 7)::(EA 7 ANone 8)::(EA 8 AWeaken 9)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_makebwmap_i) s) < (eval (ENum (256)) s))%Z)) 14)::
  (EA 9 (AGuard (fun s => ((eval (EVar V_makebwmap_i) s) >=
  (eval (ENum (256)) s))%Z)) 10)::(EA 10 AWeaken 11)::(EA 11 (AAssign
  V_makebwmap__tmp (Some (ENum (1)))) 12)::(EA 12 ANone 13)::
  (EA 13 AWeaken 44)::(EA 14 AWeaken 15)::(EA 15 ANone 35)::
  (EA 15 ANone 26)::(EA 15 ANone 21)::(EA 15 ANone 18)::(EA 15 ANone 16)::
  (EA 16 (AAssign V_makebwmap_c None) 17)::(EA 17 ANone 35)::(EA 18 (AAssign
  V_makebwmap_c None) 19)::(EA 19 (AAssign V_makebwmap_c None) 20)::
  (EA 20 ANone 35)::(EA 21 (AAssign V_makebwmap_c None) 22)::(EA 22 (AAssign
  V_makebwmap_c None) 23)::(EA 23 (AAssign V_makebwmap_c None) 24)::
  (EA 24 (AAssign V_makebwmap_c None) 25)::(EA 25 ANone 35)::(EA 26 (AAssign
  V_makebwmap_c None) 27)::(EA 27 (AAssign V_makebwmap_c None) 28)::
  (EA 28 (AAssign V_makebwmap_c None) 29)::(EA 29 (AAssign V_makebwmap_c
  None) 30)::(EA 30 (AAssign V_makebwmap_c None) 31)::(EA 31 (AAssign
  V_makebwmap_c None) 32)::(EA 32 (AAssign V_makebwmap_c None) 33)::
  (EA 33 (AAssign V_makebwmap_c None) 34)::(EA 34 ANone 35)::
  (EA 35 ANone 36)::(EA 36 (AAssign V_makebwmap_i
  (Some (EAdd (EVar V_makebwmap_i) (ENum (1))))) 37)::(EA 37 ANone 38)::
  (EA 38 ANone 39)::(EA 39 (AAssign V_makebwmap_z (Some (EAdd (ENum (1))
  (EVar V_makebwmap_z)))) 40)::(EA 40 AWeaken 9)::(EA 41 (AAssign
  V_makebwmap__tmp (Some (ENum (0)))) 42)::(EA 42 ANone 43)::
  (EA 43 AWeaken 44)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_makebwmap => Pedges_makebwmap
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_makebwmap => 44
     end)%positive;
  var_global := var_global
}.

Definition ai_makebwmap (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_makebwmap_z <= 0 /\ -1 * s V_makebwmap_z <= 0)%Z
   | 3 => (-1 * s V_makebwmap_z <= 0 /\ 1 * s V_makebwmap_z <= 0)%Z
   | 4 => (1 * s V_makebwmap_z <= 0 /\ -1 * s V_makebwmap_z <= 0)%Z
   | 5 => (-1 * s V_makebwmap_z <= 0 /\ 1 * s V_makebwmap_z <= 0)%Z
   | 6 => (1 * s V_makebwmap_z <= 0 /\ -1 * s V_makebwmap_z <= 0)%Z
   | 7 => (-1 * s V_makebwmap_z <= 0 /\ 1 * s V_makebwmap_z <= 0 /\ 1 * s V_makebwmap_i <= 0 /\ -1 * s V_makebwmap_i <= 0)%Z
   | 8 => (-1 * s V_makebwmap_i <= 0 /\ 1 * s V_makebwmap_i <= 0 /\ 1 * s V_makebwmap_z <= 0 /\ -1 * s V_makebwmap_z <= 0)%Z
   | 9 => (-1 * s V_makebwmap_z <= 0 /\ -1 * s V_makebwmap_i <= 0 /\ 1 * s V_makebwmap_i + -256 <= 0)%Z
   | 10 => (1 * s V_makebwmap_i + -256 <= 0 /\ -1 * s V_makebwmap_z <= 0 /\ -1 * s V_makebwmap_i + 256 <= 0)%Z
   | 11 => (-1 * s V_makebwmap_i + 256 <= 0 /\ -1 * s V_makebwmap_z <= 0 /\ 1 * s V_makebwmap_i + -256 <= 0)%Z
   | 12 => (1 * s V_makebwmap_i + -256 <= 0 /\ -1 * s V_makebwmap_z <= 0 /\ -1 * s V_makebwmap_i + 256 <= 0 /\ 1 * s V_makebwmap__tmp + -1 <= 0 /\ -1 * s V_makebwmap__tmp + 1 <= 0)%Z
   | 13 => (-1 * s V_makebwmap__tmp + 1 <= 0 /\ 1 * s V_makebwmap__tmp + -1 <= 0 /\ -1 * s V_makebwmap_i + 256 <= 0 /\ -1 * s V_makebwmap_z <= 0 /\ 1 * s V_makebwmap_i + -256 <= 0)%Z
   | 14 => (-1 * s V_makebwmap_i <= 0 /\ -1 * s V_makebwmap_z <= 0 /\ 1 * s V_makebwmap_i + -255 <= 0)%Z
   | 15 => (1 * s V_makebwmap_i + -255 <= 0 /\ -1 * s V_makebwmap_z <= 0 /\ -1 * s V_makebwmap_i <= 0)%Z
   | 16 => (-1 * s V_makebwmap_i <= 0 /\ -1 * s V_makebwmap_z <= 0 /\ 1 * s V_makebwmap_i + -255 <= 0)%Z
   | 17 => (1 * s V_makebwmap_i + -255 <= 0 /\ -1 * s V_makebwmap_z <= 0 /\ -1 * s V_makebwmap_i <= 0)%Z
   | 18 => (-1 * s V_makebwmap_i <= 0 /\ -1 * s V_makebwmap_z <= 0 /\ 1 * s V_makebwmap_i + -255 <= 0)%Z
   | 19 => (1 * s V_makebwmap_i + -255 <= 0 /\ -1 * s V_makebwmap_z <= 0 /\ -1 * s V_makebwmap_i <= 0)%Z
   | 20 => (-1 * s V_makebwmap_i <= 0 /\ -1 * s V_makebwmap_z <= 0 /\ 1 * s V_makebwmap_i + -255 <= 0)%Z
   | 21 => (-1 * s V_makebwmap_i <= 0 /\ -1 * s V_makebwmap_z <= 0 /\ 1 * s V_makebwmap_i + -255 <= 0)%Z
   | 22 => (1 * s V_makebwmap_i + -255 <= 0 /\ -1 * s V_makebwmap_z <= 0 /\ -1 * s V_makebwmap_i <= 0)%Z
   | 23 => (-1 * s V_makebwmap_i <= 0 /\ -1 * s V_makebwmap_z <= 0 /\ 1 * s V_makebwmap_i + -255 <= 0)%Z
   | 24 => (1 * s V_makebwmap_i + -255 <= 0 /\ -1 * s V_makebwmap_z <= 0 /\ -1 * s V_makebwmap_i <= 0)%Z
   | 25 => (-1 * s V_makebwmap_i <= 0 /\ -1 * s V_makebwmap_z <= 0 /\ 1 * s V_makebwmap_i + -255 <= 0)%Z
   | 26 => (-1 * s V_makebwmap_i <= 0 /\ -1 * s V_makebwmap_z <= 0 /\ 1 * s V_makebwmap_i + -255 <= 0)%Z
   | 27 => (1 * s V_makebwmap_i + -255 <= 0 /\ -1 * s V_makebwmap_z <= 0 /\ -1 * s V_makebwmap_i <= 0)%Z
   | 28 => (-1 * s V_makebwmap_i <= 0 /\ -1 * s V_makebwmap_z <= 0 /\ 1 * s V_makebwmap_i + -255 <= 0)%Z
   | 29 => (1 * s V_makebwmap_i + -255 <= 0 /\ -1 * s V_makebwmap_z <= 0 /\ -1 * s V_makebwmap_i <= 0)%Z
   | 30 => (-1 * s V_makebwmap_i <= 0 /\ -1 * s V_makebwmap_z <= 0 /\ 1 * s V_makebwmap_i + -255 <= 0)%Z
   | 31 => (1 * s V_makebwmap_i + -255 <= 0 /\ -1 * s V_makebwmap_z <= 0 /\ -1 * s V_makebwmap_i <= 0)%Z
   | 32 => (-1 * s V_makebwmap_i <= 0 /\ -1 * s V_makebwmap_z <= 0 /\ 1 * s V_makebwmap_i + -255 <= 0)%Z
   | 33 => (1 * s V_makebwmap_i + -255 <= 0 /\ -1 * s V_makebwmap_z <= 0 /\ -1 * s V_makebwmap_i <= 0)%Z
   | 34 => (-1 * s V_makebwmap_i <= 0 /\ -1 * s V_makebwmap_z <= 0 /\ 1 * s V_makebwmap_i + -255 <= 0)%Z
   | 35 => (1 * s V_makebwmap_i + -255 <= 0 /\ -1 * s V_makebwmap_z <= 0 /\ -1 * s V_makebwmap_i <= 0)%Z
   | 36 => (-1 * s V_makebwmap_i <= 0 /\ -1 * s V_makebwmap_z <= 0 /\ 1 * s V_makebwmap_i + -255 <= 0)%Z
   | 37 => (-1 * s V_makebwmap_z <= 0 /\ -1 * s V_makebwmap_i + 1 <= 0 /\ 1 * s V_makebwmap_i + -256 <= 0)%Z
   | 38 => (1 * s V_makebwmap_i + -256 <= 0 /\ -1 * s V_makebwmap_i + 1 <= 0 /\ -1 * s V_makebwmap_z <= 0)%Z
   | 39 => (-1 * s V_makebwmap_z <= 0 /\ -1 * s V_makebwmap_i + 1 <= 0 /\ 1 * s V_makebwmap_i + -256 <= 0)%Z
   | 40 => (1 * s V_makebwmap_i + -256 <= 0 /\ -1 * s V_makebwmap_i + 1 <= 0 /\ -1 * s V_makebwmap_z + 1 <= 0)%Z
   | 41 => (1 * s V_makebwmap_z <= 0 /\ -1 * s V_makebwmap_z <= 0)%Z
   | 42 => (-1 * s V_makebwmap_z <= 0 /\ 1 * s V_makebwmap_z <= 0 /\ 1 * s V_makebwmap__tmp <= 0 /\ -1 * s V_makebwmap__tmp <= 0)%Z
   | 43 => (-1 * s V_makebwmap__tmp <= 0 /\ 1 * s V_makebwmap__tmp <= 0 /\ 1 * s V_makebwmap_z <= 0 /\ -1 * s V_makebwmap_z <= 0)%Z
   | 44 => (1 * s V_makebwmap__tmp + -1 <= 0 /\ -1 * s V_makebwmap_z <= 0 /\ -1 * s V_makebwmap__tmp <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_makebwmap (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((256 # 1) <= z)%Q
   | 2 => ((256 # 1) + s V_makebwmap_z <= z)%Q
   | 3 => ((256 # 1) + s V_makebwmap_z <= z)%Q
   | 4 => ((256 # 1) + s V_makebwmap_z <= z)%Q
   | 5 => ((256 # 1) + s V_makebwmap_z <= z)%Q
   | 6 => ((256 # 1) + s V_makebwmap_z <= z)%Q
   | 7 => ((256 # 1) - s V_makebwmap_i + s V_makebwmap_z <= z)%Q
   | 8 => ((256 # 1) - s V_makebwmap_i + s V_makebwmap_z <= z)%Q
   | 9 => ((256 # 1) - s V_makebwmap_i + s V_makebwmap_z <= z)%Q
   | 10 => ((256 # 1) - s V_makebwmap_i + s V_makebwmap_z <= z)%Q
   | 11 => ((256 # 1) - s V_makebwmap_i + s V_makebwmap_z <= z)%Q
   | 12 => ((256 # 1) - s V_makebwmap_i + s V_makebwmap_z <= z)%Q
   | 13 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (256 - s V_makebwmap_i) (255
                                                                    - 
                                                                    s V_makebwmap_i));
      (*-1 0*) F_max0_ge_0 (255 - s V_makebwmap_i);
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (256
                                                              - s V_makebwmap_i) (0))) (F_max0_ge_0 (256
                                                                    - s V_makebwmap_i))]
     ((256 # 1) - s V_makebwmap_i + s V_makebwmap_z <= z)%Q
   | 14 => ((256 # 1) - s V_makebwmap_i + s V_makebwmap_z <= z)%Q
   | 15 => ((256 # 1) - s V_makebwmap_i + s V_makebwmap_z <= z)%Q
   | 16 => ((256 # 1) - s V_makebwmap_i + s V_makebwmap_z <= z)%Q
   | 17 => ((256 # 1) - s V_makebwmap_i + s V_makebwmap_z <= z)%Q
   | 18 => ((256 # 1) - s V_makebwmap_i + s V_makebwmap_z <= z)%Q
   | 19 => ((256 # 1) - s V_makebwmap_i + s V_makebwmap_z <= z)%Q
   | 20 => ((256 # 1) - s V_makebwmap_i + s V_makebwmap_z <= z)%Q
   | 21 => ((256 # 1) - s V_makebwmap_i + s V_makebwmap_z <= z)%Q
   | 22 => ((256 # 1) - s V_makebwmap_i + s V_makebwmap_z <= z)%Q
   | 23 => ((256 # 1) - s V_makebwmap_i + s V_makebwmap_z <= z)%Q
   | 24 => ((256 # 1) - s V_makebwmap_i + s V_makebwmap_z <= z)%Q
   | 25 => ((256 # 1) - s V_makebwmap_i + s V_makebwmap_z <= z)%Q
   | 26 => ((256 # 1) - s V_makebwmap_i + s V_makebwmap_z <= z)%Q
   | 27 => ((256 # 1) - s V_makebwmap_i + s V_makebwmap_z <= z)%Q
   | 28 => ((256 # 1) - s V_makebwmap_i + s V_makebwmap_z <= z)%Q
   | 29 => ((256 # 1) - s V_makebwmap_i + s V_makebwmap_z <= z)%Q
   | 30 => ((256 # 1) - s V_makebwmap_i + s V_makebwmap_z <= z)%Q
   | 31 => ((256 # 1) - s V_makebwmap_i + s V_makebwmap_z <= z)%Q
   | 32 => ((256 # 1) - s V_makebwmap_i + s V_makebwmap_z <= z)%Q
   | 33 => ((256 # 1) - s V_makebwmap_i + s V_makebwmap_z <= z)%Q
   | 34 => ((256 # 1) - s V_makebwmap_i + s V_makebwmap_z <= z)%Q
   | 35 => ((256 # 1) - s V_makebwmap_i + s V_makebwmap_z <= z)%Q
   | 36 => ((256 # 1) - s V_makebwmap_i + s V_makebwmap_z <= z)%Q
   | 37 => ((257 # 1) - s V_makebwmap_i + s V_makebwmap_z <= z)%Q
   | 38 => ((257 # 1) - s V_makebwmap_i + s V_makebwmap_z <= z)%Q
   | 39 => ((257 # 1) - s V_makebwmap_i + s V_makebwmap_z <= z)%Q
   | 40 => ((256 # 1) - s V_makebwmap_i + s V_makebwmap_z <= z)%Q
   | 41 => ((256 # 1) + s V_makebwmap_z <= z)%Q
   | 42 => ((256 # 1) + s V_makebwmap_z <= z)%Q
   | 43 => hints
     [(*-256 0*) F_one]
     ((256 # 1) + s V_makebwmap_z <= z)%Q
   | 44 => (s V_makebwmap_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_makebwmap =>
    [mkPA Q (fun n z s => ai_makebwmap n s /\ annot0_makebwmap n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_makebwmap (proc_start P_makebwmap) s1 (proc_end P_makebwmap) s2 ->
    (s2 V_makebwmap_z <= (256 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_makebwmap.
Qed.
