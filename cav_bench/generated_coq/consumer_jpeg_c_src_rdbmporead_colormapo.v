Require Import pasta.Pasta.

Inductive proc: Type :=
  P_read_colormap.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_read_colormap_z := 1%positive.
Notation V_read_colormap__tmp := 2%positive.
Notation V_read_colormap__tmp1 := 3%positive.
Notation V_read_colormap_i := 4%positive.
Notation V_read_colormap_cmaplen := 5%positive.
Notation V_read_colormap_mapentrysize := 6%positive.
Notation V_read_colormap_sinfo := 7%positive.
Definition Pedges_read_colormap: list (edge proc) :=
  (EA 1 (AAssign V_read_colormap_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_read_colormap__tmp (Some (EVar V_read_colormap_cmaplen))) 3)::
  (EA 3 (AAssign V_read_colormap__tmp1
  (Some (EVar V_read_colormap_mapentrysize))) 4)::(EA 4 AWeaken 5)::
  (EA 5 ANone 34)::(EA 5 ANone 20)::(EA 5 ANone 6)::(EA 6 (AAssign
  V_read_colormap_i (Some (ENum (0)))) 7)::(EA 7 ANone 8)::(EA 8 AWeaken 9)::
  (EA 9 (AGuard (fun s => ((eval (EVar V_read_colormap_i) s) <
  (eval (EVar V_read_colormap__tmp) s))%Z)) 13)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_read_colormap_i) s) >=
  (eval (EVar V_read_colormap__tmp) s))%Z)) 10)::(EA 10 AWeaken 11)::
  (EA 11 ANone 12)::(EA 12 AWeaken 36)::(EA 13 AWeaken 14)::
  (EA 14 ANone 15)::(EA 15 (AAssign V_read_colormap_i
  (Some (EAdd (EVar V_read_colormap_i) (ENum (1))))) 16)::(EA 16 ANone 17)::
  (EA 17 ANone 18)::(EA 18 (AAssign V_read_colormap_z (Some (EAdd (ENum (1))
  (EVar V_read_colormap_z)))) 19)::(EA 19 AWeaken 9)::(EA 20 (AAssign
  V_read_colormap_i (Some (ENum (0)))) 21)::(EA 21 ANone 22)::
  (EA 22 AWeaken 23)::(EA 23 (AGuard
  (fun s => ((eval (EVar V_read_colormap_i) s) <
  (eval (EVar V_read_colormap__tmp) s))%Z)) 27)::(EA 23 (AGuard
  (fun s => ((eval (EVar V_read_colormap_i) s) >=
  (eval (EVar V_read_colormap__tmp) s))%Z)) 24)::(EA 24 AWeaken 25)::
  (EA 25 ANone 26)::(EA 26 AWeaken 36)::(EA 27 AWeaken 28)::
  (EA 28 ANone 29)::(EA 29 (AAssign V_read_colormap_i
  (Some (EAdd (EVar V_read_colormap_i) (ENum (1))))) 30)::(EA 30 ANone 31)::
  (EA 31 ANone 32)::(EA 32 (AAssign V_read_colormap_z (Some (EAdd (ENum (1))
  (EVar V_read_colormap_z)))) 33)::(EA 33 AWeaken 23)::(EA 34 ANone 35)::
  (EA 35 AWeaken 36)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_read_colormap => Pedges_read_colormap
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_read_colormap => 36
     end)%positive;
  var_global := var_global
}.

Definition ai_read_colormap (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_read_colormap_z <= 0 /\ -1 * s V_read_colormap_z <= 0)%Z
   | 3 => (-1 * s V_read_colormap_z <= 0 /\ 1 * s V_read_colormap_z <= 0)%Z
   | 4 => (1 * s V_read_colormap_z <= 0 /\ -1 * s V_read_colormap_z <= 0)%Z
   | 5 => (-1 * s V_read_colormap_z <= 0 /\ 1 * s V_read_colormap_z <= 0)%Z
   | 6 => (1 * s V_read_colormap_z <= 0 /\ -1 * s V_read_colormap_z <= 0)%Z
   | 7 => (-1 * s V_read_colormap_z <= 0 /\ 1 * s V_read_colormap_z <= 0 /\ 1 * s V_read_colormap_i <= 0 /\ -1 * s V_read_colormap_i <= 0)%Z
   | 8 => (-1 * s V_read_colormap_i <= 0 /\ 1 * s V_read_colormap_i <= 0 /\ 1 * s V_read_colormap_z <= 0 /\ -1 * s V_read_colormap_z <= 0)%Z
   | 9 => (-1 * s V_read_colormap_z <= 0 /\ -1 * s V_read_colormap_i <= 0)%Z
   | 10 => (-1 * s V_read_colormap_i <= 0 /\ -1 * s V_read_colormap_z <= 0 /\ 1 * s V_read_colormap__tmp+ -1 * s V_read_colormap_i <= 0)%Z
   | 11 => (1 * s V_read_colormap__tmp+ -1 * s V_read_colormap_i <= 0 /\ -1 * s V_read_colormap_z <= 0 /\ -1 * s V_read_colormap_i <= 0)%Z
   | 12 => (-1 * s V_read_colormap_i <= 0 /\ -1 * s V_read_colormap_z <= 0 /\ 1 * s V_read_colormap__tmp+ -1 * s V_read_colormap_i <= 0)%Z
   | 13 => (-1 * s V_read_colormap_i <= 0 /\ -1 * s V_read_colormap_z <= 0 /\ -1 * s V_read_colormap__tmp+ 1 * s V_read_colormap_i + 1 <= 0)%Z
   | 14 => (-1 * s V_read_colormap__tmp+ 1 * s V_read_colormap_i + 1 <= 0 /\ -1 * s V_read_colormap_z <= 0 /\ -1 * s V_read_colormap_i <= 0)%Z
   | 15 => (-1 * s V_read_colormap_i <= 0 /\ -1 * s V_read_colormap_z <= 0 /\ -1 * s V_read_colormap__tmp+ 1 * s V_read_colormap_i + 1 <= 0)%Z
   | 16 => (-1 * s V_read_colormap_z <= 0 /\ -1 * s V_read_colormap_i + 1 <= 0 /\ -1 * s V_read_colormap__tmp+ 1 * s V_read_colormap_i <= 0)%Z
   | 17 => (-1 * s V_read_colormap__tmp+ 1 * s V_read_colormap_i <= 0 /\ -1 * s V_read_colormap_i + 1 <= 0 /\ -1 * s V_read_colormap_z <= 0)%Z
   | 18 => (-1 * s V_read_colormap_z <= 0 /\ -1 * s V_read_colormap_i + 1 <= 0 /\ -1 * s V_read_colormap__tmp+ 1 * s V_read_colormap_i <= 0)%Z
   | 19 => (-1 * s V_read_colormap__tmp+ 1 * s V_read_colormap_i <= 0 /\ -1 * s V_read_colormap_i + 1 <= 0 /\ -1 * s V_read_colormap_z + 1 <= 0)%Z
   | 20 => (1 * s V_read_colormap_z <= 0 /\ -1 * s V_read_colormap_z <= 0)%Z
   | 21 => (-1 * s V_read_colormap_z <= 0 /\ 1 * s V_read_colormap_z <= 0 /\ 1 * s V_read_colormap_i <= 0 /\ -1 * s V_read_colormap_i <= 0)%Z
   | 22 => (-1 * s V_read_colormap_i <= 0 /\ 1 * s V_read_colormap_i <= 0 /\ 1 * s V_read_colormap_z <= 0 /\ -1 * s V_read_colormap_z <= 0)%Z
   | 23 => (-1 * s V_read_colormap_z <= 0 /\ -1 * s V_read_colormap_i <= 0)%Z
   | 24 => (-1 * s V_read_colormap_i <= 0 /\ -1 * s V_read_colormap_z <= 0 /\ 1 * s V_read_colormap__tmp+ -1 * s V_read_colormap_i <= 0)%Z
   | 25 => (1 * s V_read_colormap__tmp+ -1 * s V_read_colormap_i <= 0 /\ -1 * s V_read_colormap_z <= 0 /\ -1 * s V_read_colormap_i <= 0)%Z
   | 26 => (-1 * s V_read_colormap_i <= 0 /\ -1 * s V_read_colormap_z <= 0 /\ 1 * s V_read_colormap__tmp+ -1 * s V_read_colormap_i <= 0)%Z
   | 27 => (-1 * s V_read_colormap_i <= 0 /\ -1 * s V_read_colormap_z <= 0 /\ -1 * s V_read_colormap__tmp+ 1 * s V_read_colormap_i + 1 <= 0)%Z
   | 28 => (-1 * s V_read_colormap__tmp+ 1 * s V_read_colormap_i + 1 <= 0 /\ -1 * s V_read_colormap_z <= 0 /\ -1 * s V_read_colormap_i <= 0)%Z
   | 29 => (-1 * s V_read_colormap_i <= 0 /\ -1 * s V_read_colormap_z <= 0 /\ -1 * s V_read_colormap__tmp+ 1 * s V_read_colormap_i + 1 <= 0)%Z
   | 30 => (-1 * s V_read_colormap_z <= 0 /\ -1 * s V_read_colormap_i + 1 <= 0 /\ -1 * s V_read_colormap__tmp+ 1 * s V_read_colormap_i <= 0)%Z
   | 31 => (-1 * s V_read_colormap__tmp+ 1 * s V_read_colormap_i <= 0 /\ -1 * s V_read_colormap_i + 1 <= 0 /\ -1 * s V_read_colormap_z <= 0)%Z
   | 32 => (-1 * s V_read_colormap_z <= 0 /\ -1 * s V_read_colormap_i + 1 <= 0 /\ -1 * s V_read_colormap__tmp+ 1 * s V_read_colormap_i <= 0)%Z
   | 33 => (-1 * s V_read_colormap__tmp+ 1 * s V_read_colormap_i <= 0 /\ -1 * s V_read_colormap_i + 1 <= 0 /\ -1 * s V_read_colormap_z + 1 <= 0)%Z
   | 34 => (1 * s V_read_colormap_z <= 0 /\ -1 * s V_read_colormap_z <= 0)%Z
   | 35 => (-1 * s V_read_colormap_z <= 0 /\ 1 * s V_read_colormap_z <= 0)%Z
   | 36 => (-1 * s V_read_colormap_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_read_colormap (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_read_colormap_cmaplen) <= z)%Q
   | 2 => (max0(s V_read_colormap_cmaplen) + max0(s V_read_colormap_z) <= z)%Q
   | 3 => (max0(s V_read_colormap__tmp) + max0(s V_read_colormap_z) <= z)%Q
   | 4 => (max0(s V_read_colormap__tmp) + max0(s V_read_colormap_z) <= z)%Q
   | 5 => (max0(s V_read_colormap__tmp) + max0(s V_read_colormap_z) <= z)%Q
   | 6 => (max0(s V_read_colormap__tmp) + max0(s V_read_colormap_z) <= z)%Q
   | 7 => (max0(s V_read_colormap__tmp - s V_read_colormap_i)
           + max0(s V_read_colormap_z) <= z)%Q
   | 8 => (max0(s V_read_colormap__tmp - s V_read_colormap_i)
           + max0(s V_read_colormap_z) <= z)%Q
   | 9 => (max0(s V_read_colormap__tmp - s V_read_colormap_i)
           + max0(s V_read_colormap_z) <= z)%Q
   | 10 => (max0(s V_read_colormap__tmp - s V_read_colormap_i)
            + max0(s V_read_colormap_z) <= z)%Q
   | 11 => (max0(s V_read_colormap__tmp - s V_read_colormap_i)
            + max0(s V_read_colormap_z) <= z)%Q
   | 12 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_read_colormap__tmp
                                             - s V_read_colormap_i) (-1
                                                                    + 
                                                                    s V_read_colormap__tmp
                                                                    - 
                                                                    s V_read_colormap_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V_read_colormap__tmp - s V_read_colormap_i);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_read_colormap_z)) (F_check_ge (s V_read_colormap_z) (0))]
     (max0(s V_read_colormap__tmp - s V_read_colormap_i)
      + max0(s V_read_colormap_z) <= z)%Q
   | 13 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_read_colormap__tmp
                                       - s V_read_colormap_i) (1)]
     (max0(s V_read_colormap__tmp - s V_read_colormap_i)
      + max0(s V_read_colormap_z) <= z)%Q
   | 14 => ((1 # 1) + max0(-1 + s V_read_colormap__tmp - s V_read_colormap_i)
            + max0(s V_read_colormap_z) <= z)%Q
   | 15 => ((1 # 1) + max0(-1 + s V_read_colormap__tmp - s V_read_colormap_i)
            + max0(s V_read_colormap_z) <= z)%Q
   | 16 => ((1 # 1) + max0(s V_read_colormap__tmp - s V_read_colormap_i)
            + max0(s V_read_colormap_z) <= z)%Q
   | 17 => ((1 # 1) + max0(s V_read_colormap__tmp - s V_read_colormap_i)
            + max0(s V_read_colormap_z) <= z)%Q
   | 18 => ((1 # 1) + max0(s V_read_colormap__tmp - s V_read_colormap_i)
            + max0(s V_read_colormap_z) <= z)%Q
   | 19 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_read_colormap_z) (0))) (F_max0_ge_0 (s V_read_colormap_z));
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1 + s V_read_colormap_z)) (F_check_ge (-1
                                                                    + s V_read_colormap_z) (0))]
     ((1 # 1) + max0(-1 + s V_read_colormap_z)
      + max0(s V_read_colormap__tmp - s V_read_colormap_i) <= z)%Q
   | 20 => (max0(s V_read_colormap__tmp) + max0(s V_read_colormap_z) <= z)%Q
   | 21 => (max0(s V_read_colormap__tmp - s V_read_colormap_i)
            + max0(s V_read_colormap_z) <= z)%Q
   | 22 => (max0(s V_read_colormap__tmp - s V_read_colormap_i)
            + max0(s V_read_colormap_z) <= z)%Q
   | 23 => (max0(s V_read_colormap__tmp - s V_read_colormap_i)
            + max0(s V_read_colormap_z) <= z)%Q
   | 24 => (max0(s V_read_colormap__tmp - s V_read_colormap_i)
            + max0(s V_read_colormap_z) <= z)%Q
   | 25 => (max0(s V_read_colormap__tmp - s V_read_colormap_i)
            + max0(s V_read_colormap_z) <= z)%Q
   | 26 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_read_colormap__tmp
                                             - s V_read_colormap_i) (-1
                                                                    + 
                                                                    s V_read_colormap__tmp
                                                                    - 
                                                                    s V_read_colormap_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V_read_colormap__tmp - s V_read_colormap_i);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_read_colormap_z)) (F_check_ge (s V_read_colormap_z) (0))]
     (max0(s V_read_colormap__tmp - s V_read_colormap_i)
      + max0(s V_read_colormap_z) <= z)%Q
   | 27 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_read_colormap__tmp
                                       - s V_read_colormap_i) (1)]
     (max0(s V_read_colormap__tmp - s V_read_colormap_i)
      + max0(s V_read_colormap_z) <= z)%Q
   | 28 => ((1 # 1) + max0(-1 + s V_read_colormap__tmp - s V_read_colormap_i)
            + max0(s V_read_colormap_z) <= z)%Q
   | 29 => ((1 # 1) + max0(-1 + s V_read_colormap__tmp - s V_read_colormap_i)
            + max0(s V_read_colormap_z) <= z)%Q
   | 30 => ((1 # 1) + max0(s V_read_colormap__tmp - s V_read_colormap_i)
            + max0(s V_read_colormap_z) <= z)%Q
   | 31 => ((1 # 1) + max0(s V_read_colormap__tmp - s V_read_colormap_i)
            + max0(s V_read_colormap_z) <= z)%Q
   | 32 => ((1 # 1) + max0(s V_read_colormap__tmp - s V_read_colormap_i)
            + max0(s V_read_colormap_z) <= z)%Q
   | 33 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_read_colormap_z) (0))) (F_max0_ge_0 (s V_read_colormap_z));
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1 + s V_read_colormap_z)) (F_check_ge (-1
                                                                    + s V_read_colormap_z) (0))]
     ((1 # 1) + max0(-1 + s V_read_colormap_z)
      + max0(s V_read_colormap__tmp - s V_read_colormap_i) <= z)%Q
   | 34 => (max0(s V_read_colormap__tmp) + max0(s V_read_colormap_z) <= z)%Q
   | 35 => hints
     [(*-1 0*) F_max0_ge_0 (s V_read_colormap__tmp);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_read_colormap_z)) (F_check_ge (s V_read_colormap_z) (0))]
     (max0(s V_read_colormap__tmp) + max0(s V_read_colormap_z) <= z)%Q
   | 36 => (s V_read_colormap_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_read_colormap =>
    [mkPA Q (fun n z s => ai_read_colormap n s /\ annot0_read_colormap n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_read_colormap (proc_start P_read_colormap) s1 (proc_end P_read_colormap) s2 ->
    (s2 V_read_colormap_z <= max0(s1 V_read_colormap_cmaplen))%Q.
Proof.
  prove_bound ipa admissible_ipa P_read_colormap.
Qed.
