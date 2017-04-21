Require Import pasta.Pasta.

Inductive proc: Type :=
  P_gs_image_cleanup.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_gs_image_cleanup_z := 1%positive.
Notation V_gs_image_cleanup_i := 2%positive.
Notation V_gs_image_cleanup_pie_dref_off16 := 3%positive.
Notation V_gs_image_cleanup_pie_dref_off40 := 4%positive.
Notation V_gs_image_cleanup_pie := 5%positive.
Definition Pedges_gs_image_cleanup: list (edge proc) :=
  (EA 1 (AAssign V_gs_image_cleanup_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_gs_image_cleanup_i (Some (ENum (0)))) 3)::(EA 3 ANone 4)::
  (EA 4 AWeaken 5)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_gs_image_cleanup_i) s) <
  (eval (EVar V_gs_image_cleanup_pie_dref_off40) s))%Z)) 13)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_gs_image_cleanup_i) s) >=
  (eval (EVar V_gs_image_cleanup_pie_dref_off40) s))%Z)) 6)::
  (EA 6 AWeaken 7)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_gs_image_cleanup_pie_dref_off16) s) <>
  (eval (ENum (0)) s))%Z)) 11)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_gs_image_cleanup_pie_dref_off16) s) =
  (eval (ENum (0)) s))%Z)) 8)::(EA 8 AWeaken 9)::(EA 9 ANone 10)::
  (EA 10 AWeaken 12)::(EA 11 AWeaken 12)::(EA 13 AWeaken 14)::
  (EA 14 ANone 15)::(EA 15 (AAssign V_gs_image_cleanup_i
  (Some (EAdd (EVar V_gs_image_cleanup_i) (ENum (1))))) 16)::
  (EA 16 ANone 17)::(EA 17 ANone 18)::(EA 18 (AAssign V_gs_image_cleanup_z
  (Some (EAdd (ENum (1)) (EVar V_gs_image_cleanup_z)))) 19)::
  (EA 19 AWeaken 5)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_gs_image_cleanup => Pedges_gs_image_cleanup
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_gs_image_cleanup => 12
     end)%positive;
  var_global := var_global
}.

Definition ai_gs_image_cleanup (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_gs_image_cleanup_z <= 0 /\ -1 * s V_gs_image_cleanup_z <= 0)%Z
   | 3 => (-1 * s V_gs_image_cleanup_z <= 0 /\ 1 * s V_gs_image_cleanup_z <= 0 /\ 1 * s V_gs_image_cleanup_i <= 0 /\ -1 * s V_gs_image_cleanup_i <= 0)%Z
   | 4 => (-1 * s V_gs_image_cleanup_i <= 0 /\ 1 * s V_gs_image_cleanup_i <= 0 /\ 1 * s V_gs_image_cleanup_z <= 0 /\ -1 * s V_gs_image_cleanup_z <= 0)%Z
   | 5 => (-1 * s V_gs_image_cleanup_z <= 0 /\ -1 * s V_gs_image_cleanup_i <= 0)%Z
   | 6 => (-1 * s V_gs_image_cleanup_i <= 0 /\ -1 * s V_gs_image_cleanup_z <= 0 /\ -1 * s V_gs_image_cleanup_i+ 1 * s V_gs_image_cleanup_pie_dref_off40 <= 0)%Z
   | 7 => (-1 * s V_gs_image_cleanup_i+ 1 * s V_gs_image_cleanup_pie_dref_off40 <= 0 /\ -1 * s V_gs_image_cleanup_z <= 0 /\ -1 * s V_gs_image_cleanup_i <= 0)%Z
   | 8 => (-1 * s V_gs_image_cleanup_i <= 0 /\ -1 * s V_gs_image_cleanup_z <= 0 /\ -1 * s V_gs_image_cleanup_i+ 1 * s V_gs_image_cleanup_pie_dref_off40 <= 0 /\ 1 * s V_gs_image_cleanup_pie_dref_off16 <= 0 /\ -1 * s V_gs_image_cleanup_pie_dref_off16 <= 0)%Z
   | 9 => (-1 * s V_gs_image_cleanup_pie_dref_off16 <= 0 /\ 1 * s V_gs_image_cleanup_pie_dref_off16 <= 0 /\ -1 * s V_gs_image_cleanup_i+ 1 * s V_gs_image_cleanup_pie_dref_off40 <= 0 /\ -1 * s V_gs_image_cleanup_z <= 0 /\ -1 * s V_gs_image_cleanup_i <= 0)%Z
   | 10 => (-1 * s V_gs_image_cleanup_i <= 0 /\ -1 * s V_gs_image_cleanup_z <= 0 /\ -1 * s V_gs_image_cleanup_i+ 1 * s V_gs_image_cleanup_pie_dref_off40 <= 0 /\ 1 * s V_gs_image_cleanup_pie_dref_off16 <= 0 /\ -1 * s V_gs_image_cleanup_pie_dref_off16 <= 0)%Z
   | 11 => (-1 * s V_gs_image_cleanup_i <= 0 /\ -1 * s V_gs_image_cleanup_z <= 0 /\ -1 * s V_gs_image_cleanup_i+ 1 * s V_gs_image_cleanup_pie_dref_off40 <= 0)%Z
   | 12 => (-1 * s V_gs_image_cleanup_i+ 1 * s V_gs_image_cleanup_pie_dref_off40 <= 0 /\ -1 * s V_gs_image_cleanup_z <= 0 /\ -1 * s V_gs_image_cleanup_i <= 0)%Z
   | 13 => (-1 * s V_gs_image_cleanup_i <= 0 /\ -1 * s V_gs_image_cleanup_z <= 0 /\ 1 * s V_gs_image_cleanup_i+ -1 * s V_gs_image_cleanup_pie_dref_off40 + 1 <= 0)%Z
   | 14 => (1 * s V_gs_image_cleanup_i+ -1 * s V_gs_image_cleanup_pie_dref_off40 + 1 <= 0 /\ -1 * s V_gs_image_cleanup_z <= 0 /\ -1 * s V_gs_image_cleanup_i <= 0)%Z
   | 15 => (-1 * s V_gs_image_cleanup_i <= 0 /\ -1 * s V_gs_image_cleanup_z <= 0 /\ 1 * s V_gs_image_cleanup_i+ -1 * s V_gs_image_cleanup_pie_dref_off40 + 1 <= 0)%Z
   | 16 => (-1 * s V_gs_image_cleanup_z <= 0 /\ -1 * s V_gs_image_cleanup_i + 1 <= 0 /\ 1 * s V_gs_image_cleanup_i+ -1 * s V_gs_image_cleanup_pie_dref_off40 <= 0)%Z
   | 17 => (1 * s V_gs_image_cleanup_i+ -1 * s V_gs_image_cleanup_pie_dref_off40 <= 0 /\ -1 * s V_gs_image_cleanup_i + 1 <= 0 /\ -1 * s V_gs_image_cleanup_z <= 0)%Z
   | 18 => (-1 * s V_gs_image_cleanup_z <= 0 /\ -1 * s V_gs_image_cleanup_i + 1 <= 0 /\ 1 * s V_gs_image_cleanup_i+ -1 * s V_gs_image_cleanup_pie_dref_off40 <= 0)%Z
   | 19 => (1 * s V_gs_image_cleanup_i+ -1 * s V_gs_image_cleanup_pie_dref_off40 <= 0 /\ -1 * s V_gs_image_cleanup_i + 1 <= 0 /\ -1 * s V_gs_image_cleanup_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_gs_image_cleanup (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_gs_image_cleanup_pie_dref_off40) <= z)%Q
   | 2 => (s V_gs_image_cleanup_z + max0(s V_gs_image_cleanup_pie_dref_off40) <= z)%Q
   | 3 => (s V_gs_image_cleanup_z
           + max0(-s V_gs_image_cleanup_i
                  + s V_gs_image_cleanup_pie_dref_off40) <= z)%Q
   | 4 => (s V_gs_image_cleanup_z
           + max0(-s V_gs_image_cleanup_i
                  + s V_gs_image_cleanup_pie_dref_off40) <= z)%Q
   | 5 => (s V_gs_image_cleanup_z
           + max0(-s V_gs_image_cleanup_i
                  + s V_gs_image_cleanup_pie_dref_off40) <= z)%Q
   | 6 => (s V_gs_image_cleanup_z
           + max0(-s V_gs_image_cleanup_i
                  + s V_gs_image_cleanup_pie_dref_off40) <= z)%Q
   | 7 => (s V_gs_image_cleanup_z
           + max0(-s V_gs_image_cleanup_i
                  + s V_gs_image_cleanup_pie_dref_off40) <= z)%Q
   | 8 => (s V_gs_image_cleanup_z
           + max0(-s V_gs_image_cleanup_i
                  + s V_gs_image_cleanup_pie_dref_off40) <= z)%Q
   | 9 => (s V_gs_image_cleanup_z
           + max0(-s V_gs_image_cleanup_i
                  + s V_gs_image_cleanup_pie_dref_off40) <= z)%Q
   | 10 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-s V_gs_image_cleanup_i
                                             + s V_gs_image_cleanup_pie_dref_off40) (-1
                                                                    - s V_gs_image_cleanup_i
                                                                    + s V_gs_image_cleanup_pie_dref_off40));
      (*-1 0*) F_max0_ge_0 (-1 - s V_gs_image_cleanup_i
                            + s V_gs_image_cleanup_pie_dref_off40)]
     (s V_gs_image_cleanup_z
      + max0(-s V_gs_image_cleanup_i + s V_gs_image_cleanup_pie_dref_off40) <= z)%Q
   | 11 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-s V_gs_image_cleanup_i
                                             + s V_gs_image_cleanup_pie_dref_off40) (-1
                                                                    - s V_gs_image_cleanup_i
                                                                    + s V_gs_image_cleanup_pie_dref_off40));
      (*-1 0*) F_max0_ge_0 (-1 - s V_gs_image_cleanup_i
                            + s V_gs_image_cleanup_pie_dref_off40)]
     (s V_gs_image_cleanup_z
      + max0(-s V_gs_image_cleanup_i + s V_gs_image_cleanup_pie_dref_off40) <= z)%Q
   | 12 => (s V_gs_image_cleanup_z <= z)%Q
   | 13 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (-s V_gs_image_cleanup_i
                                       + s V_gs_image_cleanup_pie_dref_off40) (1)]
     (s V_gs_image_cleanup_z
      + max0(-s V_gs_image_cleanup_i + s V_gs_image_cleanup_pie_dref_off40) <= z)%Q
   | 14 => ((1 # 1) + s V_gs_image_cleanup_z
            + max0(-1 - s V_gs_image_cleanup_i
                   + s V_gs_image_cleanup_pie_dref_off40) <= z)%Q
   | 15 => ((1 # 1) + s V_gs_image_cleanup_z
            + max0(-1 - s V_gs_image_cleanup_i
                   + s V_gs_image_cleanup_pie_dref_off40) <= z)%Q
   | 16 => ((1 # 1) + s V_gs_image_cleanup_z
            + max0(-s V_gs_image_cleanup_i
                   + s V_gs_image_cleanup_pie_dref_off40) <= z)%Q
   | 17 => ((1 # 1) + s V_gs_image_cleanup_z
            + max0(-s V_gs_image_cleanup_i
                   + s V_gs_image_cleanup_pie_dref_off40) <= z)%Q
   | 18 => ((1 # 1) + s V_gs_image_cleanup_z
            + max0(-s V_gs_image_cleanup_i
                   + s V_gs_image_cleanup_pie_dref_off40) <= z)%Q
   | 19 => (s V_gs_image_cleanup_z
            + max0(-s V_gs_image_cleanup_i
                   + s V_gs_image_cleanup_pie_dref_off40) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_gs_image_cleanup =>
    [mkPA Q (fun n z s => ai_gs_image_cleanup n s /\ annot0_gs_image_cleanup n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_gs_image_cleanup (proc_start P_gs_image_cleanup) s1 (proc_end P_gs_image_cleanup) s2 ->
    (s2 V_gs_image_cleanup_z <= max0(s1 V_gs_image_cleanup_pie_dref_off40))%Q.
Proof.
  prove_bound ipa admissible_ipa P_gs_image_cleanup.
Qed.
