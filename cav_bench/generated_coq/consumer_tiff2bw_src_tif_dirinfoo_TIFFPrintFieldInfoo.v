Require Import pasta.Pasta.

Inductive proc: Type :=
  P__TIFFPrintFieldInfo.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V__TIFFPrintFieldInfo_z := 1%positive.
Notation V__TIFFPrintFieldInfo_i := 2%positive.
Notation V__TIFFPrintFieldInfo_tif_dref_off848 := 3%positive.
Notation V__TIFFPrintFieldInfo_fd := 4%positive.
Notation V__TIFFPrintFieldInfo_tif := 5%positive.
Definition Pedges__TIFFPrintFieldInfo: list (edge proc) :=
  (EA 1 (AAssign V__TIFFPrintFieldInfo_z (Some (ENum (0)))) 2)::
  (EA 2 (AAssign V__TIFFPrintFieldInfo_i (Some (ENum (0)))) 3)::
  (EA 3 ANone 4)::(EA 4 AWeaken 5)::(EA 5 (AGuard
  (fun s => ((eval (EVar V__TIFFPrintFieldInfo_i) s) <
  (eval (EVar V__TIFFPrintFieldInfo_tif_dref_off848) s))%Z)) 8)::
  (EA 5 (AGuard (fun s => ((eval (EVar V__TIFFPrintFieldInfo_i) s) >=
  (eval (EVar V__TIFFPrintFieldInfo_tif_dref_off848) s))%Z)) 6)::
  (EA 6 AWeaken 7)::(EA 8 AWeaken 9)::(EA 9 ANone 10)::(EA 10 (AAssign
  V__TIFFPrintFieldInfo_i (Some (EAdd (EVar V__TIFFPrintFieldInfo_i)
  (ENum (1))))) 11)::(EA 11 ANone 12)::(EA 12 ANone 13)::(EA 13 (AAssign
  V__TIFFPrintFieldInfo_z (Some (EAdd (ENum (1))
  (EVar V__TIFFPrintFieldInfo_z)))) 14)::(EA 14 AWeaken 5)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P__TIFFPrintFieldInfo => Pedges__TIFFPrintFieldInfo
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P__TIFFPrintFieldInfo => 7
     end)%positive;
  var_global := var_global
}.

Definition ai__TIFFPrintFieldInfo (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V__TIFFPrintFieldInfo_z <= 0 /\ -1 * s V__TIFFPrintFieldInfo_z <= 0)%Z
   | 3 => (-1 * s V__TIFFPrintFieldInfo_z <= 0 /\ 1 * s V__TIFFPrintFieldInfo_z <= 0 /\ 1 * s V__TIFFPrintFieldInfo_i <= 0 /\ -1 * s V__TIFFPrintFieldInfo_i <= 0)%Z
   | 4 => (-1 * s V__TIFFPrintFieldInfo_i <= 0 /\ 1 * s V__TIFFPrintFieldInfo_i <= 0 /\ 1 * s V__TIFFPrintFieldInfo_z <= 0 /\ -1 * s V__TIFFPrintFieldInfo_z <= 0)%Z
   | 5 => (-1 * s V__TIFFPrintFieldInfo_z <= 0 /\ -1 * s V__TIFFPrintFieldInfo_i <= 0)%Z
   | 6 => (-1 * s V__TIFFPrintFieldInfo_i <= 0 /\ -1 * s V__TIFFPrintFieldInfo_z <= 0 /\ -1 * s V__TIFFPrintFieldInfo_i+ 1 * s V__TIFFPrintFieldInfo_tif_dref_off848 <= 0)%Z
   | 7 => (-1 * s V__TIFFPrintFieldInfo_i+ 1 * s V__TIFFPrintFieldInfo_tif_dref_off848 <= 0 /\ -1 * s V__TIFFPrintFieldInfo_z <= 0 /\ -1 * s V__TIFFPrintFieldInfo_i <= 0)%Z
   | 8 => (-1 * s V__TIFFPrintFieldInfo_i <= 0 /\ -1 * s V__TIFFPrintFieldInfo_z <= 0 /\ 1 * s V__TIFFPrintFieldInfo_i+ -1 * s V__TIFFPrintFieldInfo_tif_dref_off848 + 1 <= 0)%Z
   | 9 => (1 * s V__TIFFPrintFieldInfo_i+ -1 * s V__TIFFPrintFieldInfo_tif_dref_off848 + 1 <= 0 /\ -1 * s V__TIFFPrintFieldInfo_z <= 0 /\ -1 * s V__TIFFPrintFieldInfo_i <= 0)%Z
   | 10 => (-1 * s V__TIFFPrintFieldInfo_i <= 0 /\ -1 * s V__TIFFPrintFieldInfo_z <= 0 /\ 1 * s V__TIFFPrintFieldInfo_i+ -1 * s V__TIFFPrintFieldInfo_tif_dref_off848 + 1 <= 0)%Z
   | 11 => (-1 * s V__TIFFPrintFieldInfo_z <= 0 /\ -1 * s V__TIFFPrintFieldInfo_i + 1 <= 0 /\ 1 * s V__TIFFPrintFieldInfo_i+ -1 * s V__TIFFPrintFieldInfo_tif_dref_off848 <= 0)%Z
   | 12 => (1 * s V__TIFFPrintFieldInfo_i+ -1 * s V__TIFFPrintFieldInfo_tif_dref_off848 <= 0 /\ -1 * s V__TIFFPrintFieldInfo_i + 1 <= 0 /\ -1 * s V__TIFFPrintFieldInfo_z <= 0)%Z
   | 13 => (-1 * s V__TIFFPrintFieldInfo_z <= 0 /\ -1 * s V__TIFFPrintFieldInfo_i + 1 <= 0 /\ 1 * s V__TIFFPrintFieldInfo_i+ -1 * s V__TIFFPrintFieldInfo_tif_dref_off848 <= 0)%Z
   | 14 => (1 * s V__TIFFPrintFieldInfo_i+ -1 * s V__TIFFPrintFieldInfo_tif_dref_off848 <= 0 /\ -1 * s V__TIFFPrintFieldInfo_i + 1 <= 0 /\ -1 * s V__TIFFPrintFieldInfo_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0__TIFFPrintFieldInfo (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V__TIFFPrintFieldInfo_tif_dref_off848) <= z)%Q
   | 2 => (s V__TIFFPrintFieldInfo_z
           + max0(s V__TIFFPrintFieldInfo_tif_dref_off848) <= z)%Q
   | 3 => (s V__TIFFPrintFieldInfo_z
           + max0(-s V__TIFFPrintFieldInfo_i
                  + s V__TIFFPrintFieldInfo_tif_dref_off848) <= z)%Q
   | 4 => (s V__TIFFPrintFieldInfo_z
           + max0(-s V__TIFFPrintFieldInfo_i
                  + s V__TIFFPrintFieldInfo_tif_dref_off848) <= z)%Q
   | 5 => (s V__TIFFPrintFieldInfo_z
           + max0(-s V__TIFFPrintFieldInfo_i
                  + s V__TIFFPrintFieldInfo_tif_dref_off848) <= z)%Q
   | 6 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-s V__TIFFPrintFieldInfo_i
                                             + s V__TIFFPrintFieldInfo_tif_dref_off848) (-1
                                                                    - s V__TIFFPrintFieldInfo_i
                                                                    + s V__TIFFPrintFieldInfo_tif_dref_off848));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                 - s V__TIFFPrintFieldInfo_i
                                                 + s V__TIFFPrintFieldInfo_tif_dref_off848)) (F_check_ge (0) (0))]
     (s V__TIFFPrintFieldInfo_z
      + max0(-s V__TIFFPrintFieldInfo_i
             + s V__TIFFPrintFieldInfo_tif_dref_off848) <= z)%Q
   | 7 => (s V__TIFFPrintFieldInfo_z <= z)%Q
   | 8 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (-s V__TIFFPrintFieldInfo_i
                                       + s V__TIFFPrintFieldInfo_tif_dref_off848) (1)]
     (s V__TIFFPrintFieldInfo_z
      + max0(-s V__TIFFPrintFieldInfo_i
             + s V__TIFFPrintFieldInfo_tif_dref_off848) <= z)%Q
   | 9 => ((1 # 1) + s V__TIFFPrintFieldInfo_z
           + max0(-1 - s V__TIFFPrintFieldInfo_i
                  + s V__TIFFPrintFieldInfo_tif_dref_off848) <= z)%Q
   | 10 => ((1 # 1) + s V__TIFFPrintFieldInfo_z
            + max0(-1 - s V__TIFFPrintFieldInfo_i
                   + s V__TIFFPrintFieldInfo_tif_dref_off848) <= z)%Q
   | 11 => ((1 # 1) + s V__TIFFPrintFieldInfo_z
            + max0(-s V__TIFFPrintFieldInfo_i
                   + s V__TIFFPrintFieldInfo_tif_dref_off848) <= z)%Q
   | 12 => ((1 # 1) + s V__TIFFPrintFieldInfo_z
            + max0(-s V__TIFFPrintFieldInfo_i
                   + s V__TIFFPrintFieldInfo_tif_dref_off848) <= z)%Q
   | 13 => ((1 # 1) + s V__TIFFPrintFieldInfo_z
            + max0(-s V__TIFFPrintFieldInfo_i
                   + s V__TIFFPrintFieldInfo_tif_dref_off848) <= z)%Q
   | 14 => (s V__TIFFPrintFieldInfo_z
            + max0(-s V__TIFFPrintFieldInfo_i
                   + s V__TIFFPrintFieldInfo_tif_dref_off848) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P__TIFFPrintFieldInfo =>
    [mkPA Q (fun n z s => ai__TIFFPrintFieldInfo n s /\ annot0__TIFFPrintFieldInfo n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P__TIFFPrintFieldInfo (proc_start P__TIFFPrintFieldInfo) s1 (proc_end P__TIFFPrintFieldInfo) s2 ->
    (s2 V__TIFFPrintFieldInfo_z <= max0(s1 V__TIFFPrintFieldInfo_tif_dref_off848))%Q.
Proof.
  prove_bound ipa admissible_ipa P__TIFFPrintFieldInfo.
Qed.
