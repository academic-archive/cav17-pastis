Require Import pasta.Pasta.

Inductive proc: Type :=
  P__TIFFMergeFieldInfo.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V__TIFFMergeFieldInfo_z := 1%positive.
Notation V__TIFFMergeFieldInfo__tmp := 2%positive.
Notation V__TIFFMergeFieldInfo_i := 3%positive.
Notation V__TIFFMergeFieldInfo_tif_dref_off848 := 4%positive.
Notation V__TIFFMergeFieldInfo_info := 5%positive.
Notation V__TIFFMergeFieldInfo_n := 6%positive.
Notation V__TIFFMergeFieldInfo_tif := 7%positive.
Definition Pedges__TIFFMergeFieldInfo: list (edge proc) :=
  (EA 1 (AAssign V__TIFFMergeFieldInfo_z (Some (ENum (0)))) 2)::
  (EA 2 (AAssign V__TIFFMergeFieldInfo__tmp
  (Some (EVar V__TIFFMergeFieldInfo_n))) 3)::(EA 3 AWeaken 4)::(EA 4 (AGuard
  (fun s => ((eval (EVar V__TIFFMergeFieldInfo_tif_dref_off848) s) >
  (eval (ENum (0)) s))%Z)) 7)::(EA 4 (AGuard
  (fun s => ((eval (EVar V__TIFFMergeFieldInfo_tif_dref_off848) s) <=
  (eval (ENum (0)) s))%Z)) 5)::(EA 5 AWeaken 6)::(EA 6 ANone 9)::
  (EA 7 AWeaken 8)::(EA 8 ANone 9)::(EA 9 (AAssign V__TIFFMergeFieldInfo_i
  (Some (ENum (0)))) 10)::(EA 10 ANone 11)::(EA 11 AWeaken 12)::
  (EA 12 (AGuard (fun s => ((eval (EVar V__TIFFMergeFieldInfo_i) s) <
  (eval (EVar V__TIFFMergeFieldInfo__tmp) s))%Z)) 24)::(EA 12 (AGuard
  (fun s => ((eval (EVar V__TIFFMergeFieldInfo_i) s) >=
  (eval (EVar V__TIFFMergeFieldInfo__tmp) s))%Z)) 13)::(EA 13 AWeaken 14)::
  (EA 14 (AGuard
  (fun s => ((eval (EVar V__TIFFMergeFieldInfo_tif_dref_off848) s) >
  (eval (ENum (0)) s))%Z)) 19)::(EA 14 (AGuard
  (fun s => ((eval (EVar V__TIFFMergeFieldInfo_tif_dref_off848) s) <=
  (eval (ENum (0)) s))%Z)) 15)::(EA 15 AWeaken 16)::(EA 16 (AAssign
  V__TIFFMergeFieldInfo_tif_dref_off848
  (Some (EAdd (EVar V__TIFFMergeFieldInfo_tif_dref_off848)
  (EVar V__TIFFMergeFieldInfo__tmp)))) 17)::(EA 17 ANone 18)::
  (EA 18 AWeaken 23)::(EA 19 AWeaken 20)::(EA 20 (AAssign
  V__TIFFMergeFieldInfo_tif_dref_off848
  (Some (EAdd (EVar V__TIFFMergeFieldInfo_tif_dref_off848)
  (EVar V__TIFFMergeFieldInfo__tmp)))) 21)::(EA 21 ANone 22)::
  (EA 22 AWeaken 23)::(EA 24 AWeaken 25)::(EA 25 ANone 26)::(EA 26 (AAssign
  V__TIFFMergeFieldInfo_i (Some (EAdd (EVar V__TIFFMergeFieldInfo_i)
  (ENum (1))))) 27)::(EA 27 ANone 28)::(EA 28 ANone 29)::(EA 29 (AAssign
  V__TIFFMergeFieldInfo_z (Some (EAdd (ENum (1))
  (EVar V__TIFFMergeFieldInfo_z)))) 30)::(EA 30 AWeaken 12)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P__TIFFMergeFieldInfo => Pedges__TIFFMergeFieldInfo
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P__TIFFMergeFieldInfo => 23
     end)%positive;
  var_global := var_global
}.

Definition ai__TIFFMergeFieldInfo (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V__TIFFMergeFieldInfo_z <= 0 /\ -1 * s V__TIFFMergeFieldInfo_z <= 0)%Z
   | 3 => (-1 * s V__TIFFMergeFieldInfo_z <= 0 /\ 1 * s V__TIFFMergeFieldInfo_z <= 0)%Z
   | 4 => (1 * s V__TIFFMergeFieldInfo_z <= 0 /\ -1 * s V__TIFFMergeFieldInfo_z <= 0)%Z
   | 5 => (-1 * s V__TIFFMergeFieldInfo_z <= 0 /\ 1 * s V__TIFFMergeFieldInfo_z <= 0 /\ 1 * s V__TIFFMergeFieldInfo_tif_dref_off848 <= 0)%Z
   | 6 => (1 * s V__TIFFMergeFieldInfo_tif_dref_off848 <= 0 /\ 1 * s V__TIFFMergeFieldInfo_z <= 0 /\ -1 * s V__TIFFMergeFieldInfo_z <= 0)%Z
   | 7 => (-1 * s V__TIFFMergeFieldInfo_z <= 0 /\ 1 * s V__TIFFMergeFieldInfo_z <= 0 /\ -1 * s V__TIFFMergeFieldInfo_tif_dref_off848 + 1 <= 0)%Z
   | 8 => (-1 * s V__TIFFMergeFieldInfo_tif_dref_off848 + 1 <= 0 /\ 1 * s V__TIFFMergeFieldInfo_z <= 0 /\ -1 * s V__TIFFMergeFieldInfo_z <= 0)%Z
   | 9 => (-1 * s V__TIFFMergeFieldInfo_z <= 0 /\ 1 * s V__TIFFMergeFieldInfo_z <= 0)%Z
   | 10 => (1 * s V__TIFFMergeFieldInfo_z <= 0 /\ -1 * s V__TIFFMergeFieldInfo_z <= 0 /\ 1 * s V__TIFFMergeFieldInfo_i <= 0 /\ -1 * s V__TIFFMergeFieldInfo_i <= 0)%Z
   | 11 => (-1 * s V__TIFFMergeFieldInfo_i <= 0 /\ 1 * s V__TIFFMergeFieldInfo_i <= 0 /\ -1 * s V__TIFFMergeFieldInfo_z <= 0 /\ 1 * s V__TIFFMergeFieldInfo_z <= 0)%Z
   | 12 => (-1 * s V__TIFFMergeFieldInfo_z <= 0 /\ -1 * s V__TIFFMergeFieldInfo_i <= 0)%Z
   | 13 => (-1 * s V__TIFFMergeFieldInfo_i <= 0 /\ -1 * s V__TIFFMergeFieldInfo_z <= 0 /\ 1 * s V__TIFFMergeFieldInfo__tmp+ -1 * s V__TIFFMergeFieldInfo_i <= 0)%Z
   | 14 => (1 * s V__TIFFMergeFieldInfo__tmp+ -1 * s V__TIFFMergeFieldInfo_i <= 0 /\ -1 * s V__TIFFMergeFieldInfo_z <= 0 /\ -1 * s V__TIFFMergeFieldInfo_i <= 0)%Z
   | 15 => (-1 * s V__TIFFMergeFieldInfo_i <= 0 /\ -1 * s V__TIFFMergeFieldInfo_z <= 0 /\ 1 * s V__TIFFMergeFieldInfo__tmp+ -1 * s V__TIFFMergeFieldInfo_i <= 0 /\ 1 * s V__TIFFMergeFieldInfo_tif_dref_off848 <= 0)%Z
   | 16 => (1 * s V__TIFFMergeFieldInfo_tif_dref_off848 <= 0 /\ 1 * s V__TIFFMergeFieldInfo__tmp+ -1 * s V__TIFFMergeFieldInfo_i <= 0 /\ -1 * s V__TIFFMergeFieldInfo_z <= 0 /\ -1 * s V__TIFFMergeFieldInfo_i <= 0)%Z
   | 17 => (-1 * s V__TIFFMergeFieldInfo_i <= 0 /\ -1 * s V__TIFFMergeFieldInfo_z <= 0 /\ 1 * s V__TIFFMergeFieldInfo__tmp+ -1 * s V__TIFFMergeFieldInfo_i <= 0 /\ -1 * s V__TIFFMergeFieldInfo__tmp+ 1 * s V__TIFFMergeFieldInfo_tif_dref_off848 <= 0)%Z
   | 18 => (-1 * s V__TIFFMergeFieldInfo__tmp+ 1 * s V__TIFFMergeFieldInfo_tif_dref_off848 <= 0 /\ 1 * s V__TIFFMergeFieldInfo__tmp+ -1 * s V__TIFFMergeFieldInfo_i <= 0 /\ -1 * s V__TIFFMergeFieldInfo_z <= 0 /\ -1 * s V__TIFFMergeFieldInfo_i <= 0)%Z
   | 19 => (-1 * s V__TIFFMergeFieldInfo_i <= 0 /\ -1 * s V__TIFFMergeFieldInfo_z <= 0 /\ 1 * s V__TIFFMergeFieldInfo__tmp+ -1 * s V__TIFFMergeFieldInfo_i <= 0 /\ -1 * s V__TIFFMergeFieldInfo_tif_dref_off848 + 1 <= 0)%Z
   | 20 => (-1 * s V__TIFFMergeFieldInfo_tif_dref_off848 + 1 <= 0 /\ 1 * s V__TIFFMergeFieldInfo__tmp+ -1 * s V__TIFFMergeFieldInfo_i <= 0 /\ -1 * s V__TIFFMergeFieldInfo_z <= 0 /\ -1 * s V__TIFFMergeFieldInfo_i <= 0)%Z
   | 21 => (-1 * s V__TIFFMergeFieldInfo_i <= 0 /\ -1 * s V__TIFFMergeFieldInfo_z <= 0 /\ 1 * s V__TIFFMergeFieldInfo__tmp+ -1 * s V__TIFFMergeFieldInfo_i <= 0 /\ 1 * s V__TIFFMergeFieldInfo__tmp+ -1 * s V__TIFFMergeFieldInfo_tif_dref_off848 + 1 <= 0)%Z
   | 22 => (1 * s V__TIFFMergeFieldInfo__tmp+ -1 * s V__TIFFMergeFieldInfo_tif_dref_off848 + 1 <= 0 /\ 1 * s V__TIFFMergeFieldInfo__tmp+ -1 * s V__TIFFMergeFieldInfo_i <= 0 /\ -1 * s V__TIFFMergeFieldInfo_z <= 0 /\ -1 * s V__TIFFMergeFieldInfo_i <= 0)%Z
   | 23 => (-1 * s V__TIFFMergeFieldInfo_i <= 0 /\ -1 * s V__TIFFMergeFieldInfo_z <= 0 /\ 1 * s V__TIFFMergeFieldInfo__tmp+ -1 * s V__TIFFMergeFieldInfo_i <= 0)%Z
   | 24 => (-1 * s V__TIFFMergeFieldInfo_i <= 0 /\ -1 * s V__TIFFMergeFieldInfo_z <= 0 /\ -1 * s V__TIFFMergeFieldInfo__tmp+ 1 * s V__TIFFMergeFieldInfo_i + 1 <= 0)%Z
   | 25 => (-1 * s V__TIFFMergeFieldInfo__tmp+ 1 * s V__TIFFMergeFieldInfo_i + 1 <= 0 /\ -1 * s V__TIFFMergeFieldInfo_z <= 0 /\ -1 * s V__TIFFMergeFieldInfo_i <= 0)%Z
   | 26 => (-1 * s V__TIFFMergeFieldInfo_i <= 0 /\ -1 * s V__TIFFMergeFieldInfo_z <= 0 /\ -1 * s V__TIFFMergeFieldInfo__tmp+ 1 * s V__TIFFMergeFieldInfo_i + 1 <= 0)%Z
   | 27 => (-1 * s V__TIFFMergeFieldInfo_z <= 0 /\ -1 * s V__TIFFMergeFieldInfo_i + 1 <= 0 /\ -1 * s V__TIFFMergeFieldInfo__tmp+ 1 * s V__TIFFMergeFieldInfo_i <= 0)%Z
   | 28 => (-1 * s V__TIFFMergeFieldInfo__tmp+ 1 * s V__TIFFMergeFieldInfo_i <= 0 /\ -1 * s V__TIFFMergeFieldInfo_i + 1 <= 0 /\ -1 * s V__TIFFMergeFieldInfo_z <= 0)%Z
   | 29 => (-1 * s V__TIFFMergeFieldInfo_z <= 0 /\ -1 * s V__TIFFMergeFieldInfo_i + 1 <= 0 /\ -1 * s V__TIFFMergeFieldInfo__tmp+ 1 * s V__TIFFMergeFieldInfo_i <= 0)%Z
   | 30 => (-1 * s V__TIFFMergeFieldInfo__tmp+ 1 * s V__TIFFMergeFieldInfo_i <= 0 /\ -1 * s V__TIFFMergeFieldInfo_i + 1 <= 0 /\ -1 * s V__TIFFMergeFieldInfo_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0__TIFFMergeFieldInfo (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V__TIFFMergeFieldInfo_n) <= z)%Q
   | 2 => (s V__TIFFMergeFieldInfo_z + max0(s V__TIFFMergeFieldInfo_n) <= z)%Q
   | 3 => (s V__TIFFMergeFieldInfo_z + max0(s V__TIFFMergeFieldInfo__tmp) <= z)%Q
   | 4 => (s V__TIFFMergeFieldInfo_z + max0(s V__TIFFMergeFieldInfo__tmp) <= z)%Q
   | 5 => (s V__TIFFMergeFieldInfo_z + max0(s V__TIFFMergeFieldInfo__tmp) <= z)%Q
   | 6 => (s V__TIFFMergeFieldInfo_z + max0(s V__TIFFMergeFieldInfo__tmp) <= z)%Q
   | 7 => (s V__TIFFMergeFieldInfo_z + max0(s V__TIFFMergeFieldInfo__tmp) <= z)%Q
   | 8 => (s V__TIFFMergeFieldInfo_z + max0(s V__TIFFMergeFieldInfo__tmp) <= z)%Q
   | 9 => (s V__TIFFMergeFieldInfo_z + max0(s V__TIFFMergeFieldInfo__tmp) <= z)%Q
   | 10 => (s V__TIFFMergeFieldInfo_z
            + max0(s V__TIFFMergeFieldInfo__tmp - s V__TIFFMergeFieldInfo_i) <= z)%Q
   | 11 => (s V__TIFFMergeFieldInfo_z
            + max0(s V__TIFFMergeFieldInfo__tmp - s V__TIFFMergeFieldInfo_i) <= z)%Q
   | 12 => (s V__TIFFMergeFieldInfo_z
            + max0(s V__TIFFMergeFieldInfo__tmp - s V__TIFFMergeFieldInfo_i) <= z)%Q
   | 13 => (s V__TIFFMergeFieldInfo_z
            + max0(s V__TIFFMergeFieldInfo__tmp - s V__TIFFMergeFieldInfo_i) <= z)%Q
   | 14 => (s V__TIFFMergeFieldInfo_z
            + max0(s V__TIFFMergeFieldInfo__tmp - s V__TIFFMergeFieldInfo_i) <= z)%Q
   | 15 => (s V__TIFFMergeFieldInfo_z
            + max0(s V__TIFFMergeFieldInfo__tmp - s V__TIFFMergeFieldInfo_i) <= z)%Q
   | 16 => (s V__TIFFMergeFieldInfo_z
            + max0(s V__TIFFMergeFieldInfo__tmp - s V__TIFFMergeFieldInfo_i) <= z)%Q
   | 17 => (s V__TIFFMergeFieldInfo_z
            + max0(s V__TIFFMergeFieldInfo__tmp - s V__TIFFMergeFieldInfo_i) <= z)%Q
   | 18 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V__TIFFMergeFieldInfo__tmp
                                             - s V__TIFFMergeFieldInfo_i) (-1
                                                                    + s V__TIFFMergeFieldInfo__tmp
                                                                    - s V__TIFFMergeFieldInfo_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V__TIFFMergeFieldInfo__tmp
                            - s V__TIFFMergeFieldInfo_i)]
     (s V__TIFFMergeFieldInfo_z
      + max0(s V__TIFFMergeFieldInfo__tmp - s V__TIFFMergeFieldInfo_i) <= z)%Q
   | 19 => (s V__TIFFMergeFieldInfo_z
            + max0(s V__TIFFMergeFieldInfo__tmp - s V__TIFFMergeFieldInfo_i) <= z)%Q
   | 20 => (s V__TIFFMergeFieldInfo_z
            + max0(s V__TIFFMergeFieldInfo__tmp - s V__TIFFMergeFieldInfo_i) <= z)%Q
   | 21 => (s V__TIFFMergeFieldInfo_z
            + max0(s V__TIFFMergeFieldInfo__tmp - s V__TIFFMergeFieldInfo_i) <= z)%Q
   | 22 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V__TIFFMergeFieldInfo__tmp
                                             - s V__TIFFMergeFieldInfo_i) (-1
                                                                    + s V__TIFFMergeFieldInfo__tmp
                                                                    - s V__TIFFMergeFieldInfo_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V__TIFFMergeFieldInfo__tmp
                            - s V__TIFFMergeFieldInfo_i)]
     (s V__TIFFMergeFieldInfo_z
      + max0(s V__TIFFMergeFieldInfo__tmp - s V__TIFFMergeFieldInfo_i) <= z)%Q
   | 23 => (s V__TIFFMergeFieldInfo_z <= z)%Q
   | 24 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V__TIFFMergeFieldInfo__tmp
                                       - s V__TIFFMergeFieldInfo_i) (1)]
     (s V__TIFFMergeFieldInfo_z
      + max0(s V__TIFFMergeFieldInfo__tmp - s V__TIFFMergeFieldInfo_i) <= z)%Q
   | 25 => ((1 # 1) + s V__TIFFMergeFieldInfo_z
            + max0(-1 + s V__TIFFMergeFieldInfo__tmp
                   - s V__TIFFMergeFieldInfo_i) <= z)%Q
   | 26 => ((1 # 1) + s V__TIFFMergeFieldInfo_z
            + max0(-1 + s V__TIFFMergeFieldInfo__tmp
                   - s V__TIFFMergeFieldInfo_i) <= z)%Q
   | 27 => ((1 # 1) + s V__TIFFMergeFieldInfo_z
            + max0(s V__TIFFMergeFieldInfo__tmp - s V__TIFFMergeFieldInfo_i) <= z)%Q
   | 28 => ((1 # 1) + s V__TIFFMergeFieldInfo_z
            + max0(s V__TIFFMergeFieldInfo__tmp - s V__TIFFMergeFieldInfo_i) <= z)%Q
   | 29 => ((1 # 1) + s V__TIFFMergeFieldInfo_z
            + max0(s V__TIFFMergeFieldInfo__tmp - s V__TIFFMergeFieldInfo_i) <= z)%Q
   | 30 => (s V__TIFFMergeFieldInfo_z
            + max0(s V__TIFFMergeFieldInfo__tmp - s V__TIFFMergeFieldInfo_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P__TIFFMergeFieldInfo =>
    [mkPA Q (fun n z s => ai__TIFFMergeFieldInfo n s /\ annot0__TIFFMergeFieldInfo n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P__TIFFMergeFieldInfo (proc_start P__TIFFMergeFieldInfo) s1 (proc_end P__TIFFMergeFieldInfo) s2 ->
    (s2 V__TIFFMergeFieldInfo_z <= max0(s1 V__TIFFMergeFieldInfo_n))%Q.
Proof.
  prove_bound ipa admissible_ipa P__TIFFMergeFieldInfo.
Qed.
