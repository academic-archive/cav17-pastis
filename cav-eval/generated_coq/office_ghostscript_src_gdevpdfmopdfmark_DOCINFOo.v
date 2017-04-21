Require Import pasta.Pasta.

Inductive proc: Type :=
  P_pdfmark_DOCINFO.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_pdfmark_DOCINFO_z := 1%positive.
Notation V_pdfmark_DOCINFO__tmp := 2%positive.
Notation V_pdfmark_DOCINFO__tmp1 := 3%positive.
Notation V_pdfmark_DOCINFO_i := 4%positive.
Notation V_pdfmark_DOCINFO_info_id := 5%positive.
Notation V_pdfmark_DOCINFO_count := 6%positive.
Notation V_pdfmark_DOCINFO_pairs := 7%positive.
Notation V_pdfmark_DOCINFO_pctm := 8%positive.
Notation V_pdfmark_DOCINFO_pdev := 9%positive.
Definition Pedges_pdfmark_DOCINFO: list (edge proc) :=
  (EA 1 (AAssign V_pdfmark_DOCINFO_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_pdfmark_DOCINFO_i) s) >= (eval (ENum (0))
  s))%Z)) 3)::(EA 3 (AGuard (fun s => ((eval (EVar V_pdfmark_DOCINFO__tmp1)
  s) >= (eval (ENum (0)) s))%Z)) 4)::(EA 4 AWeaken 5)::(EA 5 (AAssign
  V_pdfmark_DOCINFO__tmp1 (Some (EVar V_pdfmark_DOCINFO_count))) 6)::
  (EA 6 AWeaken 7)::(EA 7 ANone 8)::(EA 7 ANone 10)::(EA 8 AWeaken 9)::
  (EA 9 ANone 30)::(EA 9 ANone 10)::(EA 10 (AAssign V_pdfmark_DOCINFO_info_id
  None) 11)::(EA 11 (AAssign V_pdfmark_DOCINFO_i (Some (ENum (0)))) 12)::
  (EA 12 ANone 13)::(EA 13 AWeaken 14)::(EA 14 (AGuard
  (fun s => ((eval (EVar V_pdfmark_DOCINFO_i) s) <
  (eval (EVar V_pdfmark_DOCINFO__tmp1) s))%Z)) 19)::(EA 14 (AGuard
  (fun s => ((eval (EVar V_pdfmark_DOCINFO_i) s) >=
  (eval (EVar V_pdfmark_DOCINFO__tmp1) s))%Z)) 15)::(EA 15 AWeaken 16)::
  (EA 16 (AAssign V_pdfmark_DOCINFO__tmp (Some (ENum (0)))) 17)::
  (EA 17 ANone 18)::(EA 18 AWeaken 33)::(EA 19 AWeaken 20)::
  (EA 20 ANone 24)::(EA 20 ANone 21)::(EA 21 AWeaken 22)::(EA 22 ANone 24)::
  (EA 22 ANone 23)::(EA 23 ANone 24)::(EA 24 ANone 25)::(EA 25 (AAssign
  V_pdfmark_DOCINFO_i (Some (EAdd (EVar V_pdfmark_DOCINFO_i)
  (ENum (2))))) 26)::(EA 26 ANone 27)::(EA 27 ANone 28)::(EA 28 (AAssign
  V_pdfmark_DOCINFO_z (Some (EAdd (ENum (1))
  (EVar V_pdfmark_DOCINFO_z)))) 29)::(EA 29 AWeaken 14)::(EA 30 (AAssign
  V_pdfmark_DOCINFO__tmp (Some (ENum (-13)))) 31)::(EA 31 ANone 32)::
  (EA 32 AWeaken 33)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_pdfmark_DOCINFO => Pedges_pdfmark_DOCINFO
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_pdfmark_DOCINFO => 33
     end)%positive;
  var_global := var_global
}.

Definition ai_pdfmark_DOCINFO (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_pdfmark_DOCINFO_z <= 0 /\ -1 * s V_pdfmark_DOCINFO_z <= 0)%Z
   | 3 => (-1 * s V_pdfmark_DOCINFO_z <= 0 /\ 1 * s V_pdfmark_DOCINFO_z <= 0 /\ -1 * s V_pdfmark_DOCINFO_i <= 0)%Z
   | 4 => (-1 * s V_pdfmark_DOCINFO_i <= 0 /\ 1 * s V_pdfmark_DOCINFO_z <= 0 /\ -1 * s V_pdfmark_DOCINFO_z <= 0 /\ -1 * s V_pdfmark_DOCINFO__tmp1 <= 0)%Z
   | 5 => (-1 * s V_pdfmark_DOCINFO__tmp1 <= 0 /\ -1 * s V_pdfmark_DOCINFO_z <= 0 /\ 1 * s V_pdfmark_DOCINFO_z <= 0 /\ -1 * s V_pdfmark_DOCINFO_i <= 0)%Z
   | 6 => (-1 * s V_pdfmark_DOCINFO_i <= 0 /\ 1 * s V_pdfmark_DOCINFO_z <= 0 /\ -1 * s V_pdfmark_DOCINFO_z <= 0)%Z
   | 7 => (-1 * s V_pdfmark_DOCINFO_z <= 0 /\ 1 * s V_pdfmark_DOCINFO_z <= 0 /\ -1 * s V_pdfmark_DOCINFO_i <= 0)%Z
   | 8 => (-1 * s V_pdfmark_DOCINFO_i <= 0 /\ 1 * s V_pdfmark_DOCINFO_z <= 0 /\ -1 * s V_pdfmark_DOCINFO_z <= 0)%Z
   | 9 => (-1 * s V_pdfmark_DOCINFO_z <= 0 /\ 1 * s V_pdfmark_DOCINFO_z <= 0 /\ -1 * s V_pdfmark_DOCINFO_i <= 0)%Z
   | 10 => (-1 * s V_pdfmark_DOCINFO_i <= 0 /\ 1 * s V_pdfmark_DOCINFO_z <= 0 /\ -1 * s V_pdfmark_DOCINFO_z <= 0)%Z
   | 11 => (-1 * s V_pdfmark_DOCINFO_z <= 0 /\ 1 * s V_pdfmark_DOCINFO_z <= 0 /\ -1 * s V_pdfmark_DOCINFO_i <= 0)%Z
   | 12 => (1 * s V_pdfmark_DOCINFO_z <= 0 /\ -1 * s V_pdfmark_DOCINFO_z <= 0 /\ 1 * s V_pdfmark_DOCINFO_i <= 0 /\ -1 * s V_pdfmark_DOCINFO_i <= 0)%Z
   | 13 => (-1 * s V_pdfmark_DOCINFO_i <= 0 /\ 1 * s V_pdfmark_DOCINFO_i <= 0 /\ -1 * s V_pdfmark_DOCINFO_z <= 0 /\ 1 * s V_pdfmark_DOCINFO_z <= 0)%Z
   | 14 => (-1 * s V_pdfmark_DOCINFO_z <= 0 /\ -1 * s V_pdfmark_DOCINFO_i <= 0)%Z
   | 15 => (-1 * s V_pdfmark_DOCINFO_i <= 0 /\ -1 * s V_pdfmark_DOCINFO_z <= 0 /\ 1 * s V_pdfmark_DOCINFO__tmp1+ -1 * s V_pdfmark_DOCINFO_i <= 0)%Z
   | 16 => (1 * s V_pdfmark_DOCINFO__tmp1+ -1 * s V_pdfmark_DOCINFO_i <= 0 /\ -1 * s V_pdfmark_DOCINFO_z <= 0 /\ -1 * s V_pdfmark_DOCINFO_i <= 0)%Z
   | 17 => (-1 * s V_pdfmark_DOCINFO_i <= 0 /\ -1 * s V_pdfmark_DOCINFO_z <= 0 /\ 1 * s V_pdfmark_DOCINFO__tmp1+ -1 * s V_pdfmark_DOCINFO_i <= 0 /\ 1 * s V_pdfmark_DOCINFO__tmp <= 0 /\ -1 * s V_pdfmark_DOCINFO__tmp <= 0)%Z
   | 18 => (-1 * s V_pdfmark_DOCINFO__tmp <= 0 /\ 1 * s V_pdfmark_DOCINFO__tmp <= 0 /\ 1 * s V_pdfmark_DOCINFO__tmp1+ -1 * s V_pdfmark_DOCINFO_i <= 0 /\ -1 * s V_pdfmark_DOCINFO_z <= 0 /\ -1 * s V_pdfmark_DOCINFO_i <= 0)%Z
   | 19 => (-1 * s V_pdfmark_DOCINFO_i <= 0 /\ -1 * s V_pdfmark_DOCINFO_z <= 0 /\ -1 * s V_pdfmark_DOCINFO__tmp1+ 1 * s V_pdfmark_DOCINFO_i + 1 <= 0)%Z
   | 20 => (-1 * s V_pdfmark_DOCINFO__tmp1+ 1 * s V_pdfmark_DOCINFO_i + 1 <= 0 /\ -1 * s V_pdfmark_DOCINFO_z <= 0 /\ -1 * s V_pdfmark_DOCINFO_i <= 0)%Z
   | 21 => (-1 * s V_pdfmark_DOCINFO_i <= 0 /\ -1 * s V_pdfmark_DOCINFO_z <= 0 /\ -1 * s V_pdfmark_DOCINFO__tmp1+ 1 * s V_pdfmark_DOCINFO_i + 1 <= 0)%Z
   | 22 => (-1 * s V_pdfmark_DOCINFO__tmp1+ 1 * s V_pdfmark_DOCINFO_i + 1 <= 0 /\ -1 * s V_pdfmark_DOCINFO_z <= 0 /\ -1 * s V_pdfmark_DOCINFO_i <= 0)%Z
   | 23 => (-1 * s V_pdfmark_DOCINFO_i <= 0 /\ -1 * s V_pdfmark_DOCINFO_z <= 0 /\ -1 * s V_pdfmark_DOCINFO__tmp1+ 1 * s V_pdfmark_DOCINFO_i + 1 <= 0)%Z
   | 24 => (-1 * s V_pdfmark_DOCINFO__tmp1+ 1 * s V_pdfmark_DOCINFO_i + 1 <= 0 /\ -1 * s V_pdfmark_DOCINFO_z <= 0 /\ -1 * s V_pdfmark_DOCINFO_i <= 0)%Z
   | 25 => (-1 * s V_pdfmark_DOCINFO_i <= 0 /\ -1 * s V_pdfmark_DOCINFO_z <= 0 /\ -1 * s V_pdfmark_DOCINFO__tmp1+ 1 * s V_pdfmark_DOCINFO_i + 1 <= 0)%Z
   | 26 => (-1 * s V_pdfmark_DOCINFO_z <= 0 /\ -1 * s V_pdfmark_DOCINFO_i + 2 <= 0 /\ -1 * s V_pdfmark_DOCINFO__tmp1+ 1 * s V_pdfmark_DOCINFO_i + -1 <= 0)%Z
   | 27 => (-1 * s V_pdfmark_DOCINFO__tmp1+ 1 * s V_pdfmark_DOCINFO_i + -1 <= 0 /\ -1 * s V_pdfmark_DOCINFO_i + 2 <= 0 /\ -1 * s V_pdfmark_DOCINFO_z <= 0)%Z
   | 28 => (-1 * s V_pdfmark_DOCINFO_z <= 0 /\ -1 * s V_pdfmark_DOCINFO_i + 2 <= 0 /\ -1 * s V_pdfmark_DOCINFO__tmp1+ 1 * s V_pdfmark_DOCINFO_i + -1 <= 0)%Z
   | 29 => (-1 * s V_pdfmark_DOCINFO__tmp1+ 1 * s V_pdfmark_DOCINFO_i + -1 <= 0 /\ -1 * s V_pdfmark_DOCINFO_i + 2 <= 0 /\ -1 * s V_pdfmark_DOCINFO_z + 1 <= 0)%Z
   | 30 => (-1 * s V_pdfmark_DOCINFO_i <= 0 /\ 1 * s V_pdfmark_DOCINFO_z <= 0 /\ -1 * s V_pdfmark_DOCINFO_z <= 0)%Z
   | 31 => (-1 * s V_pdfmark_DOCINFO_z <= 0 /\ 1 * s V_pdfmark_DOCINFO_z <= 0 /\ -1 * s V_pdfmark_DOCINFO_i <= 0 /\ 1 * s V_pdfmark_DOCINFO__tmp + 13 <= 0 /\ -1 * s V_pdfmark_DOCINFO__tmp + -13 <= 0)%Z
   | 32 => (-1 * s V_pdfmark_DOCINFO__tmp + -13 <= 0 /\ 1 * s V_pdfmark_DOCINFO__tmp + 13 <= 0 /\ -1 * s V_pdfmark_DOCINFO_i <= 0 /\ 1 * s V_pdfmark_DOCINFO_z <= 0 /\ -1 * s V_pdfmark_DOCINFO_z <= 0)%Z
   | 33 => (1 * s V_pdfmark_DOCINFO__tmp <= 0 /\ -1 * s V_pdfmark_DOCINFO_z <= 0 /\ -1 * s V_pdfmark_DOCINFO_i <= 0 /\ -1 * s V_pdfmark_DOCINFO__tmp + -13 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_pdfmark_DOCINFO (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((1 # 2) * max0(1 + s V_pdfmark_DOCINFO_count) <= z)%Q
   | 2 => (s V_pdfmark_DOCINFO_z
           + (1 # 2) * max0(1 + s V_pdfmark_DOCINFO_count) <= z)%Q
   | 3 => (s V_pdfmark_DOCINFO_z
           + (1 # 2) * max0(1 + s V_pdfmark_DOCINFO_count) <= z)%Q
   | 4 => (s V_pdfmark_DOCINFO_z
           + (1 # 2) * max0(1 + s V_pdfmark_DOCINFO_count) <= z)%Q
   | 5 => (s V_pdfmark_DOCINFO_z
           + (1 # 2) * max0(1 + s V_pdfmark_DOCINFO_count) <= z)%Q
   | 6 => (s V_pdfmark_DOCINFO_z
           + (1 # 2) * max0(1 + s V_pdfmark_DOCINFO__tmp1) <= z)%Q
   | 7 => (s V_pdfmark_DOCINFO_z
           + (1 # 2) * max0(1 + s V_pdfmark_DOCINFO__tmp1) <= z)%Q
   | 8 => (s V_pdfmark_DOCINFO_z
           + (1 # 2) * max0(1 + s V_pdfmark_DOCINFO__tmp1) <= z)%Q
   | 9 => (s V_pdfmark_DOCINFO_z
           + (1 # 2) * max0(1 + s V_pdfmark_DOCINFO__tmp1) <= z)%Q
   | 10 => (s V_pdfmark_DOCINFO_z
            + (1 # 2) * max0(1 + s V_pdfmark_DOCINFO__tmp1) <= z)%Q
   | 11 => (s V_pdfmark_DOCINFO_z
            + (1 # 2) * max0(1 + s V_pdfmark_DOCINFO__tmp1) <= z)%Q
   | 12 => (s V_pdfmark_DOCINFO_z
            + (1 # 2) * max0(1 + s V_pdfmark_DOCINFO__tmp1
                             - s V_pdfmark_DOCINFO_i) <= z)%Q
   | 13 => (s V_pdfmark_DOCINFO_z
            + (1 # 2) * max0(1 + s V_pdfmark_DOCINFO__tmp1
                             - s V_pdfmark_DOCINFO_i) <= z)%Q
   | 14 => (s V_pdfmark_DOCINFO_z
            + (1 # 2) * max0(1 + s V_pdfmark_DOCINFO__tmp1
                             - s V_pdfmark_DOCINFO_i) <= z)%Q
   | 15 => (s V_pdfmark_DOCINFO_z
            + (1 # 2) * max0(1 + s V_pdfmark_DOCINFO__tmp1
                             - s V_pdfmark_DOCINFO_i) <= z)%Q
   | 16 => (s V_pdfmark_DOCINFO_z
            + (1 # 2) * max0(1 + s V_pdfmark_DOCINFO__tmp1
                             - s V_pdfmark_DOCINFO_i) <= z)%Q
   | 17 => (s V_pdfmark_DOCINFO_z
            + (1 # 2) * max0(1 + s V_pdfmark_DOCINFO__tmp1
                             - s V_pdfmark_DOCINFO_i) <= z)%Q
   | 18 => hints
     [(*-0.5 0*) F_max0_monotonic (F_check_ge (1 + s V_pdfmark_DOCINFO__tmp1
                                               - s V_pdfmark_DOCINFO_i) (-1
                                                                    + s V_pdfmark_DOCINFO__tmp1
                                                                    - s V_pdfmark_DOCINFO_i));
      (*-0.5 0*) F_max0_ge_0 (-1 + s V_pdfmark_DOCINFO__tmp1
                              - s V_pdfmark_DOCINFO_i)]
     (s V_pdfmark_DOCINFO_z
      + (1 # 2) * max0(1 + s V_pdfmark_DOCINFO__tmp1 - s V_pdfmark_DOCINFO_i) <= z)%Q
   | 19 => hints
     [(*-0.5 0*) F_max0_pre_decrement 1 (1 + s V_pdfmark_DOCINFO__tmp1
                                         - s V_pdfmark_DOCINFO_i) (2)]
     (s V_pdfmark_DOCINFO_z
      + (1 # 2) * max0(1 + s V_pdfmark_DOCINFO__tmp1 - s V_pdfmark_DOCINFO_i) <= z)%Q
   | 20 => ((1 # 1) + s V_pdfmark_DOCINFO_z
            + (1 # 2) * max0(-1 + s V_pdfmark_DOCINFO__tmp1
                             - s V_pdfmark_DOCINFO_i) <= z)%Q
   | 21 => ((1 # 1) + s V_pdfmark_DOCINFO_z
            + (1 # 2) * max0(-1 + s V_pdfmark_DOCINFO__tmp1
                             - s V_pdfmark_DOCINFO_i) <= z)%Q
   | 22 => ((1 # 1) + s V_pdfmark_DOCINFO_z
            + (1 # 2) * max0(-1 + s V_pdfmark_DOCINFO__tmp1
                             - s V_pdfmark_DOCINFO_i) <= z)%Q
   | 23 => ((1 # 1) + s V_pdfmark_DOCINFO_z
            + (1 # 2) * max0(-1 + s V_pdfmark_DOCINFO__tmp1
                             - s V_pdfmark_DOCINFO_i) <= z)%Q
   | 24 => ((1 # 1) + s V_pdfmark_DOCINFO_z
            + (1 # 2) * max0(-1 + s V_pdfmark_DOCINFO__tmp1
                             - s V_pdfmark_DOCINFO_i) <= z)%Q
   | 25 => ((1 # 1) + s V_pdfmark_DOCINFO_z
            + (1 # 2) * max0(-1 + s V_pdfmark_DOCINFO__tmp1
                             - s V_pdfmark_DOCINFO_i) <= z)%Q
   | 26 => ((1 # 1) + s V_pdfmark_DOCINFO_z
            + (1 # 2) * max0(1 + s V_pdfmark_DOCINFO__tmp1
                             - s V_pdfmark_DOCINFO_i) <= z)%Q
   | 27 => ((1 # 1) + s V_pdfmark_DOCINFO_z
            + (1 # 2) * max0(1 + s V_pdfmark_DOCINFO__tmp1
                             - s V_pdfmark_DOCINFO_i) <= z)%Q
   | 28 => ((1 # 1) + s V_pdfmark_DOCINFO_z
            + (1 # 2) * max0(1 + s V_pdfmark_DOCINFO__tmp1
                             - s V_pdfmark_DOCINFO_i) <= z)%Q
   | 29 => (s V_pdfmark_DOCINFO_z
            + (1 # 2) * max0(1 + s V_pdfmark_DOCINFO__tmp1
                             - s V_pdfmark_DOCINFO_i) <= z)%Q
   | 30 => (s V_pdfmark_DOCINFO_z
            + (1 # 2) * max0(1 + s V_pdfmark_DOCINFO__tmp1) <= z)%Q
   | 31 => (s V_pdfmark_DOCINFO_z
            + (1 # 2) * max0(1 + s V_pdfmark_DOCINFO__tmp1) <= z)%Q
   | 32 => hints
     [(*-0.5 0*) F_max0_ge_0 (1 + s V_pdfmark_DOCINFO__tmp1)]
     (s V_pdfmark_DOCINFO_z + (1 # 2) * max0(1 + s V_pdfmark_DOCINFO__tmp1) <= z)%Q
   | 33 => (s V_pdfmark_DOCINFO_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_pdfmark_DOCINFO =>
    [mkPA Q (fun n z s => ai_pdfmark_DOCINFO n s /\ annot0_pdfmark_DOCINFO n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_pdfmark_DOCINFO (proc_start P_pdfmark_DOCINFO) s1 (proc_end P_pdfmark_DOCINFO) s2 ->
    (s2 V_pdfmark_DOCINFO_z <= (1 # 2) * max0(1 + s1 V_pdfmark_DOCINFO_count))%Q.
Proof.
  prove_bound ipa admissible_ipa P_pdfmark_DOCINFO.
Qed.
