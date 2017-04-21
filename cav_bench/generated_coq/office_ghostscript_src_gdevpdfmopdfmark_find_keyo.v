Require Import pasta.Pasta.

Inductive proc: Type :=
  P_pdfmark_find_key.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_pdfmark_find_key_z := 1%positive.
Notation V_pdfmark_find_key__tmp := 2%positive.
Notation V_pdfmark_find_key__tmp1 := 3%positive.
Notation V_pdfmark_find_key_i := 4%positive.
Notation V_pdfmark_find_key_count := 5%positive.
Notation V_pdfmark_find_key_key := 6%positive.
Notation V_pdfmark_find_key_pairs := 7%positive.
Notation V_pdfmark_find_key_pstr := 8%positive.
Definition Pedges_pdfmark_find_key: list (edge proc) :=
  (EA 1 (AAssign V_pdfmark_find_key_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_pdfmark_find_key_i) s) >= (eval (ENum (0))
  s))%Z)) 3)::(EA 3 (AGuard (fun s => ((eval (EVar V_pdfmark_find_key__tmp)
  s) >= (eval (ENum (0)) s))%Z)) 4)::(EA 4 AWeaken 5)::(EA 5 (AAssign
  V_pdfmark_find_key__tmp (Some (EVar V_pdfmark_find_key_count))) 6)::
  (EA 6 (AAssign V_pdfmark_find_key_i (Some (ENum (0)))) 7)::(EA 7 ANone 8)::
  (EA 8 AWeaken 9)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_pdfmark_find_key_i) s) <
  (eval (EVar V_pdfmark_find_key__tmp) s))%Z)) 14)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_pdfmark_find_key_i) s) >=
  (eval (EVar V_pdfmark_find_key__tmp) s))%Z)) 10)::(EA 10 AWeaken 11)::
  (EA 11 (AAssign V_pdfmark_find_key__tmp1 (Some (ENum (0)))) 12)::
  (EA 12 ANone 13)::(EA 13 AWeaken 25)::(EA 14 AWeaken 15)::
  (EA 15 ANone 22)::(EA 15 ANone 16)::(EA 16 ANone 17)::(EA 17 (AAssign
  V_pdfmark_find_key_i (Some (EAdd (EVar V_pdfmark_find_key_i)
  (ENum (2))))) 18)::(EA 18 ANone 19)::(EA 19 ANone 20)::(EA 20 (AAssign
  V_pdfmark_find_key_z (Some (EAdd (ENum (1))
  (EVar V_pdfmark_find_key_z)))) 21)::(EA 21 AWeaken 9)::(EA 22 (AAssign
  V_pdfmark_find_key__tmp1 (Some (ENum (1)))) 23)::(EA 23 ANone 24)::
  (EA 24 AWeaken 25)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_pdfmark_find_key => Pedges_pdfmark_find_key
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_pdfmark_find_key => 25
     end)%positive;
  var_global := var_global
}.

Definition ai_pdfmark_find_key (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_pdfmark_find_key_z <= 0 /\ -1 * s V_pdfmark_find_key_z <= 0)%Z
   | 3 => (-1 * s V_pdfmark_find_key_z <= 0 /\ 1 * s V_pdfmark_find_key_z <= 0 /\ -1 * s V_pdfmark_find_key_i <= 0)%Z
   | 4 => (-1 * s V_pdfmark_find_key_i <= 0 /\ 1 * s V_pdfmark_find_key_z <= 0 /\ -1 * s V_pdfmark_find_key_z <= 0 /\ -1 * s V_pdfmark_find_key__tmp <= 0)%Z
   | 5 => (-1 * s V_pdfmark_find_key__tmp <= 0 /\ -1 * s V_pdfmark_find_key_z <= 0 /\ 1 * s V_pdfmark_find_key_z <= 0 /\ -1 * s V_pdfmark_find_key_i <= 0)%Z
   | 6 => (-1 * s V_pdfmark_find_key_i <= 0 /\ 1 * s V_pdfmark_find_key_z <= 0 /\ -1 * s V_pdfmark_find_key_z <= 0)%Z
   | 7 => (-1 * s V_pdfmark_find_key_z <= 0 /\ 1 * s V_pdfmark_find_key_z <= 0 /\ 1 * s V_pdfmark_find_key_i <= 0 /\ -1 * s V_pdfmark_find_key_i <= 0)%Z
   | 8 => (-1 * s V_pdfmark_find_key_i <= 0 /\ 1 * s V_pdfmark_find_key_i <= 0 /\ 1 * s V_pdfmark_find_key_z <= 0 /\ -1 * s V_pdfmark_find_key_z <= 0)%Z
   | 9 => (-1 * s V_pdfmark_find_key_z <= 0 /\ -1 * s V_pdfmark_find_key_i <= 0)%Z
   | 10 => (-1 * s V_pdfmark_find_key_i <= 0 /\ -1 * s V_pdfmark_find_key_z <= 0 /\ 1 * s V_pdfmark_find_key__tmp+ -1 * s V_pdfmark_find_key_i <= 0)%Z
   | 11 => (1 * s V_pdfmark_find_key__tmp+ -1 * s V_pdfmark_find_key_i <= 0 /\ -1 * s V_pdfmark_find_key_z <= 0 /\ -1 * s V_pdfmark_find_key_i <= 0)%Z
   | 12 => (-1 * s V_pdfmark_find_key_i <= 0 /\ -1 * s V_pdfmark_find_key_z <= 0 /\ 1 * s V_pdfmark_find_key__tmp+ -1 * s V_pdfmark_find_key_i <= 0 /\ 1 * s V_pdfmark_find_key__tmp1 <= 0 /\ -1 * s V_pdfmark_find_key__tmp1 <= 0)%Z
   | 13 => (-1 * s V_pdfmark_find_key__tmp1 <= 0 /\ 1 * s V_pdfmark_find_key__tmp1 <= 0 /\ 1 * s V_pdfmark_find_key__tmp+ -1 * s V_pdfmark_find_key_i <= 0 /\ -1 * s V_pdfmark_find_key_z <= 0 /\ -1 * s V_pdfmark_find_key_i <= 0)%Z
   | 14 => (-1 * s V_pdfmark_find_key_i <= 0 /\ -1 * s V_pdfmark_find_key_z <= 0 /\ -1 * s V_pdfmark_find_key__tmp+ 1 * s V_pdfmark_find_key_i + 1 <= 0)%Z
   | 15 => (-1 * s V_pdfmark_find_key__tmp+ 1 * s V_pdfmark_find_key_i + 1 <= 0 /\ -1 * s V_pdfmark_find_key_z <= 0 /\ -1 * s V_pdfmark_find_key_i <= 0)%Z
   | 16 => (-1 * s V_pdfmark_find_key_i <= 0 /\ -1 * s V_pdfmark_find_key_z <= 0 /\ -1 * s V_pdfmark_find_key__tmp+ 1 * s V_pdfmark_find_key_i + 1 <= 0)%Z
   | 17 => (-1 * s V_pdfmark_find_key__tmp+ 1 * s V_pdfmark_find_key_i + 1 <= 0 /\ -1 * s V_pdfmark_find_key_z <= 0 /\ -1 * s V_pdfmark_find_key_i <= 0)%Z
   | 18 => (-1 * s V_pdfmark_find_key_z <= 0 /\ -1 * s V_pdfmark_find_key__tmp+ 1 * s V_pdfmark_find_key_i + -1 <= 0 /\ -1 * s V_pdfmark_find_key_i + 2 <= 0)%Z
   | 19 => (-1 * s V_pdfmark_find_key_i + 2 <= 0 /\ -1 * s V_pdfmark_find_key__tmp+ 1 * s V_pdfmark_find_key_i + -1 <= 0 /\ -1 * s V_pdfmark_find_key_z <= 0)%Z
   | 20 => (-1 * s V_pdfmark_find_key_z <= 0 /\ -1 * s V_pdfmark_find_key__tmp+ 1 * s V_pdfmark_find_key_i + -1 <= 0 /\ -1 * s V_pdfmark_find_key_i + 2 <= 0)%Z
   | 21 => (-1 * s V_pdfmark_find_key_i + 2 <= 0 /\ -1 * s V_pdfmark_find_key__tmp+ 1 * s V_pdfmark_find_key_i + -1 <= 0 /\ -1 * s V_pdfmark_find_key_z + 1 <= 0)%Z
   | 22 => (-1 * s V_pdfmark_find_key_i <= 0 /\ -1 * s V_pdfmark_find_key_z <= 0 /\ -1 * s V_pdfmark_find_key__tmp+ 1 * s V_pdfmark_find_key_i + 1 <= 0)%Z
   | 23 => (-1 * s V_pdfmark_find_key__tmp+ 1 * s V_pdfmark_find_key_i + 1 <= 0 /\ -1 * s V_pdfmark_find_key_z <= 0 /\ -1 * s V_pdfmark_find_key_i <= 0 /\ 1 * s V_pdfmark_find_key__tmp1 + -1 <= 0 /\ -1 * s V_pdfmark_find_key__tmp1 + 1 <= 0)%Z
   | 24 => (-1 * s V_pdfmark_find_key__tmp1 + 1 <= 0 /\ 1 * s V_pdfmark_find_key__tmp1 + -1 <= 0 /\ -1 * s V_pdfmark_find_key_i <= 0 /\ -1 * s V_pdfmark_find_key_z <= 0 /\ -1 * s V_pdfmark_find_key__tmp+ 1 * s V_pdfmark_find_key_i + 1 <= 0)%Z
   | 25 => (-1 * s V_pdfmark_find_key__tmp1 <= 0 /\ -1 * s V_pdfmark_find_key_z <= 0 /\ -1 * s V_pdfmark_find_key_i <= 0 /\ 1 * s V_pdfmark_find_key__tmp1 + -1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_pdfmark_find_key (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((1 # 2) * max0(1 + s V_pdfmark_find_key_count) <= z)%Q
   | 2 => (s V_pdfmark_find_key_z
           + (1 # 2) * max0(1 + s V_pdfmark_find_key_count) <= z)%Q
   | 3 => (s V_pdfmark_find_key_z
           + (1 # 2) * max0(1 + s V_pdfmark_find_key_count) <= z)%Q
   | 4 => (s V_pdfmark_find_key_z
           + (1 # 2) * max0(1 + s V_pdfmark_find_key_count) <= z)%Q
   | 5 => (s V_pdfmark_find_key_z
           + (1 # 2) * max0(1 + s V_pdfmark_find_key_count) <= z)%Q
   | 6 => (s V_pdfmark_find_key_z
           + (1 # 2) * max0(1 + s V_pdfmark_find_key__tmp) <= z)%Q
   | 7 => (s V_pdfmark_find_key_z
           + (1 # 2) * max0(1 + s V_pdfmark_find_key__tmp
                            - s V_pdfmark_find_key_i) <= z)%Q
   | 8 => (s V_pdfmark_find_key_z
           + (1 # 2) * max0(1 + s V_pdfmark_find_key__tmp
                            - s V_pdfmark_find_key_i) <= z)%Q
   | 9 => (s V_pdfmark_find_key_z
           + (1 # 2) * max0(1 + s V_pdfmark_find_key__tmp
                            - s V_pdfmark_find_key_i) <= z)%Q
   | 10 => (s V_pdfmark_find_key_z
            + (1 # 2) * max0(1 + s V_pdfmark_find_key__tmp
                             - s V_pdfmark_find_key_i) <= z)%Q
   | 11 => (s V_pdfmark_find_key_z
            + (1 # 2) * max0(1 + s V_pdfmark_find_key__tmp
                             - s V_pdfmark_find_key_i) <= z)%Q
   | 12 => (s V_pdfmark_find_key_z
            + (1 # 2) * max0(1 + s V_pdfmark_find_key__tmp
                             - s V_pdfmark_find_key_i) <= z)%Q
   | 13 => hints
     [(*-0.5 0*) F_max0_monotonic (F_check_ge (1 + s V_pdfmark_find_key__tmp
                                               - s V_pdfmark_find_key_i) (-1
                                                                    + s V_pdfmark_find_key__tmp
                                                                    - s V_pdfmark_find_key_i));
      (*-0.5 0*) F_max0_ge_0 (-1 + s V_pdfmark_find_key__tmp
                              - s V_pdfmark_find_key_i)]
     (s V_pdfmark_find_key_z
      + (1 # 2) * max0(1 + s V_pdfmark_find_key__tmp - s V_pdfmark_find_key_i) <= z)%Q
   | 14 => hints
     [(*-0.5 0*) F_max0_pre_decrement 1 (1 + s V_pdfmark_find_key__tmp
                                         - s V_pdfmark_find_key_i) (2)]
     (s V_pdfmark_find_key_z
      + (1 # 2) * max0(1 + s V_pdfmark_find_key__tmp - s V_pdfmark_find_key_i) <= z)%Q
   | 15 => ((1 # 1) + s V_pdfmark_find_key_z
            + (1 # 2) * max0(-1 + s V_pdfmark_find_key__tmp
                             - s V_pdfmark_find_key_i) <= z)%Q
   | 16 => ((1 # 1) + s V_pdfmark_find_key_z
            + (1 # 2) * max0(-1 + s V_pdfmark_find_key__tmp
                             - s V_pdfmark_find_key_i) <= z)%Q
   | 17 => ((1 # 1) + s V_pdfmark_find_key_z
            + (1 # 2) * max0(-1 + s V_pdfmark_find_key__tmp
                             - s V_pdfmark_find_key_i) <= z)%Q
   | 18 => ((1 # 1) + s V_pdfmark_find_key_z
            + (1 # 2) * max0(1 + s V_pdfmark_find_key__tmp
                             - s V_pdfmark_find_key_i) <= z)%Q
   | 19 => ((1 # 1) + s V_pdfmark_find_key_z
            + (1 # 2) * max0(1 + s V_pdfmark_find_key__tmp
                             - s V_pdfmark_find_key_i) <= z)%Q
   | 20 => ((1 # 1) + s V_pdfmark_find_key_z
            + (1 # 2) * max0(1 + s V_pdfmark_find_key__tmp
                             - s V_pdfmark_find_key_i) <= z)%Q
   | 21 => (s V_pdfmark_find_key_z
            + (1 # 2) * max0(1 + s V_pdfmark_find_key__tmp
                             - s V_pdfmark_find_key_i) <= z)%Q
   | 22 => ((1 # 1) + s V_pdfmark_find_key_z
            + (1 # 2) * max0(-1 + s V_pdfmark_find_key__tmp
                             - s V_pdfmark_find_key_i) <= z)%Q
   | 23 => ((1 # 1) + s V_pdfmark_find_key_z
            + (1 # 2) * max0(-1 + s V_pdfmark_find_key__tmp
                             - s V_pdfmark_find_key_i) <= z)%Q
   | 24 => hints
     [(*-1 0*) F_one;
      (*-0.5 0*) F_max0_ge_0 (-1 + s V_pdfmark_find_key__tmp
                              - s V_pdfmark_find_key_i)]
     ((1 # 1) + s V_pdfmark_find_key_z
      + (1 # 2) * max0(-1 + s V_pdfmark_find_key__tmp
                       - s V_pdfmark_find_key_i) <= z)%Q
   | 25 => (s V_pdfmark_find_key_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_pdfmark_find_key =>
    [mkPA Q (fun n z s => ai_pdfmark_find_key n s /\ annot0_pdfmark_find_key n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_pdfmark_find_key (proc_start P_pdfmark_find_key) s1 (proc_end P_pdfmark_find_key) s2 ->
    (s2 V_pdfmark_find_key_z <= (1 # 2) * max0(1
                                               + s1 V_pdfmark_find_key_count))%Q.
Proof.
  prove_bound ipa admissible_ipa P_pdfmark_find_key.
Qed.
