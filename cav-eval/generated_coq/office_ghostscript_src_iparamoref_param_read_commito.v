Require Import pasta.Pasta.

Inductive proc: Type :=
  P_ref_param_read_commit.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_ref_param_read_commit_z := 1%positive.
Notation V_ref_param_read_commit__tmp := 2%positive.
Notation V_ref_param_read_commit_ecode := 3%positive.
Notation V_ref_param_read_commit_i := 4%positive.
Notation V_ref_param_read_commit_plist_dref_off48 := 5%positive.
Notation V_ref_param_read_commit_plist_dref_off8_off24 := 6%positive.
Notation V_ref_param_read_commit_plist := 7%positive.
Definition Pedges_ref_param_read_commit: list (edge proc) :=
  (EA 1 (AAssign V_ref_param_read_commit_z (Some (ENum (0)))) 2)::
  (EA 2 (AGuard
  (fun s => ((eval (EVar V_ref_param_read_commit_plist_dref_off48) s) >=
  (eval (ENum (0)) s))%Z)) 3)::(EA 3 (AGuard
  (fun s => ((eval (EVar V_ref_param_read_commit_i) s) >= (eval (ENum (0))
  s))%Z)) 4)::(EA 4 AWeaken 5)::(EA 5 (AAssign V_ref_param_read_commit_ecode
  (Some (ENum (0)))) 6)::(EA 6 AWeaken 7)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_ref_param_read_commit_plist_dref_off8_off24) s) <>
  (eval (ENum (0)) s))%Z)) 12)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_ref_param_read_commit_plist_dref_off8_off24) s) =
  (eval (ENum (0)) s))%Z)) 8)::(EA 8 AWeaken 9)::(EA 9 (AAssign
  V_ref_param_read_commit__tmp (Some (ENum (0)))) 10)::(EA 10 ANone 11)::
  (EA 11 AWeaken 21)::(EA 12 AWeaken 13)::(EA 13 (AAssign
  V_ref_param_read_commit_i (Some (ENum (0)))) 14)::(EA 14 ANone 15)::
  (EA 15 AWeaken 16)::(EA 16 (AGuard
  (fun s => ((eval (EVar V_ref_param_read_commit_i) s) <
  (eval (EVar V_ref_param_read_commit_plist_dref_off48) s))%Z)) 22)::
  (EA 16 (AGuard (fun s => ((eval (EVar V_ref_param_read_commit_i) s) >=
  (eval (EVar V_ref_param_read_commit_plist_dref_off48) s))%Z)) 17)::
  (EA 17 AWeaken 18)::(EA 18 (AAssign V_ref_param_read_commit__tmp
  (Some (EVar V_ref_param_read_commit_ecode))) 19)::(EA 19 ANone 20)::
  (EA 20 AWeaken 21)::(EA 22 AWeaken 23)::(EA 23 ANone 24)::
  (EA 23 ANone 26)::(EA 24 (AAssign V_ref_param_read_commit_ecode
  (Some (ENum (-21)))) 25)::(EA 25 ANone 26)::(EA 26 ANone 27)::
  (EA 27 (AAssign V_ref_param_read_commit_i
  (Some (EAdd (EVar V_ref_param_read_commit_i) (ENum (1))))) 28)::
  (EA 28 ANone 29)::(EA 29 ANone 30)::(EA 30 (AAssign
  V_ref_param_read_commit_z (Some (EAdd (ENum (1))
  (EVar V_ref_param_read_commit_z)))) 31)::(EA 31 AWeaken 16)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_ref_param_read_commit => Pedges_ref_param_read_commit
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_ref_param_read_commit => 21
     end)%positive;
  var_global := var_global
}.

Definition ai_ref_param_read_commit (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_ref_param_read_commit_z <= 0 /\ -1 * s V_ref_param_read_commit_z <= 0)%Z
   | 3 => (-1 * s V_ref_param_read_commit_z <= 0 /\ 1 * s V_ref_param_read_commit_z <= 0 /\ -1 * s V_ref_param_read_commit_plist_dref_off48 <= 0)%Z
   | 4 => (-1 * s V_ref_param_read_commit_plist_dref_off48 <= 0 /\ 1 * s V_ref_param_read_commit_z <= 0 /\ -1 * s V_ref_param_read_commit_z <= 0 /\ -1 * s V_ref_param_read_commit_i <= 0)%Z
   | 5 => (-1 * s V_ref_param_read_commit_i <= 0 /\ -1 * s V_ref_param_read_commit_z <= 0 /\ 1 * s V_ref_param_read_commit_z <= 0 /\ -1 * s V_ref_param_read_commit_plist_dref_off48 <= 0)%Z
   | 6 => (-1 * s V_ref_param_read_commit_plist_dref_off48 <= 0 /\ 1 * s V_ref_param_read_commit_z <= 0 /\ -1 * s V_ref_param_read_commit_z <= 0 /\ -1 * s V_ref_param_read_commit_i <= 0 /\ 1 * s V_ref_param_read_commit_ecode <= 0 /\ -1 * s V_ref_param_read_commit_ecode <= 0)%Z
   | 7 => (-1 * s V_ref_param_read_commit_ecode <= 0 /\ 1 * s V_ref_param_read_commit_ecode <= 0 /\ -1 * s V_ref_param_read_commit_i <= 0 /\ -1 * s V_ref_param_read_commit_z <= 0 /\ 1 * s V_ref_param_read_commit_z <= 0 /\ -1 * s V_ref_param_read_commit_plist_dref_off48 <= 0)%Z
   | 8 => (-1 * s V_ref_param_read_commit_plist_dref_off48 <= 0 /\ 1 * s V_ref_param_read_commit_z <= 0 /\ -1 * s V_ref_param_read_commit_z <= 0 /\ -1 * s V_ref_param_read_commit_i <= 0 /\ 1 * s V_ref_param_read_commit_ecode <= 0 /\ -1 * s V_ref_param_read_commit_ecode <= 0 /\ 1 * s V_ref_param_read_commit_plist_dref_off8_off24 <= 0 /\ -1 * s V_ref_param_read_commit_plist_dref_off8_off24 <= 0)%Z
   | 9 => (-1 * s V_ref_param_read_commit_plist_dref_off8_off24 <= 0 /\ 1 * s V_ref_param_read_commit_plist_dref_off8_off24 <= 0 /\ -1 * s V_ref_param_read_commit_ecode <= 0 /\ 1 * s V_ref_param_read_commit_ecode <= 0 /\ -1 * s V_ref_param_read_commit_i <= 0 /\ -1 * s V_ref_param_read_commit_z <= 0 /\ 1 * s V_ref_param_read_commit_z <= 0 /\ -1 * s V_ref_param_read_commit_plist_dref_off48 <= 0)%Z
   | 10 => (-1 * s V_ref_param_read_commit_plist_dref_off48 <= 0 /\ 1 * s V_ref_param_read_commit_z <= 0 /\ -1 * s V_ref_param_read_commit_z <= 0 /\ -1 * s V_ref_param_read_commit_i <= 0 /\ 1 * s V_ref_param_read_commit_ecode <= 0 /\ -1 * s V_ref_param_read_commit_ecode <= 0 /\ 1 * s V_ref_param_read_commit_plist_dref_off8_off24 <= 0 /\ -1 * s V_ref_param_read_commit_plist_dref_off8_off24 <= 0 /\ 1 * s V_ref_param_read_commit__tmp <= 0 /\ -1 * s V_ref_param_read_commit__tmp <= 0)%Z
   | 11 => (-1 * s V_ref_param_read_commit__tmp <= 0 /\ 1 * s V_ref_param_read_commit__tmp <= 0 /\ -1 * s V_ref_param_read_commit_plist_dref_off8_off24 <= 0 /\ 1 * s V_ref_param_read_commit_plist_dref_off8_off24 <= 0 /\ -1 * s V_ref_param_read_commit_ecode <= 0 /\ 1 * s V_ref_param_read_commit_ecode <= 0 /\ -1 * s V_ref_param_read_commit_i <= 0 /\ -1 * s V_ref_param_read_commit_z <= 0 /\ 1 * s V_ref_param_read_commit_z <= 0 /\ -1 * s V_ref_param_read_commit_plist_dref_off48 <= 0)%Z
   | 12 => (-1 * s V_ref_param_read_commit_plist_dref_off48 <= 0 /\ 1 * s V_ref_param_read_commit_z <= 0 /\ -1 * s V_ref_param_read_commit_z <= 0 /\ -1 * s V_ref_param_read_commit_i <= 0 /\ 1 * s V_ref_param_read_commit_ecode <= 0 /\ -1 * s V_ref_param_read_commit_ecode <= 0)%Z
   | 13 => (-1 * s V_ref_param_read_commit_ecode <= 0 /\ 1 * s V_ref_param_read_commit_ecode <= 0 /\ -1 * s V_ref_param_read_commit_i <= 0 /\ -1 * s V_ref_param_read_commit_z <= 0 /\ 1 * s V_ref_param_read_commit_z <= 0 /\ -1 * s V_ref_param_read_commit_plist_dref_off48 <= 0)%Z
   | 14 => (-1 * s V_ref_param_read_commit_plist_dref_off48 <= 0 /\ 1 * s V_ref_param_read_commit_z <= 0 /\ -1 * s V_ref_param_read_commit_z <= 0 /\ 1 * s V_ref_param_read_commit_ecode <= 0 /\ -1 * s V_ref_param_read_commit_ecode <= 0 /\ 1 * s V_ref_param_read_commit_i <= 0 /\ -1 * s V_ref_param_read_commit_i <= 0)%Z
   | 15 => (-1 * s V_ref_param_read_commit_i <= 0 /\ 1 * s V_ref_param_read_commit_i <= 0 /\ -1 * s V_ref_param_read_commit_ecode <= 0 /\ 1 * s V_ref_param_read_commit_ecode <= 0 /\ -1 * s V_ref_param_read_commit_z <= 0 /\ 1 * s V_ref_param_read_commit_z <= 0 /\ -1 * s V_ref_param_read_commit_plist_dref_off48 <= 0)%Z
   | 16 => (-1 * s V_ref_param_read_commit_z <= 0 /\ -1 * s V_ref_param_read_commit_i <= 0 /\ 1 * s V_ref_param_read_commit_ecode <= 0 /\ 1 * s V_ref_param_read_commit_i+ -1 * s V_ref_param_read_commit_plist_dref_off48 <= 0)%Z
   | 17 => (1 * s V_ref_param_read_commit_i+ -1 * s V_ref_param_read_commit_plist_dref_off48 <= 0 /\ 1 * s V_ref_param_read_commit_ecode <= 0 /\ -1 * s V_ref_param_read_commit_i <= 0 /\ -1 * s V_ref_param_read_commit_z <= 0 /\ -1 * s V_ref_param_read_commit_i+ 1 * s V_ref_param_read_commit_plist_dref_off48 <= 0)%Z
   | 18 => (-1 * s V_ref_param_read_commit_i+ 1 * s V_ref_param_read_commit_plist_dref_off48 <= 0 /\ -1 * s V_ref_param_read_commit_z <= 0 /\ -1 * s V_ref_param_read_commit_i <= 0 /\ 1 * s V_ref_param_read_commit_ecode <= 0 /\ 1 * s V_ref_param_read_commit_i+ -1 * s V_ref_param_read_commit_plist_dref_off48 <= 0)%Z
   | 19 => (1 * s V_ref_param_read_commit_i+ -1 * s V_ref_param_read_commit_plist_dref_off48 <= 0 /\ 1 * s V_ref_param_read_commit_ecode <= 0 /\ -1 * s V_ref_param_read_commit_i <= 0 /\ -1 * s V_ref_param_read_commit_z <= 0 /\ -1 * s V_ref_param_read_commit_i+ 1 * s V_ref_param_read_commit_plist_dref_off48 <= 0 /\ 1 * s V_ref_param_read_commit__tmp <= 0)%Z
   | 20 => (1 * s V_ref_param_read_commit__tmp <= 0 /\ -1 * s V_ref_param_read_commit_i+ 1 * s V_ref_param_read_commit_plist_dref_off48 <= 0 /\ -1 * s V_ref_param_read_commit_z <= 0 /\ -1 * s V_ref_param_read_commit_i <= 0 /\ 1 * s V_ref_param_read_commit_ecode <= 0 /\ 1 * s V_ref_param_read_commit_i+ -1 * s V_ref_param_read_commit_plist_dref_off48 <= 0)%Z
   | 21 => (-1 * s V_ref_param_read_commit_plist_dref_off48 <= 0 /\ 1 * s V_ref_param_read_commit_ecode <= 0 /\ -1 * s V_ref_param_read_commit_i <= 0 /\ -1 * s V_ref_param_read_commit_z <= 0 /\ 1 * s V_ref_param_read_commit__tmp <= 0)%Z
   | 22 => (1 * s V_ref_param_read_commit_ecode <= 0 /\ -1 * s V_ref_param_read_commit_i <= 0 /\ -1 * s V_ref_param_read_commit_z <= 0 /\ 1 * s V_ref_param_read_commit_i+ -1 * s V_ref_param_read_commit_plist_dref_off48 + 1 <= 0)%Z
   | 23 => (1 * s V_ref_param_read_commit_i+ -1 * s V_ref_param_read_commit_plist_dref_off48 + 1 <= 0 /\ -1 * s V_ref_param_read_commit_z <= 0 /\ -1 * s V_ref_param_read_commit_i <= 0 /\ 1 * s V_ref_param_read_commit_ecode <= 0)%Z
   | 24 => (1 * s V_ref_param_read_commit_ecode <= 0 /\ -1 * s V_ref_param_read_commit_i <= 0 /\ -1 * s V_ref_param_read_commit_z <= 0 /\ 1 * s V_ref_param_read_commit_i+ -1 * s V_ref_param_read_commit_plist_dref_off48 + 1 <= 0)%Z
   | 25 => (1 * s V_ref_param_read_commit_i+ -1 * s V_ref_param_read_commit_plist_dref_off48 + 1 <= 0 /\ -1 * s V_ref_param_read_commit_z <= 0 /\ -1 * s V_ref_param_read_commit_i <= 0 /\ 1 * s V_ref_param_read_commit_ecode + 21 <= 0 /\ -1 * s V_ref_param_read_commit_ecode + -21 <= 0)%Z
   | 26 => (1 * s V_ref_param_read_commit_ecode <= 0 /\ -1 * s V_ref_param_read_commit_i <= 0 /\ -1 * s V_ref_param_read_commit_z <= 0 /\ 1 * s V_ref_param_read_commit_i+ -1 * s V_ref_param_read_commit_plist_dref_off48 + 1 <= 0)%Z
   | 27 => (1 * s V_ref_param_read_commit_i+ -1 * s V_ref_param_read_commit_plist_dref_off48 + 1 <= 0 /\ -1 * s V_ref_param_read_commit_z <= 0 /\ -1 * s V_ref_param_read_commit_i <= 0 /\ 1 * s V_ref_param_read_commit_ecode <= 0)%Z
   | 28 => (1 * s V_ref_param_read_commit_ecode <= 0 /\ -1 * s V_ref_param_read_commit_z <= 0 /\ 1 * s V_ref_param_read_commit_i+ -1 * s V_ref_param_read_commit_plist_dref_off48 <= 0 /\ -1 * s V_ref_param_read_commit_i + 1 <= 0)%Z
   | 29 => (-1 * s V_ref_param_read_commit_i + 1 <= 0 /\ 1 * s V_ref_param_read_commit_i+ -1 * s V_ref_param_read_commit_plist_dref_off48 <= 0 /\ -1 * s V_ref_param_read_commit_z <= 0 /\ 1 * s V_ref_param_read_commit_ecode <= 0)%Z
   | 30 => (1 * s V_ref_param_read_commit_ecode <= 0 /\ -1 * s V_ref_param_read_commit_z <= 0 /\ 1 * s V_ref_param_read_commit_i+ -1 * s V_ref_param_read_commit_plist_dref_off48 <= 0 /\ -1 * s V_ref_param_read_commit_i + 1 <= 0)%Z
   | 31 => (-1 * s V_ref_param_read_commit_i + 1 <= 0 /\ 1 * s V_ref_param_read_commit_i+ -1 * s V_ref_param_read_commit_plist_dref_off48 <= 0 /\ 1 * s V_ref_param_read_commit_ecode <= 0 /\ -1 * s V_ref_param_read_commit_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_ref_param_read_commit (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_ref_param_read_commit_plist_dref_off48) <= z)%Q
   | 2 => (s V_ref_param_read_commit_z
           + max0(s V_ref_param_read_commit_plist_dref_off48) <= z)%Q
   | 3 => (s V_ref_param_read_commit_z
           + max0(s V_ref_param_read_commit_plist_dref_off48) <= z)%Q
   | 4 => (s V_ref_param_read_commit_z
           + max0(s V_ref_param_read_commit_plist_dref_off48) <= z)%Q
   | 5 => (s V_ref_param_read_commit_z
           + max0(s V_ref_param_read_commit_plist_dref_off48) <= z)%Q
   | 6 => (s V_ref_param_read_commit_z
           + max0(s V_ref_param_read_commit_plist_dref_off48) <= z)%Q
   | 7 => (s V_ref_param_read_commit_z
           + max0(s V_ref_param_read_commit_plist_dref_off48) <= z)%Q
   | 8 => (s V_ref_param_read_commit_z
           + max0(s V_ref_param_read_commit_plist_dref_off48) <= z)%Q
   | 9 => (s V_ref_param_read_commit_z
           + max0(s V_ref_param_read_commit_plist_dref_off48) <= z)%Q
   | 10 => (s V_ref_param_read_commit_z
            + max0(s V_ref_param_read_commit_plist_dref_off48) <= z)%Q
   | 11 => hints
     [(*-1 0*) F_max0_ge_0 (s V_ref_param_read_commit_plist_dref_off48)]
     (s V_ref_param_read_commit_z
      + max0(s V_ref_param_read_commit_plist_dref_off48) <= z)%Q
   | 12 => (s V_ref_param_read_commit_z
            + max0(s V_ref_param_read_commit_plist_dref_off48) <= z)%Q
   | 13 => (s V_ref_param_read_commit_z
            + max0(s V_ref_param_read_commit_plist_dref_off48) <= z)%Q
   | 14 => (s V_ref_param_read_commit_z
            + max0(-s V_ref_param_read_commit_i
                   + s V_ref_param_read_commit_plist_dref_off48) <= z)%Q
   | 15 => (s V_ref_param_read_commit_z
            + max0(-s V_ref_param_read_commit_i
                   + s V_ref_param_read_commit_plist_dref_off48) <= z)%Q
   | 16 => (s V_ref_param_read_commit_z
            + max0(-s V_ref_param_read_commit_i
                   + s V_ref_param_read_commit_plist_dref_off48) <= z)%Q
   | 17 => (s V_ref_param_read_commit_z
            + max0(-s V_ref_param_read_commit_i
                   + s V_ref_param_read_commit_plist_dref_off48) <= z)%Q
   | 18 => (s V_ref_param_read_commit_z
            + max0(-s V_ref_param_read_commit_i
                   + s V_ref_param_read_commit_plist_dref_off48) <= z)%Q
   | 19 => (s V_ref_param_read_commit_z
            + max0(-s V_ref_param_read_commit_i
                   + s V_ref_param_read_commit_plist_dref_off48) <= z)%Q
   | 20 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-s V_ref_param_read_commit_i
                                             + s V_ref_param_read_commit_plist_dref_off48) (-1
                                                                    - s V_ref_param_read_commit_i
                                                                    + s V_ref_param_read_commit_plist_dref_off48));
      (*-1 0*) F_max0_ge_0 (-1 - s V_ref_param_read_commit_i
                            + s V_ref_param_read_commit_plist_dref_off48)]
     (s V_ref_param_read_commit_z
      + max0(-s V_ref_param_read_commit_i
             + s V_ref_param_read_commit_plist_dref_off48) <= z)%Q
   | 21 => (s V_ref_param_read_commit_z <= z)%Q
   | 22 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (-s V_ref_param_read_commit_i
                                       + s V_ref_param_read_commit_plist_dref_off48) (1)]
     (s V_ref_param_read_commit_z
      + max0(-s V_ref_param_read_commit_i
             + s V_ref_param_read_commit_plist_dref_off48) <= z)%Q
   | 23 => ((1 # 1) + s V_ref_param_read_commit_z
            + max0(-1 - s V_ref_param_read_commit_i
                   + s V_ref_param_read_commit_plist_dref_off48) <= z)%Q
   | 24 => ((1 # 1) + s V_ref_param_read_commit_z
            + max0(-1 - s V_ref_param_read_commit_i
                   + s V_ref_param_read_commit_plist_dref_off48) <= z)%Q
   | 25 => ((1 # 1) + s V_ref_param_read_commit_z
            + max0(-1 - s V_ref_param_read_commit_i
                   + s V_ref_param_read_commit_plist_dref_off48) <= z)%Q
   | 26 => ((1 # 1) + s V_ref_param_read_commit_z
            + max0(-1 - s V_ref_param_read_commit_i
                   + s V_ref_param_read_commit_plist_dref_off48) <= z)%Q
   | 27 => ((1 # 1) + s V_ref_param_read_commit_z
            + max0(-1 - s V_ref_param_read_commit_i
                   + s V_ref_param_read_commit_plist_dref_off48) <= z)%Q
   | 28 => ((1 # 1) + s V_ref_param_read_commit_z
            + max0(-s V_ref_param_read_commit_i
                   + s V_ref_param_read_commit_plist_dref_off48) <= z)%Q
   | 29 => ((1 # 1) + s V_ref_param_read_commit_z
            + max0(-s V_ref_param_read_commit_i
                   + s V_ref_param_read_commit_plist_dref_off48) <= z)%Q
   | 30 => ((1 # 1) + s V_ref_param_read_commit_z
            + max0(-s V_ref_param_read_commit_i
                   + s V_ref_param_read_commit_plist_dref_off48) <= z)%Q
   | 31 => (s V_ref_param_read_commit_z
            + max0(-s V_ref_param_read_commit_i
                   + s V_ref_param_read_commit_plist_dref_off48) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_ref_param_read_commit =>
    [mkPA Q (fun n z s => ai_ref_param_read_commit n s /\ annot0_ref_param_read_commit n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_ref_param_read_commit (proc_start P_ref_param_read_commit) s1 (proc_end P_ref_param_read_commit) s2 ->
    (s2 V_ref_param_read_commit_z <= max0(s1 V_ref_param_read_commit_plist_dref_off48))%Q.
Proof.
  prove_bound ipa admissible_ipa P_ref_param_read_commit.
Qed.
