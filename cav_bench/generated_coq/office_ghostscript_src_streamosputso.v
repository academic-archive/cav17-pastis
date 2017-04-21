Require Import pasta.Pasta.

Inductive proc: Type :=
  P_sputs.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_sputs_z := 1%positive.
Notation V_sputs__tmp := 2%positive.
Notation V_sputs_ch := 3%positive.
Notation V_sputs_count := 4%positive.
Notation V_sputs_len := 5%positive.
Notation V_sputs_pn_dref := 6%positive.
Notation V_sputs_status := 7%positive.
Notation V_sputs_pn := 8%positive.
Notation V_sputs_s := 9%positive.
Notation V_sputs_str := 10%positive.
Notation V_sputs_wlen := 11%positive.
Definition Pedges_sputs: list (edge proc) :=
  (EA 1 (AAssign V_sputs_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_sputs_len) s) >= (eval (ENum (0)) s))%Z)) 3)::
  (EA 3 (AGuard (fun s => ((eval (EVar V_sputs_count) s) >= (eval (ENum (0))
  s))%Z)) 4)::(EA 4 AWeaken 5)::(EA 5 (AAssign V_sputs__tmp
  (Some (EVar V_sputs_wlen))) 6)::(EA 6 (AAssign V_sputs_len
  (Some (EVar V_sputs__tmp))) 7)::(EA 7 (AAssign V_sputs_status None) 8)::
  (EA 8 AWeaken 9)::(EA 9 (AGuard (fun s => ((eval (EVar V_sputs_status)
  s) >= (eval (ENum (0)) s))%Z)) 11)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_sputs_status) s) < (eval (ENum (0)) s))%Z)) 10)::
  (EA 10 AWeaken 35)::(EA 11 AWeaken 12)::(EA 12 ANone 13)::
  (EA 13 AWeaken 14)::(EA 14 (AGuard (fun s => ((eval (EVar V_sputs_len) s) >
  (eval (ENum (0)) s))%Z)) 16)::(EA 14 (AGuard
  (fun s => ((eval (EVar V_sputs_len) s) <= (eval (ENum (0)) s))%Z)) 15)::
  (EA 15 AWeaken 34)::(EA 16 AWeaken 17)::(EA 17 (AAssign V_sputs_count
  None) 18)::(EA 18 AWeaken 19)::(EA 19 (AGuard
  (fun s => ((eval (EVar V_sputs_count) s) > (eval (ENum (0)) s))%Z)) 45)::
  (EA 19 (AGuard (fun s => ((eval (EVar V_sputs_count) s) <= (eval (ENum (0))
  s))%Z)) 20)::(EA 20 AWeaken 21)::(EA 21 (AAssign V_sputs_ch None) 22)::
  (EA 22 AWeaken 23)::(EA 23 ANone 25)::(EA 23 ANone 24)::(EA 24 ANone 30)::
  (EA 25 ANone 26)::(EA 26 (AAssign V_sputs_status None) 27)::
  (EA 27 AWeaken 28)::(EA 28 (AGuard (fun s => True)) 32)::(EA 28 (AGuard
  (fun s => True)) 29)::(EA 29 AWeaken 30)::(EA 30 (AAssign V_sputs_len
  (Some (EAdd (EVar V_sputs_len) (ENum (-1))))) 31)::(EA 31 ANone 53)::
  (EA 32 AWeaken 33)::(EA 33 ANone 34)::(EA 34 ANone 35)::(EA 35 (AAssign
  V_sputs_pn_dref (Some (ESub (EVar V_sputs__tmp) (EVar V_sputs_len)))) 36)::
  (EA 36 AWeaken 37)::(EA 37 (AGuard (fun s => ((eval (EVar V_sputs_status)
  s) >= (eval (ENum (0)) s))%Z)) 41)::(EA 37 (AGuard
  (fun s => ((eval (EVar V_sputs_status) s) < (eval (ENum (0)) s))%Z)) 38)::
  (EA 38 AWeaken 39)::(EA 39 ANone 40)::(EA 40 AWeaken 44)::
  (EA 41 AWeaken 42)::(EA 42 ANone 43)::(EA 43 AWeaken 44)::
  (EA 45 AWeaken 46)::(EA 46 (AGuard (fun s => ((eval (EVar V_sputs_count)
  s) > (eval (EVar V_sputs_len) s))%Z)) 48)::(EA 46 (AGuard
  (fun s => ((eval (EVar V_sputs_count) s) <= (eval (EVar V_sputs_len)
  s))%Z)) 47)::(EA 47 AWeaken 51)::(EA 48 AWeaken 49)::(EA 49 (AAssign
  V_sputs_count (Some (EVar V_sputs_len))) 50)::(EA 50 ANone 51)::
  (EA 51 (AAssign V_sputs_len (Some (ESub (EVar V_sputs_len)
  (EVar V_sputs_count)))) 52)::(EA 52 ANone 53)::(EA 53 ANone 54)::
  (EA 54 ANone 55)::(EA 55 (AAssign V_sputs_z (Some (EAdd (ENum (1))
  (EVar V_sputs_z)))) 56)::(EA 56 AWeaken 14)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_sputs => Pedges_sputs
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_sputs => 44
     end)%positive;
  var_global := var_global
}.

Definition ai_sputs (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_sputs_z <= 0 /\ -1 * s V_sputs_z <= 0)%Z
   | 3 => (-1 * s V_sputs_z <= 0 /\ 1 * s V_sputs_z <= 0 /\ -1 * s V_sputs_len <= 0)%Z
   | 4 => (-1 * s V_sputs_len <= 0 /\ 1 * s V_sputs_z <= 0 /\ -1 * s V_sputs_z <= 0 /\ -1 * s V_sputs_count <= 0)%Z
   | 5 => (-1 * s V_sputs_count <= 0 /\ -1 * s V_sputs_z <= 0 /\ 1 * s V_sputs_z <= 0 /\ -1 * s V_sputs_len <= 0)%Z
   | 6 => (-1 * s V_sputs_len <= 0 /\ 1 * s V_sputs_z <= 0 /\ -1 * s V_sputs_z <= 0 /\ -1 * s V_sputs_count <= 0)%Z
   | 7 => (-1 * s V_sputs_count <= 0 /\ -1 * s V_sputs_z <= 0 /\ 1 * s V_sputs_z <= 0)%Z
   | 8 => (1 * s V_sputs_z <= 0 /\ -1 * s V_sputs_z <= 0 /\ -1 * s V_sputs_count <= 0)%Z
   | 9 => (-1 * s V_sputs_count <= 0 /\ -1 * s V_sputs_z <= 0 /\ 1 * s V_sputs_z <= 0)%Z
   | 10 => (1 * s V_sputs_z <= 0 /\ -1 * s V_sputs_z <= 0 /\ -1 * s V_sputs_count <= 0 /\ 1 * s V_sputs_status + 1 <= 0)%Z
   | 11 => (1 * s V_sputs_z <= 0 /\ -1 * s V_sputs_z <= 0 /\ -1 * s V_sputs_count <= 0 /\ -1 * s V_sputs_status <= 0)%Z
   | 12 => (-1 * s V_sputs_status <= 0 /\ -1 * s V_sputs_count <= 0 /\ -1 * s V_sputs_z <= 0 /\ 1 * s V_sputs_z <= 0)%Z
   | 13 => (1 * s V_sputs_z <= 0 /\ -1 * s V_sputs_z <= 0 /\ -1 * s V_sputs_count <= 0 /\ -1 * s V_sputs_status <= 0)%Z
   | 14 => (-1 * s V_sputs_z <= 0)%Z
   | 15 => (-1 * s V_sputs_z <= 0 /\ 1 * s V_sputs_len <= 0)%Z
   | 16 => (-1 * s V_sputs_z <= 0 /\ -1 * s V_sputs_len + 1 <= 0)%Z
   | 17 => (-1 * s V_sputs_len + 1 <= 0 /\ -1 * s V_sputs_z <= 0)%Z
   | 18 => (-1 * s V_sputs_z <= 0 /\ -1 * s V_sputs_len + 1 <= 0)%Z
   | 19 => (-1 * s V_sputs_len + 1 <= 0 /\ -1 * s V_sputs_z <= 0)%Z
   | 20 => (-1 * s V_sputs_z <= 0 /\ -1 * s V_sputs_len + 1 <= 0 /\ 1 * s V_sputs_count <= 0)%Z
   | 21 => (1 * s V_sputs_count <= 0 /\ -1 * s V_sputs_len + 1 <= 0 /\ -1 * s V_sputs_z <= 0)%Z
   | 22 => (-1 * s V_sputs_z <= 0 /\ -1 * s V_sputs_len + 1 <= 0 /\ 1 * s V_sputs_count <= 0)%Z
   | 23 => (1 * s V_sputs_count <= 0 /\ -1 * s V_sputs_len + 1 <= 0 /\ -1 * s V_sputs_z <= 0)%Z
   | 24 => (-1 * s V_sputs_z <= 0 /\ -1 * s V_sputs_len + 1 <= 0 /\ 1 * s V_sputs_count <= 0)%Z
   | 25 => (-1 * s V_sputs_z <= 0 /\ -1 * s V_sputs_len + 1 <= 0 /\ 1 * s V_sputs_count <= 0)%Z
   | 26 => (1 * s V_sputs_count <= 0 /\ -1 * s V_sputs_len + 1 <= 0 /\ -1 * s V_sputs_z <= 0)%Z
   | 27 => (-1 * s V_sputs_z <= 0 /\ -1 * s V_sputs_len + 1 <= 0 /\ 1 * s V_sputs_count <= 0)%Z
   | 28 => (1 * s V_sputs_count <= 0 /\ -1 * s V_sputs_len + 1 <= 0 /\ -1 * s V_sputs_z <= 0)%Z
   | 29 => (-1 * s V_sputs_z <= 0 /\ -1 * s V_sputs_len + 1 <= 0 /\ 1 * s V_sputs_count <= 0)%Z
   | 30 => (1 * s V_sputs_count <= 0 /\ -1 * s V_sputs_len + 1 <= 0 /\ -1 * s V_sputs_z <= 0)%Z
   | 31 => (-1 * s V_sputs_z <= 0 /\ 1 * s V_sputs_count <= 0 /\ -1 * s V_sputs_len <= 0)%Z
   | 32 => (-1 * s V_sputs_z <= 0 /\ -1 * s V_sputs_len + 1 <= 0 /\ 1 * s V_sputs_count <= 0)%Z
   | 33 => (1 * s V_sputs_count <= 0 /\ -1 * s V_sputs_len + 1 <= 0 /\ -1 * s V_sputs_z <= 0)%Z
   | 34 => (-1 * s V_sputs_z <= 0)%Z
   | 35 => (-1 * s V_sputs_z <= 0)%Z
   | 36 => (-1 * s V_sputs_z <= 0)%Z
   | 37 => (-1 * s V_sputs_z <= 0)%Z
   | 38 => (-1 * s V_sputs_z <= 0 /\ 1 * s V_sputs_status + 1 <= 0)%Z
   | 39 => (1 * s V_sputs_status + 1 <= 0 /\ -1 * s V_sputs_z <= 0)%Z
   | 40 => (-1 * s V_sputs_z <= 0 /\ 1 * s V_sputs_status + 1 <= 0)%Z
   | 41 => (-1 * s V_sputs_z <= 0 /\ -1 * s V_sputs_status <= 0)%Z
   | 42 => (-1 * s V_sputs_status <= 0 /\ -1 * s V_sputs_z <= 0)%Z
   | 43 => (-1 * s V_sputs_z <= 0 /\ -1 * s V_sputs_status <= 0)%Z
   | 44 => (-1 * s V_sputs_z <= 0)%Z
   | 45 => (-1 * s V_sputs_z <= 0 /\ -1 * s V_sputs_len + 1 <= 0 /\ -1 * s V_sputs_count + 1 <= 0)%Z
   | 46 => (-1 * s V_sputs_count + 1 <= 0 /\ -1 * s V_sputs_len + 1 <= 0 /\ -1 * s V_sputs_z <= 0)%Z
   | 47 => (-1 * s V_sputs_z <= 0 /\ -1 * s V_sputs_count + 1 <= 0 /\ 1 * s V_sputs_count+ -1 * s V_sputs_len <= 0)%Z
   | 48 => (-1 * s V_sputs_z <= 0 /\ -1 * s V_sputs_len + 1 <= 0 /\ -1 * s V_sputs_count + 1 <= 0 /\ -1 * s V_sputs_count+ 1 * s V_sputs_len + 1 <= 0)%Z
   | 49 => (-1 * s V_sputs_count+ 1 * s V_sputs_len + 1 <= 0 /\ -1 * s V_sputs_count + 1 <= 0 /\ -1 * s V_sputs_len + 1 <= 0 /\ -1 * s V_sputs_z <= 0)%Z
   | 50 => (-1 * s V_sputs_z <= 0 /\ -1 * s V_sputs_len + 1 <= 0 /\ -1 * s V_sputs_count + 1 <= 0)%Z
   | 51 => (-1 * s V_sputs_count + 1 <= 0 /\ -1 * s V_sputs_len + 1 <= 0 /\ -1 * s V_sputs_z <= 0)%Z
   | 52 => (-1 * s V_sputs_z <= 0 /\ -1 * s V_sputs_count + 1 <= 0 /\ -1 * s V_sputs_count+ -1 * s V_sputs_len + 1 <= 0)%Z
   | 53 => (-1 * s V_sputs_z <= 0)%Z
   | 54 => (-1 * s V_sputs_z <= 0)%Z
   | 55 => (-1 * s V_sputs_z <= 0)%Z
   | 56 => (-1 * s V_sputs_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_sputs (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_sputs_wlen) <= z)%Q
   | 2 => (s V_sputs_z + max0(s V_sputs_wlen) <= z)%Q
   | 3 => (s V_sputs_z + max0(s V_sputs_wlen) <= z)%Q
   | 4 => (s V_sputs_z + max0(s V_sputs_wlen) <= z)%Q
   | 5 => (s V_sputs_z + max0(s V_sputs_wlen) <= z)%Q
   | 6 => (s V_sputs_z + max0(s V_sputs__tmp) <= z)%Q
   | 7 => (s V_sputs_z + max0(s V_sputs_len) <= z)%Q
   | 8 => (s V_sputs_z + max0(s V_sputs_len) <= z)%Q
   | 9 => (s V_sputs_z + max0(s V_sputs_len) <= z)%Q
   | 10 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_sputs_len) (-1
                                                             + s V_sputs_len));
      (*-1 0*) F_max0_ge_0 (-1 + s V_sputs_len)]
     (s V_sputs_z + max0(s V_sputs_len) <= z)%Q
   | 11 => (s V_sputs_z + max0(s V_sputs_len) <= z)%Q
   | 12 => (s V_sputs_z + max0(s V_sputs_len) <= z)%Q
   | 13 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_sputs_z) (0))) (F_max0_ge_0 (s V_sputs_z))]
     (s V_sputs_z + max0(s V_sputs_len) <= z)%Q
   | 14 => (max0(s V_sputs_len) + max0(s V_sputs_z) <= z)%Q
   | 15 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_sputs_len) (-1
                                                             + s V_sputs_len));
      (*-1 0*) F_max0_ge_0 (-1 + s V_sputs_len);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_sputs_z)) (F_check_ge (s V_sputs_z) (0))]
     (max0(s V_sputs_len) + max0(s V_sputs_z) <= z)%Q
   | 16 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_sputs_len)) (F_check_ge (s V_sputs_len) (0))]
     (max0(s V_sputs_len) + max0(s V_sputs_z) <= z)%Q
   | 17 => (s V_sputs_len + max0(s V_sputs_z) <= z)%Q
   | 18 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_sputs_z)) (F_check_ge (s V_sputs_z) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_sputs_len) (0))) (F_max0_ge_0 (s V_sputs_len))]
     (s V_sputs_len + max0(s V_sputs_z) <= z)%Q
   | 19 => (s V_sputs_z + max0(s V_sputs_len) <= z)%Q
   | 20 => (s V_sputs_z + max0(s V_sputs_len) <= z)%Q
   | 21 => (s V_sputs_z + max0(s V_sputs_len) <= z)%Q
   | 22 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_sputs_len) (1)]
     (s V_sputs_z + max0(s V_sputs_len) <= z)%Q
   | 23 => ((1 # 1) + s V_sputs_z + max0(-1 + s V_sputs_len) <= z)%Q
   | 24 => ((1 # 1) + s V_sputs_z + max0(-1 + s V_sputs_len) <= z)%Q
   | 25 => ((1 # 1) + s V_sputs_z + max0(-1 + s V_sputs_len) <= z)%Q
   | 26 => ((1 # 1) + s V_sputs_z + max0(-1 + s V_sputs_len) <= z)%Q
   | 27 => ((1 # 1) + s V_sputs_z + max0(-1 + s V_sputs_len) <= z)%Q
   | 28 => ((1 # 1) + s V_sputs_z + max0(-1 + s V_sputs_len) <= z)%Q
   | 29 => ((1 # 1) + s V_sputs_z + max0(-1 + s V_sputs_len) <= z)%Q
   | 30 => ((1 # 1) + s V_sputs_z + max0(-1 + s V_sputs_len) <= z)%Q
   | 31 => ((1 # 1) + s V_sputs_z + max0(s V_sputs_len) <= z)%Q
   | 32 => hints
     [(*-1 0*) F_one; (*-1 0*) F_max0_ge_0 (-1 + s V_sputs_len)]
     ((1 # 1) + s V_sputs_z + max0(-1 + s V_sputs_len) <= z)%Q
   | 33 => (s V_sputs_z <= z)%Q
   | 34 => (s V_sputs_z <= z)%Q
   | 35 => (s V_sputs_z <= z)%Q
   | 36 => (s V_sputs_z <= z)%Q
   | 37 => (s V_sputs_z <= z)%Q
   | 38 => (s V_sputs_z <= z)%Q
   | 39 => (s V_sputs_z <= z)%Q
   | 40 => (s V_sputs_z <= z)%Q
   | 41 => (s V_sputs_z <= z)%Q
   | 42 => (s V_sputs_z <= z)%Q
   | 43 => (s V_sputs_z <= z)%Q
   | 44 => (s V_sputs_z <= z)%Q
   | 45 => (s V_sputs_z + max0(s V_sputs_len) <= z)%Q
   | 46 => (s V_sputs_z + max0(s V_sputs_len) <= z)%Q
   | 47 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_sputs_len) (s V_sputs_count);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_sputs_count)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_sputs_count) (0))) (F_max0_ge_0 (-1
                                                                    + s V_sputs_count))]
     (s V_sputs_z + max0(s V_sputs_len) <= z)%Q
   | 48 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_sputs_len) (1);
      (*-1 0*) F_max0_ge_0 (-1 + s V_sputs_len)]
     (s V_sputs_z + max0(s V_sputs_len) <= z)%Q
   | 49 => ((1 # 1) + s V_sputs_z <= z)%Q
   | 50 => ((1 # 1) + s V_sputs_z + max0(-s V_sputs_count + s V_sputs_len) <= z)%Q
   | 51 => ((1 # 1) + s V_sputs_z + max0(-s V_sputs_count + s V_sputs_len) <= z)%Q
   | 52 => ((1 # 1) + s V_sputs_z + max0(s V_sputs_len) <= z)%Q
   | 53 => ((1 # 1) + s V_sputs_z + max0(s V_sputs_len) <= z)%Q
   | 54 => ((1 # 1) + s V_sputs_z + max0(s V_sputs_len) <= z)%Q
   | 55 => ((1 # 1) + s V_sputs_z + max0(s V_sputs_len) <= z)%Q
   | 56 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_sputs_z) (0))) (F_max0_ge_0 (s V_sputs_z))]
     (s V_sputs_z + max0(s V_sputs_len) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_sputs =>
    [mkPA Q (fun n z s => ai_sputs n s /\ annot0_sputs n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_sputs (proc_start P_sputs) s1 (proc_end P_sputs) s2 ->
    (s2 V_sputs_z <= max0(s1 V_sputs_wlen))%Q.
Proof.
  prove_bound ipa admissible_ipa P_sputs.
Qed.
