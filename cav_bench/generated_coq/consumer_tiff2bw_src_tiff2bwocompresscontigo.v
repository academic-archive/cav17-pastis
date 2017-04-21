Require Import pasta.Pasta.

Inductive proc: Type :=
  P_compresscontig.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_compresscontig_z := 1%positive.
Notation V_compresscontig_BLUE := 2%positive.
Notation V_compresscontig_GREEN := 3%positive.
Notation V_compresscontig_RED := 4%positive.
Notation V_compresscontig__tmp := 5%positive.
Notation V_compresscontig_blue := 6%positive.
Notation V_compresscontig_green := 7%positive.
Notation V_compresscontig_red := 8%positive.
Notation V_compresscontig_v := 9%positive.
Notation V_compresscontig_n := 10%positive.
Notation V_compresscontig_out := 11%positive.
Notation V_compresscontig_rgb := 12%positive.
Definition Pedges_compresscontig: list (edge proc) :=
  (EA 1 (AAssign V_compresscontig_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_compresscontig__tmp) s) >= (eval (ENum (0))
  s))%Z)) 3)::(EA 3 AWeaken 4)::(EA 4 (AAssign V_compresscontig__tmp
  (Some (EVar V_compresscontig_n))) 5)::(EA 5 (AAssign V_compresscontig_red
  (Some (EVar V_compresscontig_RED))) 6)::(EA 6 (AAssign
  V_compresscontig_green (Some (EVar V_compresscontig_GREEN))) 7)::
  (EA 7 (AAssign V_compresscontig_blue
  (Some (EVar V_compresscontig_BLUE))) 8)::(EA 8 ANone 9)::(EA 9 (AAssign
  V_compresscontig__tmp (Some (EAdd (EVar V_compresscontig__tmp)
  (ENum (-1))))) 10)::(EA 10 AWeaken 11)::(EA 11 (AGuard
  (fun s => ((eval (EVar V_compresscontig__tmp) s) > (eval (ENum (0))
  s))%Z)) 14)::(EA 11 (AGuard (fun s => ((eval (EVar V_compresscontig__tmp)
  s) <= (eval (ENum (0)) s))%Z)) 12)::(EA 12 AWeaken 13)::
  (EA 14 AWeaken 15)::(EA 15 (AAssign V_compresscontig_v None) 16)::
  (EA 16 (AAssign V_compresscontig_v None) 17)::(EA 17 (AAssign
  V_compresscontig_v None) 18)::(EA 18 ANone 19)::(EA 19 ANone 20)::
  (EA 20 (AAssign V_compresscontig_z (Some (EAdd (ENum (1))
  (EVar V_compresscontig_z)))) 9)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_compresscontig => Pedges_compresscontig
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_compresscontig => 13
     end)%positive;
  var_global := var_global
}.

Definition ai_compresscontig (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_compresscontig_z <= 0 /\ -1 * s V_compresscontig_z <= 0)%Z
   | 3 => (-1 * s V_compresscontig_z <= 0 /\ 1 * s V_compresscontig_z <= 0 /\ -1 * s V_compresscontig__tmp <= 0)%Z
   | 4 => (-1 * s V_compresscontig__tmp <= 0 /\ 1 * s V_compresscontig_z <= 0 /\ -1 * s V_compresscontig_z <= 0)%Z
   | 5 => (-1 * s V_compresscontig_z <= 0 /\ 1 * s V_compresscontig_z <= 0)%Z
   | 6 => (1 * s V_compresscontig_z <= 0 /\ -1 * s V_compresscontig_z <= 0)%Z
   | 7 => (-1 * s V_compresscontig_z <= 0 /\ 1 * s V_compresscontig_z <= 0)%Z
   | 8 => (1 * s V_compresscontig_z <= 0 /\ -1 * s V_compresscontig_z <= 0)%Z
   | 9 => (-1 * s V_compresscontig_z <= 0)%Z
   | 10 => (-1 * s V_compresscontig_z <= 0)%Z
   | 11 => (-1 * s V_compresscontig_z <= 0)%Z
   | 12 => (-1 * s V_compresscontig_z <= 0 /\ 1 * s V_compresscontig__tmp <= 0)%Z
   | 13 => (1 * s V_compresscontig__tmp <= 0 /\ -1 * s V_compresscontig_z <= 0)%Z
   | 14 => (-1 * s V_compresscontig_z <= 0 /\ -1 * s V_compresscontig__tmp + 1 <= 0)%Z
   | 15 => (-1 * s V_compresscontig__tmp + 1 <= 0 /\ -1 * s V_compresscontig_z <= 0)%Z
   | 16 => (-1 * s V_compresscontig_z <= 0 /\ -1 * s V_compresscontig__tmp + 1 <= 0)%Z
   | 17 => (-1 * s V_compresscontig__tmp + 1 <= 0 /\ -1 * s V_compresscontig_z <= 0)%Z
   | 18 => (-1 * s V_compresscontig_z <= 0 /\ -1 * s V_compresscontig__tmp + 1 <= 0)%Z
   | 19 => (-1 * s V_compresscontig__tmp + 1 <= 0 /\ -1 * s V_compresscontig_z <= 0)%Z
   | 20 => (-1 * s V_compresscontig_z <= 0 /\ -1 * s V_compresscontig__tmp + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_compresscontig (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(-1 + s V_compresscontig_n) <= z)%Q
   | 2 => (s V_compresscontig_z + max0(-1 + s V_compresscontig_n) <= z)%Q
   | 3 => (s V_compresscontig_z + max0(-1 + s V_compresscontig_n) <= z)%Q
   | 4 => (s V_compresscontig_z + max0(-1 + s V_compresscontig_n) <= z)%Q
   | 5 => (s V_compresscontig_z + max0(-1 + s V_compresscontig__tmp) <= z)%Q
   | 6 => (s V_compresscontig_z + max0(-1 + s V_compresscontig__tmp) <= z)%Q
   | 7 => (s V_compresscontig_z + max0(-1 + s V_compresscontig__tmp) <= z)%Q
   | 8 => (s V_compresscontig_z + max0(-1 + s V_compresscontig__tmp) <= z)%Q
   | 9 => (s V_compresscontig_z + max0(-1 + s V_compresscontig__tmp) <= z)%Q
   | 10 => (s V_compresscontig_z + max0(s V_compresscontig__tmp) <= z)%Q
   | 11 => (s V_compresscontig_z + max0(s V_compresscontig__tmp) <= z)%Q
   | 12 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_compresscontig__tmp) (-1
                                                                    + s V_compresscontig__tmp));
      (*-1 0*) F_max0_ge_0 (-1 + s V_compresscontig__tmp)]
     (s V_compresscontig_z + max0(s V_compresscontig__tmp) <= z)%Q
   | 13 => (s V_compresscontig_z <= z)%Q
   | 14 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_compresscontig__tmp) (1)]
     (s V_compresscontig_z + max0(s V_compresscontig__tmp) <= z)%Q
   | 15 => ((1 # 1) + s V_compresscontig_z
            + max0(-1 + s V_compresscontig__tmp) <= z)%Q
   | 16 => ((1 # 1) + s V_compresscontig_z
            + max0(-1 + s V_compresscontig__tmp) <= z)%Q
   | 17 => ((1 # 1) + s V_compresscontig_z
            + max0(-1 + s V_compresscontig__tmp) <= z)%Q
   | 18 => ((1 # 1) + s V_compresscontig_z
            + max0(-1 + s V_compresscontig__tmp) <= z)%Q
   | 19 => ((1 # 1) + s V_compresscontig_z
            + max0(-1 + s V_compresscontig__tmp) <= z)%Q
   | 20 => ((1 # 1) + s V_compresscontig_z
            + max0(-1 + s V_compresscontig__tmp) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_compresscontig =>
    [mkPA Q (fun n z s => ai_compresscontig n s /\ annot0_compresscontig n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_compresscontig (proc_start P_compresscontig) s1 (proc_end P_compresscontig) s2 ->
    (s2 V_compresscontig_z <= max0(-1 + s1 V_compresscontig_n))%Q.
Proof.
  prove_bound ipa admissible_ipa P_compresscontig.
Qed.
