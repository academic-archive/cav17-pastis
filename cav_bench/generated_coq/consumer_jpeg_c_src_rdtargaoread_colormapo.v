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
  V_read_colormap__tmp1 (Some (EVar V_read_colormap_cmaplen))) 3)::
  (EA 3 (AAssign V_read_colormap__tmp
  (Some (EVar V_read_colormap_mapentrysize))) 4)::(EA 4 AWeaken 5)::
  (EA 5 (AGuard (fun s => ((eval (EVar V_read_colormap__tmp) s) <>
  (eval (ENum (24)) s))%Z)) 7)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_read_colormap__tmp) s) = (eval (ENum (24))
  s))%Z)) 6)::(EA 6 AWeaken 9)::(EA 7 AWeaken 8)::(EA 8 ANone 9)::
  (EA 9 (AAssign V_read_colormap_i (Some (ENum (0)))) 10)::(EA 10 ANone 11)::
  (EA 11 AWeaken 12)::(EA 12 (AGuard
  (fun s => ((eval (EVar V_read_colormap_i) s) <
  (eval (EVar V_read_colormap__tmp1) s))%Z)) 15)::(EA 12 (AGuard
  (fun s => ((eval (EVar V_read_colormap_i) s) >=
  (eval (EVar V_read_colormap__tmp1) s))%Z)) 13)::(EA 13 AWeaken 14)::
  (EA 15 AWeaken 16)::(EA 16 ANone 17)::(EA 17 (AAssign V_read_colormap_i
  (Some (EAdd (EVar V_read_colormap_i) (ENum (1))))) 18)::(EA 18 ANone 19)::
  (EA 19 ANone 20)::(EA 20 (AAssign V_read_colormap_z (Some (EAdd (ENum (1))
  (EVar V_read_colormap_z)))) 21)::(EA 21 AWeaken 12)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_read_colormap => Pedges_read_colormap
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_read_colormap => 14
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
   | 6 => (1 * s V_read_colormap_z <= 0 /\ -1 * s V_read_colormap_z <= 0 /\ 1 * s V_read_colormap__tmp + -24 <= 0 /\ -1 * s V_read_colormap__tmp + 24 <= 0)%Z
   | 7 => (1 * s V_read_colormap_z <= 0 /\ -1 * s V_read_colormap_z <= 0)%Z
   | 8 => (-1 * s V_read_colormap_z <= 0 /\ 1 * s V_read_colormap_z <= 0)%Z
   | 9 => (1 * s V_read_colormap_z <= 0 /\ -1 * s V_read_colormap_z <= 0)%Z
   | 10 => (-1 * s V_read_colormap_z <= 0 /\ 1 * s V_read_colormap_z <= 0 /\ 1 * s V_read_colormap_i <= 0 /\ -1 * s V_read_colormap_i <= 0)%Z
   | 11 => (-1 * s V_read_colormap_i <= 0 /\ 1 * s V_read_colormap_i <= 0 /\ 1 * s V_read_colormap_z <= 0 /\ -1 * s V_read_colormap_z <= 0)%Z
   | 12 => (-1 * s V_read_colormap_z <= 0 /\ -1 * s V_read_colormap_i <= 0)%Z
   | 13 => (-1 * s V_read_colormap_i <= 0 /\ -1 * s V_read_colormap_z <= 0 /\ 1 * s V_read_colormap__tmp1+ -1 * s V_read_colormap_i <= 0)%Z
   | 14 => (1 * s V_read_colormap__tmp1+ -1 * s V_read_colormap_i <= 0 /\ -1 * s V_read_colormap_z <= 0 /\ -1 * s V_read_colormap_i <= 0)%Z
   | 15 => (-1 * s V_read_colormap_i <= 0 /\ -1 * s V_read_colormap_z <= 0 /\ -1 * s V_read_colormap__tmp1+ 1 * s V_read_colormap_i + 1 <= 0)%Z
   | 16 => (-1 * s V_read_colormap__tmp1+ 1 * s V_read_colormap_i + 1 <= 0 /\ -1 * s V_read_colormap_z <= 0 /\ -1 * s V_read_colormap_i <= 0)%Z
   | 17 => (-1 * s V_read_colormap_i <= 0 /\ -1 * s V_read_colormap_z <= 0 /\ -1 * s V_read_colormap__tmp1+ 1 * s V_read_colormap_i + 1 <= 0)%Z
   | 18 => (-1 * s V_read_colormap_z <= 0 /\ -1 * s V_read_colormap_i + 1 <= 0 /\ -1 * s V_read_colormap__tmp1+ 1 * s V_read_colormap_i <= 0)%Z
   | 19 => (-1 * s V_read_colormap__tmp1+ 1 * s V_read_colormap_i <= 0 /\ -1 * s V_read_colormap_i + 1 <= 0 /\ -1 * s V_read_colormap_z <= 0)%Z
   | 20 => (-1 * s V_read_colormap_z <= 0 /\ -1 * s V_read_colormap_i + 1 <= 0 /\ -1 * s V_read_colormap__tmp1+ 1 * s V_read_colormap_i <= 0)%Z
   | 21 => (-1 * s V_read_colormap__tmp1+ 1 * s V_read_colormap_i <= 0 /\ -1 * s V_read_colormap_i + 1 <= 0 /\ -1 * s V_read_colormap_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_read_colormap (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_read_colormap_cmaplen) <= z)%Q
   | 2 => (s V_read_colormap_z + max0(s V_read_colormap_cmaplen) <= z)%Q
   | 3 => (s V_read_colormap_z + max0(s V_read_colormap__tmp1) <= z)%Q
   | 4 => (s V_read_colormap_z + max0(s V_read_colormap__tmp1) <= z)%Q
   | 5 => (s V_read_colormap_z + max0(s V_read_colormap__tmp1) <= z)%Q
   | 6 => (s V_read_colormap_z + max0(s V_read_colormap__tmp1) <= z)%Q
   | 7 => (s V_read_colormap_z + max0(s V_read_colormap__tmp1) <= z)%Q
   | 8 => (s V_read_colormap_z + max0(s V_read_colormap__tmp1) <= z)%Q
   | 9 => (s V_read_colormap_z + max0(s V_read_colormap__tmp1) <= z)%Q
   | 10 => (s V_read_colormap_z
            + max0(s V_read_colormap__tmp1 - s V_read_colormap_i) <= z)%Q
   | 11 => (s V_read_colormap_z
            + max0(s V_read_colormap__tmp1 - s V_read_colormap_i) <= z)%Q
   | 12 => (s V_read_colormap_z
            + max0(s V_read_colormap__tmp1 - s V_read_colormap_i) <= z)%Q
   | 13 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_read_colormap__tmp1
                                             - s V_read_colormap_i) (-1
                                                                    + 
                                                                    s V_read_colormap__tmp1
                                                                    - 
                                                                    s V_read_colormap_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V_read_colormap__tmp1
                            - s V_read_colormap_i)]
     (s V_read_colormap_z
      + max0(s V_read_colormap__tmp1 - s V_read_colormap_i) <= z)%Q
   | 14 => (s V_read_colormap_z <= z)%Q
   | 15 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_read_colormap__tmp1
                                       - s V_read_colormap_i) (1)]
     (s V_read_colormap_z
      + max0(s V_read_colormap__tmp1 - s V_read_colormap_i) <= z)%Q
   | 16 => ((1 # 1) + s V_read_colormap_z
            + max0(-1 + s V_read_colormap__tmp1 - s V_read_colormap_i) <= z)%Q
   | 17 => ((1 # 1) + s V_read_colormap_z
            + max0(-1 + s V_read_colormap__tmp1 - s V_read_colormap_i) <= z)%Q
   | 18 => ((1 # 1) + s V_read_colormap_z
            + max0(s V_read_colormap__tmp1 - s V_read_colormap_i) <= z)%Q
   | 19 => ((1 # 1) + s V_read_colormap_z
            + max0(s V_read_colormap__tmp1 - s V_read_colormap_i) <= z)%Q
   | 20 => ((1 # 1) + s V_read_colormap_z
            + max0(s V_read_colormap__tmp1 - s V_read_colormap_i) <= z)%Q
   | 21 => (s V_read_colormap_z
            + max0(s V_read_colormap__tmp1 - s V_read_colormap_i) <= z)%Q
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
