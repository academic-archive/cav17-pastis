Require Import pasta.Pasta.

Inductive proc: Type :=
  P_clist_unpack_short_bits.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_clist_unpack_short_bits_z := 1%positive.
Notation V_clist_unpack_short_bits__tmp := 2%positive.
Notation V_clist_unpack_short_bits__tmp1 := 3%positive.
Notation V_clist_unpack_short_bits__tmp2 := 4%positive.
Notation V_clist_unpack_short_bits_bytes := 5%positive.
Notation V_clist_unpack_short_bits_dest := 6%positive.
Notation V_clist_unpack_short_bits_height := 7%positive.
Notation V_clist_unpack_short_bits_raster := 8%positive.
Notation V_clist_unpack_short_bits_src := 9%positive.
Notation V_clist_unpack_short_bits_width_bytes := 10%positive.
Definition Pedges_clist_unpack_short_bits: list (edge proc) :=
  (EA 1 (AAssign V_clist_unpack_short_bits_z (Some (ENum (0)))) 2)::
  (EA 2 (AAssign V_clist_unpack_short_bits__tmp1
  (Some (EVar V_clist_unpack_short_bits_width_bytes))) 3)::(EA 3 (AAssign
  V_clist_unpack_short_bits__tmp
  (Some (EVar V_clist_unpack_short_bits_height))) 4)::(EA 4 (AAssign
  V_clist_unpack_short_bits__tmp2
  (Some (EVar V_clist_unpack_short_bits_raster))) 5)::(EA 5 (AAssign
  V_clist_unpack_short_bits_bytes
  (Some (EMul (EVar V_clist_unpack_short_bits__tmp1)
  (EVar V_clist_unpack_short_bits__tmp)))) 6)::(EA 6 ANone 7)::(EA 7 (AAssign
  V_clist_unpack_short_bits__tmp
  (Some (EAdd (EVar V_clist_unpack_short_bits__tmp) (ENum (-1))))) 8)::
  (EA 8 AWeaken 9)::(EA 9 (AGuard
  (fun s => ((eval (EAdd (EVar V_clist_unpack_short_bits__tmp) (ENum (-1)))
  s) >= (eval (ENum (0)) s))%Z)) 12)::(EA 9 (AGuard
  (fun s => ((eval (EAdd (EVar V_clist_unpack_short_bits__tmp) (ENum (-1)))
  s) < (eval (ENum (0)) s))%Z)) 10)::(EA 10 AWeaken 11)::(EA 12 AWeaken 13)::
  (EA 13 ANone 21)::(EA 13 ANone 14)::(EA 13 ANone 15)::(EA 13 ANone 16)::
  (EA 13 ANone 17)::(EA 13 ANone 18)::(EA 13 ANone 19)::(EA 13 ANone 20)::
  (EA 14 ANone 15)::(EA 15 ANone 16)::(EA 16 ANone 17)::(EA 17 ANone 18)::
  (EA 18 ANone 19)::(EA 19 ANone 20)::(EA 20 ANone 22)::(EA 21 ANone 22)::
  (EA 22 ANone 23)::(EA 23 ANone 24)::(EA 24 (AAssign
  V_clist_unpack_short_bits_z (Some (EAdd (ENum (1))
  (EVar V_clist_unpack_short_bits_z)))) 7)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_clist_unpack_short_bits => Pedges_clist_unpack_short_bits
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_clist_unpack_short_bits => 11
     end)%positive;
  var_global := var_global
}.

Definition ai_clist_unpack_short_bits (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_clist_unpack_short_bits_z <= 0 /\ -1 * s V_clist_unpack_short_bits_z <= 0)%Z
   | 3 => (-1 * s V_clist_unpack_short_bits_z <= 0 /\ 1 * s V_clist_unpack_short_bits_z <= 0)%Z
   | 4 => (1 * s V_clist_unpack_short_bits_z <= 0 /\ -1 * s V_clist_unpack_short_bits_z <= 0)%Z
   | 5 => (-1 * s V_clist_unpack_short_bits_z <= 0 /\ 1 * s V_clist_unpack_short_bits_z <= 0)%Z
   | 6 => (1 * s V_clist_unpack_short_bits_z <= 0 /\ -1 * s V_clist_unpack_short_bits_z <= 0)%Z
   | 7 => (-1 * s V_clist_unpack_short_bits_z <= 0)%Z
   | 8 => (-1 * s V_clist_unpack_short_bits_z <= 0)%Z
   | 9 => (-1 * s V_clist_unpack_short_bits_z <= 0)%Z
   | 10 => (-1 * s V_clist_unpack_short_bits_z <= 0 /\ 1 * s V_clist_unpack_short_bits__tmp <= 0)%Z
   | 11 => (1 * s V_clist_unpack_short_bits__tmp <= 0 /\ -1 * s V_clist_unpack_short_bits_z <= 0)%Z
   | 12 => (-1 * s V_clist_unpack_short_bits_z <= 0 /\ -1 * s V_clist_unpack_short_bits__tmp + 1 <= 0)%Z
   | 13 => (-1 * s V_clist_unpack_short_bits__tmp + 1 <= 0 /\ -1 * s V_clist_unpack_short_bits_z <= 0)%Z
   | 14 => (-1 * s V_clist_unpack_short_bits_z <= 0 /\ -1 * s V_clist_unpack_short_bits__tmp + 1 <= 0)%Z
   | 15 => (-1 * s V_clist_unpack_short_bits__tmp + 1 <= 0 /\ -1 * s V_clist_unpack_short_bits_z <= 0)%Z
   | 16 => (-1 * s V_clist_unpack_short_bits_z <= 0 /\ -1 * s V_clist_unpack_short_bits__tmp + 1 <= 0)%Z
   | 17 => (-1 * s V_clist_unpack_short_bits__tmp + 1 <= 0 /\ -1 * s V_clist_unpack_short_bits_z <= 0)%Z
   | 18 => (-1 * s V_clist_unpack_short_bits_z <= 0 /\ -1 * s V_clist_unpack_short_bits__tmp + 1 <= 0)%Z
   | 19 => (-1 * s V_clist_unpack_short_bits__tmp + 1 <= 0 /\ -1 * s V_clist_unpack_short_bits_z <= 0)%Z
   | 20 => (-1 * s V_clist_unpack_short_bits_z <= 0 /\ -1 * s V_clist_unpack_short_bits__tmp + 1 <= 0)%Z
   | 21 => (-1 * s V_clist_unpack_short_bits_z <= 0 /\ -1 * s V_clist_unpack_short_bits__tmp + 1 <= 0)%Z
   | 22 => (-1 * s V_clist_unpack_short_bits__tmp + 1 <= 0 /\ -1 * s V_clist_unpack_short_bits_z <= 0)%Z
   | 23 => (-1 * s V_clist_unpack_short_bits_z <= 0 /\ -1 * s V_clist_unpack_short_bits__tmp + 1 <= 0)%Z
   | 24 => (-1 * s V_clist_unpack_short_bits__tmp + 1 <= 0 /\ -1 * s V_clist_unpack_short_bits_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_clist_unpack_short_bits (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(-1 + s V_clist_unpack_short_bits_height) <= z)%Q
   | 2 => (s V_clist_unpack_short_bits_z
           + max0(-1 + s V_clist_unpack_short_bits_height) <= z)%Q
   | 3 => (s V_clist_unpack_short_bits_z
           + max0(-1 + s V_clist_unpack_short_bits_height) <= z)%Q
   | 4 => (s V_clist_unpack_short_bits_z
           + max0(-1 + s V_clist_unpack_short_bits__tmp) <= z)%Q
   | 5 => (s V_clist_unpack_short_bits_z
           + max0(-1 + s V_clist_unpack_short_bits__tmp) <= z)%Q
   | 6 => (s V_clist_unpack_short_bits_z
           + max0(-1 + s V_clist_unpack_short_bits__tmp) <= z)%Q
   | 7 => (s V_clist_unpack_short_bits_z
           + max0(-1 + s V_clist_unpack_short_bits__tmp) <= z)%Q
   | 8 => (s V_clist_unpack_short_bits_z
           + max0(s V_clist_unpack_short_bits__tmp) <= z)%Q
   | 9 => (s V_clist_unpack_short_bits_z
           + max0(s V_clist_unpack_short_bits__tmp) <= z)%Q
   | 10 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_clist_unpack_short_bits__tmp) (-1
                                                                    + s V_clist_unpack_short_bits__tmp));
      (*-1 0*) F_max0_ge_0 (-1 + s V_clist_unpack_short_bits__tmp)]
     (s V_clist_unpack_short_bits_z + max0(s V_clist_unpack_short_bits__tmp) <= z)%Q
   | 11 => (s V_clist_unpack_short_bits_z <= z)%Q
   | 12 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_clist_unpack_short_bits__tmp) (1)]
     (s V_clist_unpack_short_bits_z + max0(s V_clist_unpack_short_bits__tmp) <= z)%Q
   | 13 => ((1 # 1) + s V_clist_unpack_short_bits_z
            + max0(-1 + s V_clist_unpack_short_bits__tmp) <= z)%Q
   | 14 => ((1 # 1) + s V_clist_unpack_short_bits_z
            + max0(-1 + s V_clist_unpack_short_bits__tmp) <= z)%Q
   | 15 => ((1 # 1) + s V_clist_unpack_short_bits_z
            + max0(-1 + s V_clist_unpack_short_bits__tmp) <= z)%Q
   | 16 => ((1 # 1) + s V_clist_unpack_short_bits_z
            + max0(-1 + s V_clist_unpack_short_bits__tmp) <= z)%Q
   | 17 => ((1 # 1) + s V_clist_unpack_short_bits_z
            + max0(-1 + s V_clist_unpack_short_bits__tmp) <= z)%Q
   | 18 => ((1 # 1) + s V_clist_unpack_short_bits_z
            + max0(-1 + s V_clist_unpack_short_bits__tmp) <= z)%Q
   | 19 => ((1 # 1) + s V_clist_unpack_short_bits_z
            + max0(-1 + s V_clist_unpack_short_bits__tmp) <= z)%Q
   | 20 => ((1 # 1) + s V_clist_unpack_short_bits_z
            + max0(-1 + s V_clist_unpack_short_bits__tmp) <= z)%Q
   | 21 => ((1 # 1) + s V_clist_unpack_short_bits_z
            + max0(-1 + s V_clist_unpack_short_bits__tmp) <= z)%Q
   | 22 => ((1 # 1) + s V_clist_unpack_short_bits_z
            + max0(-1 + s V_clist_unpack_short_bits__tmp) <= z)%Q
   | 23 => ((1 # 1) + s V_clist_unpack_short_bits_z
            + max0(-1 + s V_clist_unpack_short_bits__tmp) <= z)%Q
   | 24 => ((1 # 1) + s V_clist_unpack_short_bits_z
            + max0(-1 + s V_clist_unpack_short_bits__tmp) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_clist_unpack_short_bits =>
    [mkPA Q (fun n z s => ai_clist_unpack_short_bits n s /\ annot0_clist_unpack_short_bits n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_clist_unpack_short_bits (proc_start P_clist_unpack_short_bits) s1 (proc_end P_clist_unpack_short_bits) s2 ->
    (s2 V_clist_unpack_short_bits_z <= max0(-1
                                            + s1 V_clist_unpack_short_bits_height))%Q.
Proof.
  prove_bound ipa admissible_ipa P_clist_unpack_short_bits.
Qed.
