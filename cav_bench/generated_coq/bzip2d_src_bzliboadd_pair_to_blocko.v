Require Import pasta.Pasta.

Inductive proc: Type :=
  P_add_pair_to_block.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_add_pair_to_block_z := 1%positive.
Notation V_add_pair_to_block_ch := 2%positive.
Notation V_add_pair_to_block_i := 3%positive.
Notation V_add_pair_to_block_s_dref_off108 := 4%positive.
Notation V_add_pair_to_block_s_dref_off648 := 5%positive.
Notation V_add_pair_to_block_s_dref_off92 := 6%positive.
Notation V_add_pair_to_block_s_dref_off96 := 7%positive.
Notation V_add_pair_to_block_s := 8%positive.
Definition Pedges_add_pair_to_block: list (edge proc) :=
  (EA 1 (AAssign V_add_pair_to_block_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_add_pair_to_block_ch (Some (EVar V_add_pair_to_block_s_dref_off92))) 3)::
  (EA 3 (AAssign V_add_pair_to_block_i (Some (ENum (0)))) 4)::
  (EA 4 ANone 5)::(EA 5 AWeaken 6)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_add_pair_to_block_i) s) <
  (eval (EVar V_add_pair_to_block_s_dref_off96) s))%Z)) 29)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_add_pair_to_block_i) s) >=
  (eval (EVar V_add_pair_to_block_s_dref_off96) s))%Z)) 7)::
  (EA 7 AWeaken 8)::(EA 8 ANone 21)::(EA 8 ANone 18)::(EA 8 ANone 14)::
  (EA 8 ANone 9)::(EA 9 (AAssign V_add_pair_to_block_s_dref_off108
  (Some (EAdd (EVar V_add_pair_to_block_s_dref_off108) (ENum (1))))) 10)::
  (EA 10 (AAssign V_add_pair_to_block_s_dref_off108
  (Some (EAdd (EVar V_add_pair_to_block_s_dref_off108) (ENum (1))))) 11)::
  (EA 11 (AAssign V_add_pair_to_block_s_dref_off108
  (Some (EAdd (EVar V_add_pair_to_block_s_dref_off108) (ENum (1))))) 12)::
  (EA 12 ANone 13)::(EA 13 AWeaken 28)::(EA 14 (AAssign
  V_add_pair_to_block_s_dref_off108
  (Some (EAdd (EVar V_add_pair_to_block_s_dref_off108) (ENum (1))))) 15)::
  (EA 15 (AAssign V_add_pair_to_block_s_dref_off108
  (Some (EAdd (EVar V_add_pair_to_block_s_dref_off108) (ENum (1))))) 16)::
  (EA 16 ANone 17)::(EA 17 AWeaken 28)::(EA 18 (AAssign
  V_add_pair_to_block_s_dref_off108
  (Some (EAdd (EVar V_add_pair_to_block_s_dref_off108) (ENum (1))))) 19)::
  (EA 19 ANone 20)::(EA 20 AWeaken 28)::(EA 21 (AAssign
  V_add_pair_to_block_s_dref_off108
  (Some (EAdd (EVar V_add_pair_to_block_s_dref_off108) (ENum (1))))) 22)::
  (EA 22 (AAssign V_add_pair_to_block_s_dref_off108
  (Some (EAdd (EVar V_add_pair_to_block_s_dref_off108) (ENum (1))))) 23)::
  (EA 23 (AAssign V_add_pair_to_block_s_dref_off108
  (Some (EAdd (EVar V_add_pair_to_block_s_dref_off108) (ENum (1))))) 24)::
  (EA 24 (AAssign V_add_pair_to_block_s_dref_off108
  (Some (EAdd (EVar V_add_pair_to_block_s_dref_off108) (ENum (1))))) 25)::
  (EA 25 (AAssign V_add_pair_to_block_s_dref_off108
  (Some (EAdd (EVar V_add_pair_to_block_s_dref_off108) (ENum (1))))) 26)::
  (EA 26 ANone 27)::(EA 27 AWeaken 28)::(EA 29 AWeaken 30)::(EA 30 (AAssign
  V_add_pair_to_block_s_dref_off648 None) 31)::(EA 31 ANone 32)::
  (EA 32 (AAssign V_add_pair_to_block_i
  (Some (EAdd (EVar V_add_pair_to_block_i) (ENum (1))))) 33)::
  (EA 33 ANone 34)::(EA 34 ANone 35)::(EA 35 (AAssign V_add_pair_to_block_z
  (Some (EAdd (ENum (1)) (EVar V_add_pair_to_block_z)))) 36)::
  (EA 36 AWeaken 6)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_add_pair_to_block => Pedges_add_pair_to_block
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_add_pair_to_block => 28
     end)%positive;
  var_global := var_global
}.

Definition ai_add_pair_to_block (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_add_pair_to_block_z <= 0 /\ -1 * s V_add_pair_to_block_z <= 0)%Z
   | 3 => (-1 * s V_add_pair_to_block_z <= 0 /\ 1 * s V_add_pair_to_block_z <= 0)%Z
   | 4 => (1 * s V_add_pair_to_block_z <= 0 /\ -1 * s V_add_pair_to_block_z <= 0 /\ 1 * s V_add_pair_to_block_i <= 0 /\ -1 * s V_add_pair_to_block_i <= 0)%Z
   | 5 => (-1 * s V_add_pair_to_block_i <= 0 /\ 1 * s V_add_pair_to_block_i <= 0 /\ -1 * s V_add_pair_to_block_z <= 0 /\ 1 * s V_add_pair_to_block_z <= 0)%Z
   | 6 => (-1 * s V_add_pair_to_block_z <= 0 /\ -1 * s V_add_pair_to_block_i <= 0)%Z
   | 7 => (-1 * s V_add_pair_to_block_i <= 0 /\ -1 * s V_add_pair_to_block_z <= 0 /\ -1 * s V_add_pair_to_block_i+ 1 * s V_add_pair_to_block_s_dref_off96 <= 0)%Z
   | 8 => (-1 * s V_add_pair_to_block_i+ 1 * s V_add_pair_to_block_s_dref_off96 <= 0 /\ -1 * s V_add_pair_to_block_z <= 0 /\ -1 * s V_add_pair_to_block_i <= 0)%Z
   | 9 => (-1 * s V_add_pair_to_block_i <= 0 /\ -1 * s V_add_pair_to_block_z <= 0 /\ -1 * s V_add_pair_to_block_i+ 1 * s V_add_pair_to_block_s_dref_off96 <= 0)%Z
   | 10 => (-1 * s V_add_pair_to_block_i+ 1 * s V_add_pair_to_block_s_dref_off96 <= 0 /\ -1 * s V_add_pair_to_block_z <= 0 /\ -1 * s V_add_pair_to_block_i <= 0)%Z
   | 11 => (-1 * s V_add_pair_to_block_i <= 0 /\ -1 * s V_add_pair_to_block_z <= 0 /\ -1 * s V_add_pair_to_block_i+ 1 * s V_add_pair_to_block_s_dref_off96 <= 0)%Z
   | 12 => (-1 * s V_add_pair_to_block_i+ 1 * s V_add_pair_to_block_s_dref_off96 <= 0 /\ -1 * s V_add_pair_to_block_z <= 0 /\ -1 * s V_add_pair_to_block_i <= 0)%Z
   | 13 => (-1 * s V_add_pair_to_block_i <= 0 /\ -1 * s V_add_pair_to_block_z <= 0 /\ -1 * s V_add_pair_to_block_i+ 1 * s V_add_pair_to_block_s_dref_off96 <= 0)%Z
   | 14 => (-1 * s V_add_pair_to_block_i <= 0 /\ -1 * s V_add_pair_to_block_z <= 0 /\ -1 * s V_add_pair_to_block_i+ 1 * s V_add_pair_to_block_s_dref_off96 <= 0)%Z
   | 15 => (-1 * s V_add_pair_to_block_i+ 1 * s V_add_pair_to_block_s_dref_off96 <= 0 /\ -1 * s V_add_pair_to_block_z <= 0 /\ -1 * s V_add_pair_to_block_i <= 0)%Z
   | 16 => (-1 * s V_add_pair_to_block_i <= 0 /\ -1 * s V_add_pair_to_block_z <= 0 /\ -1 * s V_add_pair_to_block_i+ 1 * s V_add_pair_to_block_s_dref_off96 <= 0)%Z
   | 17 => (-1 * s V_add_pair_to_block_i+ 1 * s V_add_pair_to_block_s_dref_off96 <= 0 /\ -1 * s V_add_pair_to_block_z <= 0 /\ -1 * s V_add_pair_to_block_i <= 0)%Z
   | 18 => (-1 * s V_add_pair_to_block_i <= 0 /\ -1 * s V_add_pair_to_block_z <= 0 /\ -1 * s V_add_pair_to_block_i+ 1 * s V_add_pair_to_block_s_dref_off96 <= 0)%Z
   | 19 => (-1 * s V_add_pair_to_block_i+ 1 * s V_add_pair_to_block_s_dref_off96 <= 0 /\ -1 * s V_add_pair_to_block_z <= 0 /\ -1 * s V_add_pair_to_block_i <= 0)%Z
   | 20 => (-1 * s V_add_pair_to_block_i <= 0 /\ -1 * s V_add_pair_to_block_z <= 0 /\ -1 * s V_add_pair_to_block_i+ 1 * s V_add_pair_to_block_s_dref_off96 <= 0)%Z
   | 21 => (-1 * s V_add_pair_to_block_i <= 0 /\ -1 * s V_add_pair_to_block_z <= 0 /\ -1 * s V_add_pair_to_block_i+ 1 * s V_add_pair_to_block_s_dref_off96 <= 0)%Z
   | 22 => (-1 * s V_add_pair_to_block_i+ 1 * s V_add_pair_to_block_s_dref_off96 <= 0 /\ -1 * s V_add_pair_to_block_z <= 0 /\ -1 * s V_add_pair_to_block_i <= 0)%Z
   | 23 => (-1 * s V_add_pair_to_block_i <= 0 /\ -1 * s V_add_pair_to_block_z <= 0 /\ -1 * s V_add_pair_to_block_i+ 1 * s V_add_pair_to_block_s_dref_off96 <= 0)%Z
   | 24 => (-1 * s V_add_pair_to_block_i+ 1 * s V_add_pair_to_block_s_dref_off96 <= 0 /\ -1 * s V_add_pair_to_block_z <= 0 /\ -1 * s V_add_pair_to_block_i <= 0)%Z
   | 25 => (-1 * s V_add_pair_to_block_i <= 0 /\ -1 * s V_add_pair_to_block_z <= 0 /\ -1 * s V_add_pair_to_block_i+ 1 * s V_add_pair_to_block_s_dref_off96 <= 0)%Z
   | 26 => (-1 * s V_add_pair_to_block_i+ 1 * s V_add_pair_to_block_s_dref_off96 <= 0 /\ -1 * s V_add_pair_to_block_z <= 0 /\ -1 * s V_add_pair_to_block_i <= 0)%Z
   | 27 => (-1 * s V_add_pair_to_block_i <= 0 /\ -1 * s V_add_pair_to_block_z <= 0 /\ -1 * s V_add_pair_to_block_i+ 1 * s V_add_pair_to_block_s_dref_off96 <= 0)%Z
   | 28 => (-1 * s V_add_pair_to_block_i+ 1 * s V_add_pair_to_block_s_dref_off96 <= 0 /\ -1 * s V_add_pair_to_block_z <= 0 /\ -1 * s V_add_pair_to_block_i <= 0)%Z
   | 29 => (-1 * s V_add_pair_to_block_i <= 0 /\ -1 * s V_add_pair_to_block_z <= 0 /\ 1 * s V_add_pair_to_block_i+ -1 * s V_add_pair_to_block_s_dref_off96 + 1 <= 0)%Z
   | 30 => (1 * s V_add_pair_to_block_i+ -1 * s V_add_pair_to_block_s_dref_off96 + 1 <= 0 /\ -1 * s V_add_pair_to_block_z <= 0 /\ -1 * s V_add_pair_to_block_i <= 0)%Z
   | 31 => (-1 * s V_add_pair_to_block_i <= 0 /\ -1 * s V_add_pair_to_block_z <= 0 /\ 1 * s V_add_pair_to_block_i+ -1 * s V_add_pair_to_block_s_dref_off96 + 1 <= 0)%Z
   | 32 => (1 * s V_add_pair_to_block_i+ -1 * s V_add_pair_to_block_s_dref_off96 + 1 <= 0 /\ -1 * s V_add_pair_to_block_z <= 0 /\ -1 * s V_add_pair_to_block_i <= 0)%Z
   | 33 => (-1 * s V_add_pair_to_block_z <= 0 /\ 1 * s V_add_pair_to_block_i+ -1 * s V_add_pair_to_block_s_dref_off96 <= 0 /\ -1 * s V_add_pair_to_block_i + 1 <= 0)%Z
   | 34 => (-1 * s V_add_pair_to_block_i + 1 <= 0 /\ 1 * s V_add_pair_to_block_i+ -1 * s V_add_pair_to_block_s_dref_off96 <= 0 /\ -1 * s V_add_pair_to_block_z <= 0)%Z
   | 35 => (-1 * s V_add_pair_to_block_z <= 0 /\ 1 * s V_add_pair_to_block_i+ -1 * s V_add_pair_to_block_s_dref_off96 <= 0 /\ -1 * s V_add_pair_to_block_i + 1 <= 0)%Z
   | 36 => (-1 * s V_add_pair_to_block_i + 1 <= 0 /\ 1 * s V_add_pair_to_block_i+ -1 * s V_add_pair_to_block_s_dref_off96 <= 0 /\ -1 * s V_add_pair_to_block_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_add_pair_to_block (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_add_pair_to_block_s_dref_off96) <= z)%Q
   | 2 => (s V_add_pair_to_block_z + max0(s V_add_pair_to_block_s_dref_off96) <= z)%Q
   | 3 => (s V_add_pair_to_block_z + max0(s V_add_pair_to_block_s_dref_off96) <= z)%Q
   | 4 => (s V_add_pair_to_block_z
           + max0(-s V_add_pair_to_block_i
                  + s V_add_pair_to_block_s_dref_off96) <= z)%Q
   | 5 => (s V_add_pair_to_block_z
           + max0(-s V_add_pair_to_block_i
                  + s V_add_pair_to_block_s_dref_off96) <= z)%Q
   | 6 => (s V_add_pair_to_block_z
           + max0(-s V_add_pair_to_block_i
                  + s V_add_pair_to_block_s_dref_off96) <= z)%Q
   | 7 => (s V_add_pair_to_block_z
           + max0(-s V_add_pair_to_block_i
                  + s V_add_pair_to_block_s_dref_off96) <= z)%Q
   | 8 => (s V_add_pair_to_block_z
           + max0(-s V_add_pair_to_block_i
                  + s V_add_pair_to_block_s_dref_off96) <= z)%Q
   | 9 => (s V_add_pair_to_block_z
           + max0(-s V_add_pair_to_block_i
                  + s V_add_pair_to_block_s_dref_off96) <= z)%Q
   | 10 => (s V_add_pair_to_block_z
            + max0(-s V_add_pair_to_block_i
                   + s V_add_pair_to_block_s_dref_off96) <= z)%Q
   | 11 => (s V_add_pair_to_block_z
            + max0(-s V_add_pair_to_block_i
                   + s V_add_pair_to_block_s_dref_off96) <= z)%Q
   | 12 => (s V_add_pair_to_block_z
            + max0(-s V_add_pair_to_block_i
                   + s V_add_pair_to_block_s_dref_off96) <= z)%Q
   | 13 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-s V_add_pair_to_block_i
                                             + s V_add_pair_to_block_s_dref_off96) (-1
                                                                    - s V_add_pair_to_block_i
                                                                    + s V_add_pair_to_block_s_dref_off96));
      (*-1 0*) F_max0_ge_0 (-1 - s V_add_pair_to_block_i
                            + s V_add_pair_to_block_s_dref_off96)]
     (s V_add_pair_to_block_z
      + max0(-s V_add_pair_to_block_i + s V_add_pair_to_block_s_dref_off96) <= z)%Q
   | 14 => (s V_add_pair_to_block_z
            + max0(-s V_add_pair_to_block_i
                   + s V_add_pair_to_block_s_dref_off96) <= z)%Q
   | 15 => (s V_add_pair_to_block_z
            + max0(-s V_add_pair_to_block_i
                   + s V_add_pair_to_block_s_dref_off96) <= z)%Q
   | 16 => (s V_add_pair_to_block_z
            + max0(-s V_add_pair_to_block_i
                   + s V_add_pair_to_block_s_dref_off96) <= z)%Q
   | 17 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-s V_add_pair_to_block_i
                                             + s V_add_pair_to_block_s_dref_off96) (-1
                                                                    - s V_add_pair_to_block_i
                                                                    + s V_add_pair_to_block_s_dref_off96));
      (*-1 0*) F_max0_ge_0 (-1 - s V_add_pair_to_block_i
                            + s V_add_pair_to_block_s_dref_off96)]
     (s V_add_pair_to_block_z
      + max0(-s V_add_pair_to_block_i + s V_add_pair_to_block_s_dref_off96) <= z)%Q
   | 18 => (s V_add_pair_to_block_z
            + max0(-s V_add_pair_to_block_i
                   + s V_add_pair_to_block_s_dref_off96) <= z)%Q
   | 19 => (s V_add_pair_to_block_z
            + max0(-s V_add_pair_to_block_i
                   + s V_add_pair_to_block_s_dref_off96) <= z)%Q
   | 20 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-s V_add_pair_to_block_i
                                             + s V_add_pair_to_block_s_dref_off96) (-1
                                                                    - s V_add_pair_to_block_i
                                                                    + s V_add_pair_to_block_s_dref_off96));
      (*-1 0*) F_max0_ge_0 (-1 - s V_add_pair_to_block_i
                            + s V_add_pair_to_block_s_dref_off96)]
     (s V_add_pair_to_block_z
      + max0(-s V_add_pair_to_block_i + s V_add_pair_to_block_s_dref_off96) <= z)%Q
   | 21 => (s V_add_pair_to_block_z
            + max0(-s V_add_pair_to_block_i
                   + s V_add_pair_to_block_s_dref_off96) <= z)%Q
   | 22 => (s V_add_pair_to_block_z
            + max0(-s V_add_pair_to_block_i
                   + s V_add_pair_to_block_s_dref_off96) <= z)%Q
   | 23 => (s V_add_pair_to_block_z
            + max0(-s V_add_pair_to_block_i
                   + s V_add_pair_to_block_s_dref_off96) <= z)%Q
   | 24 => (s V_add_pair_to_block_z
            + max0(-s V_add_pair_to_block_i
                   + s V_add_pair_to_block_s_dref_off96) <= z)%Q
   | 25 => (s V_add_pair_to_block_z
            + max0(-s V_add_pair_to_block_i
                   + s V_add_pair_to_block_s_dref_off96) <= z)%Q
   | 26 => (s V_add_pair_to_block_z
            + max0(-s V_add_pair_to_block_i
                   + s V_add_pair_to_block_s_dref_off96) <= z)%Q
   | 27 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-s V_add_pair_to_block_i
                                             + s V_add_pair_to_block_s_dref_off96) (-1
                                                                    - s V_add_pair_to_block_i
                                                                    + s V_add_pair_to_block_s_dref_off96));
      (*-1 0*) F_max0_ge_0 (-1 - s V_add_pair_to_block_i
                            + s V_add_pair_to_block_s_dref_off96)]
     (s V_add_pair_to_block_z
      + max0(-s V_add_pair_to_block_i + s V_add_pair_to_block_s_dref_off96) <= z)%Q
   | 28 => (s V_add_pair_to_block_z <= z)%Q
   | 29 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (-s V_add_pair_to_block_i
                                       + s V_add_pair_to_block_s_dref_off96) (1)]
     (s V_add_pair_to_block_z
      + max0(-s V_add_pair_to_block_i + s V_add_pair_to_block_s_dref_off96) <= z)%Q
   | 30 => ((1 # 1) + s V_add_pair_to_block_z
            + max0(-1 - s V_add_pair_to_block_i
                   + s V_add_pair_to_block_s_dref_off96) <= z)%Q
   | 31 => ((1 # 1) + s V_add_pair_to_block_z
            + max0(-1 - s V_add_pair_to_block_i
                   + s V_add_pair_to_block_s_dref_off96) <= z)%Q
   | 32 => ((1 # 1) + s V_add_pair_to_block_z
            + max0(-1 - s V_add_pair_to_block_i
                   + s V_add_pair_to_block_s_dref_off96) <= z)%Q
   | 33 => ((1 # 1) + s V_add_pair_to_block_z
            + max0(-s V_add_pair_to_block_i
                   + s V_add_pair_to_block_s_dref_off96) <= z)%Q
   | 34 => ((1 # 1) + s V_add_pair_to_block_z
            + max0(-s V_add_pair_to_block_i
                   + s V_add_pair_to_block_s_dref_off96) <= z)%Q
   | 35 => ((1 # 1) + s V_add_pair_to_block_z
            + max0(-s V_add_pair_to_block_i
                   + s V_add_pair_to_block_s_dref_off96) <= z)%Q
   | 36 => (s V_add_pair_to_block_z
            + max0(-s V_add_pair_to_block_i
                   + s V_add_pair_to_block_s_dref_off96) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_add_pair_to_block =>
    [mkPA Q (fun n z s => ai_add_pair_to_block n s /\ annot0_add_pair_to_block n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_add_pair_to_block (proc_start P_add_pair_to_block) s1 (proc_end P_add_pair_to_block) s2 ->
    (s2 V_add_pair_to_block_z <= max0(s1 V_add_pair_to_block_s_dref_off96))%Q.
Proof.
  prove_bound ipa admissible_ipa P_add_pair_to_block.
Qed.
