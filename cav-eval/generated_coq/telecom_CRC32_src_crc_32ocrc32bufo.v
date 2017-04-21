Require Import pasta.Pasta.

Inductive proc: Type :=
  P_crc32buf.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_crc32buf_z := 1%positive.
Notation V_crc32buf__tmp := 2%positive.
Notation V_crc32buf_oldcrc32 := 3%positive.
Notation V_crc32buf_buf := 4%positive.
Notation V_crc32buf_len := 5%positive.
Definition Pedges_crc32buf: list (edge proc) :=
  (EA 1 (AAssign V_crc32buf_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_crc32buf__tmp (Some (EVar V_crc32buf_len))) 3)::(EA 3 (AAssign
  V_crc32buf_oldcrc32 None) 4)::(EA 4 ANone 5)::(EA 5 AWeaken 6)::
  (EA 6 (AGuard (fun s => ((eval (EVar V_crc32buf__tmp) s) <>
  (eval (ENum (0)) s))%Z)) 9)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_crc32buf__tmp) s) = (eval (ENum (0)) s))%Z)) 7)::
  (EA 7 AWeaken 8)::(EA 9 AWeaken 10)::(EA 10 (AAssign V_crc32buf_oldcrc32
  None) 11)::(EA 11 ANone 12)::(EA 12 (AAssign V_crc32buf__tmp
  (Some (EAdd (EVar V_crc32buf__tmp) (ENum (-1))))) 13)::(EA 13 ANone 14)::
  (EA 14 ANone 15)::(EA 15 (AAssign V_crc32buf_z (Some (EAdd (ENum (1))
  (EVar V_crc32buf_z)))) 16)::(EA 16 AWeaken 6)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_crc32buf => Pedges_crc32buf
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_crc32buf => 8
     end)%positive;
  var_global := var_global
}.

Definition ai_crc32buf (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_crc32buf_z <= 0 /\ -1 * s V_crc32buf_z <= 0)%Z
   | 3 => (-1 * s V_crc32buf_z <= 0 /\ 1 * s V_crc32buf_z <= 0)%Z
   | 4 => (1 * s V_crc32buf_z <= 0 /\ -1 * s V_crc32buf_z <= 0)%Z
   | 5 => (-1 * s V_crc32buf_z <= 0 /\ 1 * s V_crc32buf_z <= 0)%Z
   | 6 => (-1 * s V_crc32buf_z <= 0)%Z
   | 7 => (-1 * s V_crc32buf_z <= 0 /\ 1 * s V_crc32buf__tmp <= 0 /\ -1 * s V_crc32buf__tmp <= 0)%Z
   | 8 => (-1 * s V_crc32buf__tmp <= 0 /\ 1 * s V_crc32buf__tmp <= 0 /\ -1 * s V_crc32buf_z <= 0)%Z
   | 9 => (-1 * s V_crc32buf_z <= 0)%Z
   | 10 => (-1 * s V_crc32buf_z <= 0)%Z
   | 11 => (-1 * s V_crc32buf_z <= 0)%Z
   | 12 => (-1 * s V_crc32buf_z <= 0)%Z
   | 13 => (-1 * s V_crc32buf_z <= 0)%Z
   | 14 => (-1 * s V_crc32buf_z <= 0)%Z
   | 15 => (-1 * s V_crc32buf_z <= 0)%Z
   | 16 => (-1 * s V_crc32buf_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_crc32buf (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (s V_crc32buf_len <= z)%Q
   | 2 => (s V_crc32buf_len + s V_crc32buf_z <= z)%Q
   | 3 => (s V_crc32buf__tmp + s V_crc32buf_z <= z)%Q
   | 4 => (s V_crc32buf__tmp + s V_crc32buf_z <= z)%Q
   | 5 => (s V_crc32buf__tmp + s V_crc32buf_z <= z)%Q
   | 6 => (s V_crc32buf__tmp + s V_crc32buf_z <= z)%Q
   | 7 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_crc32buf__tmp)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_crc32buf__tmp) (0))) (F_max0_ge_0 (s V_crc32buf__tmp))]
     (s V_crc32buf__tmp + s V_crc32buf_z <= z)%Q
   | 8 => (s V_crc32buf_z <= z)%Q
   | 9 => (s V_crc32buf__tmp + s V_crc32buf_z <= z)%Q
   | 10 => (s V_crc32buf__tmp + s V_crc32buf_z <= z)%Q
   | 11 => (s V_crc32buf__tmp + s V_crc32buf_z <= z)%Q
   | 12 => (s V_crc32buf__tmp + s V_crc32buf_z <= z)%Q
   | 13 => ((1 # 1) + s V_crc32buf__tmp + s V_crc32buf_z <= z)%Q
   | 14 => ((1 # 1) + s V_crc32buf__tmp + s V_crc32buf_z <= z)%Q
   | 15 => ((1 # 1) + s V_crc32buf__tmp + s V_crc32buf_z <= z)%Q
   | 16 => (s V_crc32buf__tmp + s V_crc32buf_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_crc32buf =>
    [mkPA Q (fun n z s => ai_crc32buf n s /\ annot0_crc32buf n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_crc32buf (proc_start P_crc32buf) s1 (proc_end P_crc32buf) s2 ->
    (s2 V_crc32buf_z <= s1 V_crc32buf_len)%Q.
Proof.
  prove_bound ipa admissible_ipa P_crc32buf.
Qed.
