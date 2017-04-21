Require Import pasta.Pasta.

Inductive proc: Type :=
  P_bytes_fill_rectangle.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_bytes_fill_rectangle_z := 1%positive.
Notation V_bytes_fill_rectangle__tmp := 2%positive.
Notation V_bytes_fill_rectangle__tmp1 := 3%positive.
Notation V_bytes_fill_rectangle__tmp2 := 4%positive.
Notation V_bytes_fill_rectangle__tmp3 := 5%positive.
Notation V_bytes_fill_rectangle_dest := 6%positive.
Notation V_bytes_fill_rectangle_height := 7%positive.
Notation V_bytes_fill_rectangle_raster := 8%positive.
Notation V_bytes_fill_rectangle_value := 9%positive.
Notation V_bytes_fill_rectangle_width_bytes := 10%positive.
Definition Pedges_bytes_fill_rectangle: list (edge proc) :=
  (EA 1 (AAssign V_bytes_fill_rectangle_z (Some (ENum (0)))) 2)::
  (EA 2 (AAssign V_bytes_fill_rectangle__tmp3
  (Some (EVar V_bytes_fill_rectangle_raster))) 3)::(EA 3 (AAssign
  V_bytes_fill_rectangle__tmp2
  (Some (EVar V_bytes_fill_rectangle_value))) 4)::(EA 4 (AAssign
  V_bytes_fill_rectangle__tmp1
  (Some (EVar V_bytes_fill_rectangle_width_bytes))) 5)::(EA 5 (AAssign
  V_bytes_fill_rectangle__tmp
  (Some (EVar V_bytes_fill_rectangle_height))) 6)::(EA 6 ANone 7)::
  (EA 7 (AAssign V_bytes_fill_rectangle__tmp
  (Some (EAdd (EVar V_bytes_fill_rectangle__tmp) (ENum (-1))))) 8)::
  (EA 8 AWeaken 9)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_bytes_fill_rectangle__tmp) s) > (eval (ENum (0))
  s))%Z)) 12)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_bytes_fill_rectangle__tmp) s) <= (eval (ENum (0))
  s))%Z)) 10)::(EA 10 AWeaken 11)::(EA 12 AWeaken 13)::(EA 13 ANone 14)::
  (EA 14 ANone 15)::(EA 15 (AAssign V_bytes_fill_rectangle_z
  (Some (EAdd (ENum (1)) (EVar V_bytes_fill_rectangle_z)))) 7)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_bytes_fill_rectangle => Pedges_bytes_fill_rectangle
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_bytes_fill_rectangle => 11
     end)%positive;
  var_global := var_global
}.

Definition ai_bytes_fill_rectangle (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_bytes_fill_rectangle_z <= 0 /\ -1 * s V_bytes_fill_rectangle_z <= 0)%Z
   | 3 => (-1 * s V_bytes_fill_rectangle_z <= 0 /\ 1 * s V_bytes_fill_rectangle_z <= 0)%Z
   | 4 => (1 * s V_bytes_fill_rectangle_z <= 0 /\ -1 * s V_bytes_fill_rectangle_z <= 0)%Z
   | 5 => (-1 * s V_bytes_fill_rectangle_z <= 0 /\ 1 * s V_bytes_fill_rectangle_z <= 0)%Z
   | 6 => (1 * s V_bytes_fill_rectangle_z <= 0 /\ -1 * s V_bytes_fill_rectangle_z <= 0)%Z
   | 7 => (-1 * s V_bytes_fill_rectangle_z <= 0)%Z
   | 8 => (-1 * s V_bytes_fill_rectangle_z <= 0)%Z
   | 9 => (-1 * s V_bytes_fill_rectangle_z <= 0)%Z
   | 10 => (-1 * s V_bytes_fill_rectangle_z <= 0 /\ 1 * s V_bytes_fill_rectangle__tmp <= 0)%Z
   | 11 => (1 * s V_bytes_fill_rectangle__tmp <= 0 /\ -1 * s V_bytes_fill_rectangle_z <= 0)%Z
   | 12 => (-1 * s V_bytes_fill_rectangle_z <= 0 /\ -1 * s V_bytes_fill_rectangle__tmp + 1 <= 0)%Z
   | 13 => (-1 * s V_bytes_fill_rectangle__tmp + 1 <= 0 /\ -1 * s V_bytes_fill_rectangle_z <= 0)%Z
   | 14 => (-1 * s V_bytes_fill_rectangle_z <= 0 /\ -1 * s V_bytes_fill_rectangle__tmp + 1 <= 0)%Z
   | 15 => (-1 * s V_bytes_fill_rectangle__tmp + 1 <= 0 /\ -1 * s V_bytes_fill_rectangle_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_bytes_fill_rectangle (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(-1 + s V_bytes_fill_rectangle_height) <= z)%Q
   | 2 => (s V_bytes_fill_rectangle_z
           + max0(-1 + s V_bytes_fill_rectangle_height) <= z)%Q
   | 3 => (s V_bytes_fill_rectangle_z
           + max0(-1 + s V_bytes_fill_rectangle_height) <= z)%Q
   | 4 => (s V_bytes_fill_rectangle_z
           + max0(-1 + s V_bytes_fill_rectangle_height) <= z)%Q
   | 5 => (s V_bytes_fill_rectangle_z
           + max0(-1 + s V_bytes_fill_rectangle_height) <= z)%Q
   | 6 => (s V_bytes_fill_rectangle_z
           + max0(-1 + s V_bytes_fill_rectangle__tmp) <= z)%Q
   | 7 => (s V_bytes_fill_rectangle_z
           + max0(-1 + s V_bytes_fill_rectangle__tmp) <= z)%Q
   | 8 => (s V_bytes_fill_rectangle_z + max0(s V_bytes_fill_rectangle__tmp) <= z)%Q
   | 9 => (s V_bytes_fill_rectangle_z + max0(s V_bytes_fill_rectangle__tmp) <= z)%Q
   | 10 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_bytes_fill_rectangle__tmp) (-1
                                                                    + s V_bytes_fill_rectangle__tmp));
      (*-1 0*) F_max0_ge_0 (-1 + s V_bytes_fill_rectangle__tmp)]
     (s V_bytes_fill_rectangle_z + max0(s V_bytes_fill_rectangle__tmp) <= z)%Q
   | 11 => (s V_bytes_fill_rectangle_z <= z)%Q
   | 12 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_bytes_fill_rectangle__tmp) (1)]
     (s V_bytes_fill_rectangle_z + max0(s V_bytes_fill_rectangle__tmp) <= z)%Q
   | 13 => ((1 # 1) + s V_bytes_fill_rectangle_z
            + max0(-1 + s V_bytes_fill_rectangle__tmp) <= z)%Q
   | 14 => ((1 # 1) + s V_bytes_fill_rectangle_z
            + max0(-1 + s V_bytes_fill_rectangle__tmp) <= z)%Q
   | 15 => ((1 # 1) + s V_bytes_fill_rectangle_z
            + max0(-1 + s V_bytes_fill_rectangle__tmp) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_bytes_fill_rectangle =>
    [mkPA Q (fun n z s => ai_bytes_fill_rectangle n s /\ annot0_bytes_fill_rectangle n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_bytes_fill_rectangle (proc_start P_bytes_fill_rectangle) s1 (proc_end P_bytes_fill_rectangle) s2 ->
    (s2 V_bytes_fill_rectangle_z <= max0(-1
                                         + s1 V_bytes_fill_rectangle_height))%Q.
Proof.
  prove_bound ipa admissible_ipa P_bytes_fill_rectangle.
Qed.
