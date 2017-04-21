Require Import pasta.Pasta.

Inductive proc: Type :=
  P_TIFFYCbCrToRGBInit.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_TIFFYCbCrToRGBInit_z := 1%positive.
Notation V_TIFFYCbCrToRGBInit_D1 := 2%positive.
Notation V_TIFFYCbCrToRGBInit_D2 := 3%positive.
Notation V_TIFFYCbCrToRGBInit_D3 := 4%positive.
Notation V_TIFFYCbCrToRGBInit_D4 := 5%positive.
Notation V_TIFFYCbCrToRGBInit_i := 6%positive.
Notation V_TIFFYCbCrToRGBInit_x := 7%positive.
Notation V_TIFFYCbCrToRGBInit_tif := 8%positive.
Notation V_TIFFYCbCrToRGBInit_ycbcr := 9%positive.
Definition Pedges_TIFFYCbCrToRGBInit: list (edge proc) :=
  (EA 1 (AAssign V_TIFFYCbCrToRGBInit_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_TIFFYCbCrToRGBInit_i (Some (ENum (0)))) 3)::(EA 3 ANone 4)::
  (EA 4 AWeaken 5)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_TIFFYCbCrToRGBInit_i) s) < (eval (ENum (256))
  s))%Z)) 26)::(EA 5 (AGuard (fun s => ((eval (EVar V_TIFFYCbCrToRGBInit_i)
  s) >= (eval (ENum (256)) s))%Z)) 6)::(EA 6 AWeaken 7)::(EA 7 (AAssign
  V_TIFFYCbCrToRGBInit_D1 None) 8)::(EA 8 (AAssign V_TIFFYCbCrToRGBInit_D2
  None) 9)::(EA 9 (AAssign V_TIFFYCbCrToRGBInit_D3 None) 10)::(EA 10 (AAssign
  V_TIFFYCbCrToRGBInit_D4 None) 11)::(EA 11 (AAssign V_TIFFYCbCrToRGBInit_i
  (Some (ENum (0)))) 12)::(EA 12 (AAssign V_TIFFYCbCrToRGBInit_x
  (Some (ENum (-128)))) 13)::(EA 13 ANone 14)::(EA 14 AWeaken 15)::
  (EA 15 (AGuard (fun s => ((eval (EVar V_TIFFYCbCrToRGBInit_i) s) <
  (eval (ENum (256)) s))%Z)) 18)::(EA 15 (AGuard
  (fun s => ((eval (EVar V_TIFFYCbCrToRGBInit_i) s) >= (eval (ENum (256))
  s))%Z)) 16)::(EA 16 AWeaken 17)::(EA 18 AWeaken 19)::(EA 19 ANone 20)::
  (EA 20 (AAssign V_TIFFYCbCrToRGBInit_i
  (Some (EAdd (EVar V_TIFFYCbCrToRGBInit_i) (ENum (1))))) 21)::
  (EA 21 (AAssign V_TIFFYCbCrToRGBInit_x
  (Some (EAdd (EVar V_TIFFYCbCrToRGBInit_x) (ENum (1))))) 22)::
  (EA 22 ANone 23)::(EA 23 ANone 24)::(EA 24 (AAssign V_TIFFYCbCrToRGBInit_z
  (Some (EAdd (ENum (1)) (EVar V_TIFFYCbCrToRGBInit_z)))) 25)::
  (EA 25 AWeaken 15)::(EA 26 AWeaken 27)::(EA 27 ANone 28)::(EA 28 (AAssign
  V_TIFFYCbCrToRGBInit_i (Some (EAdd (EVar V_TIFFYCbCrToRGBInit_i)
  (ENum (1))))) 29)::(EA 29 ANone 30)::(EA 30 ANone 31)::(EA 31 (AAssign
  V_TIFFYCbCrToRGBInit_z (Some (EAdd (ENum (1))
  (EVar V_TIFFYCbCrToRGBInit_z)))) 32)::(EA 32 AWeaken 5)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_TIFFYCbCrToRGBInit => Pedges_TIFFYCbCrToRGBInit
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_TIFFYCbCrToRGBInit => 17
     end)%positive;
  var_global := var_global
}.

Definition ai_TIFFYCbCrToRGBInit (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_TIFFYCbCrToRGBInit_z <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_z <= 0)%Z
   | 3 => (-1 * s V_TIFFYCbCrToRGBInit_z <= 0 /\ 1 * s V_TIFFYCbCrToRGBInit_z <= 0 /\ 1 * s V_TIFFYCbCrToRGBInit_i <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_i <= 0)%Z
   | 4 => (-1 * s V_TIFFYCbCrToRGBInit_i <= 0 /\ 1 * s V_TIFFYCbCrToRGBInit_i <= 0 /\ 1 * s V_TIFFYCbCrToRGBInit_z <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_z <= 0)%Z
   | 5 => (-1 * s V_TIFFYCbCrToRGBInit_z <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_i <= 0 /\ 1 * s V_TIFFYCbCrToRGBInit_i + -256 <= 0)%Z
   | 6 => (1 * s V_TIFFYCbCrToRGBInit_i + -256 <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_z <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_i + 256 <= 0)%Z
   | 7 => (-1 * s V_TIFFYCbCrToRGBInit_i + 256 <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_z <= 0 /\ 1 * s V_TIFFYCbCrToRGBInit_i + -256 <= 0)%Z
   | 8 => (1 * s V_TIFFYCbCrToRGBInit_i + -256 <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_z <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_i + 256 <= 0)%Z
   | 9 => (-1 * s V_TIFFYCbCrToRGBInit_i + 256 <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_z <= 0 /\ 1 * s V_TIFFYCbCrToRGBInit_i + -256 <= 0)%Z
   | 10 => (1 * s V_TIFFYCbCrToRGBInit_i + -256 <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_z <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_i + 256 <= 0)%Z
   | 11 => (-1 * s V_TIFFYCbCrToRGBInit_i + 256 <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_z <= 0 /\ 1 * s V_TIFFYCbCrToRGBInit_i + -256 <= 0)%Z
   | 12 => (-1 * s V_TIFFYCbCrToRGBInit_z <= 0 /\ 1 * s V_TIFFYCbCrToRGBInit_i <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_i <= 0)%Z
   | 13 => (-1 * s V_TIFFYCbCrToRGBInit_i <= 0 /\ 1 * s V_TIFFYCbCrToRGBInit_i <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_z <= 0 /\ 1 * s V_TIFFYCbCrToRGBInit_x + 128 <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_x + -128 <= 0)%Z
   | 14 => (-1 * s V_TIFFYCbCrToRGBInit_x + -128 <= 0 /\ 1 * s V_TIFFYCbCrToRGBInit_x + 128 <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_z <= 0 /\ 1 * s V_TIFFYCbCrToRGBInit_i <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_i <= 0)%Z
   | 15 => (-1 * s V_TIFFYCbCrToRGBInit_i <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_z <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_x + -128 <= 0 /\ 1 * s V_TIFFYCbCrToRGBInit_i + -256 <= 0)%Z
   | 16 => (1 * s V_TIFFYCbCrToRGBInit_i + -256 <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_x + -128 <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_z <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_i + 256 <= 0)%Z
   | 17 => (-1 * s V_TIFFYCbCrToRGBInit_i + 256 <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_z <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_x + -128 <= 0 /\ 1 * s V_TIFFYCbCrToRGBInit_i + -256 <= 0)%Z
   | 18 => (-1 * s V_TIFFYCbCrToRGBInit_x + -128 <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_z <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_i <= 0 /\ 1 * s V_TIFFYCbCrToRGBInit_i + -255 <= 0)%Z
   | 19 => (1 * s V_TIFFYCbCrToRGBInit_i + -255 <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_i <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_z <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_x + -128 <= 0)%Z
   | 20 => (-1 * s V_TIFFYCbCrToRGBInit_x + -128 <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_z <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_i <= 0 /\ 1 * s V_TIFFYCbCrToRGBInit_i + -255 <= 0)%Z
   | 21 => (-1 * s V_TIFFYCbCrToRGBInit_z <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_x + -128 <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_i + 1 <= 0 /\ 1 * s V_TIFFYCbCrToRGBInit_i + -256 <= 0)%Z
   | 22 => (1 * s V_TIFFYCbCrToRGBInit_i + -256 <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_i + 1 <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_z <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_x + -127 <= 0)%Z
   | 23 => (-1 * s V_TIFFYCbCrToRGBInit_x + -127 <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_z <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_i + 1 <= 0 /\ 1 * s V_TIFFYCbCrToRGBInit_i + -256 <= 0)%Z
   | 24 => (1 * s V_TIFFYCbCrToRGBInit_i + -256 <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_i + 1 <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_z <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_x + -127 <= 0)%Z
   | 25 => (-1 * s V_TIFFYCbCrToRGBInit_x + -127 <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_i + 1 <= 0 /\ 1 * s V_TIFFYCbCrToRGBInit_i + -256 <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_z + 1 <= 0)%Z
   | 26 => (-1 * s V_TIFFYCbCrToRGBInit_i <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_z <= 0 /\ 1 * s V_TIFFYCbCrToRGBInit_i + -255 <= 0)%Z
   | 27 => (1 * s V_TIFFYCbCrToRGBInit_i + -255 <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_z <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_i <= 0)%Z
   | 28 => (-1 * s V_TIFFYCbCrToRGBInit_i <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_z <= 0 /\ 1 * s V_TIFFYCbCrToRGBInit_i + -255 <= 0)%Z
   | 29 => (-1 * s V_TIFFYCbCrToRGBInit_z <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_i + 1 <= 0 /\ 1 * s V_TIFFYCbCrToRGBInit_i + -256 <= 0)%Z
   | 30 => (1 * s V_TIFFYCbCrToRGBInit_i + -256 <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_i + 1 <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_z <= 0)%Z
   | 31 => (-1 * s V_TIFFYCbCrToRGBInit_z <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_i + 1 <= 0 /\ 1 * s V_TIFFYCbCrToRGBInit_i + -256 <= 0)%Z
   | 32 => (1 * s V_TIFFYCbCrToRGBInit_i + -256 <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_i + 1 <= 0 /\ -1 * s V_TIFFYCbCrToRGBInit_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_TIFFYCbCrToRGBInit (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((512 # 1) <= z)%Q
   | 2 => ((512 # 1) + s V_TIFFYCbCrToRGBInit_z <= z)%Q
   | 3 => ((256 # 1) + s V_TIFFYCbCrToRGBInit_z
           + max0(256 - s V_TIFFYCbCrToRGBInit_i) <= z)%Q
   | 4 => ((256 # 1) + s V_TIFFYCbCrToRGBInit_z
           + max0(256 - s V_TIFFYCbCrToRGBInit_i) <= z)%Q
   | 5 => ((256 # 1) + s V_TIFFYCbCrToRGBInit_z
           + max0(256 - s V_TIFFYCbCrToRGBInit_i) <= z)%Q
   | 6 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (256 - s V_TIFFYCbCrToRGBInit_i) (255
                                                                    - s V_TIFFYCbCrToRGBInit_i));
      (*-1 0*) F_max0_ge_0 (255 - s V_TIFFYCbCrToRGBInit_i)]
     ((256 # 1) + s V_TIFFYCbCrToRGBInit_z
      + max0(256 - s V_TIFFYCbCrToRGBInit_i) <= z)%Q
   | 7 => ((256 # 1) + s V_TIFFYCbCrToRGBInit_z <= z)%Q
   | 8 => ((256 # 1) + s V_TIFFYCbCrToRGBInit_z <= z)%Q
   | 9 => ((256 # 1) + s V_TIFFYCbCrToRGBInit_z <= z)%Q
   | 10 => ((256 # 1) + s V_TIFFYCbCrToRGBInit_z <= z)%Q
   | 11 => ((256 # 1) + s V_TIFFYCbCrToRGBInit_z <= z)%Q
   | 12 => (s V_TIFFYCbCrToRGBInit_z + max0(256 - s V_TIFFYCbCrToRGBInit_i) <= z)%Q
   | 13 => (s V_TIFFYCbCrToRGBInit_z + max0(256 - s V_TIFFYCbCrToRGBInit_i) <= z)%Q
   | 14 => (s V_TIFFYCbCrToRGBInit_z + max0(256 - s V_TIFFYCbCrToRGBInit_i) <= z)%Q
   | 15 => (s V_TIFFYCbCrToRGBInit_z + max0(256 - s V_TIFFYCbCrToRGBInit_i) <= z)%Q
   | 16 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (256 - s V_TIFFYCbCrToRGBInit_i) (255
                                                                    - s V_TIFFYCbCrToRGBInit_i));
      (*-1 0*) F_max0_ge_0 (255 - s V_TIFFYCbCrToRGBInit_i)]
     (s V_TIFFYCbCrToRGBInit_z + max0(256 - s V_TIFFYCbCrToRGBInit_i) <= z)%Q
   | 17 => (s V_TIFFYCbCrToRGBInit_z <= z)%Q
   | 18 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (256 - s V_TIFFYCbCrToRGBInit_i) (1)]
     (s V_TIFFYCbCrToRGBInit_z + max0(256 - s V_TIFFYCbCrToRGBInit_i) <= z)%Q
   | 19 => ((1 # 1) + s V_TIFFYCbCrToRGBInit_z
            + max0(255 - s V_TIFFYCbCrToRGBInit_i) <= z)%Q
   | 20 => ((1 # 1) + s V_TIFFYCbCrToRGBInit_z
            + max0(255 - s V_TIFFYCbCrToRGBInit_i) <= z)%Q
   | 21 => ((1 # 1) + s V_TIFFYCbCrToRGBInit_z
            + max0(256 - s V_TIFFYCbCrToRGBInit_i) <= z)%Q
   | 22 => ((1 # 1) + s V_TIFFYCbCrToRGBInit_z
            + max0(256 - s V_TIFFYCbCrToRGBInit_i) <= z)%Q
   | 23 => ((1 # 1) + s V_TIFFYCbCrToRGBInit_z
            + max0(256 - s V_TIFFYCbCrToRGBInit_i) <= z)%Q
   | 24 => ((1 # 1) + s V_TIFFYCbCrToRGBInit_z
            + max0(256 - s V_TIFFYCbCrToRGBInit_i) <= z)%Q
   | 25 => (s V_TIFFYCbCrToRGBInit_z + max0(256 - s V_TIFFYCbCrToRGBInit_i) <= z)%Q
   | 26 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (256 - s V_TIFFYCbCrToRGBInit_i) (1)]
     ((256 # 1) + s V_TIFFYCbCrToRGBInit_z
      + max0(256 - s V_TIFFYCbCrToRGBInit_i) <= z)%Q
   | 27 => ((257 # 1) + s V_TIFFYCbCrToRGBInit_z
            + max0(255 - s V_TIFFYCbCrToRGBInit_i) <= z)%Q
   | 28 => ((257 # 1) + s V_TIFFYCbCrToRGBInit_z
            + max0(255 - s V_TIFFYCbCrToRGBInit_i) <= z)%Q
   | 29 => ((257 # 1) + s V_TIFFYCbCrToRGBInit_z
            + max0(256 - s V_TIFFYCbCrToRGBInit_i) <= z)%Q
   | 30 => ((257 # 1) + s V_TIFFYCbCrToRGBInit_z
            + max0(256 - s V_TIFFYCbCrToRGBInit_i) <= z)%Q
   | 31 => ((257 # 1) + s V_TIFFYCbCrToRGBInit_z
            + max0(256 - s V_TIFFYCbCrToRGBInit_i) <= z)%Q
   | 32 => ((256 # 1) + s V_TIFFYCbCrToRGBInit_z
            + max0(256 - s V_TIFFYCbCrToRGBInit_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_TIFFYCbCrToRGBInit =>
    [mkPA Q (fun n z s => ai_TIFFYCbCrToRGBInit n s /\ annot0_TIFFYCbCrToRGBInit n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_TIFFYCbCrToRGBInit (proc_start P_TIFFYCbCrToRGBInit) s1 (proc_end P_TIFFYCbCrToRGBInit) s2 ->
    (s2 V_TIFFYCbCrToRGBInit_z <= (512 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_TIFFYCbCrToRGBInit.
Qed.
