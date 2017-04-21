Require Import pasta.Pasta.

Inductive proc: Type :=
  P_mad_synth_mute.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_mad_synth_mute_z := 1%positive.
Notation V_mad_synth_mute_ch := 2%positive.
Notation V_mad_synth_mute_s := 3%positive.
Notation V_mad_synth_mute_v := 4%positive.
Notation V_mad_synth_mute_synth := 5%positive.
Definition Pedges_mad_synth_mute: list (edge proc) :=
  (EA 1 (AAssign V_mad_synth_mute_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_mad_synth_mute_v) s) >= (eval (ENum (0))
  s))%Z)) 3)::(EA 3 (AGuard (fun s => ((eval (EVar V_mad_synth_mute_s) s) >=
  (eval (ENum (0)) s))%Z)) 4)::(EA 4 (AGuard
  (fun s => ((eval (EVar V_mad_synth_mute_ch) s) >= (eval (ENum (0))
  s))%Z)) 5)::(EA 5 AWeaken 6)::(EA 6 (AAssign V_mad_synth_mute_ch
  (Some (ENum (0)))) 7)::(EA 7 ANone 8)::(EA 8 AWeaken 9)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_mad_synth_mute_ch) s) < (eval (ENum (2))
  s))%Z)) 12)::(EA 9 (AGuard (fun s => ((eval (EVar V_mad_synth_mute_ch)
  s) >= (eval (ENum (2)) s))%Z)) 10)::(EA 10 AWeaken 11)::
  (EA 12 AWeaken 13)::(EA 13 (AAssign V_mad_synth_mute_s
  (Some (ENum (0)))) 14)::(EA 14 ANone 15)::(EA 15 AWeaken 16)::
  (EA 16 (AGuard (fun s => ((eval (EVar V_mad_synth_mute_s) s) <
  (eval (ENum (16)) s))%Z)) 24)::(EA 16 (AGuard
  (fun s => ((eval (EVar V_mad_synth_mute_s) s) >= (eval (ENum (16))
  s))%Z)) 17)::(EA 17 AWeaken 18)::(EA 18 ANone 19)::(EA 19 (AAssign
  V_mad_synth_mute_ch (Some (EAdd (EVar V_mad_synth_mute_ch)
  (ENum (1))))) 20)::(EA 20 ANone 21)::(EA 21 ANone 22)::(EA 22 (AAssign
  V_mad_synth_mute_z (Some (EAdd (ENum (1))
  (EVar V_mad_synth_mute_z)))) 23)::(EA 23 AWeaken 9)::(EA 24 AWeaken 25)::
  (EA 25 (AAssign V_mad_synth_mute_v (Some (ENum (0)))) 26)::
  (EA 26 ANone 27)::(EA 27 AWeaken 28)::(EA 28 (AGuard
  (fun s => ((eval (EVar V_mad_synth_mute_v) s) < (eval (ENum (8))
  s))%Z)) 36)::(EA 28 (AGuard (fun s => ((eval (EVar V_mad_synth_mute_v)
  s) >= (eval (ENum (8)) s))%Z)) 29)::(EA 29 AWeaken 30)::(EA 30 ANone 31)::
  (EA 31 (AAssign V_mad_synth_mute_s (Some (EAdd (EVar V_mad_synth_mute_s)
  (ENum (1))))) 32)::(EA 32 ANone 33)::(EA 33 ANone 34)::(EA 34 (AAssign
  V_mad_synth_mute_z (Some (EAdd (ENum (1))
  (EVar V_mad_synth_mute_z)))) 35)::(EA 35 AWeaken 16)::(EA 36 AWeaken 37)::
  (EA 37 ANone 38)::(EA 38 (AAssign V_mad_synth_mute_v
  (Some (EAdd (EVar V_mad_synth_mute_v) (ENum (1))))) 39)::(EA 39 ANone 40)::
  (EA 40 ANone 41)::(EA 41 (AAssign V_mad_synth_mute_z (Some (EAdd (ENum (1))
  (EVar V_mad_synth_mute_z)))) 42)::(EA 42 AWeaken 28)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_mad_synth_mute => Pedges_mad_synth_mute
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_mad_synth_mute => 11
     end)%positive;
  var_global := var_global
}.

Definition ai_mad_synth_mute (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_mad_synth_mute_z <= 0 /\ -1 * s V_mad_synth_mute_z <= 0)%Z
   | 3 => (-1 * s V_mad_synth_mute_z <= 0 /\ 1 * s V_mad_synth_mute_z <= 0 /\ -1 * s V_mad_synth_mute_v <= 0)%Z
   | 4 => (-1 * s V_mad_synth_mute_v <= 0 /\ 1 * s V_mad_synth_mute_z <= 0 /\ -1 * s V_mad_synth_mute_z <= 0 /\ -1 * s V_mad_synth_mute_s <= 0)%Z
   | 5 => (-1 * s V_mad_synth_mute_s <= 0 /\ -1 * s V_mad_synth_mute_z <= 0 /\ 1 * s V_mad_synth_mute_z <= 0 /\ -1 * s V_mad_synth_mute_v <= 0 /\ -1 * s V_mad_synth_mute_ch <= 0)%Z
   | 6 => (-1 * s V_mad_synth_mute_ch <= 0 /\ -1 * s V_mad_synth_mute_v <= 0 /\ 1 * s V_mad_synth_mute_z <= 0 /\ -1 * s V_mad_synth_mute_z <= 0 /\ -1 * s V_mad_synth_mute_s <= 0)%Z
   | 7 => (-1 * s V_mad_synth_mute_s <= 0 /\ -1 * s V_mad_synth_mute_z <= 0 /\ 1 * s V_mad_synth_mute_z <= 0 /\ -1 * s V_mad_synth_mute_v <= 0 /\ 1 * s V_mad_synth_mute_ch <= 0 /\ -1 * s V_mad_synth_mute_ch <= 0)%Z
   | 8 => (-1 * s V_mad_synth_mute_ch <= 0 /\ 1 * s V_mad_synth_mute_ch <= 0 /\ -1 * s V_mad_synth_mute_v <= 0 /\ 1 * s V_mad_synth_mute_z <= 0 /\ -1 * s V_mad_synth_mute_z <= 0 /\ -1 * s V_mad_synth_mute_s <= 0)%Z
   | 9 => (-1 * s V_mad_synth_mute_s <= 0 /\ -1 * s V_mad_synth_mute_z <= 0 /\ -1 * s V_mad_synth_mute_ch <= 0 /\ -1 * s V_mad_synth_mute_v <= 0)%Z
   | 10 => (-1 * s V_mad_synth_mute_v <= 0 /\ -1 * s V_mad_synth_mute_z <= 0 /\ -1 * s V_mad_synth_mute_s <= 0 /\ -1 * s V_mad_synth_mute_ch + 2 <= 0)%Z
   | 11 => (-1 * s V_mad_synth_mute_ch + 2 <= 0 /\ -1 * s V_mad_synth_mute_s <= 0 /\ -1 * s V_mad_synth_mute_z <= 0 /\ -1 * s V_mad_synth_mute_v <= 0)%Z
   | 12 => (-1 * s V_mad_synth_mute_v <= 0 /\ -1 * s V_mad_synth_mute_ch <= 0 /\ -1 * s V_mad_synth_mute_z <= 0 /\ -1 * s V_mad_synth_mute_s <= 0 /\ 1 * s V_mad_synth_mute_ch + -1 <= 0)%Z
   | 13 => (1 * s V_mad_synth_mute_ch + -1 <= 0 /\ -1 * s V_mad_synth_mute_s <= 0 /\ -1 * s V_mad_synth_mute_z <= 0 /\ -1 * s V_mad_synth_mute_ch <= 0 /\ -1 * s V_mad_synth_mute_v <= 0)%Z
   | 14 => (-1 * s V_mad_synth_mute_v <= 0 /\ -1 * s V_mad_synth_mute_ch <= 0 /\ -1 * s V_mad_synth_mute_z <= 0 /\ 1 * s V_mad_synth_mute_ch + -1 <= 0 /\ 1 * s V_mad_synth_mute_s <= 0 /\ -1 * s V_mad_synth_mute_s <= 0)%Z
   | 15 => (-1 * s V_mad_synth_mute_s <= 0 /\ 1 * s V_mad_synth_mute_s <= 0 /\ 1 * s V_mad_synth_mute_ch + -1 <= 0 /\ -1 * s V_mad_synth_mute_z <= 0 /\ -1 * s V_mad_synth_mute_ch <= 0 /\ -1 * s V_mad_synth_mute_v <= 0)%Z
   | 16 => (-1 * s V_mad_synth_mute_v <= 0 /\ -1 * s V_mad_synth_mute_z <= 0 /\ -1 * s V_mad_synth_mute_s <= 0 /\ -1 * s V_mad_synth_mute_ch <= 0)%Z
   | 17 => (-1 * s V_mad_synth_mute_ch <= 0 /\ -1 * s V_mad_synth_mute_z <= 0 /\ -1 * s V_mad_synth_mute_v <= 0 /\ -1 * s V_mad_synth_mute_s + 16 <= 0)%Z
   | 18 => (-1 * s V_mad_synth_mute_s + 16 <= 0 /\ -1 * s V_mad_synth_mute_v <= 0 /\ -1 * s V_mad_synth_mute_z <= 0 /\ -1 * s V_mad_synth_mute_ch <= 0)%Z
   | 19 => (-1 * s V_mad_synth_mute_ch <= 0 /\ -1 * s V_mad_synth_mute_z <= 0 /\ -1 * s V_mad_synth_mute_v <= 0 /\ -1 * s V_mad_synth_mute_s + 16 <= 0)%Z
   | 20 => (-1 * s V_mad_synth_mute_s + 16 <= 0 /\ -1 * s V_mad_synth_mute_v <= 0 /\ -1 * s V_mad_synth_mute_z <= 0 /\ -1 * s V_mad_synth_mute_ch + 1 <= 0)%Z
   | 21 => (-1 * s V_mad_synth_mute_ch + 1 <= 0 /\ -1 * s V_mad_synth_mute_z <= 0 /\ -1 * s V_mad_synth_mute_v <= 0 /\ -1 * s V_mad_synth_mute_s + 16 <= 0)%Z
   | 22 => (-1 * s V_mad_synth_mute_s + 16 <= 0 /\ -1 * s V_mad_synth_mute_v <= 0 /\ -1 * s V_mad_synth_mute_z <= 0 /\ -1 * s V_mad_synth_mute_ch + 1 <= 0)%Z
   | 23 => (-1 * s V_mad_synth_mute_ch + 1 <= 0 /\ -1 * s V_mad_synth_mute_v <= 0 /\ -1 * s V_mad_synth_mute_s + 16 <= 0 /\ -1 * s V_mad_synth_mute_z + 1 <= 0)%Z
   | 24 => (-1 * s V_mad_synth_mute_ch <= 0 /\ -1 * s V_mad_synth_mute_s <= 0 /\ -1 * s V_mad_synth_mute_z <= 0 /\ -1 * s V_mad_synth_mute_v <= 0 /\ 1 * s V_mad_synth_mute_s + -15 <= 0)%Z
   | 25 => (1 * s V_mad_synth_mute_s + -15 <= 0 /\ -1 * s V_mad_synth_mute_v <= 0 /\ -1 * s V_mad_synth_mute_z <= 0 /\ -1 * s V_mad_synth_mute_s <= 0 /\ -1 * s V_mad_synth_mute_ch <= 0)%Z
   | 26 => (-1 * s V_mad_synth_mute_ch <= 0 /\ -1 * s V_mad_synth_mute_s <= 0 /\ -1 * s V_mad_synth_mute_z <= 0 /\ 1 * s V_mad_synth_mute_s + -15 <= 0 /\ 1 * s V_mad_synth_mute_v <= 0 /\ -1 * s V_mad_synth_mute_v <= 0)%Z
   | 27 => (-1 * s V_mad_synth_mute_v <= 0 /\ 1 * s V_mad_synth_mute_v <= 0 /\ 1 * s V_mad_synth_mute_s + -15 <= 0 /\ -1 * s V_mad_synth_mute_z <= 0 /\ -1 * s V_mad_synth_mute_s <= 0 /\ -1 * s V_mad_synth_mute_ch <= 0)%Z
   | 28 => (-1 * s V_mad_synth_mute_z <= 0 /\ -1 * s V_mad_synth_mute_v <= 0 /\ -1 * s V_mad_synth_mute_s <= 0 /\ -1 * s V_mad_synth_mute_ch <= 0 /\ 1 * s V_mad_synth_mute_v + -8 <= 0)%Z
   | 29 => (1 * s V_mad_synth_mute_v + -8 <= 0 /\ -1 * s V_mad_synth_mute_ch <= 0 /\ -1 * s V_mad_synth_mute_s <= 0 /\ -1 * s V_mad_synth_mute_z <= 0 /\ -1 * s V_mad_synth_mute_v + 8 <= 0)%Z
   | 30 => (-1 * s V_mad_synth_mute_v + 8 <= 0 /\ -1 * s V_mad_synth_mute_z <= 0 /\ -1 * s V_mad_synth_mute_s <= 0 /\ -1 * s V_mad_synth_mute_ch <= 0 /\ 1 * s V_mad_synth_mute_v + -8 <= 0)%Z
   | 31 => (1 * s V_mad_synth_mute_v + -8 <= 0 /\ -1 * s V_mad_synth_mute_ch <= 0 /\ -1 * s V_mad_synth_mute_s <= 0 /\ -1 * s V_mad_synth_mute_z <= 0 /\ -1 * s V_mad_synth_mute_v + 8 <= 0)%Z
   | 32 => (-1 * s V_mad_synth_mute_v + 8 <= 0 /\ -1 * s V_mad_synth_mute_z <= 0 /\ -1 * s V_mad_synth_mute_ch <= 0 /\ 1 * s V_mad_synth_mute_v + -8 <= 0 /\ -1 * s V_mad_synth_mute_s + 1 <= 0)%Z
   | 33 => (-1 * s V_mad_synth_mute_s + 1 <= 0 /\ 1 * s V_mad_synth_mute_v + -8 <= 0 /\ -1 * s V_mad_synth_mute_ch <= 0 /\ -1 * s V_mad_synth_mute_z <= 0 /\ -1 * s V_mad_synth_mute_v + 8 <= 0)%Z
   | 34 => (-1 * s V_mad_synth_mute_v + 8 <= 0 /\ -1 * s V_mad_synth_mute_z <= 0 /\ -1 * s V_mad_synth_mute_ch <= 0 /\ 1 * s V_mad_synth_mute_v + -8 <= 0 /\ -1 * s V_mad_synth_mute_s + 1 <= 0)%Z
   | 35 => (-1 * s V_mad_synth_mute_s + 1 <= 0 /\ 1 * s V_mad_synth_mute_v + -8 <= 0 /\ -1 * s V_mad_synth_mute_ch <= 0 /\ -1 * s V_mad_synth_mute_v + 8 <= 0 /\ -1 * s V_mad_synth_mute_z + 1 <= 0)%Z
   | 36 => (-1 * s V_mad_synth_mute_ch <= 0 /\ -1 * s V_mad_synth_mute_s <= 0 /\ -1 * s V_mad_synth_mute_v <= 0 /\ -1 * s V_mad_synth_mute_z <= 0 /\ 1 * s V_mad_synth_mute_v + -7 <= 0)%Z
   | 37 => (1 * s V_mad_synth_mute_v + -7 <= 0 /\ -1 * s V_mad_synth_mute_z <= 0 /\ -1 * s V_mad_synth_mute_v <= 0 /\ -1 * s V_mad_synth_mute_s <= 0 /\ -1 * s V_mad_synth_mute_ch <= 0)%Z
   | 38 => (-1 * s V_mad_synth_mute_ch <= 0 /\ -1 * s V_mad_synth_mute_s <= 0 /\ -1 * s V_mad_synth_mute_v <= 0 /\ -1 * s V_mad_synth_mute_z <= 0 /\ 1 * s V_mad_synth_mute_v + -7 <= 0)%Z
   | 39 => (-1 * s V_mad_synth_mute_z <= 0 /\ -1 * s V_mad_synth_mute_s <= 0 /\ -1 * s V_mad_synth_mute_ch <= 0 /\ -1 * s V_mad_synth_mute_v + 1 <= 0 /\ 1 * s V_mad_synth_mute_v + -8 <= 0)%Z
   | 40 => (1 * s V_mad_synth_mute_v + -8 <= 0 /\ -1 * s V_mad_synth_mute_v + 1 <= 0 /\ -1 * s V_mad_synth_mute_ch <= 0 /\ -1 * s V_mad_synth_mute_s <= 0 /\ -1 * s V_mad_synth_mute_z <= 0)%Z
   | 41 => (-1 * s V_mad_synth_mute_z <= 0 /\ -1 * s V_mad_synth_mute_s <= 0 /\ -1 * s V_mad_synth_mute_ch <= 0 /\ -1 * s V_mad_synth_mute_v + 1 <= 0 /\ 1 * s V_mad_synth_mute_v + -8 <= 0)%Z
   | 42 => (1 * s V_mad_synth_mute_v + -8 <= 0 /\ -1 * s V_mad_synth_mute_v + 1 <= 0 /\ -1 * s V_mad_synth_mute_ch <= 0 /\ -1 * s V_mad_synth_mute_s <= 0 /\ -1 * s V_mad_synth_mute_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_mad_synth_mute (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((290 # 1) <= z)%Q
   | 2 => ((290 # 1) + s V_mad_synth_mute_z <= z)%Q
   | 3 => ((290 # 1) + s V_mad_synth_mute_z <= z)%Q
   | 4 => ((290 # 1) + s V_mad_synth_mute_z <= z)%Q
   | 5 => ((290 # 1) + s V_mad_synth_mute_z <= z)%Q
   | 6 => ((290 # 1) + s V_mad_synth_mute_z <= z)%Q
   | 7 => (s V_mad_synth_mute_z + (145 # 1) * max0(2 - s V_mad_synth_mute_ch) <= z)%Q
   | 8 => (s V_mad_synth_mute_z + (145 # 1) * max0(2 - s V_mad_synth_mute_ch) <= z)%Q
   | 9 => (s V_mad_synth_mute_z + (145 # 1) * max0(2 - s V_mad_synth_mute_ch) <= z)%Q
   | 10 => hints
     [(*-145 0*) F_max0_monotonic (F_check_ge (2 - s V_mad_synth_mute_ch) (1
                                                                    - s V_mad_synth_mute_ch));
      (*-145 0*) F_max0_ge_0 (1 - s V_mad_synth_mute_ch)]
     (s V_mad_synth_mute_z + (145 # 1) * max0(2 - s V_mad_synth_mute_ch) <= z)%Q
   | 11 => (s V_mad_synth_mute_z <= z)%Q
   | 12 => (s V_mad_synth_mute_z
            + (145 # 1) * max0(2 - s V_mad_synth_mute_ch) <= z)%Q
   | 13 => (s V_mad_synth_mute_z
            + (145 # 1) * max0(2 - s V_mad_synth_mute_ch) <= z)%Q
   | 14 => (-(144 # 1) + s V_mad_synth_mute_z
            + (145 # 1) * max0(2 - s V_mad_synth_mute_ch)
            + (9 # 1) * max0(16 - s V_mad_synth_mute_s) <= z)%Q
   | 15 => hints
     [(*-145 0*) F_max0_pre_decrement 1 (2 - s V_mad_synth_mute_ch) (1)]
     (-(144 # 1) + s V_mad_synth_mute_z
      + (145 # 1) * max0(2 - s V_mad_synth_mute_ch)
      + (9 # 1) * max0(16 - s V_mad_synth_mute_s) <= z)%Q
   | 16 => ((1 # 1) + s V_mad_synth_mute_z
            + (145 # 1) * max0(1 - s V_mad_synth_mute_ch)
            + (9 # 1) * max0(16 - s V_mad_synth_mute_s) <= z)%Q
   | 17 => ((1 # 1) + s V_mad_synth_mute_z
            + (145 # 1) * max0(1 - s V_mad_synth_mute_ch)
            + (9 # 1) * max0(16 - s V_mad_synth_mute_s) <= z)%Q
   | 18 => ((1 # 1) + s V_mad_synth_mute_z
            + (145 # 1) * max0(1 - s V_mad_synth_mute_ch)
            + (9 # 1) * max0(16 - s V_mad_synth_mute_s) <= z)%Q
   | 19 => ((1 # 1) + s V_mad_synth_mute_z
            + (145 # 1) * max0(1 - s V_mad_synth_mute_ch)
            + (9 # 1) * max0(16 - s V_mad_synth_mute_s) <= z)%Q
   | 20 => ((1 # 1) + s V_mad_synth_mute_z
            + (145 # 1) * max0(2 - s V_mad_synth_mute_ch)
            + (9 # 1) * max0(16 - s V_mad_synth_mute_s) <= z)%Q
   | 21 => ((1 # 1) + s V_mad_synth_mute_z
            + (145 # 1) * max0(2 - s V_mad_synth_mute_ch)
            + (9 # 1) * max0(16 - s V_mad_synth_mute_s) <= z)%Q
   | 22 => ((1 # 1) + s V_mad_synth_mute_z
            + (145 # 1) * max0(2 - s V_mad_synth_mute_ch)
            + (9 # 1) * max0(16 - s V_mad_synth_mute_s) <= z)%Q
   | 23 => hints
     [(*-9 0*) F_max0_ge_0 (16 - s V_mad_synth_mute_s)]
     (s V_mad_synth_mute_z + (145 # 1) * max0(2 - s V_mad_synth_mute_ch)
      + (9 # 1) * max0(16 - s V_mad_synth_mute_s) <= z)%Q
   | 24 => ((1 # 1) + s V_mad_synth_mute_z
            + (145 # 1) * max0(1 - s V_mad_synth_mute_ch)
            + (9 # 1) * max0(16 - s V_mad_synth_mute_s) <= z)%Q
   | 25 => ((1 # 1) + s V_mad_synth_mute_z
            + (145 # 1) * max0(1 - s V_mad_synth_mute_ch)
            + (9 # 1) * max0(16 - s V_mad_synth_mute_s) <= z)%Q
   | 26 => (-(7 # 1) + s V_mad_synth_mute_z
            + (145 # 1) * max0(1 - s V_mad_synth_mute_ch)
            + max0(8 - s V_mad_synth_mute_v)
            + (9 # 1) * max0(16 - s V_mad_synth_mute_s) <= z)%Q
   | 27 => hints
     [(*-9 0*) F_max0_pre_decrement 1 (16 - s V_mad_synth_mute_s) (1)]
     (-(7 # 1) + s V_mad_synth_mute_z
      + (145 # 1) * max0(1 - s V_mad_synth_mute_ch)
      + max0(8 - s V_mad_synth_mute_v)
      + (9 # 1) * max0(16 - s V_mad_synth_mute_s) <= z)%Q
   | 28 => ((2 # 1) + s V_mad_synth_mute_z
            + (145 # 1) * max0(1 - s V_mad_synth_mute_ch)
            + max0(8 - s V_mad_synth_mute_v)
            + (9 # 1) * max0(15 - s V_mad_synth_mute_s) <= z)%Q
   | 29 => ((2 # 1) + s V_mad_synth_mute_z
            + (145 # 1) * max0(1 - s V_mad_synth_mute_ch)
            + max0(8 - s V_mad_synth_mute_v)
            + (9 # 1) * max0(15 - s V_mad_synth_mute_s) <= z)%Q
   | 30 => ((2 # 1) + s V_mad_synth_mute_z
            + (145 # 1) * max0(1 - s V_mad_synth_mute_ch)
            + max0(8 - s V_mad_synth_mute_v)
            + (9 # 1) * max0(15 - s V_mad_synth_mute_s) <= z)%Q
   | 31 => ((2 # 1) + s V_mad_synth_mute_z
            + (145 # 1) * max0(1 - s V_mad_synth_mute_ch)
            + max0(8 - s V_mad_synth_mute_v)
            + (9 # 1) * max0(15 - s V_mad_synth_mute_s) <= z)%Q
   | 32 => ((2 # 1) + s V_mad_synth_mute_z
            + (145 # 1) * max0(1 - s V_mad_synth_mute_ch)
            + max0(8 - s V_mad_synth_mute_v)
            + (9 # 1) * max0(16 - s V_mad_synth_mute_s) <= z)%Q
   | 33 => ((2 # 1) + s V_mad_synth_mute_z
            + (145 # 1) * max0(1 - s V_mad_synth_mute_ch)
            + max0(8 - s V_mad_synth_mute_v)
            + (9 # 1) * max0(16 - s V_mad_synth_mute_s) <= z)%Q
   | 34 => ((2 # 1) + s V_mad_synth_mute_z
            + (145 # 1) * max0(1 - s V_mad_synth_mute_ch)
            + max0(8 - s V_mad_synth_mute_v)
            + (9 # 1) * max0(16 - s V_mad_synth_mute_s) <= z)%Q
   | 35 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (8 - s V_mad_synth_mute_v)) (F_check_ge (0) (0))]
     ((1 # 1) + s V_mad_synth_mute_z
      + (145 # 1) * max0(1 - s V_mad_synth_mute_ch)
      + max0(8 - s V_mad_synth_mute_v)
      + (9 # 1) * max0(16 - s V_mad_synth_mute_s) <= z)%Q
   | 36 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (8 - s V_mad_synth_mute_v) (1)]
     ((2 # 1) + s V_mad_synth_mute_z
      + (145 # 1) * max0(1 - s V_mad_synth_mute_ch)
      + max0(8 - s V_mad_synth_mute_v)
      + (9 # 1) * max0(15 - s V_mad_synth_mute_s) <= z)%Q
   | 37 => ((3 # 1) + s V_mad_synth_mute_z
            + (145 # 1) * max0(1 - s V_mad_synth_mute_ch)
            + max0(7 - s V_mad_synth_mute_v)
            + (9 # 1) * max0(15 - s V_mad_synth_mute_s) <= z)%Q
   | 38 => ((3 # 1) + s V_mad_synth_mute_z
            + (145 # 1) * max0(1 - s V_mad_synth_mute_ch)
            + max0(7 - s V_mad_synth_mute_v)
            + (9 # 1) * max0(15 - s V_mad_synth_mute_s) <= z)%Q
   | 39 => ((3 # 1) + s V_mad_synth_mute_z
            + (145 # 1) * max0(1 - s V_mad_synth_mute_ch)
            + max0(8 - s V_mad_synth_mute_v)
            + (9 # 1) * max0(15 - s V_mad_synth_mute_s) <= z)%Q
   | 40 => ((3 # 1) + s V_mad_synth_mute_z
            + (145 # 1) * max0(1 - s V_mad_synth_mute_ch)
            + max0(8 - s V_mad_synth_mute_v)
            + (9 # 1) * max0(15 - s V_mad_synth_mute_s) <= z)%Q
   | 41 => ((3 # 1) + s V_mad_synth_mute_z
            + (145 # 1) * max0(1 - s V_mad_synth_mute_ch)
            + max0(8 - s V_mad_synth_mute_v)
            + (9 # 1) * max0(15 - s V_mad_synth_mute_s) <= z)%Q
   | 42 => ((2 # 1) + s V_mad_synth_mute_z
            + (145 # 1) * max0(1 - s V_mad_synth_mute_ch)
            + max0(8 - s V_mad_synth_mute_v)
            + (9 # 1) * max0(15 - s V_mad_synth_mute_s) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_mad_synth_mute =>
    [mkPA Q (fun n z s => ai_mad_synth_mute n s /\ annot0_mad_synth_mute n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_mad_synth_mute (proc_start P_mad_synth_mute) s1 (proc_end P_mad_synth_mute) s2 ->
    (s2 V_mad_synth_mute_z <= (290 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_mad_synth_mute.
Qed.
