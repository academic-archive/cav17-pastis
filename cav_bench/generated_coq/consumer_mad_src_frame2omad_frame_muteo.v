Require Import pasta.Pasta.

Inductive proc: Type :=
  P_mad_frame_mute.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_mad_frame_mute_z := 1%positive.
Notation V_mad_frame_mute_s := 2%positive.
Notation V_mad_frame_mute_sb := 3%positive.
Notation V_mad_frame_mute_frame := 4%positive.
Definition Pedges_mad_frame_mute: list (edge proc) :=
  (EA 1 (AAssign V_mad_frame_mute_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_mad_frame_mute_sb) s) >= (eval (ENum (0))
  s))%Z)) 3)::(EA 3 (AGuard (fun s => ((eval (EVar V_mad_frame_mute_s) s) >=
  (eval (ENum (0)) s))%Z)) 4)::(EA 4 AWeaken 5)::(EA 5 (AAssign
  V_mad_frame_mute_s (Some (ENum (0)))) 6)::(EA 6 ANone 7)::
  (EA 7 AWeaken 8)::(EA 8 (AGuard (fun s => ((eval (EVar V_mad_frame_mute_s)
  s) < (eval (ENum (36)) s))%Z)) 39)::(EA 8 (AGuard
  (fun s => ((eval (EVar V_mad_frame_mute_s) s) >= (eval (ENum (36))
  s))%Z)) 9)::(EA 9 AWeaken 10)::(EA 10 ANone 12)::(EA 10 ANone 11)::
  (EA 11 AWeaken 19)::(EA 12 (AAssign V_mad_frame_mute_s
  (Some (ENum (0)))) 13)::(EA 13 ANone 14)::(EA 14 AWeaken 15)::
  (EA 15 (AGuard (fun s => ((eval (EVar V_mad_frame_mute_s) s) <
  (eval (ENum (18)) s))%Z)) 20)::(EA 15 (AGuard
  (fun s => ((eval (EVar V_mad_frame_mute_s) s) >= (eval (ENum (18))
  s))%Z)) 16)::(EA 16 AWeaken 17)::(EA 17 ANone 18)::(EA 18 AWeaken 19)::
  (EA 20 AWeaken 21)::(EA 21 (AAssign V_mad_frame_mute_sb
  (Some (ENum (0)))) 22)::(EA 22 ANone 23)::(EA 23 AWeaken 24)::
  (EA 24 (AGuard (fun s => ((eval (EVar V_mad_frame_mute_sb) s) <
  (eval (ENum (32)) s))%Z)) 32)::(EA 24 (AGuard
  (fun s => ((eval (EVar V_mad_frame_mute_sb) s) >= (eval (ENum (32))
  s))%Z)) 25)::(EA 25 AWeaken 26)::(EA 26 ANone 27)::(EA 27 (AAssign
  V_mad_frame_mute_s (Some (EAdd (EVar V_mad_frame_mute_s)
  (ENum (1))))) 28)::(EA 28 ANone 29)::(EA 29 ANone 30)::(EA 30 (AAssign
  V_mad_frame_mute_z (Some (EAdd (ENum (1))
  (EVar V_mad_frame_mute_z)))) 31)::(EA 31 AWeaken 15)::(EA 32 AWeaken 33)::
  (EA 33 ANone 34)::(EA 34 (AAssign V_mad_frame_mute_sb
  (Some (EAdd (EVar V_mad_frame_mute_sb) (ENum (1))))) 35)::
  (EA 35 ANone 36)::(EA 36 ANone 37)::(EA 37 (AAssign V_mad_frame_mute_z
  (Some (EAdd (ENum (1)) (EVar V_mad_frame_mute_z)))) 38)::
  (EA 38 AWeaken 24)::(EA 39 AWeaken 40)::(EA 40 (AAssign V_mad_frame_mute_sb
  (Some (ENum (0)))) 41)::(EA 41 ANone 42)::(EA 42 AWeaken 43)::
  (EA 43 (AGuard (fun s => ((eval (EVar V_mad_frame_mute_sb) s) <
  (eval (ENum (32)) s))%Z)) 51)::(EA 43 (AGuard
  (fun s => ((eval (EVar V_mad_frame_mute_sb) s) >= (eval (ENum (32))
  s))%Z)) 44)::(EA 44 AWeaken 45)::(EA 45 ANone 46)::(EA 46 (AAssign
  V_mad_frame_mute_s (Some (EAdd (EVar V_mad_frame_mute_s)
  (ENum (1))))) 47)::(EA 47 ANone 48)::(EA 48 ANone 49)::(EA 49 (AAssign
  V_mad_frame_mute_z (Some (EAdd (ENum (1))
  (EVar V_mad_frame_mute_z)))) 50)::(EA 50 AWeaken 8)::(EA 51 AWeaken 52)::
  (EA 52 ANone 53)::(EA 53 (AAssign V_mad_frame_mute_sb
  (Some (EAdd (EVar V_mad_frame_mute_sb) (ENum (1))))) 54)::
  (EA 54 ANone 55)::(EA 55 ANone 56)::(EA 56 (AAssign V_mad_frame_mute_z
  (Some (EAdd (ENum (1)) (EVar V_mad_frame_mute_z)))) 57)::
  (EA 57 AWeaken 43)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_mad_frame_mute => Pedges_mad_frame_mute
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_mad_frame_mute => 19
     end)%positive;
  var_global := var_global
}.

Definition ai_mad_frame_mute (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_mad_frame_mute_z <= 0 /\ -1 * s V_mad_frame_mute_z <= 0)%Z
   | 3 => (-1 * s V_mad_frame_mute_z <= 0 /\ 1 * s V_mad_frame_mute_z <= 0 /\ -1 * s V_mad_frame_mute_sb <= 0)%Z
   | 4 => (-1 * s V_mad_frame_mute_sb <= 0 /\ 1 * s V_mad_frame_mute_z <= 0 /\ -1 * s V_mad_frame_mute_z <= 0 /\ -1 * s V_mad_frame_mute_s <= 0)%Z
   | 5 => (-1 * s V_mad_frame_mute_s <= 0 /\ -1 * s V_mad_frame_mute_z <= 0 /\ 1 * s V_mad_frame_mute_z <= 0 /\ -1 * s V_mad_frame_mute_sb <= 0)%Z
   | 6 => (-1 * s V_mad_frame_mute_sb <= 0 /\ 1 * s V_mad_frame_mute_z <= 0 /\ -1 * s V_mad_frame_mute_z <= 0 /\ 1 * s V_mad_frame_mute_s <= 0 /\ -1 * s V_mad_frame_mute_s <= 0)%Z
   | 7 => (-1 * s V_mad_frame_mute_s <= 0 /\ 1 * s V_mad_frame_mute_s <= 0 /\ -1 * s V_mad_frame_mute_z <= 0 /\ 1 * s V_mad_frame_mute_z <= 0 /\ -1 * s V_mad_frame_mute_sb <= 0)%Z
   | 8 => (-1 * s V_mad_frame_mute_sb <= 0 /\ -1 * s V_mad_frame_mute_z <= 0 /\ -1 * s V_mad_frame_mute_s <= 0)%Z
   | 9 => (-1 * s V_mad_frame_mute_z <= 0 /\ -1 * s V_mad_frame_mute_sb <= 0 /\ -1 * s V_mad_frame_mute_s + 36 <= 0)%Z
   | 10 => (-1 * s V_mad_frame_mute_s + 36 <= 0 /\ -1 * s V_mad_frame_mute_sb <= 0 /\ -1 * s V_mad_frame_mute_z <= 0)%Z
   | 11 => (-1 * s V_mad_frame_mute_z <= 0 /\ -1 * s V_mad_frame_mute_sb <= 0 /\ -1 * s V_mad_frame_mute_s + 36 <= 0)%Z
   | 12 => (-1 * s V_mad_frame_mute_z <= 0 /\ -1 * s V_mad_frame_mute_sb <= 0 /\ -1 * s V_mad_frame_mute_s + 36 <= 0)%Z
   | 13 => (-1 * s V_mad_frame_mute_sb <= 0 /\ -1 * s V_mad_frame_mute_z <= 0 /\ 1 * s V_mad_frame_mute_s <= 0 /\ -1 * s V_mad_frame_mute_s <= 0)%Z
   | 14 => (-1 * s V_mad_frame_mute_s <= 0 /\ 1 * s V_mad_frame_mute_s <= 0 /\ -1 * s V_mad_frame_mute_z <= 0 /\ -1 * s V_mad_frame_mute_sb <= 0)%Z
   | 15 => (-1 * s V_mad_frame_mute_sb <= 0 /\ -1 * s V_mad_frame_mute_z <= 0 /\ -1 * s V_mad_frame_mute_s <= 0)%Z
   | 16 => (-1 * s V_mad_frame_mute_z <= 0 /\ -1 * s V_mad_frame_mute_sb <= 0 /\ -1 * s V_mad_frame_mute_s + 18 <= 0)%Z
   | 17 => (-1 * s V_mad_frame_mute_s + 18 <= 0 /\ -1 * s V_mad_frame_mute_sb <= 0 /\ -1 * s V_mad_frame_mute_z <= 0)%Z
   | 18 => (-1 * s V_mad_frame_mute_z <= 0 /\ -1 * s V_mad_frame_mute_sb <= 0 /\ -1 * s V_mad_frame_mute_s + 18 <= 0)%Z
   | 19 => (-1 * s V_mad_frame_mute_s + 18 <= 0 /\ -1 * s V_mad_frame_mute_sb <= 0 /\ -1 * s V_mad_frame_mute_z <= 0)%Z
   | 20 => (-1 * s V_mad_frame_mute_s <= 0 /\ -1 * s V_mad_frame_mute_z <= 0 /\ -1 * s V_mad_frame_mute_sb <= 0 /\ 1 * s V_mad_frame_mute_s + -17 <= 0)%Z
   | 21 => (1 * s V_mad_frame_mute_s + -17 <= 0 /\ -1 * s V_mad_frame_mute_sb <= 0 /\ -1 * s V_mad_frame_mute_z <= 0 /\ -1 * s V_mad_frame_mute_s <= 0)%Z
   | 22 => (-1 * s V_mad_frame_mute_s <= 0 /\ -1 * s V_mad_frame_mute_z <= 0 /\ 1 * s V_mad_frame_mute_s + -17 <= 0 /\ 1 * s V_mad_frame_mute_sb <= 0 /\ -1 * s V_mad_frame_mute_sb <= 0)%Z
   | 23 => (-1 * s V_mad_frame_mute_sb <= 0 /\ 1 * s V_mad_frame_mute_sb <= 0 /\ 1 * s V_mad_frame_mute_s + -17 <= 0 /\ -1 * s V_mad_frame_mute_z <= 0 /\ -1 * s V_mad_frame_mute_s <= 0)%Z
   | 24 => (-1 * s V_mad_frame_mute_z <= 0 /\ -1 * s V_mad_frame_mute_sb <= 0 /\ -1 * s V_mad_frame_mute_s <= 0 /\ 1 * s V_mad_frame_mute_sb + -32 <= 0)%Z
   | 25 => (1 * s V_mad_frame_mute_sb + -32 <= 0 /\ -1 * s V_mad_frame_mute_s <= 0 /\ -1 * s V_mad_frame_mute_z <= 0 /\ -1 * s V_mad_frame_mute_sb + 32 <= 0)%Z
   | 26 => (-1 * s V_mad_frame_mute_sb + 32 <= 0 /\ -1 * s V_mad_frame_mute_z <= 0 /\ -1 * s V_mad_frame_mute_s <= 0 /\ 1 * s V_mad_frame_mute_sb + -32 <= 0)%Z
   | 27 => (1 * s V_mad_frame_mute_sb + -32 <= 0 /\ -1 * s V_mad_frame_mute_s <= 0 /\ -1 * s V_mad_frame_mute_z <= 0 /\ -1 * s V_mad_frame_mute_sb + 32 <= 0)%Z
   | 28 => (-1 * s V_mad_frame_mute_sb + 32 <= 0 /\ -1 * s V_mad_frame_mute_z <= 0 /\ 1 * s V_mad_frame_mute_sb + -32 <= 0 /\ -1 * s V_mad_frame_mute_s + 1 <= 0)%Z
   | 29 => (-1 * s V_mad_frame_mute_s + 1 <= 0 /\ 1 * s V_mad_frame_mute_sb + -32 <= 0 /\ -1 * s V_mad_frame_mute_z <= 0 /\ -1 * s V_mad_frame_mute_sb + 32 <= 0)%Z
   | 30 => (-1 * s V_mad_frame_mute_sb + 32 <= 0 /\ -1 * s V_mad_frame_mute_z <= 0 /\ 1 * s V_mad_frame_mute_sb + -32 <= 0 /\ -1 * s V_mad_frame_mute_s + 1 <= 0)%Z
   | 31 => (-1 * s V_mad_frame_mute_s + 1 <= 0 /\ 1 * s V_mad_frame_mute_sb + -32 <= 0 /\ -1 * s V_mad_frame_mute_sb + 32 <= 0 /\ -1 * s V_mad_frame_mute_z + 1 <= 0)%Z
   | 32 => (-1 * s V_mad_frame_mute_s <= 0 /\ -1 * s V_mad_frame_mute_sb <= 0 /\ -1 * s V_mad_frame_mute_z <= 0 /\ 1 * s V_mad_frame_mute_sb + -31 <= 0)%Z
   | 33 => (1 * s V_mad_frame_mute_sb + -31 <= 0 /\ -1 * s V_mad_frame_mute_z <= 0 /\ -1 * s V_mad_frame_mute_sb <= 0 /\ -1 * s V_mad_frame_mute_s <= 0)%Z
   | 34 => (-1 * s V_mad_frame_mute_s <= 0 /\ -1 * s V_mad_frame_mute_sb <= 0 /\ -1 * s V_mad_frame_mute_z <= 0 /\ 1 * s V_mad_frame_mute_sb + -31 <= 0)%Z
   | 35 => (-1 * s V_mad_frame_mute_z <= 0 /\ -1 * s V_mad_frame_mute_s <= 0 /\ -1 * s V_mad_frame_mute_sb + 1 <= 0 /\ 1 * s V_mad_frame_mute_sb + -32 <= 0)%Z
   | 36 => (1 * s V_mad_frame_mute_sb + -32 <= 0 /\ -1 * s V_mad_frame_mute_sb + 1 <= 0 /\ -1 * s V_mad_frame_mute_s <= 0 /\ -1 * s V_mad_frame_mute_z <= 0)%Z
   | 37 => (-1 * s V_mad_frame_mute_z <= 0 /\ -1 * s V_mad_frame_mute_s <= 0 /\ -1 * s V_mad_frame_mute_sb + 1 <= 0 /\ 1 * s V_mad_frame_mute_sb + -32 <= 0)%Z
   | 38 => (1 * s V_mad_frame_mute_sb + -32 <= 0 /\ -1 * s V_mad_frame_mute_sb + 1 <= 0 /\ -1 * s V_mad_frame_mute_s <= 0 /\ -1 * s V_mad_frame_mute_z + 1 <= 0)%Z
   | 39 => (-1 * s V_mad_frame_mute_s <= 0 /\ -1 * s V_mad_frame_mute_z <= 0 /\ -1 * s V_mad_frame_mute_sb <= 0 /\ 1 * s V_mad_frame_mute_s + -35 <= 0)%Z
   | 40 => (1 * s V_mad_frame_mute_s + -35 <= 0 /\ -1 * s V_mad_frame_mute_sb <= 0 /\ -1 * s V_mad_frame_mute_z <= 0 /\ -1 * s V_mad_frame_mute_s <= 0)%Z
   | 41 => (-1 * s V_mad_frame_mute_s <= 0 /\ -1 * s V_mad_frame_mute_z <= 0 /\ 1 * s V_mad_frame_mute_s + -35 <= 0 /\ 1 * s V_mad_frame_mute_sb <= 0 /\ -1 * s V_mad_frame_mute_sb <= 0)%Z
   | 42 => (-1 * s V_mad_frame_mute_sb <= 0 /\ 1 * s V_mad_frame_mute_sb <= 0 /\ 1 * s V_mad_frame_mute_s + -35 <= 0 /\ -1 * s V_mad_frame_mute_z <= 0 /\ -1 * s V_mad_frame_mute_s <= 0)%Z
   | 43 => (-1 * s V_mad_frame_mute_z <= 0 /\ -1 * s V_mad_frame_mute_sb <= 0 /\ -1 * s V_mad_frame_mute_s <= 0 /\ 1 * s V_mad_frame_mute_sb + -32 <= 0)%Z
   | 44 => (1 * s V_mad_frame_mute_sb + -32 <= 0 /\ -1 * s V_mad_frame_mute_s <= 0 /\ -1 * s V_mad_frame_mute_z <= 0 /\ -1 * s V_mad_frame_mute_sb + 32 <= 0)%Z
   | 45 => (-1 * s V_mad_frame_mute_sb + 32 <= 0 /\ -1 * s V_mad_frame_mute_z <= 0 /\ -1 * s V_mad_frame_mute_s <= 0 /\ 1 * s V_mad_frame_mute_sb + -32 <= 0)%Z
   | 46 => (1 * s V_mad_frame_mute_sb + -32 <= 0 /\ -1 * s V_mad_frame_mute_s <= 0 /\ -1 * s V_mad_frame_mute_z <= 0 /\ -1 * s V_mad_frame_mute_sb + 32 <= 0)%Z
   | 47 => (-1 * s V_mad_frame_mute_sb + 32 <= 0 /\ -1 * s V_mad_frame_mute_z <= 0 /\ 1 * s V_mad_frame_mute_sb + -32 <= 0 /\ -1 * s V_mad_frame_mute_s + 1 <= 0)%Z
   | 48 => (-1 * s V_mad_frame_mute_s + 1 <= 0 /\ 1 * s V_mad_frame_mute_sb + -32 <= 0 /\ -1 * s V_mad_frame_mute_z <= 0 /\ -1 * s V_mad_frame_mute_sb + 32 <= 0)%Z
   | 49 => (-1 * s V_mad_frame_mute_sb + 32 <= 0 /\ -1 * s V_mad_frame_mute_z <= 0 /\ 1 * s V_mad_frame_mute_sb + -32 <= 0 /\ -1 * s V_mad_frame_mute_s + 1 <= 0)%Z
   | 50 => (-1 * s V_mad_frame_mute_s + 1 <= 0 /\ 1 * s V_mad_frame_mute_sb + -32 <= 0 /\ -1 * s V_mad_frame_mute_sb + 32 <= 0 /\ -1 * s V_mad_frame_mute_z + 1 <= 0)%Z
   | 51 => (-1 * s V_mad_frame_mute_s <= 0 /\ -1 * s V_mad_frame_mute_sb <= 0 /\ -1 * s V_mad_frame_mute_z <= 0 /\ 1 * s V_mad_frame_mute_sb + -31 <= 0)%Z
   | 52 => (1 * s V_mad_frame_mute_sb + -31 <= 0 /\ -1 * s V_mad_frame_mute_z <= 0 /\ -1 * s V_mad_frame_mute_sb <= 0 /\ -1 * s V_mad_frame_mute_s <= 0)%Z
   | 53 => (-1 * s V_mad_frame_mute_s <= 0 /\ -1 * s V_mad_frame_mute_sb <= 0 /\ -1 * s V_mad_frame_mute_z <= 0 /\ 1 * s V_mad_frame_mute_sb + -31 <= 0)%Z
   | 54 => (-1 * s V_mad_frame_mute_z <= 0 /\ -1 * s V_mad_frame_mute_s <= 0 /\ -1 * s V_mad_frame_mute_sb + 1 <= 0 /\ 1 * s V_mad_frame_mute_sb + -32 <= 0)%Z
   | 55 => (1 * s V_mad_frame_mute_sb + -32 <= 0 /\ -1 * s V_mad_frame_mute_sb + 1 <= 0 /\ -1 * s V_mad_frame_mute_s <= 0 /\ -1 * s V_mad_frame_mute_z <= 0)%Z
   | 56 => (-1 * s V_mad_frame_mute_z <= 0 /\ -1 * s V_mad_frame_mute_s <= 0 /\ -1 * s V_mad_frame_mute_sb + 1 <= 0 /\ 1 * s V_mad_frame_mute_sb + -32 <= 0)%Z
   | 57 => (1 * s V_mad_frame_mute_sb + -32 <= 0 /\ -1 * s V_mad_frame_mute_sb + 1 <= 0 /\ -1 * s V_mad_frame_mute_s <= 0 /\ -1 * s V_mad_frame_mute_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_mad_frame_mute (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((1782 # 1) <= z)%Q
   | 2 => ((1782 # 1) + s V_mad_frame_mute_z <= z)%Q
   | 3 => ((1782 # 1) + s V_mad_frame_mute_z <= z)%Q
   | 4 => ((1782 # 1) + s V_mad_frame_mute_z <= z)%Q
   | 5 => ((1782 # 1) + s V_mad_frame_mute_z <= z)%Q
   | 6 => ((594 # 1) + s V_mad_frame_mute_z
           + (33 # 1) * max0(36 - s V_mad_frame_mute_s) <= z)%Q
   | 7 => ((594 # 1) + s V_mad_frame_mute_z
           + (33 # 1) * max0(36 - s V_mad_frame_mute_s) <= z)%Q
   | 8 => ((594 # 1) + s V_mad_frame_mute_z
           + (33 # 1) * max0(36 - s V_mad_frame_mute_s) <= z)%Q
   | 9 => hints
     [(*-33 0*) F_max0_monotonic (F_check_ge (36 - s V_mad_frame_mute_s) (35
                                                                    - s V_mad_frame_mute_s));
      (*-33 0*) F_max0_ge_0 (35 - s V_mad_frame_mute_s)]
     ((594 # 1) + s V_mad_frame_mute_z
      + (33 # 1) * max0(36 - s V_mad_frame_mute_s) <= z)%Q
   | 10 => ((594 # 1) + s V_mad_frame_mute_z <= z)%Q
   | 11 => hints
     [(*-594 0*) F_one]
     ((594 # 1) + s V_mad_frame_mute_z <= z)%Q
   | 12 => ((594 # 1) + s V_mad_frame_mute_z <= z)%Q
   | 13 => (s V_mad_frame_mute_z + (33 # 1) * max0(18 - s V_mad_frame_mute_s) <= z)%Q
   | 14 => (s V_mad_frame_mute_z + (33 # 1) * max0(18 - s V_mad_frame_mute_s) <= z)%Q
   | 15 => (s V_mad_frame_mute_z + (33 # 1) * max0(18 - s V_mad_frame_mute_s) <= z)%Q
   | 16 => (s V_mad_frame_mute_z + (33 # 1) * max0(18 - s V_mad_frame_mute_s) <= z)%Q
   | 17 => (s V_mad_frame_mute_z + (33 # 1) * max0(18 - s V_mad_frame_mute_s) <= z)%Q
   | 18 => hints
     [(*-33 0*) F_max0_monotonic (F_check_ge (18 - s V_mad_frame_mute_s) (17
                                                                    - s V_mad_frame_mute_s));
      (*-33 0*) F_max0_ge_0 (17 - s V_mad_frame_mute_s)]
     (s V_mad_frame_mute_z + (33 # 1) * max0(18 - s V_mad_frame_mute_s) <= z)%Q
   | 19 => (s V_mad_frame_mute_z <= z)%Q
   | 20 => (s V_mad_frame_mute_z + (33 # 1) * max0(18 - s V_mad_frame_mute_s) <= z)%Q
   | 21 => (s V_mad_frame_mute_z + (33 # 1) * max0(18 - s V_mad_frame_mute_s) <= z)%Q
   | 22 => (-(32 # 1) + s V_mad_frame_mute_z
            + (33 # 1) * max0(18 - s V_mad_frame_mute_s)
            + max0(32 - s V_mad_frame_mute_sb) <= z)%Q
   | 23 => hints
     [(*-33 0*) F_max0_pre_decrement 1 (18 - s V_mad_frame_mute_s) (1)]
     (-(32 # 1) + s V_mad_frame_mute_z
      + (33 # 1) * max0(18 - s V_mad_frame_mute_s)
      + max0(32 - s V_mad_frame_mute_sb) <= z)%Q
   | 24 => ((1 # 1) + s V_mad_frame_mute_z
            + (33 # 1) * max0(17 - s V_mad_frame_mute_s)
            + max0(32 - s V_mad_frame_mute_sb) <= z)%Q
   | 25 => ((1 # 1) + s V_mad_frame_mute_z
            + (33 # 1) * max0(17 - s V_mad_frame_mute_s)
            + max0(32 - s V_mad_frame_mute_sb) <= z)%Q
   | 26 => ((1 # 1) + s V_mad_frame_mute_z
            + (33 # 1) * max0(17 - s V_mad_frame_mute_s)
            + max0(32 - s V_mad_frame_mute_sb) <= z)%Q
   | 27 => ((1 # 1) + s V_mad_frame_mute_z
            + (33 # 1) * max0(17 - s V_mad_frame_mute_s)
            + max0(32 - s V_mad_frame_mute_sb) <= z)%Q
   | 28 => ((1 # 1) + s V_mad_frame_mute_z
            + (33 # 1) * max0(18 - s V_mad_frame_mute_s)
            + max0(32 - s V_mad_frame_mute_sb) <= z)%Q
   | 29 => ((1 # 1) + s V_mad_frame_mute_z
            + (33 # 1) * max0(18 - s V_mad_frame_mute_s)
            + max0(32 - s V_mad_frame_mute_sb) <= z)%Q
   | 30 => ((1 # 1) + s V_mad_frame_mute_z
            + (33 # 1) * max0(18 - s V_mad_frame_mute_s)
            + max0(32 - s V_mad_frame_mute_sb) <= z)%Q
   | 31 => hints
     [(*0 1*) F_max0_monotonic (F_check_ge (32 - s V_mad_frame_mute_sb) (31
                                                                    - s V_mad_frame_mute_sb));
      (*-1 0*) F_max0_ge_0 (31 - s V_mad_frame_mute_sb)]
     (s V_mad_frame_mute_z + (33 # 1) * max0(18 - s V_mad_frame_mute_s)
      + max0(32 - s V_mad_frame_mute_sb) <= z)%Q
   | 32 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (32 - s V_mad_frame_mute_sb) (1)]
     ((1 # 1) + s V_mad_frame_mute_z
      + (33 # 1) * max0(17 - s V_mad_frame_mute_s)
      + max0(32 - s V_mad_frame_mute_sb) <= z)%Q
   | 33 => ((2 # 1) + s V_mad_frame_mute_z
            + (33 # 1) * max0(17 - s V_mad_frame_mute_s)
            + max0(31 - s V_mad_frame_mute_sb) <= z)%Q
   | 34 => ((2 # 1) + s V_mad_frame_mute_z
            + (33 # 1) * max0(17 - s V_mad_frame_mute_s)
            + max0(31 - s V_mad_frame_mute_sb) <= z)%Q
   | 35 => ((2 # 1) + s V_mad_frame_mute_z
            + (33 # 1) * max0(17 - s V_mad_frame_mute_s)
            + max0(32 - s V_mad_frame_mute_sb) <= z)%Q
   | 36 => ((2 # 1) + s V_mad_frame_mute_z
            + (33 # 1) * max0(17 - s V_mad_frame_mute_s)
            + max0(32 - s V_mad_frame_mute_sb) <= z)%Q
   | 37 => ((2 # 1) + s V_mad_frame_mute_z
            + (33 # 1) * max0(17 - s V_mad_frame_mute_s)
            + max0(32 - s V_mad_frame_mute_sb) <= z)%Q
   | 38 => ((1 # 1) + s V_mad_frame_mute_z
            + (33 # 1) * max0(17 - s V_mad_frame_mute_s)
            + max0(32 - s V_mad_frame_mute_sb) <= z)%Q
   | 39 => ((594 # 1) + s V_mad_frame_mute_z
            + (33 # 1) * max0(36 - s V_mad_frame_mute_s) <= z)%Q
   | 40 => ((594 # 1) + s V_mad_frame_mute_z
            + (33 # 1) * max0(36 - s V_mad_frame_mute_s) <= z)%Q
   | 41 => ((562 # 1) + s V_mad_frame_mute_z
            + max0(32 - s V_mad_frame_mute_sb)
            + (33 # 1) * max0(36 - s V_mad_frame_mute_s) <= z)%Q
   | 42 => hints
     [(*-33 0*) F_max0_pre_decrement 1 (36 - s V_mad_frame_mute_s) (1)]
     ((562 # 1) + s V_mad_frame_mute_z + max0(32 - s V_mad_frame_mute_sb)
      + (33 # 1) * max0(36 - s V_mad_frame_mute_s) <= z)%Q
   | 43 => ((595 # 1) + s V_mad_frame_mute_z
            + max0(32 - s V_mad_frame_mute_sb)
            + (33 # 1) * max0(35 - s V_mad_frame_mute_s) <= z)%Q
   | 44 => ((595 # 1) + s V_mad_frame_mute_z
            + max0(32 - s V_mad_frame_mute_sb)
            + (33 # 1) * max0(35 - s V_mad_frame_mute_s) <= z)%Q
   | 45 => ((595 # 1) + s V_mad_frame_mute_z
            + max0(32 - s V_mad_frame_mute_sb)
            + (33 # 1) * max0(35 - s V_mad_frame_mute_s) <= z)%Q
   | 46 => ((595 # 1) + s V_mad_frame_mute_z
            + max0(32 - s V_mad_frame_mute_sb)
            + (33 # 1) * max0(35 - s V_mad_frame_mute_s) <= z)%Q
   | 47 => ((595 # 1) + s V_mad_frame_mute_z
            + max0(32 - s V_mad_frame_mute_sb)
            + (33 # 1) * max0(36 - s V_mad_frame_mute_s) <= z)%Q
   | 48 => ((595 # 1) + s V_mad_frame_mute_z
            + max0(32 - s V_mad_frame_mute_sb)
            + (33 # 1) * max0(36 - s V_mad_frame_mute_s) <= z)%Q
   | 49 => ((595 # 1) + s V_mad_frame_mute_z
            + max0(32 - s V_mad_frame_mute_sb)
            + (33 # 1) * max0(36 - s V_mad_frame_mute_s) <= z)%Q
   | 50 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (32 - s V_mad_frame_mute_sb)) (F_check_ge (0) (0))]
     ((594 # 1) + s V_mad_frame_mute_z + max0(32 - s V_mad_frame_mute_sb)
      + (33 # 1) * max0(36 - s V_mad_frame_mute_s) <= z)%Q
   | 51 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (32 - s V_mad_frame_mute_sb)) (F_check_ge (32
                                                                    - s V_mad_frame_mute_sb) (0))]
     ((595 # 1) + s V_mad_frame_mute_z + max0(32 - s V_mad_frame_mute_sb)
      + (33 # 1) * max0(35 - s V_mad_frame_mute_s) <= z)%Q
   | 52 => ((627 # 1) - s V_mad_frame_mute_sb + s V_mad_frame_mute_z
            + (33 # 1) * max0(35 - s V_mad_frame_mute_s) <= z)%Q
   | 53 => ((627 # 1) - s V_mad_frame_mute_sb + s V_mad_frame_mute_z
            + (33 # 1) * max0(35 - s V_mad_frame_mute_s) <= z)%Q
   | 54 => ((628 # 1) - s V_mad_frame_mute_sb + s V_mad_frame_mute_z
            + (33 # 1) * max0(35 - s V_mad_frame_mute_s) <= z)%Q
   | 55 => ((628 # 1) - s V_mad_frame_mute_sb + s V_mad_frame_mute_z
            + (33 # 1) * max0(35 - s V_mad_frame_mute_s) <= z)%Q
   | 56 => ((628 # 1) - s V_mad_frame_mute_sb + s V_mad_frame_mute_z
            + (33 # 1) * max0(35 - s V_mad_frame_mute_s) <= z)%Q
   | 57 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (32
                                                               - s V_mad_frame_mute_sb) (0))) (F_max0_ge_0 (32
                                                                    - s V_mad_frame_mute_sb))]
     ((627 # 1) - s V_mad_frame_mute_sb + s V_mad_frame_mute_z
      + (33 # 1) * max0(35 - s V_mad_frame_mute_s) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_mad_frame_mute =>
    [mkPA Q (fun n z s => ai_mad_frame_mute n s /\ annot0_mad_frame_mute n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_mad_frame_mute (proc_start P_mad_frame_mute) s1 (proc_end P_mad_frame_mute) s2 ->
    (s2 V_mad_frame_mute_z <= (1782 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_mad_frame_mute.
Qed.
