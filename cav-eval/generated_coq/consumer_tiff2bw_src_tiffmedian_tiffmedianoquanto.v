Require Import pasta.Pasta.

Inductive proc: Type :=
  P_quant.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_quant_z := 1%positive.
Notation V_quant_blue := 2%positive.
Notation V_quant_green := 3%positive.
Notation V_quant_i := 4%positive.
Notation V_quant_imagelength := 5%positive.
Notation V_quant_imagewidth := 6%positive.
Notation V_quant_j := 7%positive.
Notation V_quant_red := 8%positive.
Notation V_quant_in := 9%positive.
Notation V_quant_out := 10%positive.
Definition Pedges_quant: list (edge proc) :=
  (EA 1 (AAssign V_quant_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_quant_j) s) >= (eval (ENum (0)) s))%Z)) 3)::
  (EA 3 (AGuard (fun s => ((eval (EVar V_quant_imagewidth) s) >=
  (eval (ENum (0)) s))%Z)) 4)::(EA 4 (AGuard
  (fun s => ((eval (EVar V_quant_imagelength) s) >= (eval (ENum (0))
  s))%Z)) 5)::(EA 5 (AGuard (fun s => ((eval (EVar V_quant_i) s) >=
  (eval (ENum (0)) s))%Z)) 6)::(EA 6 AWeaken 7)::(EA 7 (AAssign V_quant_i
  (Some (ENum (0)))) 8)::(EA 8 ANone 9)::(EA 9 AWeaken 10)::(EA 10 (AGuard
  (fun s => ((eval (EVar V_quant_i) s) < (eval (EVar V_quant_imagelength)
  s))%Z)) 12)::(EA 10 (AGuard (fun s => ((eval (EVar V_quant_i) s) >=
  (eval (EVar V_quant_imagelength) s))%Z)) 11)::(EA 11 AWeaken 40)::
  (EA 12 AWeaken 13)::(EA 13 ANone 38)::(EA 13 ANone 14)::(EA 14 (AAssign
  V_quant_j (Some (ENum (0)))) 15)::(EA 15 ANone 16)::(EA 16 AWeaken 17)::
  (EA 17 (AGuard (fun s => ((eval (EVar V_quant_j) s) <
  (eval (EVar V_quant_imagewidth) s))%Z)) 28)::(EA 17 (AGuard
  (fun s => ((eval (EVar V_quant_j) s) >= (eval (EVar V_quant_imagewidth)
  s))%Z)) 18)::(EA 18 AWeaken 19)::(EA 19 ANone 26)::(EA 19 ANone 20)::
  (EA 20 ANone 21)::(EA 21 (AAssign V_quant_i (Some (EAdd (EVar V_quant_i)
  (ENum (1))))) 22)::(EA 22 ANone 23)::(EA 23 ANone 24)::(EA 24 (AAssign
  V_quant_z (Some (EAdd (ENum (1)) (EVar V_quant_z)))) 25)::
  (EA 25 AWeaken 10)::(EA 26 ANone 27)::(EA 27 AWeaken 40)::
  (EA 28 AWeaken 29)::(EA 29 (AAssign V_quant_red None) 30)::(EA 30 (AAssign
  V_quant_green None) 31)::(EA 31 (AAssign V_quant_blue None) 32)::
  (EA 32 ANone 33)::(EA 33 (AAssign V_quant_j (Some (EAdd (EVar V_quant_j)
  (ENum (1))))) 34)::(EA 34 ANone 35)::(EA 35 ANone 36)::(EA 36 (AAssign
  V_quant_z (Some (EAdd (ENum (1)) (EVar V_quant_z)))) 37)::
  (EA 37 AWeaken 17)::(EA 38 ANone 39)::(EA 39 AWeaken 40)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_quant => Pedges_quant
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_quant => 40
     end)%positive;
  var_global := var_global
}.

Definition ai_quant (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_quant_z <= 0 /\ -1 * s V_quant_z <= 0)%Z
   | 3 => (-1 * s V_quant_z <= 0 /\ 1 * s V_quant_z <= 0 /\ -1 * s V_quant_j <= 0)%Z
   | 4 => (-1 * s V_quant_j <= 0 /\ 1 * s V_quant_z <= 0 /\ -1 * s V_quant_z <= 0 /\ -1 * s V_quant_imagewidth <= 0)%Z
   | 5 => (-1 * s V_quant_imagewidth <= 0 /\ -1 * s V_quant_z <= 0 /\ 1 * s V_quant_z <= 0 /\ -1 * s V_quant_j <= 0 /\ -1 * s V_quant_imagelength <= 0)%Z
   | 6 => (-1 * s V_quant_imagelength <= 0 /\ -1 * s V_quant_j <= 0 /\ 1 * s V_quant_z <= 0 /\ -1 * s V_quant_z <= 0 /\ -1 * s V_quant_imagewidth <= 0 /\ -1 * s V_quant_i <= 0)%Z
   | 7 => (-1 * s V_quant_i <= 0 /\ -1 * s V_quant_imagewidth <= 0 /\ -1 * s V_quant_z <= 0 /\ 1 * s V_quant_z <= 0 /\ -1 * s V_quant_j <= 0 /\ -1 * s V_quant_imagelength <= 0)%Z
   | 8 => (-1 * s V_quant_imagelength <= 0 /\ -1 * s V_quant_j <= 0 /\ 1 * s V_quant_z <= 0 /\ -1 * s V_quant_z <= 0 /\ -1 * s V_quant_imagewidth <= 0 /\ 1 * s V_quant_i <= 0 /\ -1 * s V_quant_i <= 0)%Z
   | 9 => (-1 * s V_quant_i <= 0 /\ 1 * s V_quant_i <= 0 /\ -1 * s V_quant_imagewidth <= 0 /\ -1 * s V_quant_z <= 0 /\ 1 * s V_quant_z <= 0 /\ -1 * s V_quant_j <= 0 /\ -1 * s V_quant_imagelength <= 0)%Z
   | 10 => (-1 * s V_quant_j <= 0 /\ -1 * s V_quant_z <= 0 /\ -1 * s V_quant_i <= 0 /\ -1 * s V_quant_imagewidth <= 0 /\ 1 * s V_quant_i+ -1 * s V_quant_imagelength <= 0)%Z
   | 11 => (1 * s V_quant_i+ -1 * s V_quant_imagelength <= 0 /\ -1 * s V_quant_imagewidth <= 0 /\ -1 * s V_quant_i <= 0 /\ -1 * s V_quant_z <= 0 /\ -1 * s V_quant_j <= 0 /\ -1 * s V_quant_i+ 1 * s V_quant_imagelength <= 0)%Z
   | 12 => (-1 * s V_quant_imagewidth <= 0 /\ -1 * s V_quant_i <= 0 /\ -1 * s V_quant_z <= 0 /\ -1 * s V_quant_j <= 0 /\ 1 * s V_quant_i+ -1 * s V_quant_imagelength + 1 <= 0)%Z
   | 13 => (1 * s V_quant_i+ -1 * s V_quant_imagelength + 1 <= 0 /\ -1 * s V_quant_j <= 0 /\ -1 * s V_quant_z <= 0 /\ -1 * s V_quant_i <= 0 /\ -1 * s V_quant_imagewidth <= 0)%Z
   | 14 => (-1 * s V_quant_imagewidth <= 0 /\ -1 * s V_quant_i <= 0 /\ -1 * s V_quant_z <= 0 /\ -1 * s V_quant_j <= 0 /\ 1 * s V_quant_i+ -1 * s V_quant_imagelength + 1 <= 0)%Z
   | 15 => (1 * s V_quant_i+ -1 * s V_quant_imagelength + 1 <= 0 /\ -1 * s V_quant_z <= 0 /\ -1 * s V_quant_i <= 0 /\ -1 * s V_quant_imagewidth <= 0 /\ 1 * s V_quant_j <= 0 /\ -1 * s V_quant_j <= 0)%Z
   | 16 => (-1 * s V_quant_j <= 0 /\ 1 * s V_quant_j <= 0 /\ -1 * s V_quant_imagewidth <= 0 /\ -1 * s V_quant_i <= 0 /\ -1 * s V_quant_z <= 0 /\ 1 * s V_quant_i+ -1 * s V_quant_imagelength + 1 <= 0)%Z
   | 17 => (-1 * s V_quant_z <= 0 /\ -1 * s V_quant_j <= 0 /\ -1 * s V_quant_i <= 0 /\ -1 * s V_quant_imagelength <= 0 /\ 1 * s V_quant_i+ -1 * s V_quant_imagelength + 1 <= 0 /\ -1 * s V_quant_imagewidth+ 1 * s V_quant_j <= 0)%Z
   | 18 => (-1 * s V_quant_imagewidth+ 1 * s V_quant_j <= 0 /\ 1 * s V_quant_i+ -1 * s V_quant_imagelength + 1 <= 0 /\ -1 * s V_quant_imagelength <= 0 /\ -1 * s V_quant_i <= 0 /\ -1 * s V_quant_j <= 0 /\ -1 * s V_quant_z <= 0 /\ 1 * s V_quant_imagewidth+ -1 * s V_quant_j <= 0)%Z
   | 19 => (1 * s V_quant_imagewidth+ -1 * s V_quant_j <= 0 /\ -1 * s V_quant_z <= 0 /\ -1 * s V_quant_j <= 0 /\ -1 * s V_quant_i <= 0 /\ -1 * s V_quant_imagelength <= 0 /\ 1 * s V_quant_i+ -1 * s V_quant_imagelength + 1 <= 0 /\ -1 * s V_quant_imagewidth+ 1 * s V_quant_j <= 0)%Z
   | 20 => (-1 * s V_quant_imagewidth+ 1 * s V_quant_j <= 0 /\ 1 * s V_quant_i+ -1 * s V_quant_imagelength + 1 <= 0 /\ -1 * s V_quant_imagelength <= 0 /\ -1 * s V_quant_i <= 0 /\ -1 * s V_quant_j <= 0 /\ -1 * s V_quant_z <= 0 /\ 1 * s V_quant_imagewidth+ -1 * s V_quant_j <= 0)%Z
   | 21 => (1 * s V_quant_imagewidth+ -1 * s V_quant_j <= 0 /\ -1 * s V_quant_z <= 0 /\ -1 * s V_quant_j <= 0 /\ -1 * s V_quant_i <= 0 /\ -1 * s V_quant_imagelength <= 0 /\ 1 * s V_quant_i+ -1 * s V_quant_imagelength + 1 <= 0 /\ -1 * s V_quant_imagewidth+ 1 * s V_quant_j <= 0)%Z
   | 22 => (-1 * s V_quant_imagewidth+ 1 * s V_quant_j <= 0 /\ -1 * s V_quant_j <= 0 /\ -1 * s V_quant_z <= 0 /\ 1 * s V_quant_imagewidth+ -1 * s V_quant_j <= 0 /\ -1 * s V_quant_i + 1 <= 0 /\ 1 * s V_quant_i+ -1 * s V_quant_imagelength <= 0)%Z
   | 23 => (1 * s V_quant_i+ -1 * s V_quant_imagelength <= 0 /\ -1 * s V_quant_i + 1 <= 0 /\ 1 * s V_quant_imagewidth+ -1 * s V_quant_j <= 0 /\ -1 * s V_quant_z <= 0 /\ -1 * s V_quant_j <= 0 /\ -1 * s V_quant_imagewidth+ 1 * s V_quant_j <= 0)%Z
   | 24 => (-1 * s V_quant_imagewidth+ 1 * s V_quant_j <= 0 /\ -1 * s V_quant_j <= 0 /\ -1 * s V_quant_z <= 0 /\ 1 * s V_quant_imagewidth+ -1 * s V_quant_j <= 0 /\ -1 * s V_quant_i + 1 <= 0 /\ 1 * s V_quant_i+ -1 * s V_quant_imagelength <= 0)%Z
   | 25 => (1 * s V_quant_i+ -1 * s V_quant_imagelength <= 0 /\ -1 * s V_quant_i + 1 <= 0 /\ 1 * s V_quant_imagewidth+ -1 * s V_quant_j <= 0 /\ -1 * s V_quant_j <= 0 /\ -1 * s V_quant_imagewidth+ 1 * s V_quant_j <= 0 /\ -1 * s V_quant_z + 1 <= 0)%Z
   | 26 => (-1 * s V_quant_imagewidth+ 1 * s V_quant_j <= 0 /\ 1 * s V_quant_i+ -1 * s V_quant_imagelength + 1 <= 0 /\ -1 * s V_quant_imagelength <= 0 /\ -1 * s V_quant_i <= 0 /\ -1 * s V_quant_j <= 0 /\ -1 * s V_quant_z <= 0 /\ 1 * s V_quant_imagewidth+ -1 * s V_quant_j <= 0)%Z
   | 27 => (1 * s V_quant_imagewidth+ -1 * s V_quant_j <= 0 /\ -1 * s V_quant_z <= 0 /\ -1 * s V_quant_j <= 0 /\ -1 * s V_quant_i <= 0 /\ -1 * s V_quant_imagelength <= 0 /\ 1 * s V_quant_i+ -1 * s V_quant_imagelength + 1 <= 0 /\ -1 * s V_quant_imagewidth+ 1 * s V_quant_j <= 0)%Z
   | 28 => (1 * s V_quant_i+ -1 * s V_quant_imagelength + 1 <= 0 /\ -1 * s V_quant_imagelength <= 0 /\ -1 * s V_quant_i <= 0 /\ -1 * s V_quant_j <= 0 /\ -1 * s V_quant_z <= 0 /\ -1 * s V_quant_imagewidth+ 1 * s V_quant_j + 1 <= 0)%Z
   | 29 => (-1 * s V_quant_imagewidth+ 1 * s V_quant_j + 1 <= 0 /\ -1 * s V_quant_z <= 0 /\ -1 * s V_quant_j <= 0 /\ -1 * s V_quant_i <= 0 /\ -1 * s V_quant_imagelength <= 0 /\ 1 * s V_quant_i+ -1 * s V_quant_imagelength + 1 <= 0)%Z
   | 30 => (1 * s V_quant_i+ -1 * s V_quant_imagelength + 1 <= 0 /\ -1 * s V_quant_imagelength <= 0 /\ -1 * s V_quant_i <= 0 /\ -1 * s V_quant_j <= 0 /\ -1 * s V_quant_z <= 0 /\ -1 * s V_quant_imagewidth+ 1 * s V_quant_j + 1 <= 0)%Z
   | 31 => (-1 * s V_quant_imagewidth+ 1 * s V_quant_j + 1 <= 0 /\ -1 * s V_quant_z <= 0 /\ -1 * s V_quant_j <= 0 /\ -1 * s V_quant_i <= 0 /\ -1 * s V_quant_imagelength <= 0 /\ 1 * s V_quant_i+ -1 * s V_quant_imagelength + 1 <= 0)%Z
   | 32 => (1 * s V_quant_i+ -1 * s V_quant_imagelength + 1 <= 0 /\ -1 * s V_quant_imagelength <= 0 /\ -1 * s V_quant_i <= 0 /\ -1 * s V_quant_j <= 0 /\ -1 * s V_quant_z <= 0 /\ -1 * s V_quant_imagewidth+ 1 * s V_quant_j + 1 <= 0)%Z
   | 33 => (-1 * s V_quant_imagewidth+ 1 * s V_quant_j + 1 <= 0 /\ -1 * s V_quant_z <= 0 /\ -1 * s V_quant_j <= 0 /\ -1 * s V_quant_i <= 0 /\ -1 * s V_quant_imagelength <= 0 /\ 1 * s V_quant_i+ -1 * s V_quant_imagelength + 1 <= 0)%Z
   | 34 => (1 * s V_quant_i+ -1 * s V_quant_imagelength + 1 <= 0 /\ -1 * s V_quant_imagelength <= 0 /\ -1 * s V_quant_i <= 0 /\ -1 * s V_quant_z <= 0 /\ -1 * s V_quant_imagewidth+ 1 * s V_quant_j <= 0 /\ -1 * s V_quant_j + 1 <= 0)%Z
   | 35 => (-1 * s V_quant_j + 1 <= 0 /\ -1 * s V_quant_imagewidth+ 1 * s V_quant_j <= 0 /\ -1 * s V_quant_z <= 0 /\ -1 * s V_quant_i <= 0 /\ -1 * s V_quant_imagelength <= 0 /\ 1 * s V_quant_i+ -1 * s V_quant_imagelength + 1 <= 0)%Z
   | 36 => (1 * s V_quant_i+ -1 * s V_quant_imagelength + 1 <= 0 /\ -1 * s V_quant_imagelength <= 0 /\ -1 * s V_quant_i <= 0 /\ -1 * s V_quant_z <= 0 /\ -1 * s V_quant_imagewidth+ 1 * s V_quant_j <= 0 /\ -1 * s V_quant_j + 1 <= 0)%Z
   | 37 => (-1 * s V_quant_j + 1 <= 0 /\ -1 * s V_quant_imagewidth+ 1 * s V_quant_j <= 0 /\ -1 * s V_quant_i <= 0 /\ -1 * s V_quant_imagelength <= 0 /\ 1 * s V_quant_i+ -1 * s V_quant_imagelength + 1 <= 0 /\ -1 * s V_quant_z + 1 <= 0)%Z
   | 38 => (-1 * s V_quant_imagewidth <= 0 /\ -1 * s V_quant_i <= 0 /\ -1 * s V_quant_z <= 0 /\ -1 * s V_quant_j <= 0 /\ 1 * s V_quant_i+ -1 * s V_quant_imagelength + 1 <= 0)%Z
   | 39 => (1 * s V_quant_i+ -1 * s V_quant_imagelength + 1 <= 0 /\ -1 * s V_quant_j <= 0 /\ -1 * s V_quant_z <= 0 /\ -1 * s V_quant_i <= 0 /\ -1 * s V_quant_imagewidth <= 0)%Z
   | 40 => (1 * s V_quant_i+ -1 * s V_quant_imagelength <= 0 /\ -1 * s V_quant_imagewidth <= 0 /\ -1 * s V_quant_i <= 0 /\ -1 * s V_quant_z <= 0 /\ -1 * s V_quant_j <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_quant (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_quant_imagelength)
           + max0(s V_quant_imagelength) * max0(s V_quant_imagewidth) <= z)%Q
   | 2 => (s V_quant_z + max0(s V_quant_imagelength)
           + max0(s V_quant_imagelength) * max0(s V_quant_imagewidth) <= z)%Q
   | 3 => (s V_quant_z + max0(s V_quant_imagelength)
           + max0(s V_quant_imagelength) * max0(s V_quant_imagewidth) <= z)%Q
   | 4 => (s V_quant_z + max0(s V_quant_imagelength)
           + max0(s V_quant_imagelength) * max0(s V_quant_imagewidth) <= z)%Q
   | 5 => (s V_quant_z + max0(s V_quant_imagelength)
           + max0(s V_quant_imagelength) * max0(s V_quant_imagewidth) <= z)%Q
   | 6 => hints
     [(*0 0.689922*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (s V_quant_imagelength) (0))) (F_max0_ge_0 (s V_quant_imagelength))]
     (s V_quant_z + max0(s V_quant_imagelength)
      + max0(s V_quant_imagelength) * max0(s V_quant_imagewidth) <= z)%Q
   | 7 => ((69 # 100) * s V_quant_imagelength
           - (69 # 100) * s V_quant_imagelength^2 + s V_quant_z
           + (31 # 100) * max0(s V_quant_imagelength)
           + max0(s V_quant_imagelength) * max0(s V_quant_imagewidth)
           + (69 # 100) * max0(s V_quant_imagelength)^2 <= z)%Q
   | 8 => (-(128 # 79) * s V_quant_i
           + (109 # 79) * s V_quant_i * s V_quant_imagelength
           + (7 # 43) * s V_quant_i * max0(-1 + s V_quant_i)
           + (3 # 43) * s V_quant_i * max0(-1 + s V_quant_imagelength)
           + (5 # 86) * s V_quant_i * max0(-s V_quant_i
                                           + s V_quant_imagelength)
           + (7 # 43) * s V_quant_i * max0(s V_quant_i)
           - (11 # 86) * s V_quant_i * max0(s V_quant_imagelength)
           - (69 # 100) * s V_quant_i^2 + (69 # 100) * s V_quant_imagelength
           + (14 # 43) * s V_quant_imagelength * max0(-1 - s V_quant_i
                                                      + s V_quant_imagelength)
           - (14 # 43) * s V_quant_imagelength * max0(-1
                                                      + s V_quant_imagelength)
           - (3 # 86) * s V_quant_imagelength * max0(-s V_quant_i
                                                     + s V_quant_imagelength)
           + (11 # 86) * s V_quant_imagelength * max0(s V_quant_i)
           + (3 # 86) * s V_quant_imagelength * max0(s V_quant_imagelength)
           - (69 # 100) * s V_quant_imagelength^2 + s V_quant_z
           - (14 # 43) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
           - (7 # 43) * max0(-1 + s V_quant_i) * max0(s V_quant_i)
           - (8 # 43) * max0(-1 + s V_quant_imagelength) * max0(-s V_quant_i
                                                                + s V_quant_imagelength)
           - (11 # 43) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_i)
           + (22 # 43) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_imagelength)
           - (49 # 79) * max0(-s V_quant_i + s V_quant_imagelength)
           - (11 # 43) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_i)
           + (10 # 43) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
           + max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagewidth)
           + (32 # 65) * max0(-s V_quant_i + s V_quant_imagelength)^2
           - (7 # 43) * max0(s V_quant_i)^2
           + (40 # 43) * max0(s V_quant_imagelength)
           - (3 # 86) * max0(s V_quant_imagelength)^2 <= z)%Q
   | 9 => hints
     [(*0 0.852713*) F_binom_monotonic 2 (F_max0_ge_arg (-s V_quant_i
                                                         + s V_quant_imagelength)) (F_check_ge (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength) (0));
      (*-0.662791 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_max0_ge_0 (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_max0_ge_0 (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_imagewidth
                                                                    - s V_quant_j)) (F_check_ge (0) (0)));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_quant_imagelength)) (F_check_ge (s V_quant_imagelength) (0))]
     (-(128 # 79) * s V_quant_i
      + (109 # 79) * s V_quant_i * s V_quant_imagelength
      + (7 # 43) * s V_quant_i * max0(-1 + s V_quant_i)
      + (3 # 43) * s V_quant_i * max0(-1 + s V_quant_imagelength)
      + (5 # 86) * s V_quant_i * max0(-s V_quant_i + s V_quant_imagelength)
      + (7 # 43) * s V_quant_i * max0(s V_quant_i)
      - (11 # 86) * s V_quant_i * max0(s V_quant_imagelength)
      - (69 # 100) * s V_quant_i^2 + (69 # 100) * s V_quant_imagelength
      + (14 # 43) * s V_quant_imagelength * max0(-1 - s V_quant_i
                                                 + s V_quant_imagelength)
      - (14 # 43) * s V_quant_imagelength * max0(-1 + s V_quant_imagelength)
      - (3 # 86) * s V_quant_imagelength * max0(-s V_quant_i
                                                + s V_quant_imagelength)
      + (11 # 86) * s V_quant_imagelength * max0(s V_quant_i)
      + (3 # 86) * s V_quant_imagelength * max0(s V_quant_imagelength)
      - (69 # 100) * s V_quant_imagelength^2 + s V_quant_z
      - (14 # 43) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
      - (7 # 43) * max0(-1 + s V_quant_i) * max0(s V_quant_i)
      - (8 # 43) * max0(-1 + s V_quant_imagelength) * max0(-s V_quant_i
                                                           + s V_quant_imagelength)
      - (11 # 43) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_i)
      + (22 # 43) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_imagelength)
      - (49 # 79) * max0(-s V_quant_i + s V_quant_imagelength)
      - (11 # 43) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_i)
      + (10 # 43) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
      + max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagewidth)
      + (32 # 65) * max0(-s V_quant_i + s V_quant_imagelength)^2
      - (7 # 43) * max0(s V_quant_i)^2
      + (40 # 43) * max0(s V_quant_imagelength)
      - (3 # 86) * max0(s V_quant_imagelength)^2 <= z)%Q
   | 10 => (-(33 # 43) * s V_quant_i
            - (14 # 43) * s V_quant_i * s V_quant_imagelength
            + (7 # 43) * s V_quant_i * max0(-1 + s V_quant_i)
            + (3 # 43) * s V_quant_i * max0(-1 + s V_quant_imagelength)
            + (31 # 43) * s V_quant_i * max0(-s V_quant_i
                                             + s V_quant_imagelength)
            + (7 # 43) * s V_quant_i * max0(s V_quant_i)
            - (11 # 86) * s V_quant_i * max0(s V_quant_imagelength)
            + s V_quant_i * max0(s V_quant_imagewidth - s V_quant_j)
            + (7 # 43) * s V_quant_i^2 + (36 # 43) * s V_quant_imagelength
            + (14 # 43) * s V_quant_imagelength * max0(-1 - s V_quant_i
                                                       + s V_quant_imagelength)
            - (14 # 43) * s V_quant_imagelength * max0(-1
                                                       + s V_quant_imagelength)
            - (30 # 43) * s V_quant_imagelength * max0(-s V_quant_i
                                                       + s V_quant_imagelength)
            + (11 # 86) * s V_quant_imagelength * max0(s V_quant_i)
            + (3 # 86) * s V_quant_imagelength * max0(s V_quant_imagelength)
            - s V_quant_imagelength * max0(s V_quant_imagewidth - s V_quant_j)
            + (7 # 43) * s V_quant_imagelength^2 + s V_quant_z
            - (14 # 43) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
            - (7 # 43) * max0(-1 + s V_quant_i) * max0(s V_quant_i)
            - (8 # 43) * max0(-1 + s V_quant_imagelength) * max0(-s V_quant_i
                                                                 + s V_quant_imagelength)
            - (11 # 43) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_i)
            + (22 # 43) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_imagelength)
            + (10 # 43) * max0(-s V_quant_i + s V_quant_imagelength)
            - (11 # 43) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_i)
            + (10 # 43) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
            + max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagewidth)
            + max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagewidth
                                                                - s V_quant_j)
            + (13 # 43) * max0(-s V_quant_i + s V_quant_imagelength)^2
            - (7 # 43) * max0(s V_quant_i)^2
            - (3 # 43) * max0(s V_quant_imagelength)
            - (3 # 86) * max0(s V_quant_imagelength)^2 <= z)%Q
   | 11 => hints
     [(*-0.534884 0*) F_max0_monotonic (F_check_ge (-s V_quant_i
                                                    + s V_quant_imagelength) (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength));
      (*-0.534884 0*) F_max0_ge_0 (-1 - s V_quant_i + s V_quant_imagelength);
      (*-0.302326 0*) F_binom_monotonic 2 (F_max0_ge_arg (-s V_quant_i
                                                          + s V_quant_imagelength)) (F_check_ge (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength) (0));
      (*-0.465116 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (s V_quant_i
                                                                    - s V_quant_imagelength) (0))) (F_max0_ge_0 (s V_quant_i
                                                                    - s V_quant_imagelength));
      (*-0.465116 0*) F_binom_monotonic 2 (F_max0_ge_0 (s V_quant_i
                                                        - s V_quant_imagelength)) (F_check_ge (0) (0));
      (*-0.186047 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_max0_ge_0 (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-s V_quant_i
                                                              + s V_quant_imagelength)) (F_check_ge (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_imagewidth
                                                                    - s V_quant_j)) (F_check_ge (0) (0)));
      (*-0.465116 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-s V_quant_i
                                                                   + 
                                                                   s V_quant_imagelength)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_i
                                                                    - s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-s V_quant_i
                                                            + s V_quant_imagelength)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_imagewidth)) (F_check_ge (0) (0)));
      (*-0.162791 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_i) (0))) (F_max0_ge_0 (s V_quant_i))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_quant_i)) (F_check_ge (0) (0)));
      (*-0.255814 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_i) (0))) (F_max0_ge_0 (s V_quant_i))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.255814 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_i) (0))) (F_max0_ge_0 (s V_quant_i))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.162791 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_i) (0))) (F_max0_ge_0 (s V_quant_i))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_i)) (F_check_ge (0) (0)));
      (*-0.127907 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_quant_i)) (F_check_ge (s V_quant_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.465116 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_i
                                                                    - s V_quant_imagelength) (0))) (F_max0_ge_0 (s V_quant_i
                                                                    - s V_quant_imagelength))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.325581 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_imagelength) (0))) (F_max0_ge_0 (s V_quant_imagelength))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.127907 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_imagelength) (0))) (F_max0_ge_0 (s V_quant_imagelength))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_i)) (F_check_ge (0) (0)));
      (*-0.0348837 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_imagelength) (0))) (F_max0_ge_0 (s V_quant_imagelength))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.511628 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_quant_imagelength)) (F_check_ge (s V_quant_imagelength) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.232558 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_quant_imagelength)) (F_check_ge (s V_quant_imagelength) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.0697674 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_imagelength) (0))) (F_max0_ge_0 (s V_quant_imagelength))]
     (-(33 # 43) * s V_quant_i
      - (14 # 43) * s V_quant_i * s V_quant_imagelength
      + (7 # 43) * s V_quant_i * max0(-1 + s V_quant_i)
      + (3 # 43) * s V_quant_i * max0(-1 + s V_quant_imagelength)
      + (31 # 43) * s V_quant_i * max0(-s V_quant_i + s V_quant_imagelength)
      + (7 # 43) * s V_quant_i * max0(s V_quant_i)
      - (11 # 86) * s V_quant_i * max0(s V_quant_imagelength)
      + s V_quant_i * max0(s V_quant_imagewidth - s V_quant_j)
      + (7 # 43) * s V_quant_i^2 + (36 # 43) * s V_quant_imagelength
      + (14 # 43) * s V_quant_imagelength * max0(-1 - s V_quant_i
                                                 + s V_quant_imagelength)
      - (14 # 43) * s V_quant_imagelength * max0(-1 + s V_quant_imagelength)
      - (30 # 43) * s V_quant_imagelength * max0(-s V_quant_i
                                                 + s V_quant_imagelength)
      + (11 # 86) * s V_quant_imagelength * max0(s V_quant_i)
      + (3 # 86) * s V_quant_imagelength * max0(s V_quant_imagelength)
      - s V_quant_imagelength * max0(s V_quant_imagewidth - s V_quant_j)
      + (7 # 43) * s V_quant_imagelength^2 + s V_quant_z
      - (14 # 43) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
      - (7 # 43) * max0(-1 + s V_quant_i) * max0(s V_quant_i)
      - (8 # 43) * max0(-1 + s V_quant_imagelength) * max0(-s V_quant_i
                                                           + s V_quant_imagelength)
      - (11 # 43) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_i)
      + (22 # 43) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_imagelength)
      + (10 # 43) * max0(-s V_quant_i + s V_quant_imagelength)
      - (11 # 43) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_i)
      + (10 # 43) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
      + max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagewidth)
      + max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagewidth
                                                          - s V_quant_j)
      + (13 # 43) * max0(-s V_quant_i + s V_quant_imagelength)^2
      - (7 # 43) * max0(s V_quant_i)^2
      - (3 # 43) * max0(s V_quant_imagelength)
      - (3 # 86) * max0(s V_quant_imagelength)^2 <= z)%Q
   | 12 => hints
     [(*0 0.0232558*) F_binom_monotonic 2 (F_max0_ge_arg (-1 - s V_quant_i
                                                          + s V_quant_imagelength)) (F_check_ge (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength) (0));
      (*-0.209302 0.186047*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_max0_ge_0 (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength));
      (*0 0.0348837*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (s V_quant_imagelength) (0))) (F_max0_ge_0 (s V_quant_imagelength));
      (*-0.0232558 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_max0_ge_0 (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.267442 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_max0_ge_0 (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*0 0.168605*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_max0_ge_0 (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*0 0.0697674*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_quant_imagelength) (0))) (F_max0_ge_0 (-1
                                                                    + s V_quant_imagelength))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.127907 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_quant_imagelength) (0))) (F_max0_ge_0 (-1
                                                                    + s V_quant_imagelength))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.255814 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_quant_imagelength) (0))) (F_max0_ge_0 (-1
                                                                    + s V_quant_imagelength))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_i)) (F_check_ge (0) (0)));
      (*0 0.0639535*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    s V_quant_imagelength)) (F_check_ge (-1
                                                                    + s V_quant_imagelength) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*0 0.697674*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-
                                                                    s V_quant_i
                                                                    + 
                                                                    s V_quant_imagelength)) (F_check_ge (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*0 0.0697674*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-
                                                                    s V_quant_i
                                                                    + 
                                                                    s V_quant_imagelength)) (F_check_ge (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-s V_quant_i
                                                              + s V_quant_imagelength)) (F_check_ge (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_imagewidth
                                                                    - s V_quant_j)) (F_check_ge (0) (0)));
      (*0 0.162791*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_i) (0))) (F_max0_ge_0 (s V_quant_i))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_quant_i)) (F_check_ge (0) (0)));
      (*0 0.162791*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_i) (0))) (F_max0_ge_0 (s V_quant_i))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_i)) (F_check_ge (0) (0)));
      (*0 0.127907*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_i) (0))) (F_max0_ge_0 (s V_quant_i))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.0542636 0.20155*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_quant_imagelength)) (F_check_ge (s V_quant_imagelength) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*0 0.127907*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_quant_imagelength)) (F_check_ge (s V_quant_imagelength) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_i)) (F_check_ge (0) (0)));
      (*0 1*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_quant_imagewidth)) (F_check_ge (s V_quant_imagewidth) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*0 0.116279*) F_binom_monotonic 1 (F_max0_ge_arg (-1 - s V_quant_i
                                                         + s V_quant_imagelength)) (F_check_ge (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength) (0))]
     (-(33 # 43) * s V_quant_i
      - (14 # 43) * s V_quant_i * s V_quant_imagelength
      + (7 # 43) * s V_quant_i * max0(-1 + s V_quant_i)
      + (3 # 43) * s V_quant_i * max0(-1 + s V_quant_imagelength)
      + (31 # 43) * s V_quant_i * max0(-s V_quant_i + s V_quant_imagelength)
      + (7 # 43) * s V_quant_i * max0(s V_quant_i)
      - (11 # 86) * s V_quant_i * max0(s V_quant_imagelength)
      + s V_quant_i * max0(s V_quant_imagewidth - s V_quant_j)
      + (7 # 43) * s V_quant_i^2 + (36 # 43) * s V_quant_imagelength
      + (14 # 43) * s V_quant_imagelength * max0(-1 - s V_quant_i
                                                 + s V_quant_imagelength)
      - (14 # 43) * s V_quant_imagelength * max0(-1 + s V_quant_imagelength)
      - (30 # 43) * s V_quant_imagelength * max0(-s V_quant_i
                                                 + s V_quant_imagelength)
      + (11 # 86) * s V_quant_imagelength * max0(s V_quant_i)
      + (3 # 86) * s V_quant_imagelength * max0(s V_quant_imagelength)
      - s V_quant_imagelength * max0(s V_quant_imagewidth - s V_quant_j)
      + (7 # 43) * s V_quant_imagelength^2 + s V_quant_z
      - (14 # 43) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
      - (7 # 43) * max0(-1 + s V_quant_i) * max0(s V_quant_i)
      - (8 # 43) * max0(-1 + s V_quant_imagelength) * max0(-s V_quant_i
                                                           + s V_quant_imagelength)
      - (11 # 43) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_i)
      + (22 # 43) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_imagelength)
      + (10 # 43) * max0(-s V_quant_i + s V_quant_imagelength)
      - (11 # 43) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_i)
      + (10 # 43) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
      + max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagewidth)
      + max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagewidth
                                                          - s V_quant_j)
      + (13 # 43) * max0(-s V_quant_i + s V_quant_imagelength)^2
      - (7 # 43) * max0(s V_quant_i)^2
      - (3 # 43) * max0(s V_quant_imagelength)
      - (3 # 86) * max0(s V_quant_imagelength)^2 <= z)%Q
   | 13 => (-(3 # 43) - (52 # 43) * s V_quant_i
            + (18 # 43) * s V_quant_i * s V_quant_imagelength
            + (1 # 43) * s V_quant_i * max0(-1 - s V_quant_i
                                            + s V_quant_imagelength)
            + (3 # 43) * s V_quant_i * max0(-1 + s V_quant_imagelength)
            + (25 # 86) * s V_quant_i * max0(-s V_quant_i
                                             + s V_quant_imagelength)
            - (19 # 121) * s V_quant_i * max0(s V_quant_imagelength)
            - (9 # 43) * s V_quant_i^2 + (113 # 86) * s V_quant_imagelength
            + (10 # 43) * s V_quant_imagelength * max0(-1 - s V_quant_i
                                                       + s V_quant_imagelength)
            - (3 # 43) * s V_quant_imagelength * max0(-1
                                                      + s V_quant_imagelength)
            - (17 # 43) * s V_quant_imagelength * max0(-s V_quant_i
                                                       + s V_quant_imagelength)
            - (21 # 86) * s V_quant_imagelength^2
            + s V_quant_imagewidth * max0(-s V_quant_i
                                          + s V_quant_imagelength)
            + s V_quant_z
            + (3 # 43) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(-1
                                                                    + s V_quant_imagelength)
            + (23 # 86) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength)
            - (19 # 121) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
            - (5 # 86) * max0(-1 + s V_quant_imagelength) * max0(-s V_quant_i
                                                                 + s V_quant_imagelength)
            + (14 # 73) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_imagelength)
            + (10 # 43) * max0(-s V_quant_i + s V_quant_imagelength)
            - (11 # 43) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_i)
            + (7 # 43) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
            + (11 # 43) * max0(s V_quant_i) <= z)%Q
   | 14 => (-(3 # 43) - (52 # 43) * s V_quant_i
            + (18 # 43) * s V_quant_i * s V_quant_imagelength
            + (1 # 43) * s V_quant_i * max0(-1 - s V_quant_i
                                            + s V_quant_imagelength)
            + (3 # 43) * s V_quant_i * max0(-1 + s V_quant_imagelength)
            + (25 # 86) * s V_quant_i * max0(-s V_quant_i
                                             + s V_quant_imagelength)
            - (19 # 121) * s V_quant_i * max0(s V_quant_imagelength)
            - (9 # 43) * s V_quant_i^2 + (113 # 86) * s V_quant_imagelength
            + (10 # 43) * s V_quant_imagelength * max0(-1 - s V_quant_i
                                                       + s V_quant_imagelength)
            - (3 # 43) * s V_quant_imagelength * max0(-1
                                                      + s V_quant_imagelength)
            - (17 # 43) * s V_quant_imagelength * max0(-s V_quant_i
                                                       + s V_quant_imagelength)
            - (21 # 86) * s V_quant_imagelength^2
            + s V_quant_imagewidth * max0(-s V_quant_i
                                          + s V_quant_imagelength)
            + s V_quant_z
            + (3 # 43) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(-1
                                                                    + s V_quant_imagelength)
            + (23 # 86) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength)
            - (19 # 121) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
            - (5 # 86) * max0(-1 + s V_quant_imagelength) * max0(-s V_quant_i
                                                                 + s V_quant_imagelength)
            + (14 # 73) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_imagelength)
            + (10 # 43) * max0(-s V_quant_i + s V_quant_imagelength)
            - (11 # 43) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_i)
            + (7 # 43) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
            + (11 # 43) * max0(s V_quant_i) <= z)%Q
   | 15 => (-(3 # 43) - (52 # 43) * s V_quant_i
            + (18 # 43) * s V_quant_i * s V_quant_imagelength
            + (1 # 43) * s V_quant_i * max0(-1 - s V_quant_i
                                            + s V_quant_imagelength)
            + (3 # 43) * s V_quant_i * max0(-1 + s V_quant_imagelength)
            + (25 # 86) * s V_quant_i * max0(-s V_quant_i
                                             + s V_quant_imagelength)
            - (19 # 121) * s V_quant_i * max0(s V_quant_imagelength)
            - s V_quant_i * max0(s V_quant_imagewidth)
            + s V_quant_i * max0(s V_quant_imagewidth - s V_quant_j)
            + (1 # 2) * s V_quant_i * max0(s V_quant_j)
            - (9 # 43) * s V_quant_i^2 + (113 # 86) * s V_quant_imagelength
            + (10 # 43) * s V_quant_imagelength * max0(-1 - s V_quant_i
                                                       + s V_quant_imagelength)
            - (3 # 43) * s V_quant_imagelength * max0(-1
                                                      + s V_quant_imagelength)
            - (17 # 43) * s V_quant_imagelength * max0(-s V_quant_i
                                                       + s V_quant_imagelength)
            + s V_quant_imagelength * max0(s V_quant_imagewidth)
            - s V_quant_imagelength * max0(s V_quant_imagewidth - s V_quant_j)
            - (1 # 2) * s V_quant_imagelength * max0(s V_quant_j)
            - (21 # 86) * s V_quant_imagelength^2
            + s V_quant_imagewidth * max0(-s V_quant_i
                                          + s V_quant_imagelength)
            - (1 # 4) * s V_quant_imagewidth * max0(s V_quant_imagewidth)
            + (1 # 4) * s V_quant_imagewidth * max0(s V_quant_imagewidth
                                                    - s V_quant_j)
            - (3 # 4) * s V_quant_j
            + (1 # 2) * s V_quant_j * max0(-1 - s V_quant_i
                                           + s V_quant_imagelength)
            - (1 # 2) * s V_quant_j * max0(-1 + s V_quant_imagelength)
            - (1 # 4) * s V_quant_j * max0(-s V_quant_i
                                           + s V_quant_imagelength)
            - (1 # 4) * s V_quant_j * max0(s V_quant_imagewidth)
            + (1 # 4) * s V_quant_j * max0(s V_quant_imagewidth - s V_quant_j)
            + (1 # 4) * s V_quant_j * max0(s V_quant_j)
            + (1 # 4) * s V_quant_j^2 + s V_quant_z
            + (3 # 43) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(-1
                                                                    + s V_quant_imagelength)
            + (23 # 86) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength)
            - (19 # 121) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
            - (1 # 4) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagewidth)
            + (1 # 4) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagewidth
                                                                    - s V_quant_j)
            - (1 # 4) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_j)
            - (5 # 86) * max0(-1 + s V_quant_imagelength) * max0(-s V_quant_i
                                                                 + s V_quant_imagelength)
            + (14 # 73) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_imagelength)
            + (1 # 2) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_imagewidth)
            - (1 # 2) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_imagewidth
                                                                - s V_quant_j)
            + (1 # 2) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_j)
            + (10 # 43) * max0(-s V_quant_i + s V_quant_imagelength)
            - (11 # 43) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_i)
            + (7 # 43) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
            - max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagewidth)
            + max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagewidth
                                                                - s V_quant_j)
            + (1 # 4) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_j)
            + (11 # 43) * max0(s V_quant_i)
            - (1 # 2) * max0(s V_quant_i) * max0(s V_quant_j)
            - (1 # 4) * max0(s V_quant_imagewidth) * max0(s V_quant_imagewidth
                                                          - s V_quant_j)
            + (1 # 4) * max0(s V_quant_imagewidth)^2
            + (1 # 4) * max0(s V_quant_j) - (1 # 4) * max0(s V_quant_j)^2 <= z)%Q
   | 16 => hints
     [(*-0.0348837 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.0348837 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.197674 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_max0_ge_0 (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_imagewidth) (0))) (F_max0_ge_0 (s V_quant_imagewidth))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_imagewidth) (0))) (F_max0_ge_0 (s V_quant_imagewidth))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_quant_imagewidth)) (F_check_ge (s V_quant_imagewidth) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_quant_imagewidth)) (F_check_ge (s V_quant_imagewidth) (0));
      (*0 0.0697674*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_i) (0))) (F_max0_ge_0 (s V_quant_i))]
     (-(3 # 43) - (52 # 43) * s V_quant_i
      + (18 # 43) * s V_quant_i * s V_quant_imagelength
      + (1 # 43) * s V_quant_i * max0(-1 - s V_quant_i
                                      + s V_quant_imagelength)
      + (3 # 43) * s V_quant_i * max0(-1 + s V_quant_imagelength)
      + (25 # 86) * s V_quant_i * max0(-s V_quant_i + s V_quant_imagelength)
      - (19 # 121) * s V_quant_i * max0(s V_quant_imagelength)
      - s V_quant_i * max0(s V_quant_imagewidth)
      + s V_quant_i * max0(s V_quant_imagewidth - s V_quant_j)
      + (1 # 2) * s V_quant_i * max0(s V_quant_j) - (9 # 43) * s V_quant_i^2
      + (113 # 86) * s V_quant_imagelength
      + (10 # 43) * s V_quant_imagelength * max0(-1 - s V_quant_i
                                                 + s V_quant_imagelength)
      - (3 # 43) * s V_quant_imagelength * max0(-1 + s V_quant_imagelength)
      - (17 # 43) * s V_quant_imagelength * max0(-s V_quant_i
                                                 + s V_quant_imagelength)
      + s V_quant_imagelength * max0(s V_quant_imagewidth)
      - s V_quant_imagelength * max0(s V_quant_imagewidth - s V_quant_j)
      - (1 # 2) * s V_quant_imagelength * max0(s V_quant_j)
      - (21 # 86) * s V_quant_imagelength^2
      + s V_quant_imagewidth * max0(-s V_quant_i + s V_quant_imagelength)
      - (1 # 4) * s V_quant_imagewidth * max0(s V_quant_imagewidth)
      + (1 # 4) * s V_quant_imagewidth * max0(s V_quant_imagewidth
                                              - s V_quant_j)
      - (3 # 4) * s V_quant_j
      + (1 # 2) * s V_quant_j * max0(-1 - s V_quant_i + s V_quant_imagelength)
      - (1 # 2) * s V_quant_j * max0(-1 + s V_quant_imagelength)
      - (1 # 4) * s V_quant_j * max0(-s V_quant_i + s V_quant_imagelength)
      - (1 # 4) * s V_quant_j * max0(s V_quant_imagewidth)
      + (1 # 4) * s V_quant_j * max0(s V_quant_imagewidth - s V_quant_j)
      + (1 # 4) * s V_quant_j * max0(s V_quant_j) + (1 # 4) * s V_quant_j^2
      + s V_quant_z
      + (3 # 43) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(-1
                                                                    + s V_quant_imagelength)
      + (23 # 86) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength)
      - (19 # 121) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
      - (1 # 4) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagewidth)
      + (1 # 4) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagewidth
                                                                    - s V_quant_j)
      - (1 # 4) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_j)
      - (5 # 86) * max0(-1 + s V_quant_imagelength) * max0(-s V_quant_i
                                                           + s V_quant_imagelength)
      + (14 # 73) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_imagelength)
      + (1 # 2) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_imagewidth)
      - (1 # 2) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_imagewidth
                                                          - s V_quant_j)
      + (1 # 2) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_j)
      + (10 # 43) * max0(-s V_quant_i + s V_quant_imagelength)
      - (11 # 43) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_i)
      + (7 # 43) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
      - max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagewidth)
      + max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagewidth
                                                          - s V_quant_j)
      + (1 # 4) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_j)
      + (11 # 43) * max0(s V_quant_i)
      - (1 # 2) * max0(s V_quant_i) * max0(s V_quant_j)
      - (1 # 4) * max0(s V_quant_imagewidth) * max0(s V_quant_imagewidth
                                                    - s V_quant_j)
      + (1 # 4) * max0(s V_quant_imagewidth)^2 + (1 # 4) * max0(s V_quant_j)
      - (1 # 4) * max0(s V_quant_j)^2 <= z)%Q
   | 17 => (-(3 # 43) - (55 # 43) * s V_quant_i
            + (18 # 43) * s V_quant_i * s V_quant_imagelength
            + (1 # 43) * s V_quant_i * max0(-1 - s V_quant_i
                                            + s V_quant_imagelength)
            + (3 # 43) * s V_quant_i * max0(-1 + s V_quant_imagelength)
            + (11 # 43) * s V_quant_i * max0(-s V_quant_i
                                             + s V_quant_imagelength)
            + (1 # 170) * s V_quant_i * max0(s V_quant_imagelength)
            - s V_quant_i * max0(s V_quant_imagewidth)
            + s V_quant_i * max0(s V_quant_imagewidth - s V_quant_j)
            + (1 # 2) * s V_quant_i * max0(s V_quant_j)
            - (9 # 43) * s V_quant_i^2 + (113 # 86) * s V_quant_imagelength
            + (10 # 43) * s V_quant_imagelength * max0(-1 - s V_quant_i
                                                       + s V_quant_imagelength)
            - (3 # 43) * s V_quant_imagelength * max0(-1
                                                      + s V_quant_imagelength)
            - (31 # 86) * s V_quant_imagelength * max0(-s V_quant_i
                                                       + s V_quant_imagelength)
            - (7 # 43) * s V_quant_imagelength * max0(s V_quant_imagelength)
            + s V_quant_imagelength * max0(s V_quant_imagewidth)
            - s V_quant_imagelength * max0(s V_quant_imagewidth - s V_quant_j)
            - (1 # 2) * s V_quant_imagelength * max0(s V_quant_j)
            - (21 # 86) * s V_quant_imagelength^2 + s V_quant_imagewidth
            - (1 # 4) * s V_quant_imagewidth * max0(-1 - s V_quant_i
                                                    + s V_quant_imagelength)
            + (1 # 2) * s V_quant_imagewidth * max0(-1
                                                    + s V_quant_imagelength)
            - (1 # 4) * s V_quant_imagewidth * max0(s V_quant_imagewidth)
            + (1 # 4) * s V_quant_imagewidth * max0(s V_quant_imagewidth
                                                    - s V_quant_j)
            - (3 # 4) * s V_quant_j
            + (1 # 2) * s V_quant_j * max0(-1 - s V_quant_i
                                           + s V_quant_imagelength)
            - (1 # 2) * s V_quant_j * max0(-1 + s V_quant_imagelength)
            - (1 # 4) * s V_quant_j * max0(-s V_quant_i
                                           + s V_quant_imagelength)
            - (1 # 4) * s V_quant_j * max0(s V_quant_imagewidth)
            + (1 # 4) * s V_quant_j * max0(s V_quant_imagewidth - s V_quant_j)
            + (1 # 4) * s V_quant_j * max0(s V_quant_j)
            + (1 # 4) * s V_quant_j^2 + s V_quant_z
            + (3 # 43) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(-1
                                                                    + s V_quant_imagelength)
            + (10 # 43) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength)
            - (14 # 73) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
            + (1 # 4) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagewidth
                                                                    - s V_quant_j)
            - (1 # 4) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_j)
            - (5 # 86) * max0(-1 + s V_quant_imagelength) * max0(-s V_quant_i
                                                                 + s V_quant_imagelength)
            + (14 # 73) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_imagelength)
            - (1 # 2) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_imagewidth
                                                                - s V_quant_j)
            + (1 # 2) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_j)
            + (17 # 86) * max0(-s V_quant_i + s V_quant_imagelength)
            - (11 # 43) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_i)
            + (31 # 86) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
            + max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagewidth
                                                                - s V_quant_j)
            + (1 # 4) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_j)
            + (14 # 43) * max0(s V_quant_i)
            - (1 # 2) * max0(s V_quant_i) * max0(s V_quant_j)
            - (3 # 86) * max0(s V_quant_imagelength)
            - max0(s V_quant_imagewidth)
            - (1 # 4) * max0(s V_quant_imagewidth) * max0(s V_quant_imagewidth
                                                          - s V_quant_j)
            + (1 # 4) * max0(s V_quant_imagewidth)^2
            + (1 # 4) * max0(s V_quant_j) - (1 # 4) * max0(s V_quant_j)^2 <= z)%Q
   | 18 => hints
     [(*0 0.0348837*) F_binom_monotonic 2 (F_max0_ge_arg (s V_quant_imagelength)) (F_check_ge (s V_quant_imagelength) (0));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-1 - s V_quant_i
                                                            + s V_quant_imagelength)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_quant_imagewidth
                                                                    + s V_quant_j)) (F_check_ge (0) (0)));
      (*-0.0697674 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + s V_quant_imagelength)) (F_check_ge (-1
                                                                    + s V_quant_imagelength) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.0581395 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_max0_ge_0 (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.232558 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-
                                                                    s V_quant_i
                                                                    + 
                                                                    s V_quant_imagelength)) (F_check_ge (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-s V_quant_i
                                                            + s V_quant_imagelength)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_imagewidth
                                                                    - s V_quant_j)) (F_check_ge (0) (0)));
      (*0 0.255814*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_i) (0))) (F_max0_ge_0 (s V_quant_i))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_i) (0))) (F_max0_ge_0 (s V_quant_i))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_j)) (F_check_ge (0) (0)));
      (*0 0.162791*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_quant_i)) (F_check_ge (s V_quant_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_i)) (F_check_ge (0) (0)));
      (*0 0.0348837*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_imagelength) (0))) (F_max0_ge_0 (s V_quant_imagelength))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*0 0.360465*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_quant_imagelength)) (F_check_ge (s V_quant_imagelength) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_quant_imagewidth
                                                                    + s V_quant_j) (0))) (F_max0_ge_0 (-
                                                                    s V_quant_imagewidth
                                                                    + s V_quant_j))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*0 0.25*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_imagewidth) (0))) (F_max0_ge_0 (s V_quant_imagewidth))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_imagewidth
                                                                    - s V_quant_j)) (F_check_ge (0) (0)));
      (*0 0.25*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_imagewidth) (0))) (F_max0_ge_0 (s V_quant_imagewidth))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_j)) (F_check_ge (0) (0)));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_quant_imagewidth)) (F_check_ge (s V_quant_imagewidth) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_imagewidth)) (F_check_ge (0) (0)));
      (*-0.75 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_imagewidth
                                                                    - s V_quant_j) (0))) (F_max0_ge_0 (s V_quant_imagewidth
                                                                    - s V_quant_j))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*0 0.25*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_quant_imagewidth
                                                                - s V_quant_j)) (F_check_ge (s V_quant_imagewidth
                                                                    - s V_quant_j) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_j)) (F_check_ge (0) (0)));
      (*0 0.25*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_j) (0))) (F_max0_ge_0 (s V_quant_j))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*0 0.25*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_j) (0))) (F_max0_ge_0 (s V_quant_j))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_imagewidth
                                                                    - s V_quant_j)) (F_check_ge (0) (0)));
      (*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_quant_j)) (F_check_ge (s V_quant_j) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_quant_j)) (F_check_ge (s V_quant_j) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_j) (0))) (F_max0_ge_0 (s V_quant_j));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_imagewidth
                                                               - s V_quant_j) (0))) (F_max0_ge_0 (s V_quant_imagewidth
                                                                    - s V_quant_j));
      (*-0.197674 0*) F_binom_monotonic 1 (F_max0_ge_arg (-s V_quant_i
                                                          + s V_quant_imagelength)) (F_check_ge (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength) (0));
      (*-0.162791 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (s V_quant_i) (0))) (F_max0_ge_0 (s V_quant_i))]
     (-(3 # 43) - (55 # 43) * s V_quant_i
      + (18 # 43) * s V_quant_i * s V_quant_imagelength
      + (1 # 43) * s V_quant_i * max0(-1 - s V_quant_i
                                      + s V_quant_imagelength)
      + (3 # 43) * s V_quant_i * max0(-1 + s V_quant_imagelength)
      + (11 # 43) * s V_quant_i * max0(-s V_quant_i + s V_quant_imagelength)
      + (1 # 170) * s V_quant_i * max0(s V_quant_imagelength)
      - s V_quant_i * max0(s V_quant_imagewidth)
      + s V_quant_i * max0(s V_quant_imagewidth - s V_quant_j)
      + (1 # 2) * s V_quant_i * max0(s V_quant_j) - (9 # 43) * s V_quant_i^2
      + (113 # 86) * s V_quant_imagelength
      + (10 # 43) * s V_quant_imagelength * max0(-1 - s V_quant_i
                                                 + s V_quant_imagelength)
      - (3 # 43) * s V_quant_imagelength * max0(-1 + s V_quant_imagelength)
      - (31 # 86) * s V_quant_imagelength * max0(-s V_quant_i
                                                 + s V_quant_imagelength)
      - (7 # 43) * s V_quant_imagelength * max0(s V_quant_imagelength)
      + s V_quant_imagelength * max0(s V_quant_imagewidth)
      - s V_quant_imagelength * max0(s V_quant_imagewidth - s V_quant_j)
      - (1 # 2) * s V_quant_imagelength * max0(s V_quant_j)
      - (21 # 86) * s V_quant_imagelength^2 + s V_quant_imagewidth
      - (1 # 4) * s V_quant_imagewidth * max0(-1 - s V_quant_i
                                              + s V_quant_imagelength)
      + (1 # 2) * s V_quant_imagewidth * max0(-1 + s V_quant_imagelength)
      - (1 # 4) * s V_quant_imagewidth * max0(s V_quant_imagewidth)
      + (1 # 4) * s V_quant_imagewidth * max0(s V_quant_imagewidth
                                              - s V_quant_j)
      - (3 # 4) * s V_quant_j
      + (1 # 2) * s V_quant_j * max0(-1 - s V_quant_i + s V_quant_imagelength)
      - (1 # 2) * s V_quant_j * max0(-1 + s V_quant_imagelength)
      - (1 # 4) * s V_quant_j * max0(-s V_quant_i + s V_quant_imagelength)
      - (1 # 4) * s V_quant_j * max0(s V_quant_imagewidth)
      + (1 # 4) * s V_quant_j * max0(s V_quant_imagewidth - s V_quant_j)
      + (1 # 4) * s V_quant_j * max0(s V_quant_j) + (1 # 4) * s V_quant_j^2
      + s V_quant_z
      + (3 # 43) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(-1
                                                                    + s V_quant_imagelength)
      + (10 # 43) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength)
      - (14 # 73) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
      + (1 # 4) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagewidth
                                                                    - s V_quant_j)
      - (1 # 4) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_j)
      - (5 # 86) * max0(-1 + s V_quant_imagelength) * max0(-s V_quant_i
                                                           + s V_quant_imagelength)
      + (14 # 73) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_imagelength)
      - (1 # 2) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_imagewidth
                                                          - s V_quant_j)
      + (1 # 2) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_j)
      + (17 # 86) * max0(-s V_quant_i + s V_quant_imagelength)
      - (11 # 43) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_i)
      + (31 # 86) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
      + max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagewidth
                                                          - s V_quant_j)
      + (1 # 4) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_j)
      + (14 # 43) * max0(s V_quant_i)
      - (1 # 2) * max0(s V_quant_i) * max0(s V_quant_j)
      - (3 # 86) * max0(s V_quant_imagelength) - max0(s V_quant_imagewidth)
      - (1 # 4) * max0(s V_quant_imagewidth) * max0(s V_quant_imagewidth
                                                    - s V_quant_j)
      + (1 # 4) * max0(s V_quant_imagewidth)^2 + (1 # 4) * max0(s V_quant_j)
      - (1 # 4) * max0(s V_quant_j)^2 <= z)%Q
   | 19 => (-(3 # 43) - (113 # 86) * s V_quant_i
            + (18 # 43) * s V_quant_i * s V_quant_imagelength
            - (9 # 43) * s V_quant_i * max0(-1 - s V_quant_i
                                            + s V_quant_imagelength)
            + (11 # 86) * s V_quant_i * max0(-1 + s V_quant_imagelength)
            + (7 # 43) * s V_quant_i * max0(s V_quant_i)
            + (1 # 170) * s V_quant_i * max0(s V_quant_imagelength)
            - s V_quant_i * max0(s V_quant_imagewidth)
            + s V_quant_i * max0(s V_quant_imagewidth - s V_quant_j)
            - (16 # 43) * s V_quant_i^2 + (127 # 86) * s V_quant_imagelength
            + (23 # 43) * s V_quant_imagelength * max0(-1 - s V_quant_i
                                                       + s V_quant_imagelength)
            - (11 # 86) * s V_quant_imagelength * max0(-1
                                                       + s V_quant_imagelength)
            - (17 # 86) * s V_quant_imagelength * max0(s V_quant_imagelength)
            + s V_quant_imagelength * max0(s V_quant_imagewidth)
            - s V_quant_imagelength * max0(s V_quant_imagewidth - s V_quant_j)
            - (1 # 2) * s V_quant_imagelength * max0(s V_quant_j)
            - (9 # 43) * s V_quant_imagelength^2
            + (1 # 2) * s V_quant_imagewidth * max0(-1
                                                    + s V_quant_imagelength)
            - (1 # 4) * s V_quant_j
            - (1 # 4) * s V_quant_j * max0(s V_quant_imagewidth)
            + (1 # 4) * s V_quant_j^2 + s V_quant_z
            - (3 # 43) * max0(-1 - s V_quant_i + s V_quant_imagelength)
            - (14 # 73) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
            + max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagewidth
                                                                    - 
                                                                    s V_quant_j)
            + (14 # 73) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_imagelength)
            - (1 # 2) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_imagewidth
                                                                - s V_quant_j)
            + (7 # 43) * max0(s V_quant_i) - max0(s V_quant_imagewidth)
            + (1 # 4) * max0(s V_quant_imagewidth) * max0(s V_quant_j)
            + max0(s V_quant_imagewidth - s V_quant_j)
            + (3 # 4) * max0(s V_quant_j) - (1 # 4) * max0(s V_quant_j)^2 <= z)%Q
   | 20 => (-(3 # 43) - (113 # 86) * s V_quant_i
            + (18 # 43) * s V_quant_i * s V_quant_imagelength
            - (9 # 43) * s V_quant_i * max0(-1 - s V_quant_i
                                            + s V_quant_imagelength)
            + (11 # 86) * s V_quant_i * max0(-1 + s V_quant_imagelength)
            + (7 # 43) * s V_quant_i * max0(s V_quant_i)
            + (1 # 170) * s V_quant_i * max0(s V_quant_imagelength)
            - s V_quant_i * max0(s V_quant_imagewidth)
            + s V_quant_i * max0(s V_quant_imagewidth - s V_quant_j)
            - (16 # 43) * s V_quant_i^2 + (127 # 86) * s V_quant_imagelength
            + (23 # 43) * s V_quant_imagelength * max0(-1 - s V_quant_i
                                                       + s V_quant_imagelength)
            - (11 # 86) * s V_quant_imagelength * max0(-1
                                                       + s V_quant_imagelength)
            - (17 # 86) * s V_quant_imagelength * max0(s V_quant_imagelength)
            + s V_quant_imagelength * max0(s V_quant_imagewidth)
            - s V_quant_imagelength * max0(s V_quant_imagewidth - s V_quant_j)
            - (1 # 2) * s V_quant_imagelength * max0(s V_quant_j)
            - (9 # 43) * s V_quant_imagelength^2
            + (1 # 2) * s V_quant_imagewidth * max0(-1
                                                    + s V_quant_imagelength)
            - (1 # 4) * s V_quant_j
            - (1 # 4) * s V_quant_j * max0(s V_quant_imagewidth)
            + (1 # 4) * s V_quant_j^2 + s V_quant_z
            - (3 # 43) * max0(-1 - s V_quant_i + s V_quant_imagelength)
            - (14 # 73) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
            + max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagewidth
                                                                    - 
                                                                    s V_quant_j)
            + (14 # 73) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_imagelength)
            - (1 # 2) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_imagewidth
                                                                - s V_quant_j)
            + (7 # 43) * max0(s V_quant_i) - max0(s V_quant_imagewidth)
            + (1 # 4) * max0(s V_quant_imagewidth) * max0(s V_quant_j)
            + max0(s V_quant_imagewidth - s V_quant_j)
            + (3 # 4) * max0(s V_quant_j) - (1 # 4) * max0(s V_quant_j)^2 <= z)%Q
   | 21 => (-(3 # 43) - (113 # 86) * s V_quant_i
            + (18 # 43) * s V_quant_i * s V_quant_imagelength
            - (9 # 43) * s V_quant_i * max0(-1 - s V_quant_i
                                            + s V_quant_imagelength)
            + (11 # 86) * s V_quant_i * max0(-1 + s V_quant_imagelength)
            + (7 # 43) * s V_quant_i * max0(s V_quant_i)
            + (1 # 170) * s V_quant_i * max0(s V_quant_imagelength)
            - s V_quant_i * max0(s V_quant_imagewidth)
            + s V_quant_i * max0(s V_quant_imagewidth - s V_quant_j)
            - (16 # 43) * s V_quant_i^2 + (127 # 86) * s V_quant_imagelength
            + (23 # 43) * s V_quant_imagelength * max0(-1 - s V_quant_i
                                                       + s V_quant_imagelength)
            - (11 # 86) * s V_quant_imagelength * max0(-1
                                                       + s V_quant_imagelength)
            - (17 # 86) * s V_quant_imagelength * max0(s V_quant_imagelength)
            + s V_quant_imagelength * max0(s V_quant_imagewidth)
            - s V_quant_imagelength * max0(s V_quant_imagewidth - s V_quant_j)
            - (1 # 2) * s V_quant_imagelength * max0(s V_quant_j)
            - (9 # 43) * s V_quant_imagelength^2
            + (1 # 2) * s V_quant_imagewidth * max0(-1
                                                    + s V_quant_imagelength)
            - (1 # 4) * s V_quant_j
            - (1 # 4) * s V_quant_j * max0(s V_quant_imagewidth)
            + (1 # 4) * s V_quant_j^2 + s V_quant_z
            - (3 # 43) * max0(-1 - s V_quant_i + s V_quant_imagelength)
            - (14 # 73) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
            + max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagewidth
                                                                    - 
                                                                    s V_quant_j)
            + (14 # 73) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_imagelength)
            - (1 # 2) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_imagewidth
                                                                - s V_quant_j)
            + (7 # 43) * max0(s V_quant_i) - max0(s V_quant_imagewidth)
            + (1 # 4) * max0(s V_quant_imagewidth) * max0(s V_quant_j)
            + max0(s V_quant_imagewidth - s V_quant_j)
            + (3 # 4) * max0(s V_quant_j) - (1 # 4) * max0(s V_quant_j)^2 <= z)%Q
   | 22 => ((75 # 86) - (49 # 86) * s V_quant_i
            + (18 # 43) * s V_quant_i * s V_quant_imagelength
            + (7 # 43) * s V_quant_i * max0(-1 + s V_quant_i)
            + (11 # 86) * s V_quant_i * max0(-1 + s V_quant_imagelength)
            - (9 # 43) * s V_quant_i * max0(-s V_quant_i
                                            + s V_quant_imagelength)
            + (1 # 170) * s V_quant_i * max0(s V_quant_imagelength)
            - s V_quant_i * max0(s V_quant_imagewidth)
            + s V_quant_i * max0(s V_quant_imagewidth - s V_quant_j)
            - (16 # 43) * s V_quant_i^2 + (91 # 86) * s V_quant_imagelength
            - (11 # 86) * s V_quant_imagelength * max0(-1
                                                       + s V_quant_imagelength)
            + (23 # 43) * s V_quant_imagelength * max0(-s V_quant_i
                                                       + s V_quant_imagelength)
            - (17 # 86) * s V_quant_imagelength * max0(s V_quant_imagelength)
            + s V_quant_imagelength * max0(s V_quant_imagewidth)
            - s V_quant_imagelength * max0(s V_quant_imagewidth - s V_quant_j)
            - (1 # 2) * s V_quant_imagelength * max0(s V_quant_j)
            - (9 # 43) * s V_quant_imagelength^2
            + (1 # 2) * s V_quant_imagewidth * max0(-1
                                                    + s V_quant_imagelength)
            - (1 # 4) * s V_quant_j
            - (1 # 4) * s V_quant_j * max0(s V_quant_imagewidth)
            + (1 # 4) * s V_quant_j^2 + s V_quant_z
            - (11 # 86) * max0(-1 + s V_quant_imagelength)
            + (14 # 73) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_imagelength)
            - (1 # 2) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_imagewidth
                                                                - s V_quant_j)
            + (6 # 43) * max0(-s V_quant_i + s V_quant_imagelength)
            - (14 # 73) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
            + max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagewidth
                                                                - s V_quant_j)
            - (1 # 170) * max0(s V_quant_imagelength)
            + (1 # 4) * max0(s V_quant_imagewidth) * max0(s V_quant_j)
            + (3 # 4) * max0(s V_quant_j) - (1 # 4) * max0(s V_quant_j)^2 <= z)%Q
   | 23 => ((75 # 86) - (49 # 86) * s V_quant_i
            + (18 # 43) * s V_quant_i * s V_quant_imagelength
            + (7 # 43) * s V_quant_i * max0(-1 + s V_quant_i)
            + (11 # 86) * s V_quant_i * max0(-1 + s V_quant_imagelength)
            - (9 # 43) * s V_quant_i * max0(-s V_quant_i
                                            + s V_quant_imagelength)
            + (1 # 170) * s V_quant_i * max0(s V_quant_imagelength)
            - s V_quant_i * max0(s V_quant_imagewidth)
            + s V_quant_i * max0(s V_quant_imagewidth - s V_quant_j)
            - (16 # 43) * s V_quant_i^2 + (91 # 86) * s V_quant_imagelength
            - (11 # 86) * s V_quant_imagelength * max0(-1
                                                       + s V_quant_imagelength)
            + (23 # 43) * s V_quant_imagelength * max0(-s V_quant_i
                                                       + s V_quant_imagelength)
            - (17 # 86) * s V_quant_imagelength * max0(s V_quant_imagelength)
            + s V_quant_imagelength * max0(s V_quant_imagewidth)
            - s V_quant_imagelength * max0(s V_quant_imagewidth - s V_quant_j)
            - (1 # 2) * s V_quant_imagelength * max0(s V_quant_j)
            - (9 # 43) * s V_quant_imagelength^2
            + (1 # 2) * s V_quant_imagewidth * max0(-1
                                                    + s V_quant_imagelength)
            - (1 # 4) * s V_quant_j
            - (1 # 4) * s V_quant_j * max0(s V_quant_imagewidth)
            + (1 # 4) * s V_quant_j^2 + s V_quant_z
            - (11 # 86) * max0(-1 + s V_quant_imagelength)
            + (14 # 73) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_imagelength)
            - (1 # 2) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_imagewidth
                                                                - s V_quant_j)
            + (6 # 43) * max0(-s V_quant_i + s V_quant_imagelength)
            - (14 # 73) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
            + max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagewidth
                                                                - s V_quant_j)
            - (1 # 170) * max0(s V_quant_imagelength)
            + (1 # 4) * max0(s V_quant_imagewidth) * max0(s V_quant_j)
            + (3 # 4) * max0(s V_quant_j) - (1 # 4) * max0(s V_quant_j)^2 <= z)%Q
   | 24 => ((75 # 86) - (49 # 86) * s V_quant_i
            + (18 # 43) * s V_quant_i * s V_quant_imagelength
            + (7 # 43) * s V_quant_i * max0(-1 + s V_quant_i)
            + (11 # 86) * s V_quant_i * max0(-1 + s V_quant_imagelength)
            - (9 # 43) * s V_quant_i * max0(-s V_quant_i
                                            + s V_quant_imagelength)
            + (1 # 170) * s V_quant_i * max0(s V_quant_imagelength)
            - s V_quant_i * max0(s V_quant_imagewidth)
            + s V_quant_i * max0(s V_quant_imagewidth - s V_quant_j)
            - (16 # 43) * s V_quant_i^2 + (91 # 86) * s V_quant_imagelength
            - (11 # 86) * s V_quant_imagelength * max0(-1
                                                       + s V_quant_imagelength)
            + (23 # 43) * s V_quant_imagelength * max0(-s V_quant_i
                                                       + s V_quant_imagelength)
            - (17 # 86) * s V_quant_imagelength * max0(s V_quant_imagelength)
            + s V_quant_imagelength * max0(s V_quant_imagewidth)
            - s V_quant_imagelength * max0(s V_quant_imagewidth - s V_quant_j)
            - (1 # 2) * s V_quant_imagelength * max0(s V_quant_j)
            - (9 # 43) * s V_quant_imagelength^2
            + (1 # 2) * s V_quant_imagewidth * max0(-1
                                                    + s V_quant_imagelength)
            - (1 # 4) * s V_quant_j
            - (1 # 4) * s V_quant_j * max0(s V_quant_imagewidth)
            + (1 # 4) * s V_quant_j^2 + s V_quant_z
            - (11 # 86) * max0(-1 + s V_quant_imagelength)
            + (14 # 73) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_imagelength)
            - (1 # 2) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_imagewidth
                                                                - s V_quant_j)
            + (6 # 43) * max0(-s V_quant_i + s V_quant_imagelength)
            - (14 # 73) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
            + max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagewidth
                                                                - s V_quant_j)
            - (1 # 170) * max0(s V_quant_imagelength)
            + (1 # 4) * max0(s V_quant_imagewidth) * max0(s V_quant_j)
            + (3 # 4) * max0(s V_quant_j) - (1 # 4) * max0(s V_quant_j)^2 <= z)%Q
   | 25 => hints
     [(*-0.0639535 0*) F_binom_monotonic 2 (F_max0_ge_arg (-1
                                                           + s V_quant_imagelength)) (F_check_ge (-1
                                                                    + s V_quant_imagelength) (0));
      (*0 0.372093*) F_binom_monotonic 2 (F_max0_ge_arg (-s V_quant_i
                                                         + s V_quant_imagelength)) (F_check_ge (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength) (0));
      (*-0.162791 0*) F_binom_monotonic 2 (F_max0_ge_arg (s V_quant_i)) (F_check_ge (s V_quant_i) (0));
      (*-0.0639535 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (s V_quant_imagelength) (0))) (F_max0_ge_0 (s V_quant_imagelength));
      (*-0.25 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (s V_quant_j) (0))) (F_max0_ge_0 (s V_quant_j));
      (*0 0.162791*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    s V_quant_i)) (F_check_ge (-1
                                                                    + s V_quant_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_i)) (F_check_ge (0) (0)));
      (*-0.0639535 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_quant_imagelength) (0))) (F_max0_ge_0 (-1
                                                                    + s V_quant_imagelength))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.127907 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    s V_quant_imagelength)) (F_check_ge (-1
                                                                    + s V_quant_imagelength) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_i)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                + s V_quant_imagelength)) (F_check_ge (-1
                                                                    + s V_quant_imagelength) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_j)) (F_check_ge (0) (0)));
      (*-0.674419 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_max0_ge_0 (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_max0_ge_0 (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_imagewidth)) (F_check_ge (0) (0)));
      (*-0.186047 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-
                                                                    s V_quant_i
                                                                    + 
                                                                    s V_quant_imagelength)) (F_check_ge (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*0 0.133721*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-
                                                                    s V_quant_i
                                                                    + 
                                                                    s V_quant_imagelength)) (F_check_ge (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.127907 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_quant_i)) (F_check_ge (s V_quant_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.255814 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_quant_i)) (F_check_ge (s V_quant_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.319767 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_imagelength) (0))) (F_max0_ge_0 (s V_quant_imagelength))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.55814 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_imagelength) (0))) (F_max0_ge_0 (s V_quant_imagelength))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.325581 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_quant_imagelength)) (F_check_ge (s V_quant_imagelength) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.0988372 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_quant_imagelength)) (F_check_ge (s V_quant_imagelength) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_imagewidth
                                                                    - s V_quant_j) (0))) (F_max0_ge_0 (s V_quant_imagewidth
                                                                    - s V_quant_j))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_j) (0))) (F_max0_ge_0 (s V_quant_j))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_quant_j)) (F_check_ge (s V_quant_j) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_imagewidth)) (F_check_ge (0) (0)));
      (*0 0.127907*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_i) (0))) (F_max0_ge_0 (s V_quant_i));
      (*0 0.27907*) F_binom_monotonic 1 (F_max0_ge_arg (-s V_quant_i
                                                        + s V_quant_imagelength)) (F_check_ge (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength) (0))]
     (-(11 # 86) - (49 # 86) * s V_quant_i
      + (18 # 43) * s V_quant_i * s V_quant_imagelength
      + (7 # 43) * s V_quant_i * max0(-1 + s V_quant_i)
      + (11 # 86) * s V_quant_i * max0(-1 + s V_quant_imagelength)
      - (9 # 43) * s V_quant_i * max0(-s V_quant_i + s V_quant_imagelength)
      + (1 # 170) * s V_quant_i * max0(s V_quant_imagelength)
      - s V_quant_i * max0(s V_quant_imagewidth)
      + s V_quant_i * max0(s V_quant_imagewidth - s V_quant_j)
      - (16 # 43) * s V_quant_i^2 + (91 # 86) * s V_quant_imagelength
      - (11 # 86) * s V_quant_imagelength * max0(-1 + s V_quant_imagelength)
      + (23 # 43) * s V_quant_imagelength * max0(-s V_quant_i
                                                 + s V_quant_imagelength)
      - (17 # 86) * s V_quant_imagelength * max0(s V_quant_imagelength)
      + s V_quant_imagelength * max0(s V_quant_imagewidth)
      - s V_quant_imagelength * max0(s V_quant_imagewidth - s V_quant_j)
      - (1 # 2) * s V_quant_imagelength * max0(s V_quant_j)
      - (9 # 43) * s V_quant_imagelength^2
      + (1 # 2) * s V_quant_imagewidth * max0(-1 + s V_quant_imagelength)
      - (1 # 4) * s V_quant_j
      - (1 # 4) * s V_quant_j * max0(s V_quant_imagewidth)
      + (1 # 4) * s V_quant_j^2 + s V_quant_z
      - (11 # 86) * max0(-1 + s V_quant_imagelength)
      + (14 # 73) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_imagelength)
      - (1 # 2) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_imagewidth
                                                          - s V_quant_j)
      + (6 # 43) * max0(-s V_quant_i + s V_quant_imagelength)
      - (14 # 73) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
      + max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagewidth
                                                          - s V_quant_j)
      - (1 # 170) * max0(s V_quant_imagelength)
      + (1 # 4) * max0(s V_quant_imagewidth) * max0(s V_quant_j)
      + (3 # 4) * max0(s V_quant_j) - (1 # 4) * max0(s V_quant_j)^2 <= z)%Q
   | 26 => (-(3 # 43) - (113 # 86) * s V_quant_i
            + (18 # 43) * s V_quant_i * s V_quant_imagelength
            - (9 # 43) * s V_quant_i * max0(-1 - s V_quant_i
                                            + s V_quant_imagelength)
            + (11 # 86) * s V_quant_i * max0(-1 + s V_quant_imagelength)
            + (7 # 43) * s V_quant_i * max0(s V_quant_i)
            + (1 # 170) * s V_quant_i * max0(s V_quant_imagelength)
            - s V_quant_i * max0(s V_quant_imagewidth)
            + s V_quant_i * max0(s V_quant_imagewidth - s V_quant_j)
            - (16 # 43) * s V_quant_i^2 + (127 # 86) * s V_quant_imagelength
            + (23 # 43) * s V_quant_imagelength * max0(-1 - s V_quant_i
                                                       + s V_quant_imagelength)
            - (11 # 86) * s V_quant_imagelength * max0(-1
                                                       + s V_quant_imagelength)
            - (17 # 86) * s V_quant_imagelength * max0(s V_quant_imagelength)
            + s V_quant_imagelength * max0(s V_quant_imagewidth)
            - s V_quant_imagelength * max0(s V_quant_imagewidth - s V_quant_j)
            - (1 # 2) * s V_quant_imagelength * max0(s V_quant_j)
            - (9 # 43) * s V_quant_imagelength^2
            + (1 # 2) * s V_quant_imagewidth * max0(-1
                                                    + s V_quant_imagelength)
            - (1 # 4) * s V_quant_j
            - (1 # 4) * s V_quant_j * max0(s V_quant_imagewidth)
            + (1 # 4) * s V_quant_j^2 + s V_quant_z
            - (3 # 43) * max0(-1 - s V_quant_i + s V_quant_imagelength)
            - (14 # 73) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
            + max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagewidth
                                                                    - 
                                                                    s V_quant_j)
            + (14 # 73) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_imagelength)
            - (1 # 2) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_imagewidth
                                                                - s V_quant_j)
            + (7 # 43) * max0(s V_quant_i) - max0(s V_quant_imagewidth)
            + (1 # 4) * max0(s V_quant_imagewidth) * max0(s V_quant_j)
            + max0(s V_quant_imagewidth - s V_quant_j)
            + (3 # 4) * max0(s V_quant_j) - (1 # 4) * max0(s V_quant_j)^2 <= z)%Q
   | 27 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_max0_ge_0 (-1 - s V_quant_i + s V_quant_imagelength);
      (*-0.534884 0*) F_binom_monotonic 2 (F_max0_ge_arg (-1 - s V_quant_i
                                                          + s V_quant_imagelength)) (F_check_ge (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength) (0));
      (*-0.325581 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_max0_ge_0 (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength));
      (*-0.162791 0*) F_binom_monotonic 2 (F_max0_ge_arg (s V_quant_i)) (F_check_ge (s V_quant_i) (0));
      (*-0.25 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (s V_quant_j) (0))) (F_max0_ge_0 (s V_quant_j));
      (*-0.534884 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_max0_ge_0 (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.19186 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_max0_ge_0 (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_max0_ge_0 (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_imagewidth)) (F_check_ge (0) (0)));
      (*-0.325581 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    - 
                                                                    s V_quant_i
                                                                    + 
                                                                    s V_quant_imagelength)) (F_check_ge (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_i)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                              - s V_quant_i
                                                              + s V_quant_imagelength)) (F_check_ge (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_imagewidth
                                                                    - s V_quant_j)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-1 - s V_quant_i
                                                            + s V_quant_imagelength)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_imagewidth)) (F_check_ge (0) (0)));
      (*0 0.127907*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_quant_imagelength) (0))) (F_max0_ge_0 (-1
                                                                    + s V_quant_imagelength))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*0 0.19186*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                   + 
                                                                   s V_quant_imagelength)) (F_check_ge (-1
                                                                    + s V_quant_imagelength) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                + s V_quant_imagelength)) (F_check_ge (-1
                                                                    + s V_quant_imagelength) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_j)) (F_check_ge (0) (0)));
      (*-0.325581 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_max0_ge_0 (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_i)) (F_check_ge (0) (0)));
      (*0 0.127907*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-
                                                                    s V_quant_i
                                                                    + 
                                                                    s V_quant_imagelength)) (F_check_ge (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.325581 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-
                                                                    s V_quant_i
                                                                    + 
                                                                    s V_quant_imagelength)) (F_check_ge (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.197674 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-
                                                                    s V_quant_i
                                                                    + 
                                                                    s V_quant_imagelength)) (F_check_ge (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.325581 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_i) (0))) (F_max0_ge_0 (s V_quant_i))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.162791 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_i) (0))) (F_max0_ge_0 (s V_quant_i))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_i)) (F_check_ge (0) (0)));
      (*-0.325581 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_quant_i)) (F_check_ge (s V_quant_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.197674 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_imagelength) (0))) (F_max0_ge_0 (s V_quant_imagelength))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_imagewidth
                                                                    - s V_quant_j) (0))) (F_max0_ge_0 (s V_quant_imagewidth
                                                                    - s V_quant_j))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_j) (0))) (F_max0_ge_0 (s V_quant_j))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_quant_j)) (F_check_ge (s V_quant_j) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_imagewidth)) (F_check_ge (0) (0)));
      (*-0.197674 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_max0_ge_0 (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength))]
     (-(3 # 43) - (113 # 86) * s V_quant_i
      + (18 # 43) * s V_quant_i * s V_quant_imagelength
      - (9 # 43) * s V_quant_i * max0(-1 - s V_quant_i
                                      + s V_quant_imagelength)
      + (11 # 86) * s V_quant_i * max0(-1 + s V_quant_imagelength)
      + (7 # 43) * s V_quant_i * max0(s V_quant_i)
      + (1 # 170) * s V_quant_i * max0(s V_quant_imagelength)
      - s V_quant_i * max0(s V_quant_imagewidth)
      + s V_quant_i * max0(s V_quant_imagewidth - s V_quant_j)
      - (16 # 43) * s V_quant_i^2 + (127 # 86) * s V_quant_imagelength
      + (23 # 43) * s V_quant_imagelength * max0(-1 - s V_quant_i
                                                 + s V_quant_imagelength)
      - (11 # 86) * s V_quant_imagelength * max0(-1 + s V_quant_imagelength)
      - (17 # 86) * s V_quant_imagelength * max0(s V_quant_imagelength)
      + s V_quant_imagelength * max0(s V_quant_imagewidth)
      - s V_quant_imagelength * max0(s V_quant_imagewidth - s V_quant_j)
      - (1 # 2) * s V_quant_imagelength * max0(s V_quant_j)
      - (9 # 43) * s V_quant_imagelength^2
      + (1 # 2) * s V_quant_imagewidth * max0(-1 + s V_quant_imagelength)
      - (1 # 4) * s V_quant_j
      - (1 # 4) * s V_quant_j * max0(s V_quant_imagewidth)
      + (1 # 4) * s V_quant_j^2 + s V_quant_z
      - (3 # 43) * max0(-1 - s V_quant_i + s V_quant_imagelength)
      - (14 # 73) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
      + max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagewidth
                                                              - s V_quant_j)
      + (14 # 73) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_imagelength)
      - (1 # 2) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_imagewidth
                                                          - s V_quant_j)
      + (7 # 43) * max0(s V_quant_i) - max0(s V_quant_imagewidth)
      + (1 # 4) * max0(s V_quant_imagewidth) * max0(s V_quant_j)
      + max0(s V_quant_imagewidth - s V_quant_j)
      + (3 # 4) * max0(s V_quant_j) - (1 # 4) * max0(s V_quant_j)^2 <= z)%Q
   | 28 => hints
     [(*-0.25 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (s V_quant_imagewidth
                                                                  - s V_quant_j) (0))) (F_max0_ge_0 (s V_quant_imagewidth
                                                                    - s V_quant_j));
      (*0 0.25*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                - s V_quant_i
                                                                + s V_quant_imagelength)) (F_check_ge (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_imagewidth
                                                                    - s V_quant_j)) (F_check_ge (0) (0)));
      (*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_quant_imagelength) (0))) (F_max0_ge_0 (-1
                                                                    + s V_quant_imagelength))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_imagewidth
                                                                    - s V_quant_j)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_quant_imagewidth
                                                                    - s V_quant_j) (0))) (F_max0_ge_0 (-1
                                                                    + s V_quant_imagewidth
                                                                    - s V_quant_j))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-1.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-s V_quant_i
                                                                 + s V_quant_imagelength)) (F_check_ge (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_imagewidth
                                                                    - s V_quant_j)) (F_check_ge (0) (0)));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-s V_quant_i
                                                                 + s V_quant_imagelength)) (F_check_ge (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_j)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_quant_i)) (F_check_ge (s V_quant_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_imagewidth
                                                                    - s V_quant_j)) (F_check_ge (0) (0)));
      (*-0.5 -9.48484e-12*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_imagewidth) (0))) (F_max0_ge_0 (s V_quant_imagewidth))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_imagewidth
                                                                    - s V_quant_j)) (F_check_ge (0) (0)));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_imagewidth
                                                                    - s V_quant_j) (0))) (F_max0_ge_0 (s V_quant_imagewidth
                                                                    - s V_quant_j))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_imagewidth
                                                                    - s V_quant_j) (0))) (F_max0_ge_0 (s V_quant_imagewidth
                                                                    - s V_quant_j))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_i)) (F_check_ge (0) (0)));
      (*0 0.25*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_quant_imagewidth
                                                                - s V_quant_j)) (F_check_ge (s V_quant_imagewidth
                                                                    - s V_quant_j) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_imagewidth)) (F_check_ge (0) (0)));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_quant_imagewidth
                                                                 - s V_quant_j)) (F_check_ge (s V_quant_imagewidth
                                                                    - s V_quant_j) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_imagewidth
                                                                    - s V_quant_j)) (F_check_ge (0) (0)));
      (*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_j) (0))) (F_max0_ge_0 (s V_quant_j))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*0 0.25*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_j) (0))) (F_max0_ge_0 (s V_quant_j))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_j)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_quant_imagewidth)) (F_check_ge (s V_quant_imagewidth) (0))]
     (-(3 # 43) - (55 # 43) * s V_quant_i
      + (18 # 43) * s V_quant_i * s V_quant_imagelength
      + (1 # 43) * s V_quant_i * max0(-1 - s V_quant_i
                                      + s V_quant_imagelength)
      + (3 # 43) * s V_quant_i * max0(-1 + s V_quant_imagelength)
      + (11 # 43) * s V_quant_i * max0(-s V_quant_i + s V_quant_imagelength)
      + (1 # 170) * s V_quant_i * max0(s V_quant_imagelength)
      - s V_quant_i * max0(s V_quant_imagewidth)
      + s V_quant_i * max0(s V_quant_imagewidth - s V_quant_j)
      + (1 # 2) * s V_quant_i * max0(s V_quant_j) - (9 # 43) * s V_quant_i^2
      + (113 # 86) * s V_quant_imagelength
      + (10 # 43) * s V_quant_imagelength * max0(-1 - s V_quant_i
                                                 + s V_quant_imagelength)
      - (3 # 43) * s V_quant_imagelength * max0(-1 + s V_quant_imagelength)
      - (31 # 86) * s V_quant_imagelength * max0(-s V_quant_i
                                                 + s V_quant_imagelength)
      - (7 # 43) * s V_quant_imagelength * max0(s V_quant_imagelength)
      + s V_quant_imagelength * max0(s V_quant_imagewidth)
      - s V_quant_imagelength * max0(s V_quant_imagewidth - s V_quant_j)
      - (1 # 2) * s V_quant_imagelength * max0(s V_quant_j)
      - (21 # 86) * s V_quant_imagelength^2 + s V_quant_imagewidth
      - (1 # 4) * s V_quant_imagewidth * max0(-1 - s V_quant_i
                                              + s V_quant_imagelength)
      + (1 # 2) * s V_quant_imagewidth * max0(-1 + s V_quant_imagelength)
      - (1 # 4) * s V_quant_imagewidth * max0(s V_quant_imagewidth)
      + (1 # 4) * s V_quant_imagewidth * max0(s V_quant_imagewidth
                                              - s V_quant_j)
      - (3 # 4) * s V_quant_j
      + (1 # 2) * s V_quant_j * max0(-1 - s V_quant_i + s V_quant_imagelength)
      - (1 # 2) * s V_quant_j * max0(-1 + s V_quant_imagelength)
      - (1 # 4) * s V_quant_j * max0(-s V_quant_i + s V_quant_imagelength)
      - (1 # 4) * s V_quant_j * max0(s V_quant_imagewidth)
      + (1 # 4) * s V_quant_j * max0(s V_quant_imagewidth - s V_quant_j)
      + (1 # 4) * s V_quant_j * max0(s V_quant_j) + (1 # 4) * s V_quant_j^2
      + s V_quant_z
      + (3 # 43) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(-1
                                                                    + s V_quant_imagelength)
      + (10 # 43) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength)
      - (14 # 73) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
      + (1 # 4) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagewidth
                                                                    - s V_quant_j)
      - (1 # 4) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_j)
      - (5 # 86) * max0(-1 + s V_quant_imagelength) * max0(-s V_quant_i
                                                           + s V_quant_imagelength)
      + (14 # 73) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_imagelength)
      - (1 # 2) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_imagewidth
                                                          - s V_quant_j)
      + (1 # 2) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_j)
      + (17 # 86) * max0(-s V_quant_i + s V_quant_imagelength)
      - (11 # 43) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_i)
      + (31 # 86) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
      + max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagewidth
                                                          - s V_quant_j)
      + (1 # 4) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_j)
      + (14 # 43) * max0(s V_quant_i)
      - (1 # 2) * max0(s V_quant_i) * max0(s V_quant_j)
      - (3 # 86) * max0(s V_quant_imagelength) - max0(s V_quant_imagewidth)
      - (1 # 4) * max0(s V_quant_imagewidth) * max0(s V_quant_imagewidth
                                                    - s V_quant_j)
      + (1 # 4) * max0(s V_quant_imagewidth)^2 + (1 # 4) * max0(s V_quant_j)
      - (1 # 4) * max0(s V_quant_j)^2 <= z)%Q
   | 29 => (-(3 # 43) - (55 # 43) * s V_quant_i
            + (18 # 43) * s V_quant_i * s V_quant_imagelength
            + (1 # 43) * s V_quant_i * max0(-1 - s V_quant_i
                                            + s V_quant_imagelength)
            + (3 # 43) * s V_quant_i * max0(-1 + s V_quant_imagelength)
            + (11 # 43) * s V_quant_i * max0(-s V_quant_i
                                             + s V_quant_imagelength)
            + (1 # 170) * s V_quant_i * max0(s V_quant_imagelength)
            - s V_quant_i * max0(s V_quant_imagewidth)
            + (1 # 4) * s V_quant_i * max0(s V_quant_j)
            - (9 # 43) * s V_quant_i^2 + (113 # 86) * s V_quant_imagelength
            + (10 # 43) * s V_quant_imagelength * max0(-1 - s V_quant_i
                                                       + s V_quant_imagelength)
            - (3 # 43) * s V_quant_imagelength * max0(-1
                                                      + s V_quant_imagelength)
            - (31 # 86) * s V_quant_imagelength * max0(-s V_quant_i
                                                       + s V_quant_imagelength)
            - (7 # 43) * s V_quant_imagelength * max0(s V_quant_imagelength)
            + s V_quant_imagelength * max0(s V_quant_imagewidth)
            - (1 # 4) * s V_quant_imagelength * max0(s V_quant_j)
            - (21 # 86) * s V_quant_imagelength^2
            + (7 # 4) * s V_quant_imagewidth
            + (1 # 2) * s V_quant_imagewidth * s V_quant_j
            - (3 # 4) * s V_quant_imagewidth * max0(-1 - s V_quant_i
                                                    + s V_quant_imagelength)
            + (1 # 2) * s V_quant_imagewidth * max0(-1
                                                    + s V_quant_imagelength)
            - (1 # 4) * s V_quant_imagewidth * max0(-s V_quant_i
                                                    + s V_quant_imagelength)
            - (1 # 2) * s V_quant_imagewidth * max0(s V_quant_i)
            - (1 # 4) * s V_quant_imagewidth^2 - s V_quant_j
            + (1 # 2) * s V_quant_j * max0(-1 - s V_quant_i
                                           + s V_quant_imagelength)
            - (1 # 2) * s V_quant_j * max0(-1 + s V_quant_imagelength)
            + (1 # 2) * s V_quant_j * max0(s V_quant_i)
            - (1 # 2) * s V_quant_j * max0(s V_quant_imagewidth)
            + s V_quant_z
            + (1 # 2) * max0(-1 - s V_quant_i + s V_quant_imagelength)
            + (3 # 43) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(-1
                                                                    + s V_quant_imagelength)
            + (1 # 2) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(-1
                                                                    + s V_quant_imagewidth
                                                                    - s V_quant_j)
            + (10 # 43) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength)
            - (14 # 73) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
            + (1 # 4) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_j)
            - (5 # 86) * max0(-1 + s V_quant_imagelength) * max0(-s V_quant_i
                                                                 + s V_quant_imagelength)
            + (14 # 73) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_imagelength)
            + (1 # 2) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_j)
            + (17 # 86) * max0(-s V_quant_i + s V_quant_imagelength)
            - (11 # 43) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_i)
            + (31 # 86) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
            + (14 # 43) * max0(s V_quant_i)
            - (1 # 2) * max0(s V_quant_i) * max0(s V_quant_j)
            - (3 # 86) * max0(s V_quant_imagelength)
            - (3 # 2) * max0(s V_quant_imagewidth)
            + (1 # 4) * max0(s V_quant_imagewidth)^2
            + (1 # 4) * max0(s V_quant_j) <= z)%Q
   | 30 => (-(3 # 43) - (55 # 43) * s V_quant_i
            + (18 # 43) * s V_quant_i * s V_quant_imagelength
            + (1 # 43) * s V_quant_i * max0(-1 - s V_quant_i
                                            + s V_quant_imagelength)
            + (3 # 43) * s V_quant_i * max0(-1 + s V_quant_imagelength)
            + (11 # 43) * s V_quant_i * max0(-s V_quant_i
                                             + s V_quant_imagelength)
            + (1 # 170) * s V_quant_i * max0(s V_quant_imagelength)
            - s V_quant_i * max0(s V_quant_imagewidth)
            + (1 # 4) * s V_quant_i * max0(s V_quant_j)
            - (9 # 43) * s V_quant_i^2 + (113 # 86) * s V_quant_imagelength
            + (10 # 43) * s V_quant_imagelength * max0(-1 - s V_quant_i
                                                       + s V_quant_imagelength)
            - (3 # 43) * s V_quant_imagelength * max0(-1
                                                      + s V_quant_imagelength)
            - (31 # 86) * s V_quant_imagelength * max0(-s V_quant_i
                                                       + s V_quant_imagelength)
            - (7 # 43) * s V_quant_imagelength * max0(s V_quant_imagelength)
            + s V_quant_imagelength * max0(s V_quant_imagewidth)
            - (1 # 4) * s V_quant_imagelength * max0(s V_quant_j)
            - (21 # 86) * s V_quant_imagelength^2
            + (7 # 4) * s V_quant_imagewidth
            + (1 # 2) * s V_quant_imagewidth * s V_quant_j
            - (3 # 4) * s V_quant_imagewidth * max0(-1 - s V_quant_i
                                                    + s V_quant_imagelength)
            + (1 # 2) * s V_quant_imagewidth * max0(-1
                                                    + s V_quant_imagelength)
            - (1 # 4) * s V_quant_imagewidth * max0(-s V_quant_i
                                                    + s V_quant_imagelength)
            - (1 # 2) * s V_quant_imagewidth * max0(s V_quant_i)
            - (1 # 4) * s V_quant_imagewidth^2 - s V_quant_j
            + (1 # 2) * s V_quant_j * max0(-1 - s V_quant_i
                                           + s V_quant_imagelength)
            - (1 # 2) * s V_quant_j * max0(-1 + s V_quant_imagelength)
            + (1 # 2) * s V_quant_j * max0(s V_quant_i)
            - (1 # 2) * s V_quant_j * max0(s V_quant_imagewidth)
            + s V_quant_z
            + (1 # 2) * max0(-1 - s V_quant_i + s V_quant_imagelength)
            + (3 # 43) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(-1
                                                                    + s V_quant_imagelength)
            + (1 # 2) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(-1
                                                                    + s V_quant_imagewidth
                                                                    - s V_quant_j)
            + (10 # 43) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength)
            - (14 # 73) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
            + (1 # 4) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_j)
            - (5 # 86) * max0(-1 + s V_quant_imagelength) * max0(-s V_quant_i
                                                                 + s V_quant_imagelength)
            + (14 # 73) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_imagelength)
            + (1 # 2) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_j)
            + (17 # 86) * max0(-s V_quant_i + s V_quant_imagelength)
            - (11 # 43) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_i)
            + (31 # 86) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
            + (14 # 43) * max0(s V_quant_i)
            - (1 # 2) * max0(s V_quant_i) * max0(s V_quant_j)
            - (3 # 86) * max0(s V_quant_imagelength)
            - (3 # 2) * max0(s V_quant_imagewidth)
            + (1 # 4) * max0(s V_quant_imagewidth)^2
            + (1 # 4) * max0(s V_quant_j) <= z)%Q
   | 31 => (-(3 # 43) - (55 # 43) * s V_quant_i
            + (18 # 43) * s V_quant_i * s V_quant_imagelength
            + (1 # 43) * s V_quant_i * max0(-1 - s V_quant_i
                                            + s V_quant_imagelength)
            + (3 # 43) * s V_quant_i * max0(-1 + s V_quant_imagelength)
            + (11 # 43) * s V_quant_i * max0(-s V_quant_i
                                             + s V_quant_imagelength)
            + (1 # 170) * s V_quant_i * max0(s V_quant_imagelength)
            - s V_quant_i * max0(s V_quant_imagewidth)
            + (1 # 4) * s V_quant_i * max0(s V_quant_j)
            - (9 # 43) * s V_quant_i^2 + (113 # 86) * s V_quant_imagelength
            + (10 # 43) * s V_quant_imagelength * max0(-1 - s V_quant_i
                                                       + s V_quant_imagelength)
            - (3 # 43) * s V_quant_imagelength * max0(-1
                                                      + s V_quant_imagelength)
            - (31 # 86) * s V_quant_imagelength * max0(-s V_quant_i
                                                       + s V_quant_imagelength)
            - (7 # 43) * s V_quant_imagelength * max0(s V_quant_imagelength)
            + s V_quant_imagelength * max0(s V_quant_imagewidth)
            - (1 # 4) * s V_quant_imagelength * max0(s V_quant_j)
            - (21 # 86) * s V_quant_imagelength^2
            + (7 # 4) * s V_quant_imagewidth
            + (1 # 2) * s V_quant_imagewidth * s V_quant_j
            - (3 # 4) * s V_quant_imagewidth * max0(-1 - s V_quant_i
                                                    + s V_quant_imagelength)
            + (1 # 2) * s V_quant_imagewidth * max0(-1
                                                    + s V_quant_imagelength)
            - (1 # 4) * s V_quant_imagewidth * max0(-s V_quant_i
                                                    + s V_quant_imagelength)
            - (1 # 2) * s V_quant_imagewidth * max0(s V_quant_i)
            - (1 # 4) * s V_quant_imagewidth^2 - s V_quant_j
            + (1 # 2) * s V_quant_j * max0(-1 - s V_quant_i
                                           + s V_quant_imagelength)
            - (1 # 2) * s V_quant_j * max0(-1 + s V_quant_imagelength)
            + (1 # 2) * s V_quant_j * max0(s V_quant_i)
            - (1 # 2) * s V_quant_j * max0(s V_quant_imagewidth)
            + s V_quant_z
            + (1 # 2) * max0(-1 - s V_quant_i + s V_quant_imagelength)
            + (3 # 43) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(-1
                                                                    + s V_quant_imagelength)
            + (1 # 2) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(-1
                                                                    + s V_quant_imagewidth
                                                                    - s V_quant_j)
            + (10 # 43) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength)
            - (14 # 73) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
            + (1 # 4) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_j)
            - (5 # 86) * max0(-1 + s V_quant_imagelength) * max0(-s V_quant_i
                                                                 + s V_quant_imagelength)
            + (14 # 73) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_imagelength)
            + (1 # 2) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_j)
            + (17 # 86) * max0(-s V_quant_i + s V_quant_imagelength)
            - (11 # 43) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_i)
            + (31 # 86) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
            + (14 # 43) * max0(s V_quant_i)
            - (1 # 2) * max0(s V_quant_i) * max0(s V_quant_j)
            - (3 # 86) * max0(s V_quant_imagelength)
            - (3 # 2) * max0(s V_quant_imagewidth)
            + (1 # 4) * max0(s V_quant_imagewidth)^2
            + (1 # 4) * max0(s V_quant_j) <= z)%Q
   | 32 => (-(3 # 43) - (55 # 43) * s V_quant_i
            + (18 # 43) * s V_quant_i * s V_quant_imagelength
            + (1 # 43) * s V_quant_i * max0(-1 - s V_quant_i
                                            + s V_quant_imagelength)
            + (3 # 43) * s V_quant_i * max0(-1 + s V_quant_imagelength)
            + (11 # 43) * s V_quant_i * max0(-s V_quant_i
                                             + s V_quant_imagelength)
            + (1 # 170) * s V_quant_i * max0(s V_quant_imagelength)
            - s V_quant_i * max0(s V_quant_imagewidth)
            + (1 # 4) * s V_quant_i * max0(s V_quant_j)
            - (9 # 43) * s V_quant_i^2 + (113 # 86) * s V_quant_imagelength
            + (10 # 43) * s V_quant_imagelength * max0(-1 - s V_quant_i
                                                       + s V_quant_imagelength)
            - (3 # 43) * s V_quant_imagelength * max0(-1
                                                      + s V_quant_imagelength)
            - (31 # 86) * s V_quant_imagelength * max0(-s V_quant_i
                                                       + s V_quant_imagelength)
            - (7 # 43) * s V_quant_imagelength * max0(s V_quant_imagelength)
            + s V_quant_imagelength * max0(s V_quant_imagewidth)
            - (1 # 4) * s V_quant_imagelength * max0(s V_quant_j)
            - (21 # 86) * s V_quant_imagelength^2
            + (7 # 4) * s V_quant_imagewidth
            + (1 # 2) * s V_quant_imagewidth * s V_quant_j
            - (3 # 4) * s V_quant_imagewidth * max0(-1 - s V_quant_i
                                                    + s V_quant_imagelength)
            + (1 # 2) * s V_quant_imagewidth * max0(-1
                                                    + s V_quant_imagelength)
            - (1 # 4) * s V_quant_imagewidth * max0(-s V_quant_i
                                                    + s V_quant_imagelength)
            - (1 # 2) * s V_quant_imagewidth * max0(s V_quant_i)
            - (1 # 4) * s V_quant_imagewidth^2 - s V_quant_j
            + (1 # 2) * s V_quant_j * max0(-1 - s V_quant_i
                                           + s V_quant_imagelength)
            - (1 # 2) * s V_quant_j * max0(-1 + s V_quant_imagelength)
            + (1 # 2) * s V_quant_j * max0(s V_quant_i)
            - (1 # 2) * s V_quant_j * max0(s V_quant_imagewidth)
            + s V_quant_z
            + (1 # 2) * max0(-1 - s V_quant_i + s V_quant_imagelength)
            + (3 # 43) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(-1
                                                                    + s V_quant_imagelength)
            + (1 # 2) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(-1
                                                                    + s V_quant_imagewidth
                                                                    - s V_quant_j)
            + (10 # 43) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength)
            - (14 # 73) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
            + (1 # 4) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_j)
            - (5 # 86) * max0(-1 + s V_quant_imagelength) * max0(-s V_quant_i
                                                                 + s V_quant_imagelength)
            + (14 # 73) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_imagelength)
            + (1 # 2) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_j)
            + (17 # 86) * max0(-s V_quant_i + s V_quant_imagelength)
            - (11 # 43) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_i)
            + (31 # 86) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
            + (14 # 43) * max0(s V_quant_i)
            - (1 # 2) * max0(s V_quant_i) * max0(s V_quant_j)
            - (3 # 86) * max0(s V_quant_imagelength)
            - (3 # 2) * max0(s V_quant_imagewidth)
            + (1 # 4) * max0(s V_quant_imagewidth)^2
            + (1 # 4) * max0(s V_quant_j) <= z)%Q
   | 33 => (-(3 # 43) - (55 # 43) * s V_quant_i
            + (18 # 43) * s V_quant_i * s V_quant_imagelength
            + (1 # 43) * s V_quant_i * max0(-1 - s V_quant_i
                                            + s V_quant_imagelength)
            + (3 # 43) * s V_quant_i * max0(-1 + s V_quant_imagelength)
            + (11 # 43) * s V_quant_i * max0(-s V_quant_i
                                             + s V_quant_imagelength)
            + (1 # 170) * s V_quant_i * max0(s V_quant_imagelength)
            - s V_quant_i * max0(s V_quant_imagewidth)
            + (1 # 4) * s V_quant_i * max0(s V_quant_j)
            - (9 # 43) * s V_quant_i^2 + (113 # 86) * s V_quant_imagelength
            + (10 # 43) * s V_quant_imagelength * max0(-1 - s V_quant_i
                                                       + s V_quant_imagelength)
            - (3 # 43) * s V_quant_imagelength * max0(-1
                                                      + s V_quant_imagelength)
            - (31 # 86) * s V_quant_imagelength * max0(-s V_quant_i
                                                       + s V_quant_imagelength)
            - (7 # 43) * s V_quant_imagelength * max0(s V_quant_imagelength)
            + s V_quant_imagelength * max0(s V_quant_imagewidth)
            - (1 # 4) * s V_quant_imagelength * max0(s V_quant_j)
            - (21 # 86) * s V_quant_imagelength^2
            + (7 # 4) * s V_quant_imagewidth
            + (1 # 2) * s V_quant_imagewidth * s V_quant_j
            - (3 # 4) * s V_quant_imagewidth * max0(-1 - s V_quant_i
                                                    + s V_quant_imagelength)
            + (1 # 2) * s V_quant_imagewidth * max0(-1
                                                    + s V_quant_imagelength)
            - (1 # 4) * s V_quant_imagewidth * max0(-s V_quant_i
                                                    + s V_quant_imagelength)
            - (1 # 2) * s V_quant_imagewidth * max0(s V_quant_i)
            - (1 # 4) * s V_quant_imagewidth^2 - s V_quant_j
            + (1 # 2) * s V_quant_j * max0(-1 - s V_quant_i
                                           + s V_quant_imagelength)
            - (1 # 2) * s V_quant_j * max0(-1 + s V_quant_imagelength)
            + (1 # 2) * s V_quant_j * max0(s V_quant_i)
            - (1 # 2) * s V_quant_j * max0(s V_quant_imagewidth)
            + s V_quant_z
            + (1 # 2) * max0(-1 - s V_quant_i + s V_quant_imagelength)
            + (3 # 43) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(-1
                                                                    + s V_quant_imagelength)
            + (1 # 2) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(-1
                                                                    + s V_quant_imagewidth
                                                                    - s V_quant_j)
            + (10 # 43) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength)
            - (14 # 73) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
            + (1 # 4) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_j)
            - (5 # 86) * max0(-1 + s V_quant_imagelength) * max0(-s V_quant_i
                                                                 + s V_quant_imagelength)
            + (14 # 73) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_imagelength)
            + (1 # 2) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_j)
            + (17 # 86) * max0(-s V_quant_i + s V_quant_imagelength)
            - (11 # 43) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_i)
            + (31 # 86) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
            + (14 # 43) * max0(s V_quant_i)
            - (1 # 2) * max0(s V_quant_i) * max0(s V_quant_j)
            - (3 # 86) * max0(s V_quant_imagelength)
            - (3 # 2) * max0(s V_quant_imagewidth)
            + (1 # 4) * max0(s V_quant_imagewidth)^2
            + (1 # 4) * max0(s V_quant_j) <= z)%Q
   | 34 => ((40 # 43) - (55 # 43) * s V_quant_i
            + (18 # 43) * s V_quant_i * s V_quant_imagelength
            + (1 # 43) * s V_quant_i * max0(-1 - s V_quant_i
                                            + s V_quant_imagelength)
            + (3 # 43) * s V_quant_i * max0(-1 + s V_quant_imagelength)
            + (1 # 4) * s V_quant_i * max0(-1 + s V_quant_j)
            + (11 # 43) * s V_quant_i * max0(-s V_quant_i
                                             + s V_quant_imagelength)
            + (1 # 170) * s V_quant_i * max0(s V_quant_imagelength)
            - s V_quant_i * max0(s V_quant_imagewidth)
            - (9 # 43) * s V_quant_i^2 + (113 # 86) * s V_quant_imagelength
            + (10 # 43) * s V_quant_imagelength * max0(-1 - s V_quant_i
                                                       + s V_quant_imagelength)
            - (3 # 43) * s V_quant_imagelength * max0(-1
                                                      + s V_quant_imagelength)
            - (1 # 4) * s V_quant_imagelength * max0(-1 + s V_quant_j)
            - (31 # 86) * s V_quant_imagelength * max0(-s V_quant_i
                                                       + s V_quant_imagelength)
            - (7 # 43) * s V_quant_imagelength * max0(s V_quant_imagelength)
            + s V_quant_imagelength * max0(s V_quant_imagewidth)
            - (21 # 86) * s V_quant_imagelength^2
            + (5 # 4) * s V_quant_imagewidth
            + (1 # 2) * s V_quant_imagewidth * s V_quant_j
            - (3 # 4) * s V_quant_imagewidth * max0(-1 - s V_quant_i
                                                    + s V_quant_imagelength)
            + (1 # 2) * s V_quant_imagewidth * max0(-1
                                                    + s V_quant_imagelength)
            - (1 # 4) * s V_quant_imagewidth * max0(-s V_quant_i
                                                    + s V_quant_imagelength)
            - (1 # 2) * s V_quant_imagewidth * max0(s V_quant_i)
            - (1 # 4) * s V_quant_imagewidth^2 - s V_quant_j
            + (1 # 2) * s V_quant_j * max0(-1 - s V_quant_i
                                           + s V_quant_imagelength)
            - (1 # 2) * s V_quant_j * max0(-1 + s V_quant_imagelength)
            + (1 # 2) * s V_quant_j * max0(s V_quant_i)
            - (1 # 2) * s V_quant_j * max0(s V_quant_imagewidth)
            + s V_quant_z
            + (3 # 43) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(-1
                                                                    + s V_quant_imagelength)
            + (1 # 4) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(-1
                                                                    + s V_quant_j)
            + (10 # 43) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength)
            - (14 # 73) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
            + (1 # 2) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagewidth
                                                                    - s V_quant_j)
            + (1 # 2) * max0(-1 + s V_quant_imagelength)
            + (1 # 2) * max0(-1 + s V_quant_imagelength) * max0(-1
                                                                + s V_quant_j)
            - (5 # 86) * max0(-1 + s V_quant_imagelength) * max0(-s V_quant_i
                                                                 + s V_quant_imagelength)
            + (14 # 73) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_imagelength)
            + (1 # 4) * max0(-1 + s V_quant_j)
            - (1 # 2) * max0(-1 + s V_quant_j) * max0(s V_quant_i)
            + (17 # 86) * max0(-s V_quant_i + s V_quant_imagelength)
            - (11 # 43) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_i)
            + (31 # 86) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
            - (15 # 86) * max0(s V_quant_i)
            - (3 # 86) * max0(s V_quant_imagelength)
            - max0(s V_quant_imagewidth)
            + (1 # 4) * max0(s V_quant_imagewidth)^2 <= z)%Q
   | 35 => ((40 # 43) - (55 # 43) * s V_quant_i
            + (18 # 43) * s V_quant_i * s V_quant_imagelength
            + (1 # 43) * s V_quant_i * max0(-1 - s V_quant_i
                                            + s V_quant_imagelength)
            + (3 # 43) * s V_quant_i * max0(-1 + s V_quant_imagelength)
            + (1 # 4) * s V_quant_i * max0(-1 + s V_quant_j)
            + (11 # 43) * s V_quant_i * max0(-s V_quant_i
                                             + s V_quant_imagelength)
            + (1 # 170) * s V_quant_i * max0(s V_quant_imagelength)
            - s V_quant_i * max0(s V_quant_imagewidth)
            - (9 # 43) * s V_quant_i^2 + (113 # 86) * s V_quant_imagelength
            + (10 # 43) * s V_quant_imagelength * max0(-1 - s V_quant_i
                                                       + s V_quant_imagelength)
            - (3 # 43) * s V_quant_imagelength * max0(-1
                                                      + s V_quant_imagelength)
            - (1 # 4) * s V_quant_imagelength * max0(-1 + s V_quant_j)
            - (31 # 86) * s V_quant_imagelength * max0(-s V_quant_i
                                                       + s V_quant_imagelength)
            - (7 # 43) * s V_quant_imagelength * max0(s V_quant_imagelength)
            + s V_quant_imagelength * max0(s V_quant_imagewidth)
            - (21 # 86) * s V_quant_imagelength^2
            + (5 # 4) * s V_quant_imagewidth
            + (1 # 2) * s V_quant_imagewidth * s V_quant_j
            - (3 # 4) * s V_quant_imagewidth * max0(-1 - s V_quant_i
                                                    + s V_quant_imagelength)
            + (1 # 2) * s V_quant_imagewidth * max0(-1
                                                    + s V_quant_imagelength)
            - (1 # 4) * s V_quant_imagewidth * max0(-s V_quant_i
                                                    + s V_quant_imagelength)
            - (1 # 2) * s V_quant_imagewidth * max0(s V_quant_i)
            - (1 # 4) * s V_quant_imagewidth^2 - s V_quant_j
            + (1 # 2) * s V_quant_j * max0(-1 - s V_quant_i
                                           + s V_quant_imagelength)
            - (1 # 2) * s V_quant_j * max0(-1 + s V_quant_imagelength)
            + (1 # 2) * s V_quant_j * max0(s V_quant_i)
            - (1 # 2) * s V_quant_j * max0(s V_quant_imagewidth)
            + s V_quant_z
            + (3 # 43) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(-1
                                                                    + s V_quant_imagelength)
            + (1 # 4) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(-1
                                                                    + s V_quant_j)
            + (10 # 43) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength)
            - (14 # 73) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
            + (1 # 2) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagewidth
                                                                    - s V_quant_j)
            + (1 # 2) * max0(-1 + s V_quant_imagelength)
            + (1 # 2) * max0(-1 + s V_quant_imagelength) * max0(-1
                                                                + s V_quant_j)
            - (5 # 86) * max0(-1 + s V_quant_imagelength) * max0(-s V_quant_i
                                                                 + s V_quant_imagelength)
            + (14 # 73) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_imagelength)
            + (1 # 4) * max0(-1 + s V_quant_j)
            - (1 # 2) * max0(-1 + s V_quant_j) * max0(s V_quant_i)
            + (17 # 86) * max0(-s V_quant_i + s V_quant_imagelength)
            - (11 # 43) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_i)
            + (31 # 86) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
            - (15 # 86) * max0(s V_quant_i)
            - (3 # 86) * max0(s V_quant_imagelength)
            - max0(s V_quant_imagewidth)
            + (1 # 4) * max0(s V_quant_imagewidth)^2 <= z)%Q
   | 36 => ((40 # 43) - (55 # 43) * s V_quant_i
            + (18 # 43) * s V_quant_i * s V_quant_imagelength
            + (1 # 43) * s V_quant_i * max0(-1 - s V_quant_i
                                            + s V_quant_imagelength)
            + (3 # 43) * s V_quant_i * max0(-1 + s V_quant_imagelength)
            + (1 # 4) * s V_quant_i * max0(-1 + s V_quant_j)
            + (11 # 43) * s V_quant_i * max0(-s V_quant_i
                                             + s V_quant_imagelength)
            + (1 # 170) * s V_quant_i * max0(s V_quant_imagelength)
            - s V_quant_i * max0(s V_quant_imagewidth)
            - (9 # 43) * s V_quant_i^2 + (113 # 86) * s V_quant_imagelength
            + (10 # 43) * s V_quant_imagelength * max0(-1 - s V_quant_i
                                                       + s V_quant_imagelength)
            - (3 # 43) * s V_quant_imagelength * max0(-1
                                                      + s V_quant_imagelength)
            - (1 # 4) * s V_quant_imagelength * max0(-1 + s V_quant_j)
            - (31 # 86) * s V_quant_imagelength * max0(-s V_quant_i
                                                       + s V_quant_imagelength)
            - (7 # 43) * s V_quant_imagelength * max0(s V_quant_imagelength)
            + s V_quant_imagelength * max0(s V_quant_imagewidth)
            - (21 # 86) * s V_quant_imagelength^2
            + (5 # 4) * s V_quant_imagewidth
            + (1 # 2) * s V_quant_imagewidth * s V_quant_j
            - (3 # 4) * s V_quant_imagewidth * max0(-1 - s V_quant_i
                                                    + s V_quant_imagelength)
            + (1 # 2) * s V_quant_imagewidth * max0(-1
                                                    + s V_quant_imagelength)
            - (1 # 4) * s V_quant_imagewidth * max0(-s V_quant_i
                                                    + s V_quant_imagelength)
            - (1 # 2) * s V_quant_imagewidth * max0(s V_quant_i)
            - (1 # 4) * s V_quant_imagewidth^2 - s V_quant_j
            + (1 # 2) * s V_quant_j * max0(-1 - s V_quant_i
                                           + s V_quant_imagelength)
            - (1 # 2) * s V_quant_j * max0(-1 + s V_quant_imagelength)
            + (1 # 2) * s V_quant_j * max0(s V_quant_i)
            - (1 # 2) * s V_quant_j * max0(s V_quant_imagewidth)
            + s V_quant_z
            + (3 # 43) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(-1
                                                                    + s V_quant_imagelength)
            + (1 # 4) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(-1
                                                                    + s V_quant_j)
            + (10 # 43) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength)
            - (14 # 73) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
            + (1 # 2) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagewidth
                                                                    - s V_quant_j)
            + (1 # 2) * max0(-1 + s V_quant_imagelength)
            + (1 # 2) * max0(-1 + s V_quant_imagelength) * max0(-1
                                                                + s V_quant_j)
            - (5 # 86) * max0(-1 + s V_quant_imagelength) * max0(-s V_quant_i
                                                                 + s V_quant_imagelength)
            + (14 # 73) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_imagelength)
            + (1 # 4) * max0(-1 + s V_quant_j)
            - (1 # 2) * max0(-1 + s V_quant_j) * max0(s V_quant_i)
            + (17 # 86) * max0(-s V_quant_i + s V_quant_imagelength)
            - (11 # 43) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_i)
            + (31 # 86) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
            - (15 # 86) * max0(s V_quant_i)
            - (3 # 86) * max0(s V_quant_imagelength)
            - max0(s V_quant_imagewidth)
            + (1 # 4) * max0(s V_quant_imagewidth)^2 <= z)%Q
   | 37 => hints
     [(*-0.25 0*) F_binom_monotonic 2 (F_max0_ge_arg (-1 - s V_quant_i
                                                      + s V_quant_imagelength)) (F_check_ge (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength) (0));
      (*-0.25 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (-s V_quant_i
                                                                  + s V_quant_imagelength) (0))) (F_max0_ge_0 (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength));
      (*-0.25 0*) F_binom_monotonic 2 (F_max0_ge_arg (s V_quant_imagelength)) (F_check_ge (s V_quant_imagelength) (0));
      (*-0.25 0*) F_binom_monotonic 2 (F_max0_ge_arg (s V_quant_imagewidth
                                                      - s V_quant_j)) (F_check_ge (s V_quant_imagewidth
                                                                    - s V_quant_j) (0));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_max0_ge_0 (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_max0_ge_0 (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_quant_imagewidth)) (F_check_ge (0) (0)));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_max0_ge_0 (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                 - s V_quant_i
                                                                 + s V_quant_imagelength)) (F_check_ge (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_quant_j)) (F_check_ge (0) (0)));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                 - s V_quant_i
                                                                 + s V_quant_imagelength)) (F_check_ge (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_i)) (F_check_ge (0) (0)));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                 - s V_quant_i
                                                                 + s V_quant_imagelength)) (F_check_ge (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_imagewidth
                                                                    - s V_quant_j)) (F_check_ge (0) (0)));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                 - s V_quant_i
                                                                 + s V_quant_imagelength)) (F_check_ge (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_j)) (F_check_ge (0) (0)));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_quant_imagelength) (0))) (F_max0_ge_0 (-1
                                                                    + s V_quant_imagelength))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_quant_imagelength) (0))) (F_max0_ge_0 (-1
                                                                    + s V_quant_imagelength))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_i)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_quant_imagelength) (0))) (F_max0_ge_0 (-1
                                                                    + s V_quant_imagelength))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_j)) (F_check_ge (0) (0)));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                 + s V_quant_imagelength)) (F_check_ge (-1
                                                                    + s V_quant_imagelength) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                + s V_quant_imagelength)) (F_check_ge (-1
                                                                    + s V_quant_imagelength) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_quant_imagewidth)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                + s V_quant_imagelength)) (F_check_ge (-1
                                                                    + s V_quant_imagelength) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_quant_imagewidth) (0))) (F_max0_ge_0 (-1
                                                                    + s V_quant_imagewidth))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                + s V_quant_imagewidth)) (F_check_ge (-1
                                                                    + s V_quant_imagewidth) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                + s V_quant_imagewidth)) (F_check_ge (-1
                                                                    + s V_quant_imagewidth) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_i)) (F_check_ge (0) (0)));
      (*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_quant_j) (0))) (F_max0_ge_0 (-1
                                                                    + s V_quant_j))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_i)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                + s V_quant_j)) (F_check_ge (-1
                                                                    + s V_quant_j) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_max0_ge_0 (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_i)) (F_check_ge (0) (0)));
      (*-1.25 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_max0_ge_0 (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_imagewidth
                                                                    - s V_quant_j)) (F_check_ge (0) (0)));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_max0_ge_0 (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_j)) (F_check_ge (0) (0)));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-s V_quant_i
                                                                 + s V_quant_imagelength)) (F_check_ge (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-s V_quant_i
                                                                 + s V_quant_imagelength)) (F_check_ge (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_i) (0))) (F_max0_ge_0 (s V_quant_i))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_i) (0))) (F_max0_ge_0 (s V_quant_i))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_quant_imagewidth)) (F_check_ge (0) (0)));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_i) (0))) (F_max0_ge_0 (s V_quant_i))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_quant_i)) (F_check_ge (s V_quant_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_quant_i)) (F_check_ge (s V_quant_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_quant_i)) (F_check_ge (s V_quant_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_j)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_imagelength) (0))) (F_max0_ge_0 (s V_quant_imagelength))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_imagelength) (0))) (F_max0_ge_0 (s V_quant_imagelength))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_quant_imagelength)) (F_check_ge (s V_quant_imagelength) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_quant_imagelength)) (F_check_ge (s V_quant_imagelength) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_i)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_quant_imagewidth)) (F_check_ge (s V_quant_imagewidth) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_imagewidth
                                                                    - s V_quant_j)) (F_check_ge (0) (0)));
      (*0 0.25*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_imagewidth
                                                                    - s V_quant_j) (0))) (F_max0_ge_0 (s V_quant_imagewidth
                                                                    - s V_quant_j))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_imagewidth)) (F_check_ge (0) (0)));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_imagewidth
                                                                    - s V_quant_j) (0))) (F_max0_ge_0 (s V_quant_imagewidth
                                                                    - s V_quant_j))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_imagewidth
                                                                    - s V_quant_j)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_quant_imagewidth
                                                                - s V_quant_j)) (F_check_ge (s V_quant_imagewidth
                                                                    - s V_quant_j) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_quant_imagewidth
                                                                 - s V_quant_j)) (F_check_ge (s V_quant_imagewidth
                                                                    - s V_quant_j) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_quant_j)) (F_check_ge (s V_quant_j) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_j)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_i) (0))) (F_max0_ge_0 (s V_quant_i));
      (*-0.25 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (-1
                                                                  + s V_quant_imagelength) (0))) (F_max0_ge_0 (-1
                                                                    + s V_quant_imagelength))]
     (-(3 # 43) - (55 # 43) * s V_quant_i
      + (18 # 43) * s V_quant_i * s V_quant_imagelength
      + (1 # 43) * s V_quant_i * max0(-1 - s V_quant_i
                                      + s V_quant_imagelength)
      + (3 # 43) * s V_quant_i * max0(-1 + s V_quant_imagelength)
      + (1 # 4) * s V_quant_i * max0(-1 + s V_quant_j)
      + (11 # 43) * s V_quant_i * max0(-s V_quant_i + s V_quant_imagelength)
      + (1 # 170) * s V_quant_i * max0(s V_quant_imagelength)
      - s V_quant_i * max0(s V_quant_imagewidth) - (9 # 43) * s V_quant_i^2
      + (113 # 86) * s V_quant_imagelength
      + (10 # 43) * s V_quant_imagelength * max0(-1 - s V_quant_i
                                                 + s V_quant_imagelength)
      - (3 # 43) * s V_quant_imagelength * max0(-1 + s V_quant_imagelength)
      - (1 # 4) * s V_quant_imagelength * max0(-1 + s V_quant_j)
      - (31 # 86) * s V_quant_imagelength * max0(-s V_quant_i
                                                 + s V_quant_imagelength)
      - (7 # 43) * s V_quant_imagelength * max0(s V_quant_imagelength)
      + s V_quant_imagelength * max0(s V_quant_imagewidth)
      - (21 # 86) * s V_quant_imagelength^2 + (5 # 4) * s V_quant_imagewidth
      + (1 # 2) * s V_quant_imagewidth * s V_quant_j
      - (3 # 4) * s V_quant_imagewidth * max0(-1 - s V_quant_i
                                              + s V_quant_imagelength)
      + (1 # 2) * s V_quant_imagewidth * max0(-1 + s V_quant_imagelength)
      - (1 # 4) * s V_quant_imagewidth * max0(-s V_quant_i
                                              + s V_quant_imagelength)
      - (1 # 2) * s V_quant_imagewidth * max0(s V_quant_i)
      - (1 # 4) * s V_quant_imagewidth^2 - s V_quant_j
      + (1 # 2) * s V_quant_j * max0(-1 - s V_quant_i + s V_quant_imagelength)
      - (1 # 2) * s V_quant_j * max0(-1 + s V_quant_imagelength)
      + (1 # 2) * s V_quant_j * max0(s V_quant_i)
      - (1 # 2) * s V_quant_j * max0(s V_quant_imagewidth) + s V_quant_z
      + (3 # 43) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(-1
                                                                    + s V_quant_imagelength)
      + (1 # 4) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(-1
                                                                    + s V_quant_j)
      + (10 # 43) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength)
      - (14 # 73) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
      + (1 # 2) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagewidth
                                                                    - s V_quant_j)
      + (1 # 2) * max0(-1 + s V_quant_imagelength)
      + (1 # 2) * max0(-1 + s V_quant_imagelength) * max0(-1 + s V_quant_j)
      - (5 # 86) * max0(-1 + s V_quant_imagelength) * max0(-s V_quant_i
                                                           + s V_quant_imagelength)
      + (14 # 73) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_imagelength)
      + (1 # 4) * max0(-1 + s V_quant_j)
      - (1 # 2) * max0(-1 + s V_quant_j) * max0(s V_quant_i)
      + (17 # 86) * max0(-s V_quant_i + s V_quant_imagelength)
      - (11 # 43) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_i)
      + (31 # 86) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
      - (15 # 86) * max0(s V_quant_i)
      - (3 # 86) * max0(s V_quant_imagelength) - max0(s V_quant_imagewidth)
      + (1 # 4) * max0(s V_quant_imagewidth)^2 <= z)%Q
   | 38 => (-(3 # 43) - (52 # 43) * s V_quant_i
            + (18 # 43) * s V_quant_i * s V_quant_imagelength
            + (1 # 43) * s V_quant_i * max0(-1 - s V_quant_i
                                            + s V_quant_imagelength)
            + (3 # 43) * s V_quant_i * max0(-1 + s V_quant_imagelength)
            + (25 # 86) * s V_quant_i * max0(-s V_quant_i
                                             + s V_quant_imagelength)
            - (19 # 121) * s V_quant_i * max0(s V_quant_imagelength)
            - (9 # 43) * s V_quant_i^2 + (113 # 86) * s V_quant_imagelength
            + (10 # 43) * s V_quant_imagelength * max0(-1 - s V_quant_i
                                                       + s V_quant_imagelength)
            - (3 # 43) * s V_quant_imagelength * max0(-1
                                                      + s V_quant_imagelength)
            - (17 # 43) * s V_quant_imagelength * max0(-s V_quant_i
                                                       + s V_quant_imagelength)
            - (21 # 86) * s V_quant_imagelength^2
            + s V_quant_imagewidth * max0(-s V_quant_i
                                          + s V_quant_imagelength)
            + s V_quant_z
            + (3 # 43) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(-1
                                                                    + s V_quant_imagelength)
            + (23 # 86) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength)
            - (19 # 121) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
            - (5 # 86) * max0(-1 + s V_quant_imagelength) * max0(-s V_quant_i
                                                                 + s V_quant_imagelength)
            + (14 # 73) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_imagelength)
            + (10 # 43) * max0(-s V_quant_i + s V_quant_imagelength)
            - (11 # 43) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_i)
            + (7 # 43) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
            + (11 # 43) * max0(s V_quant_i) <= z)%Q
   | 39 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_max0_ge_0 (-1 - s V_quant_i + s V_quant_imagelength);
      (*-0.5 0*) F_binom_monotonic 2 (F_max0_ge_arg (-1 - s V_quant_i
                                                     + s V_quant_imagelength)) (F_check_ge (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength) (0));
      (*0 0.0348837*) F_binom_monotonic 2 (F_max0_ge_arg (-1
                                                          + s V_quant_imagelength)) (F_check_ge (-1
                                                                    + s V_quant_imagelength) (0));
      (*-0.290698 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_max0_ge_0 (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_max0_ge_0 (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.255814 -0.0988372*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_max0_ge_0 (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.0697674 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.255814 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    - 
                                                                    s V_quant_i
                                                                    + 
                                                                    s V_quant_imagelength)) (F_check_ge (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_i)) (F_check_ge (0) (0)));
      (*0 0.0348837*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_quant_imagelength) (0))) (F_max0_ge_0 (-1
                                                                    + s V_quant_imagelength))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.0581395 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_quant_imagelength) (0))) (F_max0_ge_0 (-1
                                                                    + s V_quant_imagelength))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.156977 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    s V_quant_imagelength)) (F_check_ge (-1
                                                                    + s V_quant_imagelength) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.255814 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_max0_ge_0 (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_i)) (F_check_ge (0) (0)));
      (*-0.267442 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-
                                                                    s V_quant_i
                                                                    + 
                                                                    s V_quant_imagelength)) (F_check_ge (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.290698 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-
                                                                    s V_quant_i
                                                                    + 
                                                                    s V_quant_imagelength)) (F_check_ge (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-s V_quant_i
                                                            + s V_quant_imagelength)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_imagewidth)) (F_check_ge (0) (0)));
      (*-0.255814 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_i) (0))) (F_max0_ge_0 (s V_quant_i))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.0348837 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_quant_imagelength)) (F_check_ge (s V_quant_imagelength) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-0.162791 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_quant_imagelength)) (F_check_ge (s V_quant_imagelength) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_imagewidth) (0))) (F_max0_ge_0 (s V_quant_imagewidth))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength)) (F_check_ge (0) (0)))]
     (-(3 # 43) - (52 # 43) * s V_quant_i
      + (18 # 43) * s V_quant_i * s V_quant_imagelength
      + (1 # 43) * s V_quant_i * max0(-1 - s V_quant_i
                                      + s V_quant_imagelength)
      + (3 # 43) * s V_quant_i * max0(-1 + s V_quant_imagelength)
      + (25 # 86) * s V_quant_i * max0(-s V_quant_i + s V_quant_imagelength)
      - (19 # 121) * s V_quant_i * max0(s V_quant_imagelength)
      - (9 # 43) * s V_quant_i^2 + (113 # 86) * s V_quant_imagelength
      + (10 # 43) * s V_quant_imagelength * max0(-1 - s V_quant_i
                                                 + s V_quant_imagelength)
      - (3 # 43) * s V_quant_imagelength * max0(-1 + s V_quant_imagelength)
      - (17 # 43) * s V_quant_imagelength * max0(-s V_quant_i
                                                 + s V_quant_imagelength)
      - (21 # 86) * s V_quant_imagelength^2
      + s V_quant_imagewidth * max0(-s V_quant_i + s V_quant_imagelength)
      + s V_quant_z
      + (3 # 43) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(-1
                                                                    + s V_quant_imagelength)
      + (23 # 86) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(-
                                                                    s V_quant_i
                                                                    + s V_quant_imagelength)
      - (19 # 121) * max0(-1 - s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
      - (5 # 86) * max0(-1 + s V_quant_imagelength) * max0(-s V_quant_i
                                                           + s V_quant_imagelength)
      + (14 # 73) * max0(-1 + s V_quant_imagelength) * max0(s V_quant_imagelength)
      + (10 # 43) * max0(-s V_quant_i + s V_quant_imagelength)
      - (11 # 43) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_i)
      + (7 # 43) * max0(-s V_quant_i + s V_quant_imagelength) * max0(s V_quant_imagelength)
      + (11 # 43) * max0(s V_quant_i) <= z)%Q
   | 40 => (s V_quant_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_quant =>
    [mkPA Q (fun n z s => ai_quant n s /\ annot0_quant n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_quant (proc_start P_quant) s1 (proc_end P_quant) s2 ->
    (s2 V_quant_z <= max0(s1 V_quant_imagelength)
                     + max0(s1 V_quant_imagelength) * max0(s1 V_quant_imagewidth))%Q.
Proof.
  prove_bound ipa admissible_ipa P_quant.
Qed.
