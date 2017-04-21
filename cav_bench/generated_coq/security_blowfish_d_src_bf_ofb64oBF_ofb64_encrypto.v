Require Import pasta.Pasta.

Inductive proc: Type :=
  P_BF_ofb64_encrypt.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_BF_ofb64_encrypt_z := 1%positive.
Notation V_BF_ofb64_encrypt__tmp := 2%positive.
Notation V_BF_ofb64_encrypt_l := 3%positive.
Notation V_BF_ofb64_encrypt_n := 4%positive.
Notation V_BF_ofb64_encrypt_num_dref := 5%positive.
Notation V_BF_ofb64_encrypt_save := 6%positive.
Notation V_BF_ofb64_encrypt_t := 7%positive.
Notation V_BF_ofb64_encrypt_v0 := 8%positive.
Notation V_BF_ofb64_encrypt_v1 := 9%positive.
Notation V_BF_ofb64_encrypt_in := 10%positive.
Notation V_BF_ofb64_encrypt_ivec := 11%positive.
Notation V_BF_ofb64_encrypt_length := 12%positive.
Notation V_BF_ofb64_encrypt_num := 13%positive.
Notation V_BF_ofb64_encrypt_out := 14%positive.
Notation V_BF_ofb64_encrypt_schedule := 15%positive.
Definition Pedges_BF_ofb64_encrypt: list (edge proc) :=
  (EA 1 (AAssign V_BF_ofb64_encrypt_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_BF_ofb64_encrypt__tmp (Some (EVar V_BF_ofb64_encrypt_length))) 3)::
  (EA 3 (AAssign V_BF_ofb64_encrypt_n
  (Some (EVar V_BF_ofb64_encrypt_num_dref))) 4)::(EA 4 (AAssign
  V_BF_ofb64_encrypt_l (Some (EVar V_BF_ofb64_encrypt__tmp))) 5)::
  (EA 5 (AAssign V_BF_ofb64_encrypt_save (Some (ENum (0)))) 6)::
  (EA 6 (AAssign V_BF_ofb64_encrypt_v0 None) 7)::(EA 7 (AAssign
  V_BF_ofb64_encrypt_v0 None) 8)::(EA 8 (AAssign V_BF_ofb64_encrypt_v0
  None) 9)::(EA 9 (AAssign V_BF_ofb64_encrypt_v0 None) 10)::(EA 10 (AAssign
  V_BF_ofb64_encrypt_v1 None) 11)::(EA 11 (AAssign V_BF_ofb64_encrypt_v1
  None) 12)::(EA 12 (AAssign V_BF_ofb64_encrypt_v1 None) 13)::(EA 13 (AAssign
  V_BF_ofb64_encrypt_v1 None) 14)::(EA 14 ANone 15)::(EA 15 (AAssign
  V_BF_ofb64_encrypt_l (Some (EAdd (EVar V_BF_ofb64_encrypt_l)
  (ENum (-1))))) 16)::(EA 16 AWeaken 17)::(EA 17 (AGuard
  (fun s => ((eval (EVar V_BF_ofb64_encrypt_l) s) <> (eval (ENum (0))
  s))%Z)) 31)::(EA 17 (AGuard (fun s => ((eval (EVar V_BF_ofb64_encrypt_l)
  s) = (eval (ENum (0)) s))%Z)) 18)::(EA 18 AWeaken 19)::(EA 19 (AGuard
  (fun s => ((eval (EVar V_BF_ofb64_encrypt_save) s) <> (eval (ENum (0))
  s))%Z)) 21)::(EA 19 (AGuard (fun s => ((eval (EVar V_BF_ofb64_encrypt_save)
  s) = (eval (ENum (0)) s))%Z)) 20)::(EA 20 AWeaken 25)::(EA 21 AWeaken 22)::
  (EA 22 (AAssign V_BF_ofb64_encrypt_v0 None) 23)::(EA 23 (AAssign
  V_BF_ofb64_encrypt_v1 None) 24)::(EA 24 ANone 25)::(EA 25 (AAssign
  V_BF_ofb64_encrypt_v1 (Some (ENum (0)))) 26)::(EA 26 (AAssign
  V_BF_ofb64_encrypt_v0 (Some (ENum (0)))) 27)::(EA 27 (AAssign
  V_BF_ofb64_encrypt_t (Some (ENum (0)))) 28)::(EA 28 (AAssign
  V_BF_ofb64_encrypt_num_dref (Some (EVar V_BF_ofb64_encrypt_n))) 29)::
  (EA 29 AWeaken 30)::(EA 31 AWeaken 32)::(EA 32 (AGuard
  (fun s => ((eval (EVar V_BF_ofb64_encrypt_n) s) = (eval (ENum (0))
  s))%Z)) 34)::(EA 32 (AGuard (fun s => ((eval (EVar V_BF_ofb64_encrypt_n)
  s) <> (eval (ENum (0)) s))%Z)) 33)::(EA 33 AWeaken 39)::
  (EA 34 AWeaken 35)::(EA 35 (AAssign V_BF_ofb64_encrypt_t None) 36)::
  (EA 36 (AAssign V_BF_ofb64_encrypt_t None) 37)::(EA 37 (AAssign
  V_BF_ofb64_encrypt_save (Some (EAdd (EVar V_BF_ofb64_encrypt_save)
  (ENum (1))))) 38)::(EA 38 ANone 39)::(EA 39 (AAssign V_BF_ofb64_encrypt_n
  None) 40)::(EA 40 ANone 41)::(EA 41 ANone 42)::(EA 42 (AAssign
  V_BF_ofb64_encrypt_z (Some (EAdd (ENum (1))
  (EVar V_BF_ofb64_encrypt_z)))) 15)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_BF_ofb64_encrypt => Pedges_BF_ofb64_encrypt
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_BF_ofb64_encrypt => 30
     end)%positive;
  var_global := var_global
}.

Definition ai_BF_ofb64_encrypt (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_BF_ofb64_encrypt_z <= 0 /\ -1 * s V_BF_ofb64_encrypt_z <= 0)%Z
   | 3 => (-1 * s V_BF_ofb64_encrypt_z <= 0 /\ 1 * s V_BF_ofb64_encrypt_z <= 0)%Z
   | 4 => (1 * s V_BF_ofb64_encrypt_z <= 0 /\ -1 * s V_BF_ofb64_encrypt_z <= 0)%Z
   | 5 => (-1 * s V_BF_ofb64_encrypt_z <= 0 /\ 1 * s V_BF_ofb64_encrypt_z <= 0)%Z
   | 6 => (1 * s V_BF_ofb64_encrypt_z <= 0 /\ -1 * s V_BF_ofb64_encrypt_z <= 0 /\ 1 * s V_BF_ofb64_encrypt_save <= 0 /\ -1 * s V_BF_ofb64_encrypt_save <= 0)%Z
   | 7 => (-1 * s V_BF_ofb64_encrypt_save <= 0 /\ 1 * s V_BF_ofb64_encrypt_save <= 0 /\ -1 * s V_BF_ofb64_encrypt_z <= 0 /\ 1 * s V_BF_ofb64_encrypt_z <= 0)%Z
   | 8 => (1 * s V_BF_ofb64_encrypt_z <= 0 /\ -1 * s V_BF_ofb64_encrypt_z <= 0 /\ 1 * s V_BF_ofb64_encrypt_save <= 0 /\ -1 * s V_BF_ofb64_encrypt_save <= 0)%Z
   | 9 => (-1 * s V_BF_ofb64_encrypt_save <= 0 /\ 1 * s V_BF_ofb64_encrypt_save <= 0 /\ -1 * s V_BF_ofb64_encrypt_z <= 0 /\ 1 * s V_BF_ofb64_encrypt_z <= 0)%Z
   | 10 => (1 * s V_BF_ofb64_encrypt_z <= 0 /\ -1 * s V_BF_ofb64_encrypt_z <= 0 /\ 1 * s V_BF_ofb64_encrypt_save <= 0 /\ -1 * s V_BF_ofb64_encrypt_save <= 0)%Z
   | 11 => (-1 * s V_BF_ofb64_encrypt_save <= 0 /\ 1 * s V_BF_ofb64_encrypt_save <= 0 /\ -1 * s V_BF_ofb64_encrypt_z <= 0 /\ 1 * s V_BF_ofb64_encrypt_z <= 0)%Z
   | 12 => (1 * s V_BF_ofb64_encrypt_z <= 0 /\ -1 * s V_BF_ofb64_encrypt_z <= 0 /\ 1 * s V_BF_ofb64_encrypt_save <= 0 /\ -1 * s V_BF_ofb64_encrypt_save <= 0)%Z
   | 13 => (-1 * s V_BF_ofb64_encrypt_save <= 0 /\ 1 * s V_BF_ofb64_encrypt_save <= 0 /\ -1 * s V_BF_ofb64_encrypt_z <= 0 /\ 1 * s V_BF_ofb64_encrypt_z <= 0)%Z
   | 14 => (1 * s V_BF_ofb64_encrypt_z <= 0 /\ -1 * s V_BF_ofb64_encrypt_z <= 0 /\ 1 * s V_BF_ofb64_encrypt_save <= 0 /\ -1 * s V_BF_ofb64_encrypt_save <= 0)%Z
   | 15 => (-1 * s V_BF_ofb64_encrypt_z <= 0 /\ -1 * s V_BF_ofb64_encrypt_save <= 0)%Z
   | 16 => (-1 * s V_BF_ofb64_encrypt_save <= 0 /\ -1 * s V_BF_ofb64_encrypt_z <= 0)%Z
   | 17 => (-1 * s V_BF_ofb64_encrypt_z <= 0 /\ -1 * s V_BF_ofb64_encrypt_save <= 0)%Z
   | 18 => (-1 * s V_BF_ofb64_encrypt_save <= 0 /\ -1 * s V_BF_ofb64_encrypt_z <= 0 /\ 1 * s V_BF_ofb64_encrypt_l <= 0 /\ -1 * s V_BF_ofb64_encrypt_l <= 0)%Z
   | 19 => (-1 * s V_BF_ofb64_encrypt_l <= 0 /\ 1 * s V_BF_ofb64_encrypt_l <= 0 /\ -1 * s V_BF_ofb64_encrypt_z <= 0 /\ -1 * s V_BF_ofb64_encrypt_save <= 0)%Z
   | 20 => (-1 * s V_BF_ofb64_encrypt_save <= 0 /\ -1 * s V_BF_ofb64_encrypt_z <= 0 /\ 1 * s V_BF_ofb64_encrypt_l <= 0 /\ -1 * s V_BF_ofb64_encrypt_l <= 0 /\ 1 * s V_BF_ofb64_encrypt_save <= 0)%Z
   | 21 => (-1 * s V_BF_ofb64_encrypt_z <= 0 /\ 1 * s V_BF_ofb64_encrypt_l <= 0 /\ -1 * s V_BF_ofb64_encrypt_l <= 0 /\ -1 * s V_BF_ofb64_encrypt_save + 1 <= 0)%Z
   | 22 => (-1 * s V_BF_ofb64_encrypt_save + 1 <= 0 /\ -1 * s V_BF_ofb64_encrypt_l <= 0 /\ 1 * s V_BF_ofb64_encrypt_l <= 0 /\ -1 * s V_BF_ofb64_encrypt_z <= 0)%Z
   | 23 => (-1 * s V_BF_ofb64_encrypt_z <= 0 /\ 1 * s V_BF_ofb64_encrypt_l <= 0 /\ -1 * s V_BF_ofb64_encrypt_l <= 0 /\ -1 * s V_BF_ofb64_encrypt_save + 1 <= 0)%Z
   | 24 => (-1 * s V_BF_ofb64_encrypt_save + 1 <= 0 /\ -1 * s V_BF_ofb64_encrypt_l <= 0 /\ 1 * s V_BF_ofb64_encrypt_l <= 0 /\ -1 * s V_BF_ofb64_encrypt_z <= 0)%Z
   | 25 => (-1 * s V_BF_ofb64_encrypt_save <= 0 /\ -1 * s V_BF_ofb64_encrypt_z <= 0 /\ 1 * s V_BF_ofb64_encrypt_l <= 0 /\ -1 * s V_BF_ofb64_encrypt_l <= 0)%Z
   | 26 => (-1 * s V_BF_ofb64_encrypt_l <= 0 /\ 1 * s V_BF_ofb64_encrypt_l <= 0 /\ -1 * s V_BF_ofb64_encrypt_z <= 0 /\ -1 * s V_BF_ofb64_encrypt_save <= 0 /\ 1 * s V_BF_ofb64_encrypt_v1 <= 0 /\ -1 * s V_BF_ofb64_encrypt_v1 <= 0)%Z
   | 27 => (-1 * s V_BF_ofb64_encrypt_v1 <= 0 /\ 1 * s V_BF_ofb64_encrypt_v1 <= 0 /\ -1 * s V_BF_ofb64_encrypt_save <= 0 /\ -1 * s V_BF_ofb64_encrypt_z <= 0 /\ 1 * s V_BF_ofb64_encrypt_l <= 0 /\ -1 * s V_BF_ofb64_encrypt_l <= 0 /\ 1 * s V_BF_ofb64_encrypt_v0 <= 0 /\ -1 * s V_BF_ofb64_encrypt_v0 <= 0)%Z
   | 28 => (-1 * s V_BF_ofb64_encrypt_v0 <= 0 /\ 1 * s V_BF_ofb64_encrypt_v0 <= 0 /\ -1 * s V_BF_ofb64_encrypt_l <= 0 /\ 1 * s V_BF_ofb64_encrypt_l <= 0 /\ -1 * s V_BF_ofb64_encrypt_z <= 0 /\ -1 * s V_BF_ofb64_encrypt_save <= 0 /\ 1 * s V_BF_ofb64_encrypt_v1 <= 0 /\ -1 * s V_BF_ofb64_encrypt_v1 <= 0 /\ 1 * s V_BF_ofb64_encrypt_t <= 0 /\ -1 * s V_BF_ofb64_encrypt_t <= 0)%Z
   | 29 => (-1 * s V_BF_ofb64_encrypt_t <= 0 /\ 1 * s V_BF_ofb64_encrypt_t <= 0 /\ -1 * s V_BF_ofb64_encrypt_v1 <= 0 /\ 1 * s V_BF_ofb64_encrypt_v1 <= 0 /\ -1 * s V_BF_ofb64_encrypt_save <= 0 /\ -1 * s V_BF_ofb64_encrypt_z <= 0 /\ 1 * s V_BF_ofb64_encrypt_l <= 0 /\ -1 * s V_BF_ofb64_encrypt_l <= 0 /\ 1 * s V_BF_ofb64_encrypt_v0 <= 0 /\ -1 * s V_BF_ofb64_encrypt_v0 <= 0)%Z
   | 30 => (-1 * s V_BF_ofb64_encrypt_v0 <= 0 /\ 1 * s V_BF_ofb64_encrypt_v0 <= 0 /\ -1 * s V_BF_ofb64_encrypt_l <= 0 /\ 1 * s V_BF_ofb64_encrypt_l <= 0 /\ -1 * s V_BF_ofb64_encrypt_z <= 0 /\ -1 * s V_BF_ofb64_encrypt_save <= 0 /\ 1 * s V_BF_ofb64_encrypt_v1 <= 0 /\ -1 * s V_BF_ofb64_encrypt_v1 <= 0 /\ 1 * s V_BF_ofb64_encrypt_t <= 0 /\ -1 * s V_BF_ofb64_encrypt_t <= 0)%Z
   | 31 => (-1 * s V_BF_ofb64_encrypt_save <= 0 /\ -1 * s V_BF_ofb64_encrypt_z <= 0)%Z
   | 32 => (-1 * s V_BF_ofb64_encrypt_z <= 0 /\ -1 * s V_BF_ofb64_encrypt_save <= 0)%Z
   | 33 => (-1 * s V_BF_ofb64_encrypt_save <= 0 /\ -1 * s V_BF_ofb64_encrypt_z <= 0)%Z
   | 34 => (-1 * s V_BF_ofb64_encrypt_save <= 0 /\ -1 * s V_BF_ofb64_encrypt_z <= 0 /\ 1 * s V_BF_ofb64_encrypt_n <= 0 /\ -1 * s V_BF_ofb64_encrypt_n <= 0)%Z
   | 35 => (-1 * s V_BF_ofb64_encrypt_n <= 0 /\ 1 * s V_BF_ofb64_encrypt_n <= 0 /\ -1 * s V_BF_ofb64_encrypt_z <= 0 /\ -1 * s V_BF_ofb64_encrypt_save <= 0)%Z
   | 36 => (-1 * s V_BF_ofb64_encrypt_save <= 0 /\ -1 * s V_BF_ofb64_encrypt_z <= 0 /\ 1 * s V_BF_ofb64_encrypt_n <= 0 /\ -1 * s V_BF_ofb64_encrypt_n <= 0)%Z
   | 37 => (-1 * s V_BF_ofb64_encrypt_n <= 0 /\ 1 * s V_BF_ofb64_encrypt_n <= 0 /\ -1 * s V_BF_ofb64_encrypt_z <= 0 /\ -1 * s V_BF_ofb64_encrypt_save <= 0)%Z
   | 38 => (-1 * s V_BF_ofb64_encrypt_z <= 0 /\ 1 * s V_BF_ofb64_encrypt_n <= 0 /\ -1 * s V_BF_ofb64_encrypt_n <= 0 /\ -1 * s V_BF_ofb64_encrypt_save + 1 <= 0)%Z
   | 39 => (-1 * s V_BF_ofb64_encrypt_save <= 0 /\ -1 * s V_BF_ofb64_encrypt_z <= 0)%Z
   | 40 => (-1 * s V_BF_ofb64_encrypt_z <= 0 /\ -1 * s V_BF_ofb64_encrypt_save <= 0)%Z
   | 41 => (-1 * s V_BF_ofb64_encrypt_save <= 0 /\ -1 * s V_BF_ofb64_encrypt_z <= 0)%Z
   | 42 => (-1 * s V_BF_ofb64_encrypt_z <= 0 /\ -1 * s V_BF_ofb64_encrypt_save <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_BF_ofb64_encrypt (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (s V_BF_ofb64_encrypt_length <= z)%Q
   | 2 => (s V_BF_ofb64_encrypt_length + s V_BF_ofb64_encrypt_z <= z)%Q
   | 3 => (s V_BF_ofb64_encrypt__tmp + s V_BF_ofb64_encrypt_z <= z)%Q
   | 4 => (s V_BF_ofb64_encrypt__tmp + s V_BF_ofb64_encrypt_z <= z)%Q
   | 5 => (s V_BF_ofb64_encrypt_l + s V_BF_ofb64_encrypt_z <= z)%Q
   | 6 => (s V_BF_ofb64_encrypt_l + s V_BF_ofb64_encrypt_z <= z)%Q
   | 7 => (s V_BF_ofb64_encrypt_l + s V_BF_ofb64_encrypt_z <= z)%Q
   | 8 => (s V_BF_ofb64_encrypt_l + s V_BF_ofb64_encrypt_z <= z)%Q
   | 9 => (s V_BF_ofb64_encrypt_l + s V_BF_ofb64_encrypt_z <= z)%Q
   | 10 => (s V_BF_ofb64_encrypt_l + s V_BF_ofb64_encrypt_z <= z)%Q
   | 11 => (s V_BF_ofb64_encrypt_l + s V_BF_ofb64_encrypt_z <= z)%Q
   | 12 => (s V_BF_ofb64_encrypt_l + s V_BF_ofb64_encrypt_z <= z)%Q
   | 13 => (s V_BF_ofb64_encrypt_l + s V_BF_ofb64_encrypt_z <= z)%Q
   | 14 => (s V_BF_ofb64_encrypt_l + s V_BF_ofb64_encrypt_z <= z)%Q
   | 15 => (s V_BF_ofb64_encrypt_l + s V_BF_ofb64_encrypt_z <= z)%Q
   | 16 => ((1 # 1) + s V_BF_ofb64_encrypt_l + s V_BF_ofb64_encrypt_z <= z)%Q
   | 17 => ((1 # 1) + s V_BF_ofb64_encrypt_l + s V_BF_ofb64_encrypt_z <= z)%Q
   | 18 => ((1 # 1) + s V_BF_ofb64_encrypt_l + s V_BF_ofb64_encrypt_z <= z)%Q
   | 19 => ((1 # 1) + s V_BF_ofb64_encrypt_l + s V_BF_ofb64_encrypt_z <= z)%Q
   | 20 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_BF_ofb64_encrypt_l)) (F_check_ge (0) (0))]
     ((1 # 1) + s V_BF_ofb64_encrypt_l + s V_BF_ofb64_encrypt_z <= z)%Q
   | 21 => hints
     [(*0 1*) F_one;
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_0 (s V_BF_ofb64_encrypt_l)) (F_check_ge (0) (0))]
     ((1 # 1) + s V_BF_ofb64_encrypt_l + s V_BF_ofb64_encrypt_z <= z)%Q
   | 22 => (s V_BF_ofb64_encrypt_l + s V_BF_ofb64_encrypt_z
            - max0(s V_BF_ofb64_encrypt_l) <= z)%Q
   | 23 => (s V_BF_ofb64_encrypt_l + s V_BF_ofb64_encrypt_z
            - max0(s V_BF_ofb64_encrypt_l) <= z)%Q
   | 24 => (s V_BF_ofb64_encrypt_l + s V_BF_ofb64_encrypt_z
            - max0(s V_BF_ofb64_encrypt_l) <= z)%Q
   | 25 => (s V_BF_ofb64_encrypt_l + s V_BF_ofb64_encrypt_z
            - max0(s V_BF_ofb64_encrypt_l) <= z)%Q
   | 26 => (s V_BF_ofb64_encrypt_l + s V_BF_ofb64_encrypt_z
            - max0(s V_BF_ofb64_encrypt_l) <= z)%Q
   | 27 => (s V_BF_ofb64_encrypt_l + s V_BF_ofb64_encrypt_z
            - max0(s V_BF_ofb64_encrypt_l) <= z)%Q
   | 28 => (s V_BF_ofb64_encrypt_l + s V_BF_ofb64_encrypt_z
            - max0(s V_BF_ofb64_encrypt_l) <= z)%Q
   | 29 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_BF_ofb64_encrypt_l) (0))) (F_max0_ge_0 (s V_BF_ofb64_encrypt_l))]
     (s V_BF_ofb64_encrypt_l + s V_BF_ofb64_encrypt_z
      - max0(s V_BF_ofb64_encrypt_l) <= z)%Q
   | 30 => (s V_BF_ofb64_encrypt_z <= z)%Q
   | 31 => ((1 # 1) + s V_BF_ofb64_encrypt_l + s V_BF_ofb64_encrypt_z <= z)%Q
   | 32 => ((1 # 1) + s V_BF_ofb64_encrypt_l + s V_BF_ofb64_encrypt_z <= z)%Q
   | 33 => ((1 # 1) + s V_BF_ofb64_encrypt_l + s V_BF_ofb64_encrypt_z <= z)%Q
   | 34 => ((1 # 1) + s V_BF_ofb64_encrypt_l + s V_BF_ofb64_encrypt_z <= z)%Q
   | 35 => ((1 # 1) + s V_BF_ofb64_encrypt_l + s V_BF_ofb64_encrypt_z <= z)%Q
   | 36 => ((1 # 1) + s V_BF_ofb64_encrypt_l + s V_BF_ofb64_encrypt_z <= z)%Q
   | 37 => ((1 # 1) + s V_BF_ofb64_encrypt_l + s V_BF_ofb64_encrypt_z <= z)%Q
   | 38 => ((1 # 1) + s V_BF_ofb64_encrypt_l + s V_BF_ofb64_encrypt_z <= z)%Q
   | 39 => ((1 # 1) + s V_BF_ofb64_encrypt_l + s V_BF_ofb64_encrypt_z <= z)%Q
   | 40 => ((1 # 1) + s V_BF_ofb64_encrypt_l + s V_BF_ofb64_encrypt_z <= z)%Q
   | 41 => ((1 # 1) + s V_BF_ofb64_encrypt_l + s V_BF_ofb64_encrypt_z <= z)%Q
   | 42 => ((1 # 1) + s V_BF_ofb64_encrypt_l + s V_BF_ofb64_encrypt_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_BF_ofb64_encrypt =>
    [mkPA Q (fun n z s => ai_BF_ofb64_encrypt n s /\ annot0_BF_ofb64_encrypt n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_BF_ofb64_encrypt (proc_start P_BF_ofb64_encrypt) s1 (proc_end P_BF_ofb64_encrypt) s2 ->
    (s2 V_BF_ofb64_encrypt_z <= s1 V_BF_ofb64_encrypt_length)%Q.
Proof.
  prove_bound ipa admissible_ipa P_BF_ofb64_encrypt.
Qed.
