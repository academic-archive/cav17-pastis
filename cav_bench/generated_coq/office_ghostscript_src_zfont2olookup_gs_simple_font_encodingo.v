Require Import pasta.Pasta.

Inductive proc: Type :=
  P_lookup_gs_simple_font_encoding.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_lookup_gs_simple_font_encoding_z := 1%positive.
Notation V_lookup_gs_simple_font_encoding_best := 2%positive.
Notation V_lookup_gs_simple_font_encoding_esize := 3%positive.
Notation V_lookup_gs_simple_font_encoding_f_packed := 4%positive.
Notation V_lookup_gs_simple_font_encoding_i := 5%positive.
Notation V_lookup_gs_simple_font_encoding_index := 6%positive.
Notation V_lookup_gs_simple_font_encoding_match := 7%positive.
Notation V_lookup_gs_simple_font_encoding_near_index := 8%positive.
Notation V_lookup_gs_simple_font_encoding_pfont_dref_off328 := 9%positive.
Notation V_lookup_gs_simple_font_encoding_pfont_dref_off332 := 10%positive.
Notation V_lookup_gs_simple_font_encoding_r_packed := 11%positive.
Notation V_lookup_gs_simple_font_encoding_rnidx := 12%positive.
Notation V_lookup_gs_simple_font_encoding_pfont := 13%positive.
Definition Pedges_lookup_gs_simple_font_encoding: list (edge proc) :=
  (EA 1 (AAssign V_lookup_gs_simple_font_encoding_z (Some (ENum (0)))) 2)::
  (EA 2 (AGuard
  (fun s => ((eval (EVar V_lookup_gs_simple_font_encoding_match) s) >=
  (eval (ENum (0)) s))%Z)) 3)::(EA 3 (AGuard
  (fun s => ((eval (EVar V_lookup_gs_simple_font_encoding_best) s) >=
  (eval (ENum (0)) s))%Z)) 4)::(EA 4 AWeaken 5)::(EA 5 (AAssign
  V_lookup_gs_simple_font_encoding_index (Some (ENum (5)))) 6)::
  (EA 6 ANone 7)::(EA 7 (AAssign V_lookup_gs_simple_font_encoding_index
  (Some (EAdd (EVar V_lookup_gs_simple_font_encoding_index)
  (ENum (-1))))) 8)::(EA 8 AWeaken 9)::(EA 9 (AGuard
  (fun s => ((eval (EAdd (EVar V_lookup_gs_simple_font_encoding_index)
  (ENum (-1))) s) >= (eval (ENum (0)) s))%Z)) 11)::(EA 9 (AGuard
  (fun s => ((eval (EAdd (EVar V_lookup_gs_simple_font_encoding_index)
  (ENum (-1))) s) < (eval (ENum (0)) s))%Z)) 10)::(EA 10 AWeaken 17)::
  (EA 11 AWeaken 12)::(EA 12 ANone 16)::(EA 12 ANone 13)::(EA 13 ANone 14)::
  (EA 14 ANone 15)::(EA 15 (AAssign V_lookup_gs_simple_font_encoding_z
  (Some (EAdd (ENum (1)) (EVar V_lookup_gs_simple_font_encoding_z)))) 7)::
  (EA 16 ANone 17)::(EA 17 (AAssign
  V_lookup_gs_simple_font_encoding_pfont_dref_off328
  (Some (EVar V_lookup_gs_simple_font_encoding_index))) 18)::
  (EA 18 AWeaken 19)::(EA 19 (AGuard
  (fun s => ((eval (EVar V_lookup_gs_simple_font_encoding_index) s) <
  (eval (ENum (0)) s))%Z)) 21)::(EA 19 (AGuard
  (fun s => ((eval (EVar V_lookup_gs_simple_font_encoding_index) s) >=
  (eval (ENum (0)) s))%Z)) 20)::(EA 20 AWeaken 33)::(EA 21 AWeaken 22)::
  (EA 22 (AAssign V_lookup_gs_simple_font_encoding_near_index
  (Some (ENum (-1)))) 23)::(EA 23 (AAssign
  V_lookup_gs_simple_font_encoding_esize None) 24)::(EA 24 (AAssign
  V_lookup_gs_simple_font_encoding_best None) 25)::(EA 25 (AAssign
  V_lookup_gs_simple_font_encoding_index (Some (ENum (5)))) 26)::
  (EA 26 ANone 27)::(EA 27 (AAssign V_lookup_gs_simple_font_encoding_index
  (Some (EAdd (EVar V_lookup_gs_simple_font_encoding_index)
  (ENum (-1))))) 28)::(EA 28 AWeaken 29)::(EA 29 (AGuard
  (fun s => ((eval (EAdd (EVar V_lookup_gs_simple_font_encoding_index)
  (ENum (-1))) s) >= (eval (ENum (0)) s))%Z)) 36)::(EA 29 (AGuard
  (fun s => ((eval (EAdd (EVar V_lookup_gs_simple_font_encoding_index)
  (ENum (-1))) s) < (eval (ENum (0)) s))%Z)) 30)::(EA 30 AWeaken 31)::
  (EA 31 (AAssign V_lookup_gs_simple_font_encoding_index
  (Some (EVar V_lookup_gs_simple_font_encoding_near_index))) 32)::
  (EA 32 ANone 33)::(EA 33 (AAssign
  V_lookup_gs_simple_font_encoding_pfont_dref_off332
  (Some (EVar V_lookup_gs_simple_font_encoding_index))) 34)::
  (EA 34 AWeaken 35)::(EA 36 AWeaken 37)::(EA 37 (AAssign
  V_lookup_gs_simple_font_encoding_r_packed None) 38)::(EA 38 (AAssign
  V_lookup_gs_simple_font_encoding_f_packed None) 39)::(EA 39 (AAssign
  V_lookup_gs_simple_font_encoding_match
  (Some (EVar V_lookup_gs_simple_font_encoding_esize))) 40)::
  (EA 40 AWeaken 41)::(EA 41 ANone 86)::(EA 41 ANone 42)::(EA 42 (AAssign
  V_lookup_gs_simple_font_encoding_i
  (Some (EVar V_lookup_gs_simple_font_encoding_esize))) 43)::
  (EA 43 ANone 44)::(EA 44 (AAssign V_lookup_gs_simple_font_encoding_i
  (Some (EAdd (EVar V_lookup_gs_simple_font_encoding_i) (ENum (-1))))) 45)::
  (EA 45 AWeaken 46)::(EA 46 (AGuard
  (fun s => ((eval (EAdd (EVar V_lookup_gs_simple_font_encoding_i)
  (ENum (-1))) s) >= (eval (ENum (0)) s))%Z)) 48)::(EA 46 (AGuard
  (fun s => ((eval (EAdd (EVar V_lookup_gs_simple_font_encoding_i)
  (ENum (-1))) s) < (eval (ENum (0)) s))%Z)) 47)::(EA 47 AWeaken 79)::
  (EA 48 AWeaken 49)::(EA 49 (AGuard
  (fun s => ((eval (EVar V_lookup_gs_simple_font_encoding_r_packed) s) <>
  (eval (ENum (0)) s))%Z)) 54)::(EA 49 (AGuard
  (fun s => ((eval (EVar V_lookup_gs_simple_font_encoding_r_packed) s) =
  (eval (ENum (0)) s))%Z)) 50)::(EA 50 AWeaken 51)::(EA 51 (AAssign
  V_lookup_gs_simple_font_encoding_rnidx None) 52)::(EA 52 ANone 53)::
  (EA 53 AWeaken 58)::(EA 54 AWeaken 55)::(EA 55 (AAssign
  V_lookup_gs_simple_font_encoding_rnidx None) 56)::(EA 56 ANone 57)::
  (EA 57 AWeaken 58)::(EA 58 (AGuard
  (fun s => ((eval (EVar V_lookup_gs_simple_font_encoding_f_packed) s) <>
  (eval (ENum (0)) s))%Z)) 62)::(EA 58 (AGuard
  (fun s => ((eval (EVar V_lookup_gs_simple_font_encoding_f_packed) s) =
  (eval (ENum (0)) s))%Z)) 59)::(EA 59 AWeaken 60)::(EA 60 ANone 61)::
  (EA 61 AWeaken 65)::(EA 62 AWeaken 63)::(EA 63 ANone 64)::
  (EA 64 AWeaken 65)::(EA 65 ANone 66)::(EA 65 ANone 68)::
  (EA 66 AWeaken 67)::(EA 67 ANone 68)::(EA 67 ANone 73)::(EA 68 (AAssign
  V_lookup_gs_simple_font_encoding_match
  (Some (EAdd (EVar V_lookup_gs_simple_font_encoding_match)
  (ENum (-1))))) 69)::(EA 69 AWeaken 70)::(EA 70 (AGuard
  (fun s => ((eval (EAdd (EVar V_lookup_gs_simple_font_encoding_match)
  (ENum (-1))) s) <= (eval (EVar V_lookup_gs_simple_font_encoding_best)
  s))%Z)) 76)::(EA 70 (AGuard
  (fun s => ((eval (EAdd (EVar V_lookup_gs_simple_font_encoding_match)
  (ENum (-1))) s) > (eval (EVar V_lookup_gs_simple_font_encoding_best)
  s))%Z)) 71)::(EA 71 AWeaken 72)::(EA 72 ANone 73)::(EA 73 ANone 74)::
  (EA 74 ANone 75)::(EA 75 (AAssign V_lookup_gs_simple_font_encoding_z
  (Some (EAdd (ENum (1)) (EVar V_lookup_gs_simple_font_encoding_z)))) 44)::
  (EA 76 AWeaken 77)::(EA 77 ANone 78)::(EA 78 AWeaken 79)::(EA 79 (AGuard
  (fun s => ((eval (EVar V_lookup_gs_simple_font_encoding_match) s) >
  (eval (EVar V_lookup_gs_simple_font_encoding_best) s))%Z)) 81)::
  (EA 79 (AGuard
  (fun s => ((eval (EVar V_lookup_gs_simple_font_encoding_match) s) <=
  (eval (EVar V_lookup_gs_simple_font_encoding_best) s))%Z)) 80)::
  (EA 80 AWeaken 85)::(EA 81 AWeaken 82)::(EA 82 (AAssign
  V_lookup_gs_simple_font_encoding_best
  (Some (EVar V_lookup_gs_simple_font_encoding_match))) 83)::(EA 83 (AAssign
  V_lookup_gs_simple_font_encoding_near_index
  (Some (EVar V_lookup_gs_simple_font_encoding_index))) 84)::
  (EA 84 ANone 85)::(EA 85 ANone 87)::(EA 86 ANone 87)::(EA 87 ANone 88)::
  (EA 88 (AAssign V_lookup_gs_simple_font_encoding_z (Some (EAdd (ENum (1))
  (EVar V_lookup_gs_simple_font_encoding_z)))) 27)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_lookup_gs_simple_font_encoding => Pedges_lookup_gs_simple_font_encoding
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_lookup_gs_simple_font_encoding => 35
     end)%positive;
  var_global := var_global
}.

Definition ai_lookup_gs_simple_font_encoding (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_lookup_gs_simple_font_encoding_z <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_z <= 0)%Z
   | 3 => (-1 * s V_lookup_gs_simple_font_encoding_z <= 0 /\ 1 * s V_lookup_gs_simple_font_encoding_z <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_match <= 0)%Z
   | 4 => (-1 * s V_lookup_gs_simple_font_encoding_match <= 0 /\ 1 * s V_lookup_gs_simple_font_encoding_z <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_z <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_best <= 0)%Z
   | 5 => (-1 * s V_lookup_gs_simple_font_encoding_best <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_z <= 0 /\ 1 * s V_lookup_gs_simple_font_encoding_z <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_match <= 0)%Z
   | 6 => (-1 * s V_lookup_gs_simple_font_encoding_match <= 0 /\ 1 * s V_lookup_gs_simple_font_encoding_z <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_z <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_best <= 0 /\ 1 * s V_lookup_gs_simple_font_encoding_index + -5 <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_index + 5 <= 0)%Z
   | 7 => (1 * s V_lookup_gs_simple_font_encoding_index + -5 <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_z <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_index + 1 <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_best <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_match <= 0)%Z
   | 8 => (-1 * s V_lookup_gs_simple_font_encoding_match <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_best <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_z <= 0 /\ 1 * s V_lookup_gs_simple_font_encoding_index + -4 <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_index <= 0)%Z
   | 9 => (-1 * s V_lookup_gs_simple_font_encoding_index <= 0 /\ 1 * s V_lookup_gs_simple_font_encoding_index + -4 <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_z <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_best <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_match <= 0)%Z
   | 10 => (-1 * s V_lookup_gs_simple_font_encoding_match <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_best <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_z <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_index <= 0 /\ 1 * s V_lookup_gs_simple_font_encoding_index <= 0)%Z
   | 11 => (-1 * s V_lookup_gs_simple_font_encoding_match <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_best <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_z <= 0 /\ 1 * s V_lookup_gs_simple_font_encoding_index + -4 <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_index + 1 <= 0)%Z
   | 12 => (-1 * s V_lookup_gs_simple_font_encoding_index + 1 <= 0 /\ 1 * s V_lookup_gs_simple_font_encoding_index + -4 <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_z <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_best <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_match <= 0)%Z
   | 13 => (-1 * s V_lookup_gs_simple_font_encoding_match <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_best <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_z <= 0 /\ 1 * s V_lookup_gs_simple_font_encoding_index + -4 <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_index + 1 <= 0)%Z
   | 14 => (-1 * s V_lookup_gs_simple_font_encoding_index + 1 <= 0 /\ 1 * s V_lookup_gs_simple_font_encoding_index + -4 <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_z <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_best <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_match <= 0)%Z
   | 15 => (-1 * s V_lookup_gs_simple_font_encoding_match <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_best <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_z <= 0 /\ 1 * s V_lookup_gs_simple_font_encoding_index + -4 <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_index + 1 <= 0)%Z
   | 16 => (-1 * s V_lookup_gs_simple_font_encoding_match <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_best <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_z <= 0 /\ 1 * s V_lookup_gs_simple_font_encoding_index + -4 <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_index + 1 <= 0)%Z
   | 17 => (-1 * s V_lookup_gs_simple_font_encoding_index <= 0 /\ 1 * s V_lookup_gs_simple_font_encoding_index + -4 <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_z <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_best <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_match <= 0)%Z
   | 18 => (-1 * s V_lookup_gs_simple_font_encoding_match <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_best <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_z <= 0 /\ 1 * s V_lookup_gs_simple_font_encoding_index + -4 <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_index <= 0 /\ 1 * s V_lookup_gs_simple_font_encoding_pfont_dref_off328 + -4 <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_pfont_dref_off328 <= 0)%Z
   | 19 => (-1 * s V_lookup_gs_simple_font_encoding_pfont_dref_off328 <= 0 /\ 1 * s V_lookup_gs_simple_font_encoding_pfont_dref_off328 + -4 <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_index <= 0 /\ 1 * s V_lookup_gs_simple_font_encoding_index + -4 <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_z <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_best <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_match <= 0)%Z
   | 20 => (-1 * s V_lookup_gs_simple_font_encoding_match <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_best <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_z <= 0 /\ 1 * s V_lookup_gs_simple_font_encoding_index + -4 <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_index <= 0 /\ 1 * s V_lookup_gs_simple_font_encoding_pfont_dref_off328 + -4 <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_pfont_dref_off328 <= 0)%Z
   | 21 => (False)%Z
   | 22 => (False)%Z
   | 23 => (False)%Z
   | 24 => (False)%Z
   | 25 => (False)%Z
   | 26 => (False)%Z
   | 27 => (False)%Z
   | 28 => (False)%Z
   | 29 => (False)%Z
   | 30 => (False)%Z
   | 31 => (False)%Z
   | 32 => (False)%Z
   | 33 => (-1 * s V_lookup_gs_simple_font_encoding_pfont_dref_off328 <= 0 /\ 1 * s V_lookup_gs_simple_font_encoding_pfont_dref_off328 + -4 <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_index <= 0 /\ 1 * s V_lookup_gs_simple_font_encoding_index + -4 <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_z <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_best <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_match <= 0)%Z
   | 34 => (-1 * s V_lookup_gs_simple_font_encoding_match <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_best <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_z <= 0 /\ 1 * s V_lookup_gs_simple_font_encoding_index + -4 <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_index <= 0 /\ 1 * s V_lookup_gs_simple_font_encoding_pfont_dref_off328 + -4 <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_pfont_dref_off328 <= 0 /\ 1 * s V_lookup_gs_simple_font_encoding_pfont_dref_off332 + -4 <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_pfont_dref_off332 <= 0)%Z
   | 35 => (-1 * s V_lookup_gs_simple_font_encoding_pfont_dref_off332 <= 0 /\ 1 * s V_lookup_gs_simple_font_encoding_pfont_dref_off332 + -4 <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_pfont_dref_off328 <= 0 /\ 1 * s V_lookup_gs_simple_font_encoding_pfont_dref_off328 + -4 <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_index <= 0 /\ 1 * s V_lookup_gs_simple_font_encoding_index + -4 <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_z <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_best <= 0 /\ -1 * s V_lookup_gs_simple_font_encoding_match <= 0)%Z
   | 36 => (False)%Z
   | 37 => (False)%Z
   | 38 => (False)%Z
   | 39 => (False)%Z
   | 40 => (False)%Z
   | 41 => (False)%Z
   | 42 => (False)%Z
   | 43 => (False)%Z
   | 44 => (False)%Z
   | 45 => (False)%Z
   | 46 => (False)%Z
   | 47 => (False)%Z
   | 48 => (False)%Z
   | 49 => (False)%Z
   | 50 => (False)%Z
   | 51 => (False)%Z
   | 52 => (False)%Z
   | 53 => (False)%Z
   | 54 => (False)%Z
   | 55 => (False)%Z
   | 56 => (False)%Z
   | 57 => (False)%Z
   | 58 => (False)%Z
   | 59 => (False)%Z
   | 60 => (False)%Z
   | 61 => (False)%Z
   | 62 => (False)%Z
   | 63 => (False)%Z
   | 64 => (False)%Z
   | 65 => (False)%Z
   | 66 => (False)%Z
   | 67 => (False)%Z
   | 68 => (False)%Z
   | 69 => (False)%Z
   | 70 => (False)%Z
   | 71 => (False)%Z
   | 72 => (False)%Z
   | 73 => (False)%Z
   | 74 => (False)%Z
   | 75 => (False)%Z
   | 76 => (False)%Z
   | 77 => (False)%Z
   | 78 => (False)%Z
   | 79 => (False)%Z
   | 80 => (False)%Z
   | 81 => (False)%Z
   | 82 => (False)%Z
   | 83 => (False)%Z
   | 84 => (False)%Z
   | 85 => (False)%Z
   | 86 => (False)%Z
   | 87 => (False)%Z
   | 88 => (False)%Z
   | _ => False
   end)%positive.

Definition annot0_lookup_gs_simple_font_encoding (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((4 # 1) <= z)%Q
   | 2 => ((4 # 1) + s V_lookup_gs_simple_font_encoding_z <= z)%Q
   | 3 => ((4 # 1) + s V_lookup_gs_simple_font_encoding_z <= z)%Q
   | 4 => ((4 # 1) + s V_lookup_gs_simple_font_encoding_z <= z)%Q
   | 5 => ((4 # 1) + s V_lookup_gs_simple_font_encoding_z <= z)%Q
   | 6 => ((3 # 2) + (1 # 2) * s V_lookup_gs_simple_font_encoding_index
           + s V_lookup_gs_simple_font_encoding_z
           - (1 # 2) * max0(5 - s V_lookup_gs_simple_font_encoding_index) <= z)%Q
   | 7 => ((3 # 2) + (1 # 2) * s V_lookup_gs_simple_font_encoding_index
           + s V_lookup_gs_simple_font_encoding_z
           - (1 # 2) * max0(5 - s V_lookup_gs_simple_font_encoding_index) <= z)%Q
   | 8 => hints
     [(*0 0.5*) F_binom_monotonic 1 (F_max0_ge_arg (5
                                                    - s V_lookup_gs_simple_font_encoding_index)) (F_check_ge (5
                                                                    - s V_lookup_gs_simple_font_encoding_index) (0))]
     ((2 # 1) + (1 # 2) * s V_lookup_gs_simple_font_encoding_index
      + s V_lookup_gs_simple_font_encoding_z
      - (1 # 2) * max0(4 - s V_lookup_gs_simple_font_encoding_index) <= z)%Q
   | 9 => ((9 # 2) + s V_lookup_gs_simple_font_encoding_z
           - (1 # 2) * max0(4 - s V_lookup_gs_simple_font_encoding_index)
           - (1 # 2) * max0(5 - s V_lookup_gs_simple_font_encoding_index) <= z)%Q
   | 10 => hints
     [(*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (4
                                                                 - s V_lookup_gs_simple_font_encoding_index) (0))) (F_max0_ge_0 (4
                                                                    - s V_lookup_gs_simple_font_encoding_index))]
     ((9 # 2) + s V_lookup_gs_simple_font_encoding_z
      - (1 # 2) * max0(4 - s V_lookup_gs_simple_font_encoding_index)
      - (1 # 2) * max0(5 - s V_lookup_gs_simple_font_encoding_index) <= z)%Q
   | 11 => hints
     [(*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (4
                                                                 - s V_lookup_gs_simple_font_encoding_index) (0))) (F_max0_ge_0 (4
                                                                    - s V_lookup_gs_simple_font_encoding_index))]
     ((9 # 2) + s V_lookup_gs_simple_font_encoding_z
      - (1 # 2) * max0(4 - s V_lookup_gs_simple_font_encoding_index)
      - (1 # 2) * max0(5 - s V_lookup_gs_simple_font_encoding_index) <= z)%Q
   | 12 => ((5 # 2) + (1 # 2) * s V_lookup_gs_simple_font_encoding_index
            + s V_lookup_gs_simple_font_encoding_z
            - (1 # 2) * max0(5 - s V_lookup_gs_simple_font_encoding_index) <= z)%Q
   | 13 => ((5 # 2) + (1 # 2) * s V_lookup_gs_simple_font_encoding_index
            + s V_lookup_gs_simple_font_encoding_z
            - (1 # 2) * max0(5 - s V_lookup_gs_simple_font_encoding_index) <= z)%Q
   | 14 => ((5 # 2) + (1 # 2) * s V_lookup_gs_simple_font_encoding_index
            + s V_lookup_gs_simple_font_encoding_z
            - (1 # 2) * max0(5 - s V_lookup_gs_simple_font_encoding_index) <= z)%Q
   | 15 => ((5 # 2) + (1 # 2) * s V_lookup_gs_simple_font_encoding_index
            + s V_lookup_gs_simple_font_encoding_z
            - (1 # 2) * max0(5 - s V_lookup_gs_simple_font_encoding_index) <= z)%Q
   | 16 => ((5 # 2) + (1 # 2) * s V_lookup_gs_simple_font_encoding_index
            + s V_lookup_gs_simple_font_encoding_z
            - (1 # 2) * max0(5 - s V_lookup_gs_simple_font_encoding_index) <= z)%Q
   | 17 => ((5 # 2) + (1 # 2) * s V_lookup_gs_simple_font_encoding_index
            + s V_lookup_gs_simple_font_encoding_z
            - (1 # 2) * max0(5 - s V_lookup_gs_simple_font_encoding_index) <= z)%Q
   | 18 => ((5 # 2) - (1 # 2) * s V_lookup_gs_simple_font_encoding_index
            + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
            + s V_lookup_gs_simple_font_encoding_z
            - (1 # 2) * max0(5 - s V_lookup_gs_simple_font_encoding_index) <= z)%Q
   | 19 => ((5 # 2) - (1 # 2) * s V_lookup_gs_simple_font_encoding_index
            + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
            + s V_lookup_gs_simple_font_encoding_z
            - (1 # 2) * max0(5 - s V_lookup_gs_simple_font_encoding_index) <= z)%Q
   | 20 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_lookup_gs_simple_font_encoding_pfont_dref_off328)) (F_check_ge (0) (0));
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_lookup_gs_simple_font_encoding_pfont_dref_off328) (0))) (F_max0_ge_0 (s V_lookup_gs_simple_font_encoding_pfont_dref_off328))]
     ((5 # 2) - (1 # 2) * s V_lookup_gs_simple_font_encoding_index
      + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
      + s V_lookup_gs_simple_font_encoding_z
      - (1 # 2) * max0(5 - s V_lookup_gs_simple_font_encoding_index) <= z)%Q
   | 21 => hints
     [(*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (5
                                                                 - s V_lookup_gs_simple_font_encoding_index) (0))) (F_max0_ge_0 (5
                                                                    - s V_lookup_gs_simple_font_encoding_index))]
     ((5 # 2) - (1 # 2) * s V_lookup_gs_simple_font_encoding_index
      + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
      + s V_lookup_gs_simple_font_encoding_z
      - (1 # 2) * max0(5 - s V_lookup_gs_simple_font_encoding_index) <= z)%Q
   | 22 => (s V_lookup_gs_simple_font_encoding_pfont_dref_off328
            + s V_lookup_gs_simple_font_encoding_z <= z)%Q
   | 23 => (-(1 # 6)
            + (2 # 15) * s V_lookup_gs_simple_font_encoding_near_index
            + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
            + s V_lookup_gs_simple_font_encoding_z
            + (1 # 20) * max0(5
                              - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 24 => (-(1 # 6)
            + (2 # 15) * s V_lookup_gs_simple_font_encoding_near_index
            + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
            + s V_lookup_gs_simple_font_encoding_z
            + (1 # 20) * max0(5
                              - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 25 => (-(1 # 6)
            + (2 # 15) * s V_lookup_gs_simple_font_encoding_near_index
            + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
            + s V_lookup_gs_simple_font_encoding_z
            + (1 # 20) * max0(5
                              - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 26 => (-(1 # 1) + (1 # 6) * s V_lookup_gs_simple_font_encoding_index
            + (2 # 15) * s V_lookup_gs_simple_font_encoding_near_index
            + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
            + s V_lookup_gs_simple_font_encoding_z
            + (1 # 20) * max0(5
                              - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 27 => (-(1 # 1) + (1 # 6) * s V_lookup_gs_simple_font_encoding_index
            + (2 # 15) * s V_lookup_gs_simple_font_encoding_near_index
            + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
            + s V_lookup_gs_simple_font_encoding_z
            + (1 # 20) * max0(5
                              - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 28 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_lookup_gs_simple_font_encoding_pfont_dref_off328) (0))) (F_max0_ge_0 (s V_lookup_gs_simple_font_encoding_pfont_dref_off328));
      (*0 0.166667*) F_binom_monotonic 1 (F_max0_ge_arg (5
                                                         - s V_lookup_gs_simple_font_encoding_index)) (F_check_ge (5
                                                                    - s V_lookup_gs_simple_font_encoding_index) (0))]
     (-(5 # 6) + (1 # 6) * s V_lookup_gs_simple_font_encoding_index
      + (2 # 15) * s V_lookup_gs_simple_font_encoding_near_index
      + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
      + s V_lookup_gs_simple_font_encoding_z
      + (1 # 20) * max0(5 - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 29 => ((2 # 15) * s V_lookup_gs_simple_font_encoding_near_index
            + s V_lookup_gs_simple_font_encoding_z
            - (1 # 6) * max0(5 - s V_lookup_gs_simple_font_encoding_index)
            + (1 # 20) * max0(5
                              - s V_lookup_gs_simple_font_encoding_near_index)
            + max0(s V_lookup_gs_simple_font_encoding_pfont_dref_off328) <= z)%Q
   | 30 => hints
     [(*-0.166667 0*) F_max0_pre_decrement 1 (s V_lookup_gs_simple_font_encoding_index) (1);
      (*-0.166667 0*) F_max0_ge_0 (-1
                                   + s V_lookup_gs_simple_font_encoding_index);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_lookup_gs_simple_font_encoding_pfont_dref_off328)) (F_check_ge (0) (0));
      (*-0.166667 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_lookup_gs_simple_font_encoding_index) (0))) (F_max0_ge_0 (s V_lookup_gs_simple_font_encoding_index));
      (*-0.55 0*) F_binom_monotonic 1 (F_max0_ge_arg (5
                                                      - s V_lookup_gs_simple_font_encoding_near_index)) (F_check_ge (5
                                                                    - s V_lookup_gs_simple_font_encoding_near_index) (0));
      (*-0.166667 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (5
                                                                    - s V_lookup_gs_simple_font_encoding_index) (0))) (F_max0_ge_0 (5
                                                                    - s V_lookup_gs_simple_font_encoding_index));
      (*-0.0833333 0*) F_binom_monotonic 1 (F_max0_ge_0 (-5
                                                         + s V_lookup_gs_simple_font_encoding_near_index)) (F_check_ge (0) (0));
      (*-0.0833333 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-5
                                                                    + s V_lookup_gs_simple_font_encoding_near_index) (0))) (F_max0_ge_0 (-5
                                                                    + s V_lookup_gs_simple_font_encoding_near_index))]
     ((2 # 15) * s V_lookup_gs_simple_font_encoding_near_index
      + s V_lookup_gs_simple_font_encoding_z
      - (1 # 6) * max0(5 - s V_lookup_gs_simple_font_encoding_index)
      + (1 # 20) * max0(5 - s V_lookup_gs_simple_font_encoding_near_index)
      + max0(s V_lookup_gs_simple_font_encoding_pfont_dref_off328) <= z)%Q
   | 31 => ((5 # 2) - (1 # 2) * s V_lookup_gs_simple_font_encoding_near_index
            + s V_lookup_gs_simple_font_encoding_z
            - (1 # 2) * max0(5
                             - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 32 => ((5 # 2) - (1 # 2) * s V_lookup_gs_simple_font_encoding_index
            + s V_lookup_gs_simple_font_encoding_z
            - (1 # 2) * max0(5 - s V_lookup_gs_simple_font_encoding_index) <= z)%Q
   | 33 => ((5 # 2) - (1 # 2) * s V_lookup_gs_simple_font_encoding_index
            + s V_lookup_gs_simple_font_encoding_z
            - (1 # 2) * max0(5 - s V_lookup_gs_simple_font_encoding_index) <= z)%Q
   | 34 => hints
     [(*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (5
                                                                 - s V_lookup_gs_simple_font_encoding_index) (0))) (F_max0_ge_0 (5
                                                                    - s V_lookup_gs_simple_font_encoding_index))]
     ((5 # 2) - (1 # 2) * s V_lookup_gs_simple_font_encoding_index
      + s V_lookup_gs_simple_font_encoding_z
      - (1 # 2) * max0(5 - s V_lookup_gs_simple_font_encoding_index) <= z)%Q
   | 35 => (s V_lookup_gs_simple_font_encoding_z <= z)%Q
   | 36 => hints
     [(*-0.166667 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-5
                                                                    + s V_lookup_gs_simple_font_encoding_index) (0))) (F_max0_ge_0 (-5
                                                                    + s V_lookup_gs_simple_font_encoding_index))]
     ((2 # 15) * s V_lookup_gs_simple_font_encoding_near_index
      + s V_lookup_gs_simple_font_encoding_z
      - (1 # 6) * max0(5 - s V_lookup_gs_simple_font_encoding_index)
      + (1 # 20) * max0(5 - s V_lookup_gs_simple_font_encoding_near_index)
      + max0(s V_lookup_gs_simple_font_encoding_pfont_dref_off328) <= z)%Q
   | 37 => ((5 # 6) - (1 # 6) * s V_lookup_gs_simple_font_encoding_index
            + (2 # 15) * s V_lookup_gs_simple_font_encoding_near_index
            + s V_lookup_gs_simple_font_encoding_z
            + (1 # 6) * max0(-5 + s V_lookup_gs_simple_font_encoding_index)
            - (1 # 6) * max0(5 - s V_lookup_gs_simple_font_encoding_index)
            + (1 # 20) * max0(5
                              - s V_lookup_gs_simple_font_encoding_near_index)
            + max0(s V_lookup_gs_simple_font_encoding_pfont_dref_off328) <= z)%Q
   | 38 => ((5 # 6) - (1 # 6) * s V_lookup_gs_simple_font_encoding_index
            + (2 # 15) * s V_lookup_gs_simple_font_encoding_near_index
            + s V_lookup_gs_simple_font_encoding_z
            + (1 # 6) * max0(-5 + s V_lookup_gs_simple_font_encoding_index)
            - (1 # 6) * max0(5 - s V_lookup_gs_simple_font_encoding_index)
            + (1 # 20) * max0(5
                              - s V_lookup_gs_simple_font_encoding_near_index)
            + max0(s V_lookup_gs_simple_font_encoding_pfont_dref_off328) <= z)%Q
   | 39 => ((5 # 6) - (1 # 6) * s V_lookup_gs_simple_font_encoding_index
            + (2 # 15) * s V_lookup_gs_simple_font_encoding_near_index
            + s V_lookup_gs_simple_font_encoding_z
            + (1 # 6) * max0(-5 + s V_lookup_gs_simple_font_encoding_index)
            - (1 # 6) * max0(5 - s V_lookup_gs_simple_font_encoding_index)
            + (1 # 20) * max0(5
                              - s V_lookup_gs_simple_font_encoding_near_index)
            + max0(s V_lookup_gs_simple_font_encoding_pfont_dref_off328) <= z)%Q
   | 40 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_lookup_gs_simple_font_encoding_pfont_dref_off328)) (F_check_ge (s V_lookup_gs_simple_font_encoding_pfont_dref_off328) (0));
      (*-0.166667 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_lookup_gs_simple_font_encoding_index)) (F_check_ge (0) (0));
      (*-0.166667 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_lookup_gs_simple_font_encoding_index) (0))) (F_max0_ge_0 (-
                                                                    s V_lookup_gs_simple_font_encoding_index));
      (*-0.166667 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (5
                                                                    - s V_lookup_gs_simple_font_encoding_index) (0))) (F_max0_ge_0 (5
                                                                    - s V_lookup_gs_simple_font_encoding_index));
      (*-0.166667 0*) F_binom_monotonic 1 (F_max0_ge_0 (-5
                                                        + s V_lookup_gs_simple_font_encoding_index)) (F_check_ge (0) (0))]
     ((5 # 6) - (1 # 6) * s V_lookup_gs_simple_font_encoding_index
      + (2 # 15) * s V_lookup_gs_simple_font_encoding_near_index
      + s V_lookup_gs_simple_font_encoding_z
      + (1 # 6) * max0(-5 + s V_lookup_gs_simple_font_encoding_index)
      - (1 # 6) * max0(5 - s V_lookup_gs_simple_font_encoding_index)
      + (1 # 20) * max0(5 - s V_lookup_gs_simple_font_encoding_near_index)
      + max0(s V_lookup_gs_simple_font_encoding_pfont_dref_off328) <= z)%Q
   | 41 => ((1 # 6) * s V_lookup_gs_simple_font_encoding_index
            + (2 # 15) * s V_lookup_gs_simple_font_encoding_near_index
            + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
            + s V_lookup_gs_simple_font_encoding_z
            + (1 # 20) * max0(5
                              - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 42 => ((1 # 6) * s V_lookup_gs_simple_font_encoding_index
            + (2 # 15) * s V_lookup_gs_simple_font_encoding_near_index
            + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
            + s V_lookup_gs_simple_font_encoding_z
            + (1 # 20) * max0(5
                              - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 43 => ((1 # 6) * s V_lookup_gs_simple_font_encoding_index
            + (2 # 15) * s V_lookup_gs_simple_font_encoding_near_index
            + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
            + s V_lookup_gs_simple_font_encoding_z
            + (1 # 20) * max0(5
                              - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 44 => ((1 # 6) * s V_lookup_gs_simple_font_encoding_index
            + (2 # 15) * s V_lookup_gs_simple_font_encoding_near_index
            + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
            + s V_lookup_gs_simple_font_encoding_z
            + (1 # 20) * max0(5
                              - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 45 => ((1 # 6) * s V_lookup_gs_simple_font_encoding_index
            + (2 # 15) * s V_lookup_gs_simple_font_encoding_near_index
            + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
            + s V_lookup_gs_simple_font_encoding_z
            + (1 # 20) * max0(5
                              - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 46 => ((1 # 6) * s V_lookup_gs_simple_font_encoding_index
            + (2 # 15) * s V_lookup_gs_simple_font_encoding_near_index
            + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
            + s V_lookup_gs_simple_font_encoding_z
            + (1 # 20) * max0(5
                              - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 47 => ((1 # 6) * s V_lookup_gs_simple_font_encoding_index
            + (2 # 15) * s V_lookup_gs_simple_font_encoding_near_index
            + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
            + s V_lookup_gs_simple_font_encoding_z
            + (1 # 20) * max0(5
                              - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 48 => hints
     [(*0 0.05*) F_binom_monotonic 1 (F_max0_ge_0 (5
                                                   - s V_lookup_gs_simple_font_encoding_near_index)) (F_check_ge (0) (0));
      (*-8.33555e-13 1.3125*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (4
                                                                    - s V_lookup_gs_simple_font_encoding_near_index) (0))) (F_max0_ge_0 (4
                                                                    - s V_lookup_gs_simple_font_encoding_near_index));
      (*1.36642e-12 1.05*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-5
                                                                    + s V_lookup_gs_simple_font_encoding_near_index) (0))) (F_max0_ge_0 (-5
                                                                    + s V_lookup_gs_simple_font_encoding_near_index))]
     ((1 # 6) * s V_lookup_gs_simple_font_encoding_index
      + (2 # 15) * s V_lookup_gs_simple_font_encoding_near_index
      + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
      + s V_lookup_gs_simple_font_encoding_z
      + (1 # 20) * max0(5 - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 49 => ((1 # 6) * s V_lookup_gs_simple_font_encoding_index
            + (19 # 48) * s V_lookup_gs_simple_font_encoding_near_index
            + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
            + s V_lookup_gs_simple_font_encoding_z
            + (21 # 20) * max0(-5
                               + s V_lookup_gs_simple_font_encoding_near_index)
            + (21 # 16) * max0(4
                               - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 50 => ((1 # 6) * s V_lookup_gs_simple_font_encoding_index
            + (19 # 48) * s V_lookup_gs_simple_font_encoding_near_index
            + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
            + s V_lookup_gs_simple_font_encoding_z
            + (21 # 20) * max0(-5
                               + s V_lookup_gs_simple_font_encoding_near_index)
            + (21 # 16) * max0(4
                               - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 51 => ((1 # 6) * s V_lookup_gs_simple_font_encoding_index
            + (19 # 48) * s V_lookup_gs_simple_font_encoding_near_index
            + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
            + s V_lookup_gs_simple_font_encoding_z
            + (21 # 20) * max0(-5
                               + s V_lookup_gs_simple_font_encoding_near_index)
            + (21 # 16) * max0(4
                               - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 52 => ((1 # 6) * s V_lookup_gs_simple_font_encoding_index
            + (19 # 48) * s V_lookup_gs_simple_font_encoding_near_index
            + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
            + s V_lookup_gs_simple_font_encoding_z
            + (21 # 20) * max0(-5
                               + s V_lookup_gs_simple_font_encoding_near_index)
            + (21 # 16) * max0(4
                               - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 53 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_lookup_gs_simple_font_encoding_pfont_dref_off328)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_lookup_gs_simple_font_encoding_pfont_dref_off328) (0))) (F_max0_ge_0 (s V_lookup_gs_simple_font_encoding_pfont_dref_off328));
      (*-0.05 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (5
                                                                  - s V_lookup_gs_simple_font_encoding_near_index) (0))) (F_max0_ge_0 (5
                                                                    - s V_lookup_gs_simple_font_encoding_near_index));
      (*-1.05 0*) F_binom_monotonic 1 (F_max0_ge_0 (-5
                                                    + s V_lookup_gs_simple_font_encoding_near_index)) (F_check_ge (0) (0))]
     ((1 # 6) * s V_lookup_gs_simple_font_encoding_index
      + (19 # 48) * s V_lookup_gs_simple_font_encoding_near_index
      + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
      + s V_lookup_gs_simple_font_encoding_z
      + (21 # 20) * max0(-5 + s V_lookup_gs_simple_font_encoding_near_index)
      + (21 # 16) * max0(4 - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 54 => ((1 # 6) * s V_lookup_gs_simple_font_encoding_index
            + (19 # 48) * s V_lookup_gs_simple_font_encoding_near_index
            + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
            + s V_lookup_gs_simple_font_encoding_z
            + (21 # 20) * max0(-5
                               + s V_lookup_gs_simple_font_encoding_near_index)
            + (21 # 16) * max0(4
                               - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 55 => ((1 # 6) * s V_lookup_gs_simple_font_encoding_index
            + (19 # 48) * s V_lookup_gs_simple_font_encoding_near_index
            + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
            + s V_lookup_gs_simple_font_encoding_z
            + (21 # 20) * max0(-5
                               + s V_lookup_gs_simple_font_encoding_near_index)
            + (21 # 16) * max0(4
                               - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 56 => ((1 # 6) * s V_lookup_gs_simple_font_encoding_index
            + (19 # 48) * s V_lookup_gs_simple_font_encoding_near_index
            + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
            + s V_lookup_gs_simple_font_encoding_z
            + (21 # 20) * max0(-5
                               + s V_lookup_gs_simple_font_encoding_near_index)
            + (21 # 16) * max0(4
                               - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 57 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_lookup_gs_simple_font_encoding_pfont_dref_off328)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_lookup_gs_simple_font_encoding_pfont_dref_off328) (0))) (F_max0_ge_0 (s V_lookup_gs_simple_font_encoding_pfont_dref_off328));
      (*-0.05 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (5
                                                                  - s V_lookup_gs_simple_font_encoding_near_index) (0))) (F_max0_ge_0 (5
                                                                    - s V_lookup_gs_simple_font_encoding_near_index));
      (*-1.05 0*) F_binom_monotonic 1 (F_max0_ge_0 (-5
                                                    + s V_lookup_gs_simple_font_encoding_near_index)) (F_check_ge (0) (0))]
     ((1 # 6) * s V_lookup_gs_simple_font_encoding_index
      + (19 # 48) * s V_lookup_gs_simple_font_encoding_near_index
      + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
      + s V_lookup_gs_simple_font_encoding_z
      + (21 # 20) * max0(-5 + s V_lookup_gs_simple_font_encoding_near_index)
      + (21 # 16) * max0(4 - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 58 => (-(1 # 4) + (1 # 6) * s V_lookup_gs_simple_font_encoding_index
            + (37 # 83) * s V_lookup_gs_simple_font_encoding_near_index
            + s V_lookup_gs_simple_font_encoding_z
            + (21 # 16) * max0(4
                               - s V_lookup_gs_simple_font_encoding_near_index)
            + (1 # 20) * max0(5
                              - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 59 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_lookup_gs_simple_font_encoding_near_index) (0))) (F_max0_ge_0 (-
                                                                    s V_lookup_gs_simple_font_encoding_near_index))]
     (-(1 # 4) + (1 # 6) * s V_lookup_gs_simple_font_encoding_index
      + (37 # 83) * s V_lookup_gs_simple_font_encoding_near_index
      + s V_lookup_gs_simple_font_encoding_z
      + (21 # 16) * max0(4 - s V_lookup_gs_simple_font_encoding_near_index)
      + (1 # 20) * max0(5 - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 60 => (-(1 # 4) + (1 # 6) * s V_lookup_gs_simple_font_encoding_index
            + (120 # 83) * s V_lookup_gs_simple_font_encoding_near_index
            + s V_lookup_gs_simple_font_encoding_z
            + (21 # 16) * max0(4
                               - s V_lookup_gs_simple_font_encoding_near_index)
            + (1 # 20) * max0(5
                              - s V_lookup_gs_simple_font_encoding_near_index)
            + max0(-s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 61 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_lookup_gs_simple_font_encoding_near_index)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (4
                                                 - s V_lookup_gs_simple_font_encoding_pfont_dref_off328)) (F_check_ge (0) (0));
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (4
                                                              - s V_lookup_gs_simple_font_encoding_pfont_dref_off328) (0))) (F_max0_ge_0 (4
                                                                    - s V_lookup_gs_simple_font_encoding_pfont_dref_off328));
      (*-1.3125 0*) F_binom_monotonic 1 (F_max0_ge_arg (4
                                                        - s V_lookup_gs_simple_font_encoding_near_index)) (F_check_ge (4
                                                                    - s V_lookup_gs_simple_font_encoding_near_index) (0))]
     (-(1 # 4) + (1 # 6) * s V_lookup_gs_simple_font_encoding_index
      + (120 # 83) * s V_lookup_gs_simple_font_encoding_near_index
      + s V_lookup_gs_simple_font_encoding_z
      + (21 # 16) * max0(4 - s V_lookup_gs_simple_font_encoding_near_index)
      + (1 # 20) * max0(5 - s V_lookup_gs_simple_font_encoding_near_index)
      + max0(-s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 62 => hints
     [(*0 0.05*) F_binom_monotonic 1 (F_max0_ge_arg (5
                                                     - s V_lookup_gs_simple_font_encoding_near_index)) (F_check_ge (5
                                                                    - s V_lookup_gs_simple_font_encoding_near_index) (0))]
     (-(1 # 4) + (1 # 6) * s V_lookup_gs_simple_font_encoding_index
      + (37 # 83) * s V_lookup_gs_simple_font_encoding_near_index
      + s V_lookup_gs_simple_font_encoding_z
      + (21 # 16) * max0(4 - s V_lookup_gs_simple_font_encoding_near_index)
      + (1 # 20) * max0(5 - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 63 => ((1 # 6) * s V_lookup_gs_simple_font_encoding_index
            + (19 # 48) * s V_lookup_gs_simple_font_encoding_near_index
            + s V_lookup_gs_simple_font_encoding_z
            + (21 # 16) * max0(4
                               - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 64 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_lookup_gs_simple_font_encoding_near_index)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_lookup_gs_simple_font_encoding_near_index) (0))) (F_max0_ge_0 (-
                                                                    s V_lookup_gs_simple_font_encoding_near_index));
      (*0 0.05*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (5
                                                                 - s V_lookup_gs_simple_font_encoding_near_index) (0))) (F_max0_ge_0 (5
                                                                    - s V_lookup_gs_simple_font_encoding_near_index));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (4
                                                 - s V_lookup_gs_simple_font_encoding_pfont_dref_off328)) (F_check_ge (0) (0));
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (4
                                                              - s V_lookup_gs_simple_font_encoding_pfont_dref_off328) (0))) (F_max0_ge_0 (4
                                                                    - s V_lookup_gs_simple_font_encoding_pfont_dref_off328));
      (*-1.3125 0*) F_binom_monotonic 1 (F_max0_ge_arg (4
                                                        - s V_lookup_gs_simple_font_encoding_near_index)) (F_check_ge (4
                                                                    - s V_lookup_gs_simple_font_encoding_near_index) (0))]
     ((1 # 6) * s V_lookup_gs_simple_font_encoding_index
      + (19 # 48) * s V_lookup_gs_simple_font_encoding_near_index
      + s V_lookup_gs_simple_font_encoding_z
      + (21 # 16) * max0(4 - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 65 => ((1 # 1) + (1 # 6) * s V_lookup_gs_simple_font_encoding_index
            + (2 # 15) * s V_lookup_gs_simple_font_encoding_near_index
            + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
            + s V_lookup_gs_simple_font_encoding_z
            + (1 # 20) * max0(5
                              - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 66 => ((1 # 1) + (1 # 6) * s V_lookup_gs_simple_font_encoding_index
            + (2 # 15) * s V_lookup_gs_simple_font_encoding_near_index
            + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
            + s V_lookup_gs_simple_font_encoding_z
            + (1 # 20) * max0(5
                              - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 67 => ((1 # 1) + (1 # 6) * s V_lookup_gs_simple_font_encoding_index
            + (2 # 15) * s V_lookup_gs_simple_font_encoding_near_index
            + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
            + s V_lookup_gs_simple_font_encoding_z
            + (1 # 20) * max0(5
                              - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 68 => ((1 # 1) + (1 # 6) * s V_lookup_gs_simple_font_encoding_index
            + (2 # 15) * s V_lookup_gs_simple_font_encoding_near_index
            + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
            + s V_lookup_gs_simple_font_encoding_z
            + (1 # 20) * max0(5
                              - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 69 => ((1 # 1) + (1 # 6) * s V_lookup_gs_simple_font_encoding_index
            + (2 # 15) * s V_lookup_gs_simple_font_encoding_near_index
            + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
            + s V_lookup_gs_simple_font_encoding_z
            + (1 # 20) * max0(5
                              - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 70 => ((1 # 1) + (1 # 6) * s V_lookup_gs_simple_font_encoding_index
            + (2 # 15) * s V_lookup_gs_simple_font_encoding_near_index
            + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
            + s V_lookup_gs_simple_font_encoding_z
            + (1 # 20) * max0(5
                              - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 71 => ((1 # 1) + (1 # 6) * s V_lookup_gs_simple_font_encoding_index
            + (2 # 15) * s V_lookup_gs_simple_font_encoding_near_index
            + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
            + s V_lookup_gs_simple_font_encoding_z
            + (1 # 20) * max0(5
                              - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 72 => ((1 # 1) + (1 # 6) * s V_lookup_gs_simple_font_encoding_index
            + (2 # 15) * s V_lookup_gs_simple_font_encoding_near_index
            + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
            + s V_lookup_gs_simple_font_encoding_z
            + (1 # 20) * max0(5
                              - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 73 => ((1 # 1) + (1 # 6) * s V_lookup_gs_simple_font_encoding_index
            + (2 # 15) * s V_lookup_gs_simple_font_encoding_near_index
            + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
            + s V_lookup_gs_simple_font_encoding_z
            + (1 # 20) * max0(5
                              - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 74 => ((1 # 1) + (1 # 6) * s V_lookup_gs_simple_font_encoding_index
            + (2 # 15) * s V_lookup_gs_simple_font_encoding_near_index
            + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
            + s V_lookup_gs_simple_font_encoding_z
            + (1 # 20) * max0(5
                              - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 75 => ((1 # 1) + (1 # 6) * s V_lookup_gs_simple_font_encoding_index
            + (2 # 15) * s V_lookup_gs_simple_font_encoding_near_index
            + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
            + s V_lookup_gs_simple_font_encoding_z
            + (1 # 20) * max0(5
                              - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 76 => hints
     [(*0 0.2*) F_max0_monotonic (F_check_ge (s V_lookup_gs_simple_font_encoding_index) (-1
                                                                    + s V_lookup_gs_simple_font_encoding_index));
      (*0 0.2*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_lookup_gs_simple_font_encoding_index) (0))) (F_max0_ge_0 (s V_lookup_gs_simple_font_encoding_index))]
     ((1 # 1) + (1 # 6) * s V_lookup_gs_simple_font_encoding_index
      + (2 # 15) * s V_lookup_gs_simple_font_encoding_near_index
      + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
      + s V_lookup_gs_simple_font_encoding_z
      + (1 # 20) * max0(5 - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 77 => ((1 # 1) - (1 # 30) * s V_lookup_gs_simple_font_encoding_index
            + (2 # 15) * s V_lookup_gs_simple_font_encoding_near_index
            + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
            + s V_lookup_gs_simple_font_encoding_z
            + (1 # 5) * max0(-1 + s V_lookup_gs_simple_font_encoding_index)
            + (1 # 20) * max0(5
                              - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 78 => hints
     [(*-0.2 0*) F_max0_ge_0 (-1 + s V_lookup_gs_simple_font_encoding_index);
      (*-0.2 0*) F_binom_monotonic 1 (F_max0_ge_0 (5
                                                   - s V_lookup_gs_simple_font_encoding_index)) (F_check_ge (0) (0));
      (*-0.2 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (5
                                                                 - s V_lookup_gs_simple_font_encoding_index) (0))) (F_max0_ge_0 (5
                                                                    - s V_lookup_gs_simple_font_encoding_index))]
     ((1 # 1) - (1 # 30) * s V_lookup_gs_simple_font_encoding_index
      + (2 # 15) * s V_lookup_gs_simple_font_encoding_near_index
      + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
      + s V_lookup_gs_simple_font_encoding_z
      + (1 # 5) * max0(-1 + s V_lookup_gs_simple_font_encoding_index)
      + (1 # 20) * max0(5 - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 79 => ((1 # 6) * s V_lookup_gs_simple_font_encoding_index
            + (2 # 15) * s V_lookup_gs_simple_font_encoding_near_index
            + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
            + s V_lookup_gs_simple_font_encoding_z
            + (1 # 20) * max0(5
                              - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 80 => ((1 # 6) * s V_lookup_gs_simple_font_encoding_index
            + (2 # 15) * s V_lookup_gs_simple_font_encoding_near_index
            + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
            + s V_lookup_gs_simple_font_encoding_z
            + (1 # 20) * max0(5
                              - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 81 => hints
     [(*-0.133333 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_lookup_gs_simple_font_encoding_near_index)) (F_check_ge (0) (0));
      (*-0.133333 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_lookup_gs_simple_font_encoding_near_index) (0))) (F_max0_ge_0 (s V_lookup_gs_simple_font_encoding_near_index));
      (*-0.133333 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_lookup_gs_simple_font_encoding_index)) (F_check_ge (0) (0));
      (*-0.133333 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_lookup_gs_simple_font_encoding_index) (0))) (F_max0_ge_0 (-
                                                                    s V_lookup_gs_simple_font_encoding_index));
      (*-0.05 0*) F_binom_monotonic 1 (F_max0_ge_0 (5
                                                    - s V_lookup_gs_simple_font_encoding_near_index)) (F_check_ge (0) (0));
      (*-0.05 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (5
                                                                  - s V_lookup_gs_simple_font_encoding_index) (0))) (F_max0_ge_0 (5
                                                                    - s V_lookup_gs_simple_font_encoding_index));
      (*-0.05 0*) F_binom_monotonic 1 (F_max0_ge_0 (-5
                                                    + s V_lookup_gs_simple_font_encoding_index)) (F_check_ge (0) (0));
      (*-0.05 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-5
                                                                  + s V_lookup_gs_simple_font_encoding_index) (0))) (F_max0_ge_0 (-5
                                                                    + s V_lookup_gs_simple_font_encoding_index))]
     ((1 # 6) * s V_lookup_gs_simple_font_encoding_index
      + (2 # 15) * s V_lookup_gs_simple_font_encoding_near_index
      + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
      + s V_lookup_gs_simple_font_encoding_z
      + (1 # 20) * max0(5 - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 82 => ((3 # 10) * s V_lookup_gs_simple_font_encoding_index
            + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
            + s V_lookup_gs_simple_font_encoding_z
            + (1 # 20) * max0(5 - s V_lookup_gs_simple_font_encoding_index) <= z)%Q
   | 83 => ((3 # 10) * s V_lookup_gs_simple_font_encoding_index
            + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
            + s V_lookup_gs_simple_font_encoding_z
            + (1 # 20) * max0(5 - s V_lookup_gs_simple_font_encoding_index) <= z)%Q
   | 84 => ((1 # 6) * s V_lookup_gs_simple_font_encoding_index
            + (2 # 15) * s V_lookup_gs_simple_font_encoding_near_index
            + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
            + s V_lookup_gs_simple_font_encoding_z
            + (1 # 20) * max0(5
                              - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 85 => ((1 # 6) * s V_lookup_gs_simple_font_encoding_index
            + (2 # 15) * s V_lookup_gs_simple_font_encoding_near_index
            + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
            + s V_lookup_gs_simple_font_encoding_z
            + (1 # 20) * max0(5
                              - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 86 => ((1 # 6) * s V_lookup_gs_simple_font_encoding_index
            + (2 # 15) * s V_lookup_gs_simple_font_encoding_near_index
            + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
            + s V_lookup_gs_simple_font_encoding_z
            + (1 # 20) * max0(5
                              - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 87 => ((1 # 6) * s V_lookup_gs_simple_font_encoding_index
            + (2 # 15) * s V_lookup_gs_simple_font_encoding_near_index
            + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
            + s V_lookup_gs_simple_font_encoding_z
            + (1 # 20) * max0(5
                              - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | 88 => ((1 # 6) * s V_lookup_gs_simple_font_encoding_index
            + (2 # 15) * s V_lookup_gs_simple_font_encoding_near_index
            + s V_lookup_gs_simple_font_encoding_pfont_dref_off328
            + s V_lookup_gs_simple_font_encoding_z
            + (1 # 20) * max0(5
                              - s V_lookup_gs_simple_font_encoding_near_index) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_lookup_gs_simple_font_encoding =>
    [mkPA Q (fun n z s => ai_lookup_gs_simple_font_encoding n s /\ annot0_lookup_gs_simple_font_encoding n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_lookup_gs_simple_font_encoding (proc_start P_lookup_gs_simple_font_encoding) s1 (proc_end P_lookup_gs_simple_font_encoding) s2 ->
    (s2 V_lookup_gs_simple_font_encoding_z <= (4 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_lookup_gs_simple_font_encoding.
Qed.
