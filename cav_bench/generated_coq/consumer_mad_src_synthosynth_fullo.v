Require Import pasta.Pasta.

Inductive proc: Type :=
  P_synth_full.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_synth_full_z := 1%positive.
Notation V_synth_full__tmp := 2%positive.
Notation V_synth_full__tmp1 := 3%positive.
Notation V_synth_full_ch := 4%positive.
Notation V_synth_full_lo := 5%positive.
Notation V_synth_full_pe := 6%positive.
Notation V_synth_full_phase := 7%positive.
Notation V_synth_full_po := 8%positive.
Notation V_synth_full_s := 9%positive.
Notation V_synth_full_sb := 10%positive.
Notation V_synth_full_frame := 11%positive.
Notation V_synth_full_nch := 12%positive.
Notation V_synth_full_ns := 13%positive.
Notation V_synth_full_synth := 14%positive.
Definition Pedges_synth_full: list (edge proc) :=
  (EA 1 (AAssign V_synth_full_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_synth_full_sb) s) >= (eval (ENum (0)) s))%Z)) 3)::
  (EA 3 (AGuard (fun s => ((eval (EVar V_synth_full_s) s) >= (eval (ENum (0))
  s))%Z)) 4)::(EA 4 (AGuard (fun s => ((eval (EVar V_synth_full_ch) s) >=
  (eval (ENum (0)) s))%Z)) 5)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_synth_full__tmp1) s) >= (eval (ENum (0))
  s))%Z)) 6)::(EA 6 (AGuard (fun s => ((eval (EVar V_synth_full__tmp) s) >=
  (eval (ENum (0)) s))%Z)) 7)::(EA 7 AWeaken 8)::(EA 8 (AAssign
  V_synth_full__tmp (Some (EVar V_synth_full_nch))) 9)::(EA 9 (AAssign
  V_synth_full__tmp1 (Some (EVar V_synth_full_ns))) 10)::(EA 10 (AAssign
  V_synth_full_ch (Some (ENum (0)))) 11)::(EA 11 ANone 12)::
  (EA 12 AWeaken 13)::(EA 13 (AGuard (fun s => ((eval (EVar V_synth_full_ch)
  s) < (eval (EVar V_synth_full__tmp) s))%Z)) 16)::(EA 13 (AGuard
  (fun s => ((eval (EVar V_synth_full_ch) s) >=
  (eval (EVar V_synth_full__tmp) s))%Z)) 14)::(EA 14 AWeaken 15)::
  (EA 16 AWeaken 17)::(EA 17 (AAssign V_synth_full_phase None) 18)::
  (EA 18 (AAssign V_synth_full_s (Some (ENum (0)))) 19)::(EA 19 ANone 20)::
  (EA 20 AWeaken 21)::(EA 21 (AGuard (fun s => ((eval (EVar V_synth_full_s)
  s) < (eval (EVar V_synth_full__tmp1) s))%Z)) 29)::(EA 21 (AGuard
  (fun s => ((eval (EVar V_synth_full_s) s) >=
  (eval (EVar V_synth_full__tmp1) s))%Z)) 22)::(EA 22 AWeaken 23)::
  (EA 23 ANone 24)::(EA 24 (AAssign V_synth_full_ch
  (Some (EAdd (EVar V_synth_full_ch) (ENum (1))))) 25)::(EA 25 ANone 26)::
  (EA 26 ANone 27)::(EA 27 (AAssign V_synth_full_z (Some (EAdd (ENum (1))
  (EVar V_synth_full_z)))) 28)::(EA 28 AWeaken 13)::(EA 29 AWeaken 30)::
  (EA 30 (AAssign V_synth_full_pe None) 31)::(EA 31 (AAssign V_synth_full_po
  None) 32)::(EA 32 (AAssign V_synth_full_lo None) 33)::(EA 33 (AAssign
  V_synth_full_lo None) 34)::(EA 34 (AAssign V_synth_full_lo None) 35)::
  (EA 35 (AAssign V_synth_full_lo None) 36)::(EA 36 (AAssign V_synth_full_lo
  None) 37)::(EA 37 (AAssign V_synth_full_lo None) 38)::(EA 38 (AAssign
  V_synth_full_lo None) 39)::(EA 39 (AAssign V_synth_full_lo None) 40)::
  (EA 40 (AAssign V_synth_full_lo (Some (ESub (ENum (0))
  (EVar V_synth_full_lo)))) 41)::(EA 41 (AAssign V_synth_full_lo None) 42)::
  (EA 42 (AAssign V_synth_full_lo None) 43)::(EA 43 (AAssign V_synth_full_lo
  None) 44)::(EA 44 (AAssign V_synth_full_lo None) 45)::(EA 45 (AAssign
  V_synth_full_lo None) 46)::(EA 46 (AAssign V_synth_full_lo None) 47)::
  (EA 47 (AAssign V_synth_full_lo None) 48)::(EA 48 (AAssign V_synth_full_lo
  None) 49)::(EA 49 (AAssign V_synth_full_sb (Some (ENum (1)))) 50)::
  (EA 50 ANone 51)::(EA 51 AWeaken 52)::(EA 52 (AGuard
  (fun s => ((eval (EVar V_synth_full_sb) s) < (eval (ENum (16))
  s))%Z)) 69)::(EA 52 (AGuard (fun s => ((eval (EVar V_synth_full_sb) s) >=
  (eval (ENum (16)) s))%Z)) 53)::(EA 53 AWeaken 54)::(EA 54 (AAssign
  V_synth_full_lo None) 55)::(EA 55 (AAssign V_synth_full_lo None) 56)::
  (EA 56 (AAssign V_synth_full_lo None) 57)::(EA 57 (AAssign V_synth_full_lo
  None) 58)::(EA 58 (AAssign V_synth_full_lo None) 59)::(EA 59 (AAssign
  V_synth_full_lo None) 60)::(EA 60 (AAssign V_synth_full_lo None) 61)::
  (EA 61 (AAssign V_synth_full_lo None) 62)::(EA 62 (AAssign
  V_synth_full_phase None) 63)::(EA 63 ANone 64)::(EA 64 (AAssign
  V_synth_full_s (Some (EAdd (EVar V_synth_full_s) (ENum (1))))) 65)::
  (EA 65 ANone 66)::(EA 66 ANone 67)::(EA 67 (AAssign V_synth_full_z
  (Some (EAdd (ENum (1)) (EVar V_synth_full_z)))) 68)::(EA 68 AWeaken 21)::
  (EA 69 AWeaken 70)::(EA 70 (AAssign V_synth_full_lo None) 71)::
  (EA 71 (AAssign V_synth_full_lo None) 72)::(EA 72 (AAssign V_synth_full_lo
  None) 73)::(EA 73 (AAssign V_synth_full_lo None) 74)::(EA 74 (AAssign
  V_synth_full_lo None) 75)::(EA 75 (AAssign V_synth_full_lo None) 76)::
  (EA 76 (AAssign V_synth_full_lo None) 77)::(EA 77 (AAssign V_synth_full_lo
  None) 78)::(EA 78 (AAssign V_synth_full_lo (Some (ESub (ENum (0))
  (EVar V_synth_full_lo)))) 79)::(EA 79 (AAssign V_synth_full_lo None) 80)::
  (EA 80 (AAssign V_synth_full_lo None) 81)::(EA 81 (AAssign V_synth_full_lo
  None) 82)::(EA 82 (AAssign V_synth_full_lo None) 83)::(EA 83 (AAssign
  V_synth_full_lo None) 84)::(EA 84 (AAssign V_synth_full_lo None) 85)::
  (EA 85 (AAssign V_synth_full_lo None) 86)::(EA 86 (AAssign V_synth_full_lo
  None) 87)::(EA 87 (AAssign V_synth_full_lo None) 88)::(EA 88 (AAssign
  V_synth_full_lo None) 89)::(EA 89 (AAssign V_synth_full_lo None) 90)::
  (EA 90 (AAssign V_synth_full_lo None) 91)::(EA 91 (AAssign V_synth_full_lo
  None) 92)::(EA 92 (AAssign V_synth_full_lo None) 93)::(EA 93 (AAssign
  V_synth_full_lo None) 94)::(EA 94 (AAssign V_synth_full_lo None) 95)::
  (EA 95 (AAssign V_synth_full_lo None) 96)::(EA 96 (AAssign V_synth_full_lo
  None) 97)::(EA 97 (AAssign V_synth_full_lo None) 98)::(EA 98 (AAssign
  V_synth_full_lo None) 99)::(EA 99 (AAssign V_synth_full_lo None) 100)::
  (EA 100 (AAssign V_synth_full_lo None) 101)::(EA 101 (AAssign
  V_synth_full_lo None) 102)::(EA 102 (AAssign V_synth_full_lo None) 103)::
  (EA 103 ANone 104)::(EA 104 (AAssign V_synth_full_sb
  (Some (EAdd (EVar V_synth_full_sb) (ENum (1))))) 105)::(EA 105 ANone 106)::
  (EA 106 ANone 107)::(EA 107 (AAssign V_synth_full_z (Some (EAdd (ENum (1))
  (EVar V_synth_full_z)))) 108)::(EA 108 AWeaken 52)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_synth_full => Pedges_synth_full
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_synth_full => 15
     end)%positive;
  var_global := var_global
}.

Definition ai_synth_full (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_z <= 0)%Z
   | 3 => (-1 * s V_synth_full_z <= 0 /\ 1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_sb <= 0)%Z
   | 4 => (-1 * s V_synth_full_sb <= 0 /\ 1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_s <= 0)%Z
   | 5 => (-1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_z <= 0 /\ 1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_sb <= 0 /\ -1 * s V_synth_full_ch <= 0)%Z
   | 6 => (-1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_sb <= 0 /\ 1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full__tmp1 <= 0)%Z
   | 7 => (-1 * s V_synth_full__tmp1 <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_z <= 0 /\ 1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_sb <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full__tmp <= 0)%Z
   | 8 => (-1 * s V_synth_full__tmp <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_sb <= 0 /\ 1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full__tmp1 <= 0)%Z
   | 9 => (-1 * s V_synth_full__tmp1 <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_z <= 0 /\ 1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_sb <= 0 /\ -1 * s V_synth_full_ch <= 0)%Z
   | 10 => (-1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_sb <= 0 /\ 1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_s <= 0)%Z
   | 11 => (-1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_z <= 0 /\ 1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_sb <= 0 /\ 1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_ch <= 0)%Z
   | 12 => (-1 * s V_synth_full_ch <= 0 /\ 1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_sb <= 0 /\ 1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_s <= 0)%Z
   | 13 => (-1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_sb <= 0 /\ -1 * s V_synth_full_s <= 0)%Z
   | 14 => (-1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_sb <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_z <= 0 /\ 1 * s V_synth_full__tmp+ -1 * s V_synth_full_ch <= 0)%Z
   | 15 => (1 * s V_synth_full__tmp+ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_sb <= 0 /\ -1 * s V_synth_full_s <= 0)%Z
   | 16 => (-1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_sb <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0)%Z
   | 17 => (-1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_sb <= 0 /\ -1 * s V_synth_full_s <= 0)%Z
   | 18 => (-1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_sb <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0)%Z
   | 19 => (-1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_sb <= 0 /\ 1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_s <= 0)%Z
   | 20 => (-1 * s V_synth_full_s <= 0 /\ 1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_sb <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0)%Z
   | 21 => (-1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_sb <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0)%Z
   | 22 => (-1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_sb <= 0 /\ -1 * s V_synth_full_z <= 0 /\ 1 * s V_synth_full__tmp1+ -1 * s V_synth_full_s <= 0)%Z
   | 23 => (1 * s V_synth_full__tmp1+ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_sb <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0)%Z
   | 24 => (-1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_sb <= 0 /\ -1 * s V_synth_full_z <= 0 /\ 1 * s V_synth_full__tmp1+ -1 * s V_synth_full_s <= 0)%Z
   | 25 => (1 * s V_synth_full__tmp1+ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_sb <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_ch + 1 <= 0)%Z
   | 26 => (-1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_sb <= 0 /\ -1 * s V_synth_full_z <= 0 /\ 1 * s V_synth_full__tmp1+ -1 * s V_synth_full_s <= 0)%Z
   | 27 => (1 * s V_synth_full__tmp1+ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_sb <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_ch + 1 <= 0)%Z
   | 28 => (-1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_sb <= 0 /\ 1 * s V_synth_full__tmp1+ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_z + 1 <= 0)%Z
   | 29 => (-1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_sb <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0)%Z
   | 30 => (-1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_sb <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0)%Z
   | 31 => (-1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_sb <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0)%Z
   | 32 => (-1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_sb <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0)%Z
   | 33 => (-1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_sb <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0)%Z
   | 34 => (-1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_sb <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0)%Z
   | 35 => (-1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_sb <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0)%Z
   | 36 => (-1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_sb <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0)%Z
   | 37 => (-1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_sb <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0)%Z
   | 38 => (-1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_sb <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0)%Z
   | 39 => (-1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_sb <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0)%Z
   | 40 => (-1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_sb <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0)%Z
   | 41 => (-1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_sb <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0)%Z
   | 42 => (-1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_sb <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0)%Z
   | 43 => (-1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_sb <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0)%Z
   | 44 => (-1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_sb <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0)%Z
   | 45 => (-1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_sb <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0)%Z
   | 46 => (-1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_sb <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0)%Z
   | 47 => (-1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_sb <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0)%Z
   | 48 => (-1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_sb <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0)%Z
   | 49 => (-1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_sb <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0)%Z
   | 50 => (-1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ 1 * s V_synth_full_sb + -1 <= 0 /\ -1 * s V_synth_full_sb + 1 <= 0)%Z
   | 51 => (-1 * s V_synth_full_sb + 1 <= 0 /\ 1 * s V_synth_full_sb + -1 <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0)%Z
   | 52 => (-1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_sb + 1 <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0 /\ 1 * s V_synth_full_sb + -16 <= 0)%Z
   | 53 => (1 * s V_synth_full_sb + -16 <= 0 /\ -1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_sb + 16 <= 0)%Z
   | 54 => (-1 * s V_synth_full_sb + 16 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0 /\ 1 * s V_synth_full_sb + -16 <= 0)%Z
   | 55 => (1 * s V_synth_full_sb + -16 <= 0 /\ -1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_sb + 16 <= 0)%Z
   | 56 => (-1 * s V_synth_full_sb + 16 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0 /\ 1 * s V_synth_full_sb + -16 <= 0)%Z
   | 57 => (1 * s V_synth_full_sb + -16 <= 0 /\ -1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_sb + 16 <= 0)%Z
   | 58 => (-1 * s V_synth_full_sb + 16 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0 /\ 1 * s V_synth_full_sb + -16 <= 0)%Z
   | 59 => (1 * s V_synth_full_sb + -16 <= 0 /\ -1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_sb + 16 <= 0)%Z
   | 60 => (-1 * s V_synth_full_sb + 16 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0 /\ 1 * s V_synth_full_sb + -16 <= 0)%Z
   | 61 => (1 * s V_synth_full_sb + -16 <= 0 /\ -1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_sb + 16 <= 0)%Z
   | 62 => (-1 * s V_synth_full_sb + 16 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0 /\ 1 * s V_synth_full_sb + -16 <= 0)%Z
   | 63 => (1 * s V_synth_full_sb + -16 <= 0 /\ -1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_sb + 16 <= 0)%Z
   | 64 => (-1 * s V_synth_full_sb + 16 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0 /\ 1 * s V_synth_full_sb + -16 <= 0)%Z
   | 65 => (1 * s V_synth_full_sb + -16 <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_sb + 16 <= 0 /\ -1 * s V_synth_full_s + 1 <= 0 /\ -1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s <= 0)%Z
   | 66 => (-1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_s + 1 <= 0 /\ -1 * s V_synth_full_sb + 16 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ 1 * s V_synth_full_sb + -16 <= 0)%Z
   | 67 => (1 * s V_synth_full_sb + -16 <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_sb + 16 <= 0 /\ -1 * s V_synth_full_s + 1 <= 0 /\ -1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s <= 0)%Z
   | 68 => (-1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_s + 1 <= 0 /\ -1 * s V_synth_full_sb + 16 <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ 1 * s V_synth_full_sb + -16 <= 0 /\ -1 * s V_synth_full_z + 1 <= 0)%Z
   | 69 => (-1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_sb + 1 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ 1 * s V_synth_full_sb + -15 <= 0)%Z
   | 70 => (1 * s V_synth_full_sb + -15 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_sb + 1 <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0)%Z
   | 71 => (-1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_sb + 1 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ 1 * s V_synth_full_sb + -15 <= 0)%Z
   | 72 => (1 * s V_synth_full_sb + -15 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_sb + 1 <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0)%Z
   | 73 => (-1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_sb + 1 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ 1 * s V_synth_full_sb + -15 <= 0)%Z
   | 74 => (1 * s V_synth_full_sb + -15 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_sb + 1 <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0)%Z
   | 75 => (-1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_sb + 1 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ 1 * s V_synth_full_sb + -15 <= 0)%Z
   | 76 => (1 * s V_synth_full_sb + -15 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_sb + 1 <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0)%Z
   | 77 => (-1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_sb + 1 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ 1 * s V_synth_full_sb + -15 <= 0)%Z
   | 78 => (1 * s V_synth_full_sb + -15 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_sb + 1 <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0)%Z
   | 79 => (-1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_sb + 1 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ 1 * s V_synth_full_sb + -15 <= 0)%Z
   | 80 => (1 * s V_synth_full_sb + -15 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_sb + 1 <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0)%Z
   | 81 => (-1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_sb + 1 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ 1 * s V_synth_full_sb + -15 <= 0)%Z
   | 82 => (1 * s V_synth_full_sb + -15 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_sb + 1 <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0)%Z
   | 83 => (-1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_sb + 1 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ 1 * s V_synth_full_sb + -15 <= 0)%Z
   | 84 => (1 * s V_synth_full_sb + -15 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_sb + 1 <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0)%Z
   | 85 => (-1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_sb + 1 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ 1 * s V_synth_full_sb + -15 <= 0)%Z
   | 86 => (1 * s V_synth_full_sb + -15 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_sb + 1 <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0)%Z
   | 87 => (-1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_sb + 1 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ 1 * s V_synth_full_sb + -15 <= 0)%Z
   | 88 => (1 * s V_synth_full_sb + -15 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_sb + 1 <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0)%Z
   | 89 => (-1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_sb + 1 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ 1 * s V_synth_full_sb + -15 <= 0)%Z
   | 90 => (1 * s V_synth_full_sb + -15 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_sb + 1 <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0)%Z
   | 91 => (-1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_sb + 1 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ 1 * s V_synth_full_sb + -15 <= 0)%Z
   | 92 => (1 * s V_synth_full_sb + -15 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_sb + 1 <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0)%Z
   | 93 => (-1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_sb + 1 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ 1 * s V_synth_full_sb + -15 <= 0)%Z
   | 94 => (1 * s V_synth_full_sb + -15 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_sb + 1 <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0)%Z
   | 95 => (-1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_sb + 1 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ 1 * s V_synth_full_sb + -15 <= 0)%Z
   | 96 => (1 * s V_synth_full_sb + -15 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_sb + 1 <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0)%Z
   | 97 => (-1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_sb + 1 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ 1 * s V_synth_full_sb + -15 <= 0)%Z
   | 98 => (1 * s V_synth_full_sb + -15 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_sb + 1 <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0)%Z
   | 99 => (-1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_sb + 1 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ 1 * s V_synth_full_sb + -15 <= 0)%Z
   | 100 => (1 * s V_synth_full_sb + -15 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_sb + 1 <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0)%Z
   | 101 => (-1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_sb + 1 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ 1 * s V_synth_full_sb + -15 <= 0)%Z
   | 102 => (1 * s V_synth_full_sb + -15 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_sb + 1 <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0)%Z
   | 103 => (-1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_sb + 1 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ 1 * s V_synth_full_sb + -15 <= 0)%Z
   | 104 => (1 * s V_synth_full_sb + -15 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_sb + 1 <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0)%Z
   | 105 => (-1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_z <= 0 /\ 1 * s V_synth_full_sb + -16 <= 0 /\ -1 * s V_synth_full_sb + 2 <= 0)%Z
   | 106 => (-1 * s V_synth_full_sb + 2 <= 0 /\ 1 * s V_synth_full_sb + -16 <= 0 /\ -1 * s V_synth_full_z <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0)%Z
   | 107 => (-1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_z <= 0 /\ 1 * s V_synth_full_sb + -16 <= 0 /\ -1 * s V_synth_full_sb + 2 <= 0)%Z
   | 108 => (-1 * s V_synth_full_sb + 2 <= 0 /\ 1 * s V_synth_full_sb + -16 <= 0 /\ -1 * s V_synth_full_s <= 0 /\ -1 * s V_synth_full_ch <= 0 /\ -1 * s V_synth_full__tmp+ 1 * s V_synth_full_ch + 1 <= 0 /\ -1 * s V_synth_full__tmp1+ 1 * s V_synth_full_s + 1 <= 0 /\ -1 * s V_synth_full_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_synth_full (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_synth_full_nch)
           + (16 # 1) * max0(s V_synth_full_nch) * max0(s V_synth_full_ns) <= z)%Q
   | 2 => (s V_synth_full_z + max0(s V_synth_full_nch)
           + (16 # 1) * max0(s V_synth_full_nch) * max0(s V_synth_full_ns) <= z)%Q
   | 3 => (s V_synth_full_z + max0(s V_synth_full_nch)
           + (16 # 1) * max0(s V_synth_full_nch) * max0(s V_synth_full_ns) <= z)%Q
   | 4 => (s V_synth_full_z + max0(s V_synth_full_nch)
           + (16 # 1) * max0(s V_synth_full_nch) * max0(s V_synth_full_ns) <= z)%Q
   | 5 => (s V_synth_full_z + max0(s V_synth_full_nch)
           + (16 # 1) * max0(s V_synth_full_nch) * max0(s V_synth_full_ns) <= z)%Q
   | 6 => (s V_synth_full_z + max0(s V_synth_full_nch)
           + (16 # 1) * max0(s V_synth_full_nch) * max0(s V_synth_full_ns) <= z)%Q
   | 7 => (s V_synth_full_z + max0(s V_synth_full_nch)
           + (16 # 1) * max0(s V_synth_full_nch) * max0(s V_synth_full_ns) <= z)%Q
   | 8 => (s V_synth_full_z + max0(s V_synth_full_nch)
           + (16 # 1) * max0(s V_synth_full_nch) * max0(s V_synth_full_ns) <= z)%Q
   | 9 => (s V_synth_full_z + max0(s V_synth_full__tmp)
           + (16 # 1) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ns) <= z)%Q
   | 10 => (s V_synth_full_z + max0(s V_synth_full__tmp)
            + (16 # 1) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1) <= z)%Q
   | 11 => (s V_synth_full_z + max0(s V_synth_full__tmp - s V_synth_full_ch)
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1) <= z)%Q
   | 12 => (s V_synth_full_z + max0(s V_synth_full__tmp - s V_synth_full_ch)
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1) <= z)%Q
   | 13 => (s V_synth_full_z + max0(s V_synth_full__tmp - s V_synth_full_ch)
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1) <= z)%Q
   | 14 => hints
     [(*-1 0*) F_max0_ge_0 (s V_synth_full__tmp - s V_synth_full_ch);
      (*-16 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp
                                                             - s V_synth_full_ch)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp1)) (F_check_ge (0) (0)))]
     (s V_synth_full_z + max0(s V_synth_full__tmp - s V_synth_full_ch)
      + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1) <= z)%Q
   | 15 => (s V_synth_full_z <= z)%Q
   | 16 => hints
     [(*0 1.07043*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_synth_full__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp
                                                                    - s V_synth_full_ch)) (F_check_ge (0) (0)));
      (*-0.328148 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_synth_full__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_ch)) (F_check_ge (0) (0)));
      (*0 1.07043*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                   + 
                                                                   s V_synth_full__tmp
                                                                   - 
                                                                   s V_synth_full_ch)) (F_check_ge (-1
                                                                    + s V_synth_full__tmp
                                                                    - s V_synth_full_ch) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp
                                                                    - s V_synth_full_ch)) (F_check_ge (0) (0)));
      (*0 1.07043*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                   + 
                                                                   s V_synth_full__tmp
                                                                   - 
                                                                   s V_synth_full_ch)) (F_check_ge (-1
                                                                    + s V_synth_full__tmp
                                                                    - s V_synth_full_ch) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp
                                                                    - s V_synth_full_ch)) (F_check_ge (0) (0)));
      (*0 1.07043*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full__tmp
                                                                   - 
                                                                   s V_synth_full_ch)) (F_check_ge (s V_synth_full__tmp
                                                                    - s V_synth_full_ch) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp)) (F_check_ge (0) (0)))]
     (s V_synth_full_z + max0(s V_synth_full__tmp - s V_synth_full_ch)
      + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1) <= z)%Q
   | 17 => ((76 # 71) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (76 # 71) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (21 # 64) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + s V_synth_full_z
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + max0(s V_synth_full__tmp - s V_synth_full_ch)
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (21 # 64) * max0(s V_synth_full_ch) <= z)%Q
   | 18 => ((76 # 71) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (76 # 71) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (21 # 64) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + s V_synth_full_z
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + max0(s V_synth_full__tmp - s V_synth_full_ch)
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (21 # 64) * max0(s V_synth_full_ch) <= z)%Q
   | 19 => ((76 # 71) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (76 # 71) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                        - s V_synth_full_s)
            - (21 # 64) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                      - s V_synth_full_s)
            - (205 # 47) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + s V_synth_full_z
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + max0(s V_synth_full__tmp - s V_synth_full_ch)
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            - (467 # 105) * max0(s V_synth_full__tmp1)
            + (467 # 105) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
            + (21 # 64) * max0(s V_synth_full_ch) <= z)%Q
   | 20 => hints
     [(*-0.885987 0*) F_max0_pre_decrement 1 (s V_synth_full__tmp
                                              - s V_synth_full_ch) (1);
      (*-0.328148 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (s V_synth_full__tmp
                                                                    - s V_synth_full_ch) (0))) (F_max0_ge_0 (s V_synth_full__tmp
                                                                    - s V_synth_full_ch));
      (*-1.54228 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    s V_synth_full__tmp)) (F_check_ge (-1
                                                                    + s V_synth_full__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp
                                                                    - s V_synth_full_ch)) (F_check_ge (0) (0)));
      (*0 0.299289*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    s V_synth_full__tmp)) (F_check_ge (-1
                                                                    + s V_synth_full__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp)) (F_check_ge (0) (0)));
      (*-0.228148 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    s V_synth_full__tmp
                                                                    - 
                                                                    s V_synth_full_ch)) (F_check_ge (-1
                                                                    + s V_synth_full__tmp
                                                                    - s V_synth_full_ch) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_ch)) (F_check_ge (0) (0)));
      (*-0.699289 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full__tmp) (0))) (F_max0_ge_0 (s V_synth_full__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp)) (F_check_ge (0) (0)));
      (*-0.371141 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full__tmp) (0))) (F_max0_ge_0 (s V_synth_full__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp)) (F_check_ge (0) (0)));
      (*-0.742282 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full__tmp)) (F_check_ge (s V_synth_full__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp
                                                                    - s V_synth_full_ch)) (F_check_ge (0) (0)));
      (*-0.328148 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full__tmp)) (F_check_ge (s V_synth_full__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_ch)) (F_check_ge (0) (0)));
      (*-0.742282 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full__tmp
                                                                    - s V_synth_full_ch) (0))) (F_max0_ge_0 (s V_synth_full__tmp
                                                                    - s V_synth_full_ch))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp)) (F_check_ge (0) (0)));
      (*-0.228148 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full_ch) (0))) (F_max0_ge_0 (s V_synth_full_ch))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp
                                                                    - s V_synth_full_ch)) (F_check_ge (0) (0)));
      (*-0.856296 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full__tmp
                                                          - s V_synth_full_ch)) (F_check_ge (s V_synth_full__tmp
                                                                    - s V_synth_full_ch) (0))]
     ((76 # 71) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
      + (76 # 71) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp
                                               - s V_synth_full_ch)
      - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
      + (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                  - s V_synth_full_s)
      - (21 # 64) * s V_synth_full__tmp * max0(s V_synth_full_ch)
      - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
      - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                             - s V_synth_full_ch)
      - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                             - s V_synth_full_ch)
      + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
      - (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                - s V_synth_full_s)
      - (205 # 47) * s V_synth_full_s * max0(s V_synth_full__tmp
                                             - s V_synth_full_ch)
      + s V_synth_full_z
      + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
      - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
      - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
      + (1213 # 105) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
      - (1213 # 105) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
      - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
      + max0(s V_synth_full__tmp - s V_synth_full_ch)
      + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
      + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
      - (467 # 105) * max0(s V_synth_full__tmp1)
      + (467 # 105) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
      + (21 # 64) * max0(s V_synth_full_ch) <= z)%Q
   | 21 => ((70 # 79) + (122 # 103) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                        - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            - (21 # 64) * s V_synth_full__tmp^2
            - (122 # 103) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                      - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (21 # 64) * s V_synth_full_ch^2
            - (205 # 47) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + s V_synth_full_z
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            - (259 # 150) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            - (38 # 127) * max0(s V_synth_full__tmp)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            - (76 # 71) * max0(s V_synth_full__tmp - s V_synth_full_ch)
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            - (467 # 105) * max0(s V_synth_full__tmp1)
            + (467 # 105) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
            + (1 # 10) * max0(s V_synth_full_ch) <= z)%Q
   | 22 => hints
     [(*0 0.328148*) F_binom_monotonic 2 (F_max0_ge_arg (-1
                                                         + s V_synth_full__tmp
                                                         - s V_synth_full_ch)) (F_check_ge (-1
                                                                    + s V_synth_full__tmp
                                                                    - s V_synth_full_ch) (0));
      (*0 1.39858*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_synth_full__tmp
                                                                    - s V_synth_full_ch) (0))) (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp
                                                                    - s V_synth_full_ch))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp
                                                                    - s V_synth_full_ch)) (F_check_ge (0) (0)));
      (*-1.07043 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_synth_full__tmp
                                                                    - s V_synth_full_ch) (0))) (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp
                                                                    - s V_synth_full_ch))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp
                                                                    - s V_synth_full_ch)) (F_check_ge (0) (0)));
      (*-0.328148 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full__tmp
                                                                    - s V_synth_full_ch) (0))) (F_max0_ge_0 (s V_synth_full__tmp
                                                                    - s V_synth_full_ch))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_ch)) (F_check_ge (0) (0)));
      (*-0.328148 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full__tmp
                                                                    - 
                                                                    s V_synth_full_ch)) (F_check_ge (s V_synth_full__tmp
                                                                    - s V_synth_full_ch) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp
                                                                    - s V_synth_full_ch)) (F_check_ge (0) (0)));
      (*-16 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full__tmp
                                                               - s V_synth_full_ch)) (F_check_ge (s V_synth_full__tmp
                                                                    - s V_synth_full_ch) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp1)) (F_check_ge (0) (0)));
      (*-0.328148 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full_ch)) (F_check_ge (s V_synth_full_ch) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp)) (F_check_ge (0) (0)));
      (*0 0.0718521*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full_ch)) (F_check_ge (s V_synth_full_ch) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp)) (F_check_ge (0) (0)));
      (*-0.328148 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full_ch)) (F_check_ge (s V_synth_full_ch) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp
                                                                    - s V_synth_full_ch)) (F_check_ge (0) (0)));
      (*-4.36164 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full_s)) (F_check_ge (s V_synth_full_s) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp
                                                                    - s V_synth_full_ch)) (F_check_ge (0) (0)));
      (*0 0.942282*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full__tmp) (0))) (F_max0_ge_0 (s V_synth_full__tmp))]
     ((70 # 79) + (122 # 103) * s V_synth_full__tmp
      + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
      + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
      + (371 # 142) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp
                                                 - s V_synth_full_ch)
      - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
      + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                               - s V_synth_full_ch)
      - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
      + (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                  - s V_synth_full_s)
      + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
      - (21 # 64) * s V_synth_full__tmp^2 - (122 # 103) * s V_synth_full_ch
      - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
      - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                             - s V_synth_full_ch)
      + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
      - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                             - s V_synth_full_ch)
      + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
      - (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                - s V_synth_full_s)
      - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
      - (21 # 64) * s V_synth_full_ch^2
      - (205 # 47) * s V_synth_full_s * max0(s V_synth_full__tmp
                                             - s V_synth_full_ch)
      + s V_synth_full_z
      - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                          + s V_synth_full__tmp
                                                          - s V_synth_full_ch)
      + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
      + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
      - (259 # 150) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
      - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
      + (1213 # 105) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
      - (1213 # 105) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
      - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
      - (38 # 127) * max0(s V_synth_full__tmp)
      - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
      + (36 # 97) * max0(s V_synth_full__tmp)^2
      - (76 # 71) * max0(s V_synth_full__tmp - s V_synth_full_ch)
      + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
      + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
      + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
      - (467 # 105) * max0(s V_synth_full__tmp1)
      + (467 # 105) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
      + (1 # 10) * max0(s V_synth_full_ch) <= z)%Q
   | 23 => ((91 # 59) - (72 # 97) * s V_synth_full__tmp
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (17 # 14) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (467 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                        - s V_synth_full_s)
            - (1 # 10) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            - (1 # 5) * s V_synth_full_ch
            - (72 # 97) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            + (1 # 10) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + (92 # 113) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (467 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                      - s V_synth_full_s)
            + (1 # 10) * s V_synth_full_ch * max0(s V_synth_full_ch)
            + s V_synth_full_z
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            + (9 # 14) * max0(s V_synth_full__tmp)
            - (2 # 5) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            - (467 # 105) * max0(s V_synth_full__tmp1)
            + (467 # 105) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
            + (1 # 10) * max0(s V_synth_full_ch) <= z)%Q
   | 24 => ((91 # 59) - (72 # 97) * s V_synth_full__tmp
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (17 # 14) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (467 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                        - s V_synth_full_s)
            - (1 # 10) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            - (1 # 5) * s V_synth_full_ch
            - (72 # 97) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            + (1 # 10) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + (92 # 113) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (467 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                      - s V_synth_full_s)
            + (1 # 10) * s V_synth_full_ch * max0(s V_synth_full_ch)
            + s V_synth_full_z
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            + (9 # 14) * max0(s V_synth_full__tmp)
            - (2 # 5) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            - (467 # 105) * max0(s V_synth_full__tmp1)
            + (467 # 105) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
            + (1 # 10) * max0(s V_synth_full_ch) <= z)%Q
   | 25 => ((169 # 97) - (72 # 97) * s V_synth_full__tmp
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            - (1 # 10) * s V_synth_full__tmp * max0(-1 + s V_synth_full_ch)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (17 # 14) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            + (467 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                        - s V_synth_full_s)
            - (1 # 5) * s V_synth_full_ch
            - (72 # 97) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            + (1 # 10) * s V_synth_full_ch * max0(-1 + s V_synth_full_ch)
            + (92 # 113) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            + (1 # 10) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            - (467 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                      - s V_synth_full_s)
            + s V_synth_full_z + (72 # 97) * max0(-1 + s V_synth_full__tmp)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            - (2 # 5) * max0(-1 + s V_synth_full_ch) * max0(s V_synth_full__tmp)
            - (13 # 76) * max0(s V_synth_full__tmp)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            - (1 # 10) * max0(s V_synth_full__tmp - s V_synth_full_ch)
            + (1213 # 105) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
            + (16 # 1) * max0(s V_synth_full__tmp1 - s V_synth_full_s) <= z)%Q
   | 26 => ((169 # 97) - (72 # 97) * s V_synth_full__tmp
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            - (1 # 10) * s V_synth_full__tmp * max0(-1 + s V_synth_full_ch)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (17 # 14) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            + (467 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                        - s V_synth_full_s)
            - (1 # 5) * s V_synth_full_ch
            - (72 # 97) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            + (1 # 10) * s V_synth_full_ch * max0(-1 + s V_synth_full_ch)
            + (92 # 113) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            + (1 # 10) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            - (467 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                      - s V_synth_full_s)
            + s V_synth_full_z + (72 # 97) * max0(-1 + s V_synth_full__tmp)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            - (2 # 5) * max0(-1 + s V_synth_full_ch) * max0(s V_synth_full__tmp)
            - (13 # 76) * max0(s V_synth_full__tmp)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            - (1 # 10) * max0(s V_synth_full__tmp - s V_synth_full_ch)
            + (1213 # 105) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
            + (16 # 1) * max0(s V_synth_full__tmp1 - s V_synth_full_s) <= z)%Q
   | 27 => ((169 # 97) - (72 # 97) * s V_synth_full__tmp
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            - (1 # 10) * s V_synth_full__tmp * max0(-1 + s V_synth_full_ch)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (17 # 14) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            + (467 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                        - s V_synth_full_s)
            - (1 # 5) * s V_synth_full_ch
            - (72 # 97) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            + (1 # 10) * s V_synth_full_ch * max0(-1 + s V_synth_full_ch)
            + (92 # 113) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            + (1 # 10) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            - (467 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                      - s V_synth_full_s)
            + s V_synth_full_z + (72 # 97) * max0(-1 + s V_synth_full__tmp)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            - (2 # 5) * max0(-1 + s V_synth_full_ch) * max0(s V_synth_full__tmp)
            - (13 # 76) * max0(s V_synth_full__tmp)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            - (1 # 10) * max0(s V_synth_full__tmp - s V_synth_full_ch)
            + (1213 # 105) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
            + (16 # 1) * max0(s V_synth_full__tmp1 - s V_synth_full_s) <= z)%Q
   | 28 => hints
     [(*-0.371141 0*) F_binom_monotonic 2 (F_max0_ge_arg (s V_synth_full__tmp)) (F_check_ge (s V_synth_full__tmp) (0));
      (*0 0.8*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_synth_full__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp
                                                                    - s V_synth_full_ch)) (F_check_ge (0) (0)));
      (*-0.371141 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    s V_synth_full__tmp)) (F_check_ge (-1
                                                                    + s V_synth_full__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp)) (F_check_ge (0) (0)));
      (*-0.4 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                + s V_synth_full__tmp)) (F_check_ge (-1
                                                                    + s V_synth_full__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp)) (F_check_ge (0) (0)));
      (*0 0.4*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_synth_full_ch) (0))) (F_max0_ge_0 (-1
                                                                    + s V_synth_full_ch))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp)) (F_check_ge (0) (0)));
      (*0 0.1*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_synth_full_ch) (0))) (F_max0_ge_0 (-1
                                                                    + s V_synth_full_ch))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp
                                                                    - s V_synth_full_ch)) (F_check_ge (0) (0)));
      (*0 0.2*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_synth_full_z) (0))) (F_max0_ge_0 (-1
                                                                    + s V_synth_full_z))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp
                                                                    - s V_synth_full_ch)) (F_check_ge (0) (0)));
      (*-0.2 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_synth_full_z) (0))) (F_max0_ge_0 (-1
                                                                    + s V_synth_full_z))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_ch)) (F_check_ge (0) (0)));
      (*0 0.2*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                               + s V_synth_full_z)) (F_check_ge (-1
                                                                    + s V_synth_full_z) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp)) (F_check_ge (0) (0)));
      (*-0.2 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full__tmp) (0))) (F_max0_ge_0 (s V_synth_full__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full_z)) (F_check_ge (0) (0)));
      (*-0.414135 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full__tmp) (0))) (F_max0_ge_0 (s V_synth_full__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp
                                                                    - s V_synth_full_ch)) (F_check_ge (0) (0)));
      (*-0.2 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full__tmp)) (F_check_ge (s V_synth_full__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_z)) (F_check_ge (0) (0)));
      (*-0.742282 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full__tmp
                                                                    - s V_synth_full_ch) (0))) (F_max0_ge_0 (s V_synth_full__tmp
                                                                    - s V_synth_full_ch))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp)) (F_check_ge (0) (0)));
      (*-4.44761 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full__tmp
                                                                    - s V_synth_full_ch) (0))) (F_max0_ge_0 (s V_synth_full__tmp
                                                                    - s V_synth_full_ch))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp1)) (F_check_ge (0) (0)));
      (*-11.5524 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full__tmp
                                                                    - s V_synth_full_ch) (0))) (F_max0_ge_0 (s V_synth_full__tmp
                                                                    - s V_synth_full_ch))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp1
                                                                    - s V_synth_full_s)) (F_check_ge (0) (0)));
      (*-0.2 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full__tmp
                                                                    - s V_synth_full_ch) (0))) (F_max0_ge_0 (s V_synth_full__tmp
                                                                    - s V_synth_full_ch))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_z)) (F_check_ge (0) (0)));
      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full__tmp
                                                                - s V_synth_full_ch)) (F_check_ge (s V_synth_full__tmp
                                                                    - s V_synth_full_ch) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full_ch)) (F_check_ge (0) (0)));
      (*-0.2 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full__tmp
                                                                - s V_synth_full_ch)) (F_check_ge (s V_synth_full__tmp
                                                                    - s V_synth_full_ch) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full_z)) (F_check_ge (0) (0)));
      (*-0.414135 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full__tmp
                                                                    - 
                                                                    s V_synth_full_ch)) (F_check_ge (s V_synth_full__tmp
                                                                    - s V_synth_full_ch) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp)) (F_check_ge (0) (0)));
      (*-0.2 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full_ch) (0))) (F_max0_ge_0 (s V_synth_full_ch))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_z)) (F_check_ge (0) (0)));
      (*-0.2 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full_ch)) (F_check_ge (s V_synth_full_ch) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full_z)) (F_check_ge (0) (0)));
      (*-0.2 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full_z) (0))) (F_max0_ge_0 (s V_synth_full_z))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp)) (F_check_ge (0) (0)));
      (*-0.2 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full_z)) (F_check_ge (s V_synth_full_z) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp
                                                                    - s V_synth_full_ch)) (F_check_ge (0) (0)));
      (*-0.2 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full_z)) (F_check_ge (s V_synth_full_z) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_ch)) (F_check_ge (0) (0)));
      (*-0.2 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full_ch)) (F_check_ge (s V_synth_full_ch) (0));
      (*-16 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp1
                                                  - s V_synth_full_s)) (F_check_ge (0) (0));
      (*-0.371141 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_synth_full__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp))]
     ((72 # 97) - (72 # 97) * s V_synth_full__tmp
      + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
      - (1 # 10) * s V_synth_full__tmp * max0(-1 + s V_synth_full_ch)
      - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
      + (17 # 14) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                               - s V_synth_full_ch)
      + (467 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
      + (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                  - s V_synth_full_s)
      - (1 # 5) * s V_synth_full_ch
      - (72 # 97) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
      + (1 # 10) * s V_synth_full_ch * max0(-1 + s V_synth_full_ch)
      + (92 # 113) * s V_synth_full_ch * max0(s V_synth_full__tmp)
      + (1 # 10) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                            - s V_synth_full_ch)
      - (467 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
      - (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                - s V_synth_full_s)
      + s V_synth_full_z + (72 # 97) * max0(-1 + s V_synth_full__tmp)
      + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
      - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp
                                                          - s V_synth_full_ch)
      - (2 # 5) * max0(-1 + s V_synth_full_ch) * max0(s V_synth_full__tmp)
      - (13 # 76) * max0(s V_synth_full__tmp)
      + (36 # 97) * max0(s V_synth_full__tmp)^2
      - (1 # 10) * max0(s V_synth_full__tmp - s V_synth_full_ch)
      + (1213 # 105) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
      - (1213 # 105) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
      + (16 # 1) * max0(s V_synth_full__tmp1 - s V_synth_full_s) <= z)%Q
   | 29 => hints
     [(*0 1.72673*) F_max0_pre_decrement 1 (s V_synth_full__tmp
                                            - s V_synth_full_ch) (1);
      (*0 4.44761*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_synth_full__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp1)) (F_check_ge (0) (0)));
      (*0 4.44761*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                   + 
                                                                   s V_synth_full__tmp)) (F_check_ge (-1
                                                                    + s V_synth_full__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp1
                                                                    - s V_synth_full_s)) (F_check_ge (0) (0)));
      (*0 2.79716*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_synth_full__tmp1
                                                                    - s V_synth_full_s) (0))) (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp1
                                                                    - s V_synth_full_s))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp
                                                                    - s V_synth_full_ch)) (F_check_ge (0) (0)))]
     ((70 # 79) + (122 # 103) * s V_synth_full__tmp
      + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
      + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
      + (371 # 142) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp
                                                 - s V_synth_full_ch)
      - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
      + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                               - s V_synth_full_ch)
      - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
      + (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                  - s V_synth_full_s)
      + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
      - (21 # 64) * s V_synth_full__tmp^2 - (122 # 103) * s V_synth_full_ch
      - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
      - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                             - s V_synth_full_ch)
      + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
      - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                             - s V_synth_full_ch)
      + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
      - (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                - s V_synth_full_s)
      - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
      - (21 # 64) * s V_synth_full_ch^2
      - (205 # 47) * s V_synth_full_s * max0(s V_synth_full__tmp
                                             - s V_synth_full_ch)
      + s V_synth_full_z
      - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                          + s V_synth_full__tmp
                                                          - s V_synth_full_ch)
      + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
      + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
      - (259 # 150) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
      - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
      + (1213 # 105) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
      - (1213 # 105) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
      - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
      - (38 # 127) * max0(s V_synth_full__tmp)
      - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
      + (36 # 97) * max0(s V_synth_full__tmp)^2
      - (76 # 71) * max0(s V_synth_full__tmp - s V_synth_full_ch)
      + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
      + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
      + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
      - (467 # 105) * max0(s V_synth_full__tmp1)
      + (467 # 105) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
      + (1 # 10) * max0(s V_synth_full_ch) <= z)%Q
   | 30 => ((371 # 142) + (122 # 103) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (16 # 1) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (16 # 1) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            - (21 # 64) * s V_synth_full__tmp^2
            - (193 # 69) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (122 # 103) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                      - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (21 # 64) * s V_synth_full_ch^2
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + s V_synth_full_z
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            - (38 # 127) * max0(s V_synth_full__tmp)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            + (1 # 10) * max0(s V_synth_full_ch) <= z)%Q
   | 31 => ((371 # 142) + (122 # 103) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (16 # 1) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (16 # 1) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            - (21 # 64) * s V_synth_full__tmp^2
            - (193 # 69) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (122 # 103) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                      - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (21 # 64) * s V_synth_full_ch^2
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + s V_synth_full_z
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            - (38 # 127) * max0(s V_synth_full__tmp)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            + (1 # 10) * max0(s V_synth_full_ch) <= z)%Q
   | 32 => ((371 # 142) + (122 # 103) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (16 # 1) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (16 # 1) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            - (21 # 64) * s V_synth_full__tmp^2
            - (193 # 69) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (122 # 103) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                      - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (21 # 64) * s V_synth_full_ch^2
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + s V_synth_full_z
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            - (38 # 127) * max0(s V_synth_full__tmp)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            + (1 # 10) * max0(s V_synth_full_ch) <= z)%Q
   | 33 => ((371 # 142) + (122 # 103) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (16 # 1) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (16 # 1) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            - (21 # 64) * s V_synth_full__tmp^2
            - (193 # 69) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (122 # 103) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                      - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (21 # 64) * s V_synth_full_ch^2
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + s V_synth_full_z
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            - (38 # 127) * max0(s V_synth_full__tmp)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            + (1 # 10) * max0(s V_synth_full_ch) <= z)%Q
   | 34 => ((371 # 142) + (122 # 103) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (16 # 1) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (16 # 1) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            - (21 # 64) * s V_synth_full__tmp^2
            - (193 # 69) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (122 # 103) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                      - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (21 # 64) * s V_synth_full_ch^2
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + s V_synth_full_z
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            - (38 # 127) * max0(s V_synth_full__tmp)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            + (1 # 10) * max0(s V_synth_full_ch) <= z)%Q
   | 35 => ((371 # 142) + (122 # 103) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (16 # 1) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (16 # 1) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            - (21 # 64) * s V_synth_full__tmp^2
            - (193 # 69) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (122 # 103) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                      - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (21 # 64) * s V_synth_full_ch^2
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + s V_synth_full_z
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            - (38 # 127) * max0(s V_synth_full__tmp)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            + (1 # 10) * max0(s V_synth_full_ch) <= z)%Q
   | 36 => ((371 # 142) + (122 # 103) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (16 # 1) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (16 # 1) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            - (21 # 64) * s V_synth_full__tmp^2
            - (193 # 69) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (122 # 103) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                      - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (21 # 64) * s V_synth_full_ch^2
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + s V_synth_full_z
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            - (38 # 127) * max0(s V_synth_full__tmp)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            + (1 # 10) * max0(s V_synth_full_ch) <= z)%Q
   | 37 => ((371 # 142) + (122 # 103) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (16 # 1) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (16 # 1) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            - (21 # 64) * s V_synth_full__tmp^2
            - (193 # 69) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (122 # 103) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                      - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (21 # 64) * s V_synth_full_ch^2
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + s V_synth_full_z
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            - (38 # 127) * max0(s V_synth_full__tmp)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            + (1 # 10) * max0(s V_synth_full_ch) <= z)%Q
   | 38 => ((371 # 142) + (122 # 103) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (16 # 1) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (16 # 1) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            - (21 # 64) * s V_synth_full__tmp^2
            - (193 # 69) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (122 # 103) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                      - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (21 # 64) * s V_synth_full_ch^2
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + s V_synth_full_z
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            - (38 # 127) * max0(s V_synth_full__tmp)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            + (1 # 10) * max0(s V_synth_full_ch) <= z)%Q
   | 39 => ((371 # 142) + (122 # 103) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (16 # 1) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (16 # 1) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            - (21 # 64) * s V_synth_full__tmp^2
            - (193 # 69) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (122 # 103) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                      - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (21 # 64) * s V_synth_full_ch^2
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + s V_synth_full_z
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            - (38 # 127) * max0(s V_synth_full__tmp)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            + (1 # 10) * max0(s V_synth_full_ch) <= z)%Q
   | 40 => ((371 # 142) + (122 # 103) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (16 # 1) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (16 # 1) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            - (21 # 64) * s V_synth_full__tmp^2
            - (193 # 69) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (122 # 103) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                      - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (21 # 64) * s V_synth_full_ch^2
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + s V_synth_full_z
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            - (38 # 127) * max0(s V_synth_full__tmp)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            + (1 # 10) * max0(s V_synth_full_ch) <= z)%Q
   | 41 => ((371 # 142) + (122 # 103) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (16 # 1) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (16 # 1) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            - (21 # 64) * s V_synth_full__tmp^2
            - (193 # 69) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (122 # 103) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                      - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (21 # 64) * s V_synth_full_ch^2
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + s V_synth_full_z
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            - (38 # 127) * max0(s V_synth_full__tmp)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            + (1 # 10) * max0(s V_synth_full_ch) <= z)%Q
   | 42 => ((371 # 142) + (122 # 103) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (16 # 1) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (16 # 1) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            - (21 # 64) * s V_synth_full__tmp^2
            - (193 # 69) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (122 # 103) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                      - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (21 # 64) * s V_synth_full_ch^2
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + s V_synth_full_z
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            - (38 # 127) * max0(s V_synth_full__tmp)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            + (1 # 10) * max0(s V_synth_full_ch) <= z)%Q
   | 43 => ((371 # 142) + (122 # 103) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (16 # 1) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (16 # 1) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            - (21 # 64) * s V_synth_full__tmp^2
            - (193 # 69) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (122 # 103) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                      - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (21 # 64) * s V_synth_full_ch^2
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + s V_synth_full_z
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            - (38 # 127) * max0(s V_synth_full__tmp)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            + (1 # 10) * max0(s V_synth_full_ch) <= z)%Q
   | 44 => ((371 # 142) + (122 # 103) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (16 # 1) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (16 # 1) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            - (21 # 64) * s V_synth_full__tmp^2
            - (193 # 69) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (122 # 103) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                      - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (21 # 64) * s V_synth_full_ch^2
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + s V_synth_full_z
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            - (38 # 127) * max0(s V_synth_full__tmp)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            + (1 # 10) * max0(s V_synth_full_ch) <= z)%Q
   | 45 => ((371 # 142) + (122 # 103) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (16 # 1) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (16 # 1) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            - (21 # 64) * s V_synth_full__tmp^2
            - (193 # 69) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (122 # 103) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                      - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (21 # 64) * s V_synth_full_ch^2
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + s V_synth_full_z
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            - (38 # 127) * max0(s V_synth_full__tmp)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            + (1 # 10) * max0(s V_synth_full_ch) <= z)%Q
   | 46 => ((371 # 142) + (122 # 103) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (16 # 1) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (16 # 1) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            - (21 # 64) * s V_synth_full__tmp^2
            - (193 # 69) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (122 # 103) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                      - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (21 # 64) * s V_synth_full_ch^2
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + s V_synth_full_z
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            - (38 # 127) * max0(s V_synth_full__tmp)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            + (1 # 10) * max0(s V_synth_full_ch) <= z)%Q
   | 47 => ((371 # 142) + (122 # 103) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (16 # 1) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (16 # 1) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            - (21 # 64) * s V_synth_full__tmp^2
            - (193 # 69) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (122 # 103) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                      - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (21 # 64) * s V_synth_full_ch^2
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + s V_synth_full_z
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            - (38 # 127) * max0(s V_synth_full__tmp)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            + (1 # 10) * max0(s V_synth_full_ch) <= z)%Q
   | 48 => ((371 # 142) + (122 # 103) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (16 # 1) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (16 # 1) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            - (21 # 64) * s V_synth_full__tmp^2
            - (193 # 69) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (122 # 103) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                      - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (21 # 64) * s V_synth_full_ch^2
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + s V_synth_full_z
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            - (38 # 127) * max0(s V_synth_full__tmp)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            + (1 # 10) * max0(s V_synth_full_ch) <= z)%Q
   | 49 => ((371 # 142) + (122 # 103) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (16 # 1) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (16 # 1) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            - (21 # 64) * s V_synth_full__tmp^2
            - (193 # 69) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (122 # 103) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                      - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (21 # 64) * s V_synth_full_ch^2
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + s V_synth_full_z
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            - (38 # 127) * max0(s V_synth_full__tmp)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            + (1 # 10) * max0(s V_synth_full_ch) <= z)%Q
   | 50 => ((1445 # 87) - (508 # 57) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            - (87 # 142) * s V_synth_full__tmp * max0(-16 + s V_synth_full_sb)
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (1 # 52) * s V_synth_full__tmp * max0(-1 + s V_synth_full_sb)
            - (3 # 113) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
            + (71 # 102) * s V_synth_full__tmp * max0(16 - s V_synth_full_sb)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (16 # 1) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (16 # 1) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            + (4 # 153) * s V_synth_full__tmp * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full__tmp^2
            + (2 # 87) * s V_synth_full__tmp1 * max0(-2 + s V_synth_full_sb)
            - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
            - (1 # 16) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
            + (5 # 92) * s V_synth_full__tmp1 * max0(16 - s V_synth_full_sb)
            - (193 # 69) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            + (8 # 129) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
            - (122 # 103) * s V_synth_full_ch
            + (87 # 142) * s V_synth_full_ch * max0(-16 + s V_synth_full_sb)
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
            + (1 # 52) * s V_synth_full_ch * max0(16 - s V_synth_full_sb)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                      - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full_ch^2
            - (58 # 111) * s V_synth_full_s * max0(-16 + s V_synth_full_sb)
            + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
            + (3 # 67) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
            - (5 # 92) * s V_synth_full_s * max0(16 - s V_synth_full_sb)
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + (14 # 75) * s V_synth_full_s * max0(s V_synth_full_sb)
            - (83 # 110) * s V_synth_full_sb
            - (9 # 131) * s V_synth_full_sb * max0(-16 + s V_synth_full_sb)
            + (2 # 55) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
            + (1 # 51) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            - (41 # 86) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
            + (100 # 123) * s V_synth_full_sb * max0(-1
                                                     + s V_synth_full__tmp1
                                                     - s V_synth_full_s)
            - (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
            + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
            + (0 # 1) * s V_synth_full_sb * max0(16 - s V_synth_full_sb)
            + (59 # 104) * s V_synth_full_sb * max0(s V_synth_full__tmp)
            - (54 # 151) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            - (21 # 71) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
            - (21 # 82) * s V_synth_full_sb * max0(s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            + (3 # 11) * s V_synth_full_sb * max0(s V_synth_full_s)
            + (0 # 1) * s V_synth_full_sb * max0(s V_synth_full_sb)
            - (0 # 1) * s V_synth_full_sb^2 + s V_synth_full_z
            - (7 # 116) * s V_synth_full_z * max0(-16 + s V_synth_full_sb)
            + (72 # 109) * max0(-16 + s V_synth_full_sb)
            + (87 # 142) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (58 # 111) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                                + s V_synth_full__tmp1)
            - (58 # 111) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                                + s V_synth_full__tmp1
                                                                - s V_synth_full_s)
            - (0 # 1) * max0(-16 + s V_synth_full_sb) * max0(16
                                                             - s V_synth_full_sb)
            + (5 # 76) * max0(-16 + s V_synth_full_sb) * max0(s V_synth_full_sb)
            + (7 # 116) * max0(-16 + s V_synth_full_sb) * max0(s V_synth_full_z)
            - (2 # 87) * max0(-2 + s V_synth_full_sb) * max0(-1
                                                             + s V_synth_full__tmp1)
            + (1 # 103) * max0(-2 + s V_synth_full_sb) * max0(-1
                                                              + s V_synth_full_sb)
            - (0 # 1) * max0(-2 + s V_synth_full_sb) * max0(15
                                                            - s V_synth_full_sb)
            - (1 # 95) * max0(-2 + s V_synth_full_sb) * max0(s V_synth_full_sb)
            - (83 # 124) * max0(-1 + s V_synth_full__tmp)
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (3 # 113) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                                - s V_synth_full_sb)
            + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(16
                                                               - s V_synth_full_sb)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (49 # 5) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
            - (36 # 55) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(16
                                                                    - s V_synth_full_sb)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (233 # 58) * max0(-1 + s V_synth_full__tmp1)
            - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                + s V_synth_full_sb)
            - (13 # 95) * max0(-1 + s V_synth_full__tmp1) * max0(15
                                                                 - s V_synth_full_sb)
            - (9 # 83) * max0(-1 + s V_synth_full__tmp1) * max0(16
                                                                - s V_synth_full_sb)
            + (19 # 116) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s)
            - (5 # 82) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(16
                                                                    - s V_synth_full_sb)
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            - (5 # 79) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
            + (5 # 97) * max0(-1 + s V_synth_full_sb)
            + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                             - s V_synth_full_ch)
            + (0 # 1) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
            + (1 # 90) * max0(-1 + s V_synth_full_sb)^2
            - (65 # 72) * max0(15 - s V_synth_full_sb)
            + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (1 # 150) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
            - (6 # 151) * max0(16 - s V_synth_full_sb)
            - (5 # 82) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp)
            + (26 # 83) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
            - (25 # 156) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (1 # 57) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                             - s V_synth_full_s)
            - (61 # 96) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full_ch)
            + (1 # 57) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full_s)
            + (5 # 106) * max0(s V_synth_full__tmp)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            - (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch)
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            + (0 # 1) * max0(s V_synth_full__tmp1) * max0(s V_synth_full_sb)
            - (6 # 73) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
            - (1 # 60) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
            + (963 # 100) * max0(s V_synth_full_ch)
            + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
            - (3 # 46) * max0(s V_synth_full_sb)
            - (0 # 1) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 51 => hints
     [(*-0.790753 0*) F_max0_pre_decrement 1 (16 - s V_synth_full_sb) (1);
      (*-0.655044 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_synth_full__tmp
                                                                    - s V_synth_full_ch) (0))) (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp
                                                                    - s V_synth_full_ch))) (F_binom_monotonic 1 (F_max0_ge_0 (16
                                                                    - s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*0 4.36164*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_synth_full__tmp1) (0))) (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp1))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp
                                                                    - s V_synth_full_ch)) (F_check_ge (0) (0)));
      (*0 0.0859738*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    s V_synth_full__tmp1)) (F_check_ge (-1
                                                                    + s V_synth_full__tmp1) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp)) (F_check_ge (0) (0)));
      (*0 0.0542892*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_synth_full__tmp1
                                                                    - s V_synth_full_s) (0))) (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp1
                                                                    - s V_synth_full_s))) (F_binom_monotonic 1 (F_max0_ge_0 (16
                                                                    - s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*0 0.0319525*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_synth_full__tmp1
                                                                    - s V_synth_full_s) (0))) (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp1
                                                                    - s V_synth_full_s))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp)) (F_check_ge (0) (0)));
      (*-0.0609199 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full__tmp) (0))) (F_max0_ge_0 (s V_synth_full__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (16
                                                                    - s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-4.44761 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full__tmp)) (F_check_ge (s V_synth_full__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp1)) (F_check_ge (0) (0)));
      (*-7.1588 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full__tmp
                                                                    - s V_synth_full_ch) (0))) (F_max0_ge_0 (s V_synth_full__tmp
                                                                    - s V_synth_full_ch))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp1
                                                                    - s V_synth_full_s)) (F_check_ge (0) (0)));
      (*-0.160213 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full__tmp1) (0))) (F_max0_ge_0 (s V_synth_full__tmp1))) (F_binom_monotonic 1 (F_max0_ge_0 (16
                                                                    - s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-4.39359 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full__tmp1
                                                                    - s V_synth_full_s) (0))) (F_max0_ge_0 (s V_synth_full__tmp1
                                                                    - s V_synth_full_s))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_ch)) (F_check_ge (0) (0)));
      (*-0.635399 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full_ch) (0))) (F_max0_ge_0 (s V_synth_full_ch))) (F_binom_monotonic 1 (F_max0_ge_0 (16
                                                                    - s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.0175013 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full_s)) (F_check_ge (s V_synth_full_s) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (16
                                                                    - s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.00652225 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full_sb)) (F_check_ge (s V_synth_full_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_sb)) (F_check_ge (0) (0)))]
     ((1445 # 87) - (508 # 57) * s V_synth_full__tmp
      + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
      - (87 # 142) * s V_synth_full__tmp * max0(-16 + s V_synth_full_sb)
      + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
      + (371 # 142) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp
                                                 - s V_synth_full_ch)
      - (1 # 52) * s V_synth_full__tmp * max0(-1 + s V_synth_full_sb)
      - (3 # 113) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
      + (71 # 102) * s V_synth_full__tmp * max0(16 - s V_synth_full_sb)
      - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
      + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                               - s V_synth_full_ch)
      - (16 # 1) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
      + (16 # 1) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                              - s V_synth_full_s)
      + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
      + (4 # 153) * s V_synth_full__tmp * max0(s V_synth_full_sb)
      - (21 # 64) * s V_synth_full__tmp^2
      + (2 # 87) * s V_synth_full__tmp1 * max0(-2 + s V_synth_full_sb)
      - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
      - (1 # 16) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
      + (5 # 92) * s V_synth_full__tmp1 * max0(16 - s V_synth_full_sb)
      - (193 # 69) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                 - s V_synth_full_ch)
      + (8 # 129) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
      - (122 # 103) * s V_synth_full_ch
      + (87 # 142) * s V_synth_full_ch * max0(-16 + s V_synth_full_sb)
      - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
      - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                             - s V_synth_full_ch)
      - (17 # 59) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
      + (1 # 52) * s V_synth_full_ch * max0(16 - s V_synth_full_sb)
      + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
      - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                             - s V_synth_full_ch)
      + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
      - (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                - s V_synth_full_s)
      - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
      - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
      - (21 # 64) * s V_synth_full_ch^2
      - (58 # 111) * s V_synth_full_s * max0(-16 + s V_synth_full_sb)
      + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
      + (3 # 67) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
      - (5 # 92) * s V_synth_full_s * max0(16 - s V_synth_full_sb)
      - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                            - s V_synth_full_ch)
      + (14 # 75) * s V_synth_full_s * max0(s V_synth_full_sb)
      - (83 # 110) * s V_synth_full_sb
      - (9 # 131) * s V_synth_full_sb * max0(-16 + s V_synth_full_sb)
      + (2 # 55) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
      + (1 # 51) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp
                                            - s V_synth_full_ch)
      - (41 # 86) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
      + (100 # 123) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1
                                               - s V_synth_full_s)
      - (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
      + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
      + (0 # 1) * s V_synth_full_sb * max0(16 - s V_synth_full_sb)
      + (59 # 104) * s V_synth_full_sb * max0(s V_synth_full__tmp)
      - (54 # 151) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                              - s V_synth_full_ch)
      - (21 # 71) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
      - (21 # 82) * s V_synth_full_sb * max0(s V_synth_full__tmp1
                                             - s V_synth_full_s)
      + (3 # 11) * s V_synth_full_sb * max0(s V_synth_full_s)
      + (0 # 1) * s V_synth_full_sb * max0(s V_synth_full_sb)
      - (0 # 1) * s V_synth_full_sb^2 + s V_synth_full_z
      - (7 # 116) * s V_synth_full_z * max0(-16 + s V_synth_full_sb)
      + (72 # 109) * max0(-16 + s V_synth_full_sb)
      + (87 # 142) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                          + s V_synth_full__tmp
                                                          - s V_synth_full_ch)
      + (58 # 111) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                          + s V_synth_full__tmp1)
      - (58 # 111) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                          + s V_synth_full__tmp1
                                                          - s V_synth_full_s)
      - (0 # 1) * max0(-16 + s V_synth_full_sb) * max0(16 - s V_synth_full_sb)
      + (5 # 76) * max0(-16 + s V_synth_full_sb) * max0(s V_synth_full_sb)
      + (7 # 116) * max0(-16 + s V_synth_full_sb) * max0(s V_synth_full_z)
      - (2 # 87) * max0(-2 + s V_synth_full_sb) * max0(-1
                                                       + s V_synth_full__tmp1)
      + (1 # 103) * max0(-2 + s V_synth_full_sb) * max0(-1
                                                        + s V_synth_full_sb)
      - (0 # 1) * max0(-2 + s V_synth_full_sb) * max0(15 - s V_synth_full_sb)
      - (1 # 95) * max0(-2 + s V_synth_full_sb) * max0(s V_synth_full_sb)
      - (83 # 124) * max0(-1 + s V_synth_full__tmp)
      - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                          + s V_synth_full__tmp
                                                          - s V_synth_full_ch)
      + (3 # 113) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                          - s V_synth_full_sb)
      + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(16
                                                         - s V_synth_full_sb)
      + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
      + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
      - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                            - s V_synth_full_s)
      + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
      + (49 # 5) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
      - (36 # 55) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(16
                                                                    - s V_synth_full_sb)
      - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
      + (1213 # 105) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
      - (1213 # 105) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
      - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
      + (233 # 58) * max0(-1 + s V_synth_full__tmp1)
      - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                          + s V_synth_full_sb)
      - (13 # 95) * max0(-1 + s V_synth_full__tmp1) * max0(15
                                                           - s V_synth_full_sb)
      - (9 # 83) * max0(-1 + s V_synth_full__tmp1) * max0(16
                                                          - s V_synth_full_sb)
      + (19 # 116) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s)
      - (5 # 82) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(16
                                                                    - s V_synth_full_sb)
      + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
      - (5 # 79) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
      + (5 # 97) * max0(-1 + s V_synth_full_sb)
      + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
      + (0 # 1) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
      + (1 # 90) * max0(-1 + s V_synth_full_sb)^2
      - (65 # 72) * max0(15 - s V_synth_full_sb)
      + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
      + (1 # 150) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                        - s V_synth_full_s)
      - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
      - (6 # 151) * max0(16 - s V_synth_full_sb)
      - (5 # 82) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp)
      + (26 # 83) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                        - s V_synth_full_ch)
      - (25 # 156) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
      + (1 # 57) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                       - s V_synth_full_s)
      - (61 # 96) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full_ch)
      + (1 # 57) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full_s)
      + (5 # 106) * max0(s V_synth_full__tmp)
      - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
      + (36 # 97) * max0(s V_synth_full__tmp)^2
      - (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch)
      + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
      + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
      + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
      + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
      + (0 # 1) * max0(s V_synth_full__tmp1) * max0(s V_synth_full_sb)
      - (6 # 73) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
      - (1 # 60) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
      + (963 # 100) * max0(s V_synth_full_ch)
      + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
      - (3 # 46) * max0(s V_synth_full_sb)
      - (0 # 1) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 52 => ((87 # 5) - (508 # 57) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            - (87 # 142) * s V_synth_full__tmp * max0(-16 + s V_synth_full_sb)
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (1 # 52) * s V_synth_full__tmp * max0(-1 + s V_synth_full_sb)
            - (3 # 113) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
            - (2 # 101) * s V_synth_full__tmp * max0(16 - s V_synth_full_sb)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (557 # 63) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                      - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            + (4 # 153) * s V_synth_full__tmp * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full__tmp^2
            + (2 # 87) * s V_synth_full__tmp1 * max0(-2 + s V_synth_full_sb)
            - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
            - (1 # 16) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
            - (21 # 131) * s V_synth_full__tmp1 * max0(16 - s V_synth_full_sb)
            + (2 # 37) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
            - (451 # 63) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
            + (8 # 129) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
            - (122 # 103) * s V_synth_full_ch
            + (87 # 142) * s V_synth_full_ch * max0(-16 + s V_synth_full_sb)
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
            + (4 # 103) * s V_synth_full_ch * max0(16 - s V_synth_full_sb)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (413 # 94) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full_ch^2
            - (58 # 111) * s V_synth_full_s * max0(-16 + s V_synth_full_sb)
            + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
            + (3 # 67) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
            + (1 # 57) * s V_synth_full_s * max0(16 - s V_synth_full_sb)
            + (3 # 94) * s V_synth_full_s * max0(s V_synth_full__tmp)
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
            + (14 # 75) * s V_synth_full_s * max0(s V_synth_full_sb)
            - (83 # 110) * s V_synth_full_sb
            - (9 # 131) * s V_synth_full_sb * max0(-16 + s V_synth_full_sb)
            + (2 # 55) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
            + (1 # 51) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            - (41 # 86) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
            + (100 # 123) * s V_synth_full_sb * max0(-1
                                                     + s V_synth_full__tmp1
                                                     - s V_synth_full_s)
            - (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
            + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
            + (0 # 1) * s V_synth_full_sb * max0(16 - s V_synth_full_sb)
            + (59 # 104) * s V_synth_full_sb * max0(s V_synth_full__tmp)
            - (54 # 151) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            - (21 # 71) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
            - (21 # 82) * s V_synth_full_sb * max0(s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            + (3 # 11) * s V_synth_full_sb * max0(s V_synth_full_s)
            + (1 # 95) * s V_synth_full_sb * max0(s V_synth_full_sb)
            - (0 # 1) * s V_synth_full_sb^2 + s V_synth_full_z
            - (7 # 116) * s V_synth_full_z * max0(-16 + s V_synth_full_sb)
            + (72 # 109) * max0(-16 + s V_synth_full_sb)
            + (87 # 142) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (58 # 111) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                                + s V_synth_full__tmp1)
            - (58 # 111) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                                + s V_synth_full__tmp1
                                                                - s V_synth_full_s)
            - (0 # 1) * max0(-16 + s V_synth_full_sb) * max0(16
                                                             - s V_synth_full_sb)
            + (5 # 76) * max0(-16 + s V_synth_full_sb) * max0(s V_synth_full_sb)
            + (7 # 116) * max0(-16 + s V_synth_full_sb) * max0(s V_synth_full_z)
            - (2 # 87) * max0(-2 + s V_synth_full_sb) * max0(-1
                                                             + s V_synth_full__tmp1)
            + (1 # 103) * max0(-2 + s V_synth_full_sb) * max0(-1
                                                              + s V_synth_full_sb)
            - (0 # 1) * max0(-2 + s V_synth_full_sb) * max0(15
                                                            - s V_synth_full_sb)
            - (1 # 95) * max0(-2 + s V_synth_full_sb) * max0(s V_synth_full_sb)
            - (83 # 124) * max0(-1 + s V_synth_full__tmp)
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (3 # 113) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                                - s V_synth_full_sb)
            + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(16
                                                               - s V_synth_full_sb)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (49 # 5) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
            + (0 # 1) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(16
                                                                    - s V_synth_full_sb)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (233 # 58) * max0(-1 + s V_synth_full__tmp1)
            - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                + s V_synth_full_sb)
            - (13 # 95) * max0(-1 + s V_synth_full__tmp1) * max0(15
                                                                 - s V_synth_full_sb)
            - (9 # 83) * max0(-1 + s V_synth_full__tmp1) * max0(16
                                                                - s V_synth_full_sb)
            - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
            + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                  - s V_synth_full_ch)
            + (19 # 116) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s)
            - (1 # 150) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(16
                                                                    - s V_synth_full_sb)
            + (3 # 94) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp)
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            - (5 # 79) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
            + (5 # 97) * max0(-1 + s V_synth_full_sb)
            + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                             - s V_synth_full_ch)
            + (0 # 1) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
            + (1 # 90) * max0(-1 + s V_synth_full_sb)^2
            - (12 # 107) * max0(15 - s V_synth_full_sb)
            + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (1 # 150) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
            - (4 # 33) * max0(16 - s V_synth_full_sb)
            + (26 # 83) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
            + (0 # 1) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (1 # 57) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                             - s V_synth_full_s)
            - (1 # 145) * max0(s V_synth_full__tmp)
            - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (451 # 63) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            + (0 # 1) * max0(s V_synth_full__tmp1) * max0(s V_synth_full_sb)
            - (6 # 73) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
            + (413 # 94) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_ch)
            - (1 # 60) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
            + (963 # 100) * max0(s V_synth_full_ch)
            + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
            - (3 # 46) * max0(s V_synth_full_sb)
            - (1 # 139) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 53 => hints
     [(*0 0.0388577*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (-16
                                                                    + s V_synth_full_sb) (0))) (F_max0_ge_0 (-16
                                                                    + s V_synth_full_sb));
      (*0 0.0469986*) F_binom_monotonic 2 (F_max0_ge_arg (s V_synth_full_sb)) (F_check_ge (s V_synth_full_sb) (0));
      (*0 0.00716331*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-16
                                                                    + s V_synth_full_sb) (0))) (F_max0_ge_0 (-16
                                                                    + s V_synth_full_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (16
                                                                    - s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*0 0.0102382*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-16
                                                                    + 
                                                                    s V_synth_full_sb)) (F_check_ge (-16
                                                                    + s V_synth_full_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp1
                                                                    - s V_synth_full_s)) (F_check_ge (0) (0)));
      (*0 0.0541303*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-16
                                                                    + 
                                                                    s V_synth_full_sb)) (F_check_ge (-16
                                                                    + s V_synth_full_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.00719803 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                    + s V_synth_full_sb) (0))) (F_max0_ge_0 (-2
                                                                    + s V_synth_full_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp1)) (F_check_ge (0) (0)));
      (*0 0.0105143*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                    + s V_synth_full_sb) (0))) (F_max0_ge_0 (-2
                                                                    + s V_synth_full_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-4.44761 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_synth_full__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp1
                                                                    - s V_synth_full_s)) (F_check_ge (0) (0)));
      (*0 0.0192597*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    s V_synth_full__tmp
                                                                    - 
                                                                    s V_synth_full_ch)) (F_check_ge (-1
                                                                    + s V_synth_full__tmp
                                                                    - s V_synth_full_ch) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.0859738 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_synth_full__tmp1) (0))) (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp1))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp)) (F_check_ge (0) (0)));
      (*-0.136759 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_synth_full__tmp1) (0))) (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp1))) (F_binom_monotonic 1 (F_max0_ge_0 (15
                                                                    - s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*0 0.006608*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_synth_full__tmp1
                                                                    - s V_synth_full_s) (0))) (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp1
                                                                    - s V_synth_full_s))) (F_binom_monotonic 1 (F_max0_ge_0 (16
                                                                    - s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*0 0.0192597*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_synth_full_sb) (0))) (F_max0_ge_0 (-1
                                                                    + s V_synth_full_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp
                                                                    - s V_synth_full_ch)) (F_check_ge (0) (0)));
      (*-0.00121181 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_synth_full_sb) (0))) (F_max0_ge_0 (-1
                                                                    + s V_synth_full_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp1)) (F_check_ge (0) (0)));
      (*-0.272839 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (16
                                                                    - s V_synth_full_sb) (0))) (F_max0_ge_0 (16
                                                                    - s V_synth_full_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp1
                                                                    - s V_synth_full_s)) (F_check_ge (0) (0)));
      (*0 0.000426755*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (16
                                                                    - s V_synth_full_sb)) (F_check_ge (16
                                                                    - s V_synth_full_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp
                                                                    - s V_synth_full_ch)) (F_check_ge (0) (0)));
      (*-0.29034 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (16
                                                                  - s V_synth_full_sb)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp1
                                                                    - s V_synth_full_s)) (F_check_ge (0) (0)));
      (*-4.39359 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full__tmp) (0))) (F_max0_ge_0 (s V_synth_full__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp1
                                                                    - s V_synth_full_s)) (F_check_ge (0) (0)));
      (*0 0.0319525*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full__tmp)) (F_check_ge (s V_synth_full__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp1
                                                                    - s V_synth_full_s)) (F_check_ge (0) (0)));
      (*0 0.00121181*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full__tmp1)) (F_check_ge (s V_synth_full__tmp1) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-11.5524 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full__tmp1
                                                                    - s V_synth_full_s) (0))) (F_max0_ge_0 (s V_synth_full__tmp1
                                                                    - s V_synth_full_s))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp
                                                                    - s V_synth_full_ch)) (F_check_ge (0) (0)));
      (*-0.006608 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full__tmp1
                                                                    - 
                                                                    s V_synth_full_s)) (F_check_ge (s V_synth_full__tmp1
                                                                    - s V_synth_full_s) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (15
                                                                    - s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-4.39359 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full__tmp1
                                                                    - 
                                                                    s V_synth_full_s)) (F_check_ge (s V_synth_full__tmp1
                                                                    - s V_synth_full_s) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp)) (F_check_ge (0) (0)));
      (*-7.1588 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full__tmp1
                                                                   - 
                                                                   s V_synth_full_s)) (F_check_ge (s V_synth_full__tmp1
                                                                    - s V_synth_full_s) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp
                                                                    - s V_synth_full_ch)) (F_check_ge (0) (0)));
      (*-4.39359 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full_ch)) (F_check_ge (s V_synth_full_ch) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp1
                                                                    - s V_synth_full_s)) (F_check_ge (0) (0)));
      (*0 0.00113607*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full_sb) (0))) (F_max0_ge_0 (s V_synth_full_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp1)) (F_check_ge (0) (0)));
      (*-0.0166922 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full_sb) (0))) (F_max0_ge_0 (s V_synth_full_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp1
                                                                    - s V_synth_full_s)) (F_check_ge (0) (0)));
      (*0 1.04993*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full_sb) (0))) (F_max0_ge_0 (s V_synth_full_sb));
      (*-0.0111239 0*) F_binom_monotonic 2 (F_max0_ge_arg (-1
                                                           + s V_synth_full_sb)) (F_check_ge (-1
                                                                    + s V_synth_full_sb) (0))]
     ((87 # 5) - (508 # 57) * s V_synth_full__tmp
      + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
      - (87 # 142) * s V_synth_full__tmp * max0(-16 + s V_synth_full_sb)
      + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
      + (371 # 142) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp
                                                 - s V_synth_full_ch)
      - (1 # 52) * s V_synth_full__tmp * max0(-1 + s V_synth_full_sb)
      - (3 # 113) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
      - (2 # 101) * s V_synth_full__tmp * max0(16 - s V_synth_full_sb)
      - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
      + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                               - s V_synth_full_ch)
      - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
      + (557 # 63) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                - s V_synth_full_s)
      + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
      + (4 # 153) * s V_synth_full__tmp * max0(s V_synth_full_sb)
      - (21 # 64) * s V_synth_full__tmp^2
      + (2 # 87) * s V_synth_full__tmp1 * max0(-2 + s V_synth_full_sb)
      - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
      - (1 # 16) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
      - (21 # 131) * s V_synth_full__tmp1 * max0(16 - s V_synth_full_sb)
      + (2 # 37) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
      - (451 # 63) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                 - s V_synth_full_ch)
      - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
      + (8 # 129) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
      - (122 # 103) * s V_synth_full_ch
      + (87 # 142) * s V_synth_full_ch * max0(-16 + s V_synth_full_sb)
      - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
      - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                             - s V_synth_full_ch)
      - (17 # 59) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
      + (4 # 103) * s V_synth_full_ch * max0(16 - s V_synth_full_sb)
      + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
      - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                             - s V_synth_full_ch)
      + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
      - (413 # 94) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                              - s V_synth_full_s)
      - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
      - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
      - (21 # 64) * s V_synth_full_ch^2
      - (58 # 111) * s V_synth_full_s * max0(-16 + s V_synth_full_sb)
      + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
      + (3 # 67) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
      + (1 # 57) * s V_synth_full_s * max0(16 - s V_synth_full_sb)
      + (3 # 94) * s V_synth_full_s * max0(s V_synth_full__tmp)
      - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                            - s V_synth_full_ch)
      + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
      + (14 # 75) * s V_synth_full_s * max0(s V_synth_full_sb)
      - (83 # 110) * s V_synth_full_sb
      - (9 # 131) * s V_synth_full_sb * max0(-16 + s V_synth_full_sb)
      + (2 # 55) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
      + (1 # 51) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp
                                            - s V_synth_full_ch)
      - (41 # 86) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
      + (100 # 123) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1
                                               - s V_synth_full_s)
      - (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
      + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
      + (0 # 1) * s V_synth_full_sb * max0(16 - s V_synth_full_sb)
      + (59 # 104) * s V_synth_full_sb * max0(s V_synth_full__tmp)
      - (54 # 151) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                              - s V_synth_full_ch)
      - (21 # 71) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
      - (21 # 82) * s V_synth_full_sb * max0(s V_synth_full__tmp1
                                             - s V_synth_full_s)
      + (3 # 11) * s V_synth_full_sb * max0(s V_synth_full_s)
      + (1 # 95) * s V_synth_full_sb * max0(s V_synth_full_sb)
      - (0 # 1) * s V_synth_full_sb^2 + s V_synth_full_z
      - (7 # 116) * s V_synth_full_z * max0(-16 + s V_synth_full_sb)
      + (72 # 109) * max0(-16 + s V_synth_full_sb)
      + (87 # 142) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                          + s V_synth_full__tmp
                                                          - s V_synth_full_ch)
      + (58 # 111) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                          + s V_synth_full__tmp1)
      - (58 # 111) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                          + s V_synth_full__tmp1
                                                          - s V_synth_full_s)
      - (0 # 1) * max0(-16 + s V_synth_full_sb) * max0(16 - s V_synth_full_sb)
      + (5 # 76) * max0(-16 + s V_synth_full_sb) * max0(s V_synth_full_sb)
      + (7 # 116) * max0(-16 + s V_synth_full_sb) * max0(s V_synth_full_z)
      - (2 # 87) * max0(-2 + s V_synth_full_sb) * max0(-1
                                                       + s V_synth_full__tmp1)
      + (1 # 103) * max0(-2 + s V_synth_full_sb) * max0(-1
                                                        + s V_synth_full_sb)
      - (0 # 1) * max0(-2 + s V_synth_full_sb) * max0(15 - s V_synth_full_sb)
      - (1 # 95) * max0(-2 + s V_synth_full_sb) * max0(s V_synth_full_sb)
      - (83 # 124) * max0(-1 + s V_synth_full__tmp)
      - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                          + s V_synth_full__tmp
                                                          - s V_synth_full_ch)
      + (3 # 113) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                          - s V_synth_full_sb)
      + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(16
                                                         - s V_synth_full_sb)
      + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
      + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
      - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                            - s V_synth_full_s)
      + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
      + (49 # 5) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
      + (0 # 1) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(16
                                                                    - s V_synth_full_sb)
      - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
      + (1213 # 105) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
      - (1213 # 105) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
      - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
      + (233 # 58) * max0(-1 + s V_synth_full__tmp1)
      - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                          + s V_synth_full_sb)
      - (13 # 95) * max0(-1 + s V_synth_full__tmp1) * max0(15
                                                           - s V_synth_full_sb)
      - (9 # 83) * max0(-1 + s V_synth_full__tmp1) * max0(16
                                                          - s V_synth_full_sb)
      - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
      + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                            - s V_synth_full_ch)
      + (19 # 116) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s)
      - (1 # 150) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(16
                                                                    - s V_synth_full_sb)
      + (3 # 94) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp)
      + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
      - (5 # 79) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
      + (5 # 97) * max0(-1 + s V_synth_full_sb)
      + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
      + (0 # 1) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
      + (1 # 90) * max0(-1 + s V_synth_full_sb)^2
      - (12 # 107) * max0(15 - s V_synth_full_sb)
      + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
      + (1 # 150) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                        - s V_synth_full_s)
      - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
      - (4 # 33) * max0(16 - s V_synth_full_sb)
      + (26 # 83) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                        - s V_synth_full_ch)
      + (0 # 1) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
      + (1 # 57) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                       - s V_synth_full_s)
      - (1 # 145) * max0(s V_synth_full__tmp)
      - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
      - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
      + (36 # 97) * max0(s V_synth_full__tmp)^2
      + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
      + (451 # 63) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
      + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
      + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
      + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
      + (0 # 1) * max0(s V_synth_full__tmp1) * max0(s V_synth_full_sb)
      - (6 # 73) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
      + (413 # 94) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_ch)
      - (1 # 60) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
      + (963 # 100) * max0(s V_synth_full_ch)
      + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
      - (3 # 46) * max0(s V_synth_full_sb)
      - (1 # 139) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 54 => ((233 # 34) - (508 # 57) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            - (87 # 142) * s V_synth_full__tmp * max0(-16 + s V_synth_full_sb)
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            + (3 # 94) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (3 # 113) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
            - (2 # 101) * s V_synth_full__tmp * max0(16 - s V_synth_full_sb)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            + (4 # 153) * s V_synth_full__tmp * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full__tmp^2
            + (2 # 87) * s V_synth_full__tmp1 * max0(-2 + s V_synth_full_sb)
            - (8 # 93) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full__tmp)
            - (1213 # 105) * s V_synth_full__tmp1 * max0(-1
                                                         + s V_synth_full__tmp
                                                         - s V_synth_full_ch)
            - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
            - (16 # 83) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
            - (1 # 6) * s V_synth_full__tmp1 * max0(16 - s V_synth_full_sb)
            + (467 # 105) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
            - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
            + (5 # 79) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
            - (122 # 103) * s V_synth_full_ch
            + (87 # 142) * s V_synth_full_ch * max0(-16 + s V_synth_full_sb)
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            - (4 # 13) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
            + (4 # 103) * s V_synth_full_ch * max0(16 - s V_synth_full_sb)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full_ch^2
            - (58 # 111) * s V_synth_full_s * max0(-16 + s V_synth_full_sb)
            + (1213 # 105) * s V_synth_full_s * max0(-1 + s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
            + (5 # 131) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
            + (2 # 83) * s V_synth_full_s * max0(16 - s V_synth_full_sb)
            - (205 # 47) * s V_synth_full_s * max0(s V_synth_full__tmp)
            - (977 # 112) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
            + (14 # 75) * s V_synth_full_s * max0(s V_synth_full_sb)
            - (47 # 78) * s V_synth_full_sb
            - (9 # 131) * s V_synth_full_sb * max0(-16 + s V_synth_full_sb)
            + (2 # 55) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
            - (76 # 157) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
            + (121 # 147) * s V_synth_full_sb * max0(-1
                                                     + s V_synth_full__tmp1
                                                     - s V_synth_full_s)
            - (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
            + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
            - (0 # 1) * s V_synth_full_sb * max0(16 - s V_synth_full_sb)
            + (59 # 104) * s V_synth_full_sb * max0(s V_synth_full__tmp)
            - (54 # 151) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            - (31 # 104) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
            + (3 # 11) * s V_synth_full_sb * max0(s V_synth_full_s)
            + (2 # 37) * s V_synth_full_sb * max0(s V_synth_full_sb)
            + (1 # 55) * s V_synth_full_sb^2 + s V_synth_full_z
            - (7 # 116) * s V_synth_full_z * max0(-16 + s V_synth_full_sb)
            + (97 # 156) * max0(-16 + s V_synth_full_sb)
            + (87 # 142) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (58 # 111) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                                + s V_synth_full__tmp1)
            - (65 # 122) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                                + s V_synth_full__tmp1
                                                                - s V_synth_full_s)
            + (0 # 1) * max0(-16 + s V_synth_full_sb) * max0(16
                                                             - s V_synth_full_sb)
            + (1 # 85) * max0(-16 + s V_synth_full_sb) * max0(s V_synth_full_sb)
            + (7 # 116) * max0(-16 + s V_synth_full_sb) * max0(s V_synth_full_z)
            + (4 # 103) * max0(-16 + s V_synth_full_sb)^2
            - (1 # 64) * max0(-2 + s V_synth_full_sb) * max0(-1
                                                             + s V_synth_full__tmp1)
            + (1 # 103) * max0(-2 + s V_synth_full_sb) * max0(-1
                                                              + s V_synth_full_sb)
            - (0 # 1) * max0(-2 + s V_synth_full_sb) * max0(15
                                                            - s V_synth_full_sb)
            - (7 # 12) * max0(-1 + s V_synth_full__tmp)
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (8 # 93) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                               + s V_synth_full__tmp1)
            + (3 # 113) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                                - s V_synth_full_sb)
            + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(16
                                                               - s V_synth_full_sb)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (845 # 86) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (254 # 63) * max0(-1 + s V_synth_full__tmp1)
            - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                + s V_synth_full_sb)
            - (9 # 83) * max0(-1 + s V_synth_full__tmp1) * max0(16
                                                                - s V_synth_full_sb)
            - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
            + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                  - s V_synth_full_ch)
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            - (5 # 79) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
            + (8 # 185) * max0(-1 + s V_synth_full_sb)
            + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                             - s V_synth_full_ch)
            + (0 # 1) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (0 # 1) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
            + (2 # 81) * max0(15 - s V_synth_full_sb)
            + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
            + (26 # 83) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
            + (0 # 1) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            - (1 # 145) * max0(s V_synth_full__tmp)
            - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            + (0 # 1) * max0(s V_synth_full__tmp1)
            + (963 # 100) * max0(s V_synth_full_ch)
            + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (14 # 75) * max0(s V_synth_full_sb)
            - (2 # 37) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 55 => ((233 # 34) - (508 # 57) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            - (87 # 142) * s V_synth_full__tmp * max0(-16 + s V_synth_full_sb)
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            + (3 # 94) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (3 # 113) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
            - (2 # 101) * s V_synth_full__tmp * max0(16 - s V_synth_full_sb)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            + (4 # 153) * s V_synth_full__tmp * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full__tmp^2
            + (2 # 87) * s V_synth_full__tmp1 * max0(-2 + s V_synth_full_sb)
            - (8 # 93) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full__tmp)
            - (1213 # 105) * s V_synth_full__tmp1 * max0(-1
                                                         + s V_synth_full__tmp
                                                         - s V_synth_full_ch)
            - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
            - (16 # 83) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
            - (1 # 6) * s V_synth_full__tmp1 * max0(16 - s V_synth_full_sb)
            + (467 # 105) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
            - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
            + (5 # 79) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
            - (122 # 103) * s V_synth_full_ch
            + (87 # 142) * s V_synth_full_ch * max0(-16 + s V_synth_full_sb)
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            - (4 # 13) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
            + (4 # 103) * s V_synth_full_ch * max0(16 - s V_synth_full_sb)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full_ch^2
            - (58 # 111) * s V_synth_full_s * max0(-16 + s V_synth_full_sb)
            + (1213 # 105) * s V_synth_full_s * max0(-1 + s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
            + (5 # 131) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
            + (2 # 83) * s V_synth_full_s * max0(16 - s V_synth_full_sb)
            - (205 # 47) * s V_synth_full_s * max0(s V_synth_full__tmp)
            - (977 # 112) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
            + (14 # 75) * s V_synth_full_s * max0(s V_synth_full_sb)
            - (47 # 78) * s V_synth_full_sb
            - (9 # 131) * s V_synth_full_sb * max0(-16 + s V_synth_full_sb)
            + (2 # 55) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
            - (76 # 157) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
            + (121 # 147) * s V_synth_full_sb * max0(-1
                                                     + s V_synth_full__tmp1
                                                     - s V_synth_full_s)
            - (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
            + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
            - (0 # 1) * s V_synth_full_sb * max0(16 - s V_synth_full_sb)
            + (59 # 104) * s V_synth_full_sb * max0(s V_synth_full__tmp)
            - (54 # 151) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            - (31 # 104) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
            + (3 # 11) * s V_synth_full_sb * max0(s V_synth_full_s)
            + (2 # 37) * s V_synth_full_sb * max0(s V_synth_full_sb)
            + (1 # 55) * s V_synth_full_sb^2 + s V_synth_full_z
            - (7 # 116) * s V_synth_full_z * max0(-16 + s V_synth_full_sb)
            + (97 # 156) * max0(-16 + s V_synth_full_sb)
            + (87 # 142) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (58 # 111) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                                + s V_synth_full__tmp1)
            - (65 # 122) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                                + s V_synth_full__tmp1
                                                                - s V_synth_full_s)
            + (0 # 1) * max0(-16 + s V_synth_full_sb) * max0(16
                                                             - s V_synth_full_sb)
            + (1 # 85) * max0(-16 + s V_synth_full_sb) * max0(s V_synth_full_sb)
            + (7 # 116) * max0(-16 + s V_synth_full_sb) * max0(s V_synth_full_z)
            + (4 # 103) * max0(-16 + s V_synth_full_sb)^2
            - (1 # 64) * max0(-2 + s V_synth_full_sb) * max0(-1
                                                             + s V_synth_full__tmp1)
            + (1 # 103) * max0(-2 + s V_synth_full_sb) * max0(-1
                                                              + s V_synth_full_sb)
            - (0 # 1) * max0(-2 + s V_synth_full_sb) * max0(15
                                                            - s V_synth_full_sb)
            - (7 # 12) * max0(-1 + s V_synth_full__tmp)
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (8 # 93) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                               + s V_synth_full__tmp1)
            + (3 # 113) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                                - s V_synth_full_sb)
            + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(16
                                                               - s V_synth_full_sb)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (845 # 86) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (254 # 63) * max0(-1 + s V_synth_full__tmp1)
            - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                + s V_synth_full_sb)
            - (9 # 83) * max0(-1 + s V_synth_full__tmp1) * max0(16
                                                                - s V_synth_full_sb)
            - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
            + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                  - s V_synth_full_ch)
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            - (5 # 79) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
            + (8 # 185) * max0(-1 + s V_synth_full_sb)
            + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                             - s V_synth_full_ch)
            + (0 # 1) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (0 # 1) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
            + (2 # 81) * max0(15 - s V_synth_full_sb)
            + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
            + (26 # 83) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
            + (0 # 1) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            - (1 # 145) * max0(s V_synth_full__tmp)
            - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            + (0 # 1) * max0(s V_synth_full__tmp1)
            + (963 # 100) * max0(s V_synth_full_ch)
            + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (14 # 75) * max0(s V_synth_full_sb)
            - (2 # 37) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 56 => ((233 # 34) - (508 # 57) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            - (87 # 142) * s V_synth_full__tmp * max0(-16 + s V_synth_full_sb)
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            + (3 # 94) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (3 # 113) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
            - (2 # 101) * s V_synth_full__tmp * max0(16 - s V_synth_full_sb)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            + (4 # 153) * s V_synth_full__tmp * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full__tmp^2
            + (2 # 87) * s V_synth_full__tmp1 * max0(-2 + s V_synth_full_sb)
            - (8 # 93) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full__tmp)
            - (1213 # 105) * s V_synth_full__tmp1 * max0(-1
                                                         + s V_synth_full__tmp
                                                         - s V_synth_full_ch)
            - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
            - (16 # 83) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
            - (1 # 6) * s V_synth_full__tmp1 * max0(16 - s V_synth_full_sb)
            + (467 # 105) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
            - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
            + (5 # 79) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
            - (122 # 103) * s V_synth_full_ch
            + (87 # 142) * s V_synth_full_ch * max0(-16 + s V_synth_full_sb)
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            - (4 # 13) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
            + (4 # 103) * s V_synth_full_ch * max0(16 - s V_synth_full_sb)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full_ch^2
            - (58 # 111) * s V_synth_full_s * max0(-16 + s V_synth_full_sb)
            + (1213 # 105) * s V_synth_full_s * max0(-1 + s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
            + (5 # 131) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
            + (2 # 83) * s V_synth_full_s * max0(16 - s V_synth_full_sb)
            - (205 # 47) * s V_synth_full_s * max0(s V_synth_full__tmp)
            - (977 # 112) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
            + (14 # 75) * s V_synth_full_s * max0(s V_synth_full_sb)
            - (47 # 78) * s V_synth_full_sb
            - (9 # 131) * s V_synth_full_sb * max0(-16 + s V_synth_full_sb)
            + (2 # 55) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
            - (76 # 157) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
            + (121 # 147) * s V_synth_full_sb * max0(-1
                                                     + s V_synth_full__tmp1
                                                     - s V_synth_full_s)
            - (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
            + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
            - (0 # 1) * s V_synth_full_sb * max0(16 - s V_synth_full_sb)
            + (59 # 104) * s V_synth_full_sb * max0(s V_synth_full__tmp)
            - (54 # 151) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            - (31 # 104) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
            + (3 # 11) * s V_synth_full_sb * max0(s V_synth_full_s)
            + (2 # 37) * s V_synth_full_sb * max0(s V_synth_full_sb)
            + (1 # 55) * s V_synth_full_sb^2 + s V_synth_full_z
            - (7 # 116) * s V_synth_full_z * max0(-16 + s V_synth_full_sb)
            + (97 # 156) * max0(-16 + s V_synth_full_sb)
            + (87 # 142) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (58 # 111) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                                + s V_synth_full__tmp1)
            - (65 # 122) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                                + s V_synth_full__tmp1
                                                                - s V_synth_full_s)
            + (0 # 1) * max0(-16 + s V_synth_full_sb) * max0(16
                                                             - s V_synth_full_sb)
            + (1 # 85) * max0(-16 + s V_synth_full_sb) * max0(s V_synth_full_sb)
            + (7 # 116) * max0(-16 + s V_synth_full_sb) * max0(s V_synth_full_z)
            + (4 # 103) * max0(-16 + s V_synth_full_sb)^2
            - (1 # 64) * max0(-2 + s V_synth_full_sb) * max0(-1
                                                             + s V_synth_full__tmp1)
            + (1 # 103) * max0(-2 + s V_synth_full_sb) * max0(-1
                                                              + s V_synth_full_sb)
            - (0 # 1) * max0(-2 + s V_synth_full_sb) * max0(15
                                                            - s V_synth_full_sb)
            - (7 # 12) * max0(-1 + s V_synth_full__tmp)
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (8 # 93) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                               + s V_synth_full__tmp1)
            + (3 # 113) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                                - s V_synth_full_sb)
            + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(16
                                                               - s V_synth_full_sb)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (845 # 86) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (254 # 63) * max0(-1 + s V_synth_full__tmp1)
            - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                + s V_synth_full_sb)
            - (9 # 83) * max0(-1 + s V_synth_full__tmp1) * max0(16
                                                                - s V_synth_full_sb)
            - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
            + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                  - s V_synth_full_ch)
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            - (5 # 79) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
            + (8 # 185) * max0(-1 + s V_synth_full_sb)
            + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                             - s V_synth_full_ch)
            + (0 # 1) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (0 # 1) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
            + (2 # 81) * max0(15 - s V_synth_full_sb)
            + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
            + (26 # 83) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
            + (0 # 1) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            - (1 # 145) * max0(s V_synth_full__tmp)
            - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            + (0 # 1) * max0(s V_synth_full__tmp1)
            + (963 # 100) * max0(s V_synth_full_ch)
            + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (14 # 75) * max0(s V_synth_full_sb)
            - (2 # 37) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 57 => ((233 # 34) - (508 # 57) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            - (87 # 142) * s V_synth_full__tmp * max0(-16 + s V_synth_full_sb)
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            + (3 # 94) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (3 # 113) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
            - (2 # 101) * s V_synth_full__tmp * max0(16 - s V_synth_full_sb)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            + (4 # 153) * s V_synth_full__tmp * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full__tmp^2
            + (2 # 87) * s V_synth_full__tmp1 * max0(-2 + s V_synth_full_sb)
            - (8 # 93) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full__tmp)
            - (1213 # 105) * s V_synth_full__tmp1 * max0(-1
                                                         + s V_synth_full__tmp
                                                         - s V_synth_full_ch)
            - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
            - (16 # 83) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
            - (1 # 6) * s V_synth_full__tmp1 * max0(16 - s V_synth_full_sb)
            + (467 # 105) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
            - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
            + (5 # 79) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
            - (122 # 103) * s V_synth_full_ch
            + (87 # 142) * s V_synth_full_ch * max0(-16 + s V_synth_full_sb)
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            - (4 # 13) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
            + (4 # 103) * s V_synth_full_ch * max0(16 - s V_synth_full_sb)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full_ch^2
            - (58 # 111) * s V_synth_full_s * max0(-16 + s V_synth_full_sb)
            + (1213 # 105) * s V_synth_full_s * max0(-1 + s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
            + (5 # 131) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
            + (2 # 83) * s V_synth_full_s * max0(16 - s V_synth_full_sb)
            - (205 # 47) * s V_synth_full_s * max0(s V_synth_full__tmp)
            - (977 # 112) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
            + (14 # 75) * s V_synth_full_s * max0(s V_synth_full_sb)
            - (47 # 78) * s V_synth_full_sb
            - (9 # 131) * s V_synth_full_sb * max0(-16 + s V_synth_full_sb)
            + (2 # 55) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
            - (76 # 157) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
            + (121 # 147) * s V_synth_full_sb * max0(-1
                                                     + s V_synth_full__tmp1
                                                     - s V_synth_full_s)
            - (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
            + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
            - (0 # 1) * s V_synth_full_sb * max0(16 - s V_synth_full_sb)
            + (59 # 104) * s V_synth_full_sb * max0(s V_synth_full__tmp)
            - (54 # 151) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            - (31 # 104) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
            + (3 # 11) * s V_synth_full_sb * max0(s V_synth_full_s)
            + (2 # 37) * s V_synth_full_sb * max0(s V_synth_full_sb)
            + (1 # 55) * s V_synth_full_sb^2 + s V_synth_full_z
            - (7 # 116) * s V_synth_full_z * max0(-16 + s V_synth_full_sb)
            + (97 # 156) * max0(-16 + s V_synth_full_sb)
            + (87 # 142) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (58 # 111) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                                + s V_synth_full__tmp1)
            - (65 # 122) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                                + s V_synth_full__tmp1
                                                                - s V_synth_full_s)
            + (0 # 1) * max0(-16 + s V_synth_full_sb) * max0(16
                                                             - s V_synth_full_sb)
            + (1 # 85) * max0(-16 + s V_synth_full_sb) * max0(s V_synth_full_sb)
            + (7 # 116) * max0(-16 + s V_synth_full_sb) * max0(s V_synth_full_z)
            + (4 # 103) * max0(-16 + s V_synth_full_sb)^2
            - (1 # 64) * max0(-2 + s V_synth_full_sb) * max0(-1
                                                             + s V_synth_full__tmp1)
            + (1 # 103) * max0(-2 + s V_synth_full_sb) * max0(-1
                                                              + s V_synth_full_sb)
            - (0 # 1) * max0(-2 + s V_synth_full_sb) * max0(15
                                                            - s V_synth_full_sb)
            - (7 # 12) * max0(-1 + s V_synth_full__tmp)
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (8 # 93) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                               + s V_synth_full__tmp1)
            + (3 # 113) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                                - s V_synth_full_sb)
            + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(16
                                                               - s V_synth_full_sb)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (845 # 86) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (254 # 63) * max0(-1 + s V_synth_full__tmp1)
            - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                + s V_synth_full_sb)
            - (9 # 83) * max0(-1 + s V_synth_full__tmp1) * max0(16
                                                                - s V_synth_full_sb)
            - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
            + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                  - s V_synth_full_ch)
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            - (5 # 79) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
            + (8 # 185) * max0(-1 + s V_synth_full_sb)
            + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                             - s V_synth_full_ch)
            + (0 # 1) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (0 # 1) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
            + (2 # 81) * max0(15 - s V_synth_full_sb)
            + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
            + (26 # 83) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
            + (0 # 1) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            - (1 # 145) * max0(s V_synth_full__tmp)
            - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            + (0 # 1) * max0(s V_synth_full__tmp1)
            + (963 # 100) * max0(s V_synth_full_ch)
            + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (14 # 75) * max0(s V_synth_full_sb)
            - (2 # 37) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 58 => ((233 # 34) - (508 # 57) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            - (87 # 142) * s V_synth_full__tmp * max0(-16 + s V_synth_full_sb)
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            + (3 # 94) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (3 # 113) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
            - (2 # 101) * s V_synth_full__tmp * max0(16 - s V_synth_full_sb)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            + (4 # 153) * s V_synth_full__tmp * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full__tmp^2
            + (2 # 87) * s V_synth_full__tmp1 * max0(-2 + s V_synth_full_sb)
            - (8 # 93) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full__tmp)
            - (1213 # 105) * s V_synth_full__tmp1 * max0(-1
                                                         + s V_synth_full__tmp
                                                         - s V_synth_full_ch)
            - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
            - (16 # 83) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
            - (1 # 6) * s V_synth_full__tmp1 * max0(16 - s V_synth_full_sb)
            + (467 # 105) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
            - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
            + (5 # 79) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
            - (122 # 103) * s V_synth_full_ch
            + (87 # 142) * s V_synth_full_ch * max0(-16 + s V_synth_full_sb)
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            - (4 # 13) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
            + (4 # 103) * s V_synth_full_ch * max0(16 - s V_synth_full_sb)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full_ch^2
            - (58 # 111) * s V_synth_full_s * max0(-16 + s V_synth_full_sb)
            + (1213 # 105) * s V_synth_full_s * max0(-1 + s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
            + (5 # 131) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
            + (2 # 83) * s V_synth_full_s * max0(16 - s V_synth_full_sb)
            - (205 # 47) * s V_synth_full_s * max0(s V_synth_full__tmp)
            - (977 # 112) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
            + (14 # 75) * s V_synth_full_s * max0(s V_synth_full_sb)
            - (47 # 78) * s V_synth_full_sb
            - (9 # 131) * s V_synth_full_sb * max0(-16 + s V_synth_full_sb)
            + (2 # 55) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
            - (76 # 157) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
            + (121 # 147) * s V_synth_full_sb * max0(-1
                                                     + s V_synth_full__tmp1
                                                     - s V_synth_full_s)
            - (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
            + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
            - (0 # 1) * s V_synth_full_sb * max0(16 - s V_synth_full_sb)
            + (59 # 104) * s V_synth_full_sb * max0(s V_synth_full__tmp)
            - (54 # 151) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            - (31 # 104) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
            + (3 # 11) * s V_synth_full_sb * max0(s V_synth_full_s)
            + (2 # 37) * s V_synth_full_sb * max0(s V_synth_full_sb)
            + (1 # 55) * s V_synth_full_sb^2 + s V_synth_full_z
            - (7 # 116) * s V_synth_full_z * max0(-16 + s V_synth_full_sb)
            + (97 # 156) * max0(-16 + s V_synth_full_sb)
            + (87 # 142) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (58 # 111) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                                + s V_synth_full__tmp1)
            - (65 # 122) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                                + s V_synth_full__tmp1
                                                                - s V_synth_full_s)
            + (0 # 1) * max0(-16 + s V_synth_full_sb) * max0(16
                                                             - s V_synth_full_sb)
            + (1 # 85) * max0(-16 + s V_synth_full_sb) * max0(s V_synth_full_sb)
            + (7 # 116) * max0(-16 + s V_synth_full_sb) * max0(s V_synth_full_z)
            + (4 # 103) * max0(-16 + s V_synth_full_sb)^2
            - (1 # 64) * max0(-2 + s V_synth_full_sb) * max0(-1
                                                             + s V_synth_full__tmp1)
            + (1 # 103) * max0(-2 + s V_synth_full_sb) * max0(-1
                                                              + s V_synth_full_sb)
            - (0 # 1) * max0(-2 + s V_synth_full_sb) * max0(15
                                                            - s V_synth_full_sb)
            - (7 # 12) * max0(-1 + s V_synth_full__tmp)
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (8 # 93) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                               + s V_synth_full__tmp1)
            + (3 # 113) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                                - s V_synth_full_sb)
            + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(16
                                                               - s V_synth_full_sb)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (845 # 86) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (254 # 63) * max0(-1 + s V_synth_full__tmp1)
            - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                + s V_synth_full_sb)
            - (9 # 83) * max0(-1 + s V_synth_full__tmp1) * max0(16
                                                                - s V_synth_full_sb)
            - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
            + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                  - s V_synth_full_ch)
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            - (5 # 79) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
            + (8 # 185) * max0(-1 + s V_synth_full_sb)
            + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                             - s V_synth_full_ch)
            + (0 # 1) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (0 # 1) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
            + (2 # 81) * max0(15 - s V_synth_full_sb)
            + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
            + (26 # 83) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
            + (0 # 1) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            - (1 # 145) * max0(s V_synth_full__tmp)
            - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            + (0 # 1) * max0(s V_synth_full__tmp1)
            + (963 # 100) * max0(s V_synth_full_ch)
            + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (14 # 75) * max0(s V_synth_full_sb)
            - (2 # 37) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 59 => ((233 # 34) - (508 # 57) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            - (87 # 142) * s V_synth_full__tmp * max0(-16 + s V_synth_full_sb)
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            + (3 # 94) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (3 # 113) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
            - (2 # 101) * s V_synth_full__tmp * max0(16 - s V_synth_full_sb)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            + (4 # 153) * s V_synth_full__tmp * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full__tmp^2
            + (2 # 87) * s V_synth_full__tmp1 * max0(-2 + s V_synth_full_sb)
            - (8 # 93) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full__tmp)
            - (1213 # 105) * s V_synth_full__tmp1 * max0(-1
                                                         + s V_synth_full__tmp
                                                         - s V_synth_full_ch)
            - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
            - (16 # 83) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
            - (1 # 6) * s V_synth_full__tmp1 * max0(16 - s V_synth_full_sb)
            + (467 # 105) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
            - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
            + (5 # 79) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
            - (122 # 103) * s V_synth_full_ch
            + (87 # 142) * s V_synth_full_ch * max0(-16 + s V_synth_full_sb)
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            - (4 # 13) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
            + (4 # 103) * s V_synth_full_ch * max0(16 - s V_synth_full_sb)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full_ch^2
            - (58 # 111) * s V_synth_full_s * max0(-16 + s V_synth_full_sb)
            + (1213 # 105) * s V_synth_full_s * max0(-1 + s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
            + (5 # 131) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
            + (2 # 83) * s V_synth_full_s * max0(16 - s V_synth_full_sb)
            - (205 # 47) * s V_synth_full_s * max0(s V_synth_full__tmp)
            - (977 # 112) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
            + (14 # 75) * s V_synth_full_s * max0(s V_synth_full_sb)
            - (47 # 78) * s V_synth_full_sb
            - (9 # 131) * s V_synth_full_sb * max0(-16 + s V_synth_full_sb)
            + (2 # 55) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
            - (76 # 157) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
            + (121 # 147) * s V_synth_full_sb * max0(-1
                                                     + s V_synth_full__tmp1
                                                     - s V_synth_full_s)
            - (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
            + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
            - (0 # 1) * s V_synth_full_sb * max0(16 - s V_synth_full_sb)
            + (59 # 104) * s V_synth_full_sb * max0(s V_synth_full__tmp)
            - (54 # 151) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            - (31 # 104) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
            + (3 # 11) * s V_synth_full_sb * max0(s V_synth_full_s)
            + (2 # 37) * s V_synth_full_sb * max0(s V_synth_full_sb)
            + (1 # 55) * s V_synth_full_sb^2 + s V_synth_full_z
            - (7 # 116) * s V_synth_full_z * max0(-16 + s V_synth_full_sb)
            + (97 # 156) * max0(-16 + s V_synth_full_sb)
            + (87 # 142) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (58 # 111) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                                + s V_synth_full__tmp1)
            - (65 # 122) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                                + s V_synth_full__tmp1
                                                                - s V_synth_full_s)
            + (0 # 1) * max0(-16 + s V_synth_full_sb) * max0(16
                                                             - s V_synth_full_sb)
            + (1 # 85) * max0(-16 + s V_synth_full_sb) * max0(s V_synth_full_sb)
            + (7 # 116) * max0(-16 + s V_synth_full_sb) * max0(s V_synth_full_z)
            + (4 # 103) * max0(-16 + s V_synth_full_sb)^2
            - (1 # 64) * max0(-2 + s V_synth_full_sb) * max0(-1
                                                             + s V_synth_full__tmp1)
            + (1 # 103) * max0(-2 + s V_synth_full_sb) * max0(-1
                                                              + s V_synth_full_sb)
            - (0 # 1) * max0(-2 + s V_synth_full_sb) * max0(15
                                                            - s V_synth_full_sb)
            - (7 # 12) * max0(-1 + s V_synth_full__tmp)
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (8 # 93) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                               + s V_synth_full__tmp1)
            + (3 # 113) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                                - s V_synth_full_sb)
            + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(16
                                                               - s V_synth_full_sb)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (845 # 86) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (254 # 63) * max0(-1 + s V_synth_full__tmp1)
            - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                + s V_synth_full_sb)
            - (9 # 83) * max0(-1 + s V_synth_full__tmp1) * max0(16
                                                                - s V_synth_full_sb)
            - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
            + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                  - s V_synth_full_ch)
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            - (5 # 79) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
            + (8 # 185) * max0(-1 + s V_synth_full_sb)
            + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                             - s V_synth_full_ch)
            + (0 # 1) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (0 # 1) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
            + (2 # 81) * max0(15 - s V_synth_full_sb)
            + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
            + (26 # 83) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
            + (0 # 1) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            - (1 # 145) * max0(s V_synth_full__tmp)
            - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            + (0 # 1) * max0(s V_synth_full__tmp1)
            + (963 # 100) * max0(s V_synth_full_ch)
            + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (14 # 75) * max0(s V_synth_full_sb)
            - (2 # 37) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 60 => ((233 # 34) - (508 # 57) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            - (87 # 142) * s V_synth_full__tmp * max0(-16 + s V_synth_full_sb)
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            + (3 # 94) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (3 # 113) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
            - (2 # 101) * s V_synth_full__tmp * max0(16 - s V_synth_full_sb)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            + (4 # 153) * s V_synth_full__tmp * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full__tmp^2
            + (2 # 87) * s V_synth_full__tmp1 * max0(-2 + s V_synth_full_sb)
            - (8 # 93) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full__tmp)
            - (1213 # 105) * s V_synth_full__tmp1 * max0(-1
                                                         + s V_synth_full__tmp
                                                         - s V_synth_full_ch)
            - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
            - (16 # 83) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
            - (1 # 6) * s V_synth_full__tmp1 * max0(16 - s V_synth_full_sb)
            + (467 # 105) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
            - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
            + (5 # 79) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
            - (122 # 103) * s V_synth_full_ch
            + (87 # 142) * s V_synth_full_ch * max0(-16 + s V_synth_full_sb)
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            - (4 # 13) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
            + (4 # 103) * s V_synth_full_ch * max0(16 - s V_synth_full_sb)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full_ch^2
            - (58 # 111) * s V_synth_full_s * max0(-16 + s V_synth_full_sb)
            + (1213 # 105) * s V_synth_full_s * max0(-1 + s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
            + (5 # 131) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
            + (2 # 83) * s V_synth_full_s * max0(16 - s V_synth_full_sb)
            - (205 # 47) * s V_synth_full_s * max0(s V_synth_full__tmp)
            - (977 # 112) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
            + (14 # 75) * s V_synth_full_s * max0(s V_synth_full_sb)
            - (47 # 78) * s V_synth_full_sb
            - (9 # 131) * s V_synth_full_sb * max0(-16 + s V_synth_full_sb)
            + (2 # 55) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
            - (76 # 157) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
            + (121 # 147) * s V_synth_full_sb * max0(-1
                                                     + s V_synth_full__tmp1
                                                     - s V_synth_full_s)
            - (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
            + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
            - (0 # 1) * s V_synth_full_sb * max0(16 - s V_synth_full_sb)
            + (59 # 104) * s V_synth_full_sb * max0(s V_synth_full__tmp)
            - (54 # 151) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            - (31 # 104) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
            + (3 # 11) * s V_synth_full_sb * max0(s V_synth_full_s)
            + (2 # 37) * s V_synth_full_sb * max0(s V_synth_full_sb)
            + (1 # 55) * s V_synth_full_sb^2 + s V_synth_full_z
            - (7 # 116) * s V_synth_full_z * max0(-16 + s V_synth_full_sb)
            + (97 # 156) * max0(-16 + s V_synth_full_sb)
            + (87 # 142) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (58 # 111) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                                + s V_synth_full__tmp1)
            - (65 # 122) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                                + s V_synth_full__tmp1
                                                                - s V_synth_full_s)
            + (0 # 1) * max0(-16 + s V_synth_full_sb) * max0(16
                                                             - s V_synth_full_sb)
            + (1 # 85) * max0(-16 + s V_synth_full_sb) * max0(s V_synth_full_sb)
            + (7 # 116) * max0(-16 + s V_synth_full_sb) * max0(s V_synth_full_z)
            + (4 # 103) * max0(-16 + s V_synth_full_sb)^2
            - (1 # 64) * max0(-2 + s V_synth_full_sb) * max0(-1
                                                             + s V_synth_full__tmp1)
            + (1 # 103) * max0(-2 + s V_synth_full_sb) * max0(-1
                                                              + s V_synth_full_sb)
            - (0 # 1) * max0(-2 + s V_synth_full_sb) * max0(15
                                                            - s V_synth_full_sb)
            - (7 # 12) * max0(-1 + s V_synth_full__tmp)
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (8 # 93) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                               + s V_synth_full__tmp1)
            + (3 # 113) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                                - s V_synth_full_sb)
            + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(16
                                                               - s V_synth_full_sb)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (845 # 86) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (254 # 63) * max0(-1 + s V_synth_full__tmp1)
            - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                + s V_synth_full_sb)
            - (9 # 83) * max0(-1 + s V_synth_full__tmp1) * max0(16
                                                                - s V_synth_full_sb)
            - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
            + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                  - s V_synth_full_ch)
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            - (5 # 79) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
            + (8 # 185) * max0(-1 + s V_synth_full_sb)
            + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                             - s V_synth_full_ch)
            + (0 # 1) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (0 # 1) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
            + (2 # 81) * max0(15 - s V_synth_full_sb)
            + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
            + (26 # 83) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
            + (0 # 1) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            - (1 # 145) * max0(s V_synth_full__tmp)
            - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            + (0 # 1) * max0(s V_synth_full__tmp1)
            + (963 # 100) * max0(s V_synth_full_ch)
            + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (14 # 75) * max0(s V_synth_full_sb)
            - (2 # 37) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 61 => ((233 # 34) - (508 # 57) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            - (87 # 142) * s V_synth_full__tmp * max0(-16 + s V_synth_full_sb)
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            + (3 # 94) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (3 # 113) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
            - (2 # 101) * s V_synth_full__tmp * max0(16 - s V_synth_full_sb)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            + (4 # 153) * s V_synth_full__tmp * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full__tmp^2
            + (2 # 87) * s V_synth_full__tmp1 * max0(-2 + s V_synth_full_sb)
            - (8 # 93) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full__tmp)
            - (1213 # 105) * s V_synth_full__tmp1 * max0(-1
                                                         + s V_synth_full__tmp
                                                         - s V_synth_full_ch)
            - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
            - (16 # 83) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
            - (1 # 6) * s V_synth_full__tmp1 * max0(16 - s V_synth_full_sb)
            + (467 # 105) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
            - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
            + (5 # 79) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
            - (122 # 103) * s V_synth_full_ch
            + (87 # 142) * s V_synth_full_ch * max0(-16 + s V_synth_full_sb)
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            - (4 # 13) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
            + (4 # 103) * s V_synth_full_ch * max0(16 - s V_synth_full_sb)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full_ch^2
            - (58 # 111) * s V_synth_full_s * max0(-16 + s V_synth_full_sb)
            + (1213 # 105) * s V_synth_full_s * max0(-1 + s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
            + (5 # 131) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
            + (2 # 83) * s V_synth_full_s * max0(16 - s V_synth_full_sb)
            - (205 # 47) * s V_synth_full_s * max0(s V_synth_full__tmp)
            - (977 # 112) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
            + (14 # 75) * s V_synth_full_s * max0(s V_synth_full_sb)
            - (47 # 78) * s V_synth_full_sb
            - (9 # 131) * s V_synth_full_sb * max0(-16 + s V_synth_full_sb)
            + (2 # 55) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
            - (76 # 157) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
            + (121 # 147) * s V_synth_full_sb * max0(-1
                                                     + s V_synth_full__tmp1
                                                     - s V_synth_full_s)
            - (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
            + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
            - (0 # 1) * s V_synth_full_sb * max0(16 - s V_synth_full_sb)
            + (59 # 104) * s V_synth_full_sb * max0(s V_synth_full__tmp)
            - (54 # 151) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            - (31 # 104) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
            + (3 # 11) * s V_synth_full_sb * max0(s V_synth_full_s)
            + (2 # 37) * s V_synth_full_sb * max0(s V_synth_full_sb)
            + (1 # 55) * s V_synth_full_sb^2 + s V_synth_full_z
            - (7 # 116) * s V_synth_full_z * max0(-16 + s V_synth_full_sb)
            + (97 # 156) * max0(-16 + s V_synth_full_sb)
            + (87 # 142) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (58 # 111) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                                + s V_synth_full__tmp1)
            - (65 # 122) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                                + s V_synth_full__tmp1
                                                                - s V_synth_full_s)
            + (0 # 1) * max0(-16 + s V_synth_full_sb) * max0(16
                                                             - s V_synth_full_sb)
            + (1 # 85) * max0(-16 + s V_synth_full_sb) * max0(s V_synth_full_sb)
            + (7 # 116) * max0(-16 + s V_synth_full_sb) * max0(s V_synth_full_z)
            + (4 # 103) * max0(-16 + s V_synth_full_sb)^2
            - (1 # 64) * max0(-2 + s V_synth_full_sb) * max0(-1
                                                             + s V_synth_full__tmp1)
            + (1 # 103) * max0(-2 + s V_synth_full_sb) * max0(-1
                                                              + s V_synth_full_sb)
            - (0 # 1) * max0(-2 + s V_synth_full_sb) * max0(15
                                                            - s V_synth_full_sb)
            - (7 # 12) * max0(-1 + s V_synth_full__tmp)
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (8 # 93) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                               + s V_synth_full__tmp1)
            + (3 # 113) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                                - s V_synth_full_sb)
            + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(16
                                                               - s V_synth_full_sb)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (845 # 86) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (254 # 63) * max0(-1 + s V_synth_full__tmp1)
            - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                + s V_synth_full_sb)
            - (9 # 83) * max0(-1 + s V_synth_full__tmp1) * max0(16
                                                                - s V_synth_full_sb)
            - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
            + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                  - s V_synth_full_ch)
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            - (5 # 79) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
            + (8 # 185) * max0(-1 + s V_synth_full_sb)
            + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                             - s V_synth_full_ch)
            + (0 # 1) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (0 # 1) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
            + (2 # 81) * max0(15 - s V_synth_full_sb)
            + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
            + (26 # 83) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
            + (0 # 1) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            - (1 # 145) * max0(s V_synth_full__tmp)
            - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            + (0 # 1) * max0(s V_synth_full__tmp1)
            + (963 # 100) * max0(s V_synth_full_ch)
            + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (14 # 75) * max0(s V_synth_full_sb)
            - (2 # 37) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 62 => ((233 # 34) - (508 # 57) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            - (87 # 142) * s V_synth_full__tmp * max0(-16 + s V_synth_full_sb)
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            + (3 # 94) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (3 # 113) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
            - (2 # 101) * s V_synth_full__tmp * max0(16 - s V_synth_full_sb)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            + (4 # 153) * s V_synth_full__tmp * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full__tmp^2
            + (2 # 87) * s V_synth_full__tmp1 * max0(-2 + s V_synth_full_sb)
            - (8 # 93) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full__tmp)
            - (1213 # 105) * s V_synth_full__tmp1 * max0(-1
                                                         + s V_synth_full__tmp
                                                         - s V_synth_full_ch)
            - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
            - (16 # 83) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
            - (1 # 6) * s V_synth_full__tmp1 * max0(16 - s V_synth_full_sb)
            + (467 # 105) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
            - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
            + (5 # 79) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
            - (122 # 103) * s V_synth_full_ch
            + (87 # 142) * s V_synth_full_ch * max0(-16 + s V_synth_full_sb)
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            - (4 # 13) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
            + (4 # 103) * s V_synth_full_ch * max0(16 - s V_synth_full_sb)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full_ch^2
            - (58 # 111) * s V_synth_full_s * max0(-16 + s V_synth_full_sb)
            + (1213 # 105) * s V_synth_full_s * max0(-1 + s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
            + (5 # 131) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
            + (2 # 83) * s V_synth_full_s * max0(16 - s V_synth_full_sb)
            - (205 # 47) * s V_synth_full_s * max0(s V_synth_full__tmp)
            - (977 # 112) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
            + (14 # 75) * s V_synth_full_s * max0(s V_synth_full_sb)
            - (47 # 78) * s V_synth_full_sb
            - (9 # 131) * s V_synth_full_sb * max0(-16 + s V_synth_full_sb)
            + (2 # 55) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
            - (76 # 157) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
            + (121 # 147) * s V_synth_full_sb * max0(-1
                                                     + s V_synth_full__tmp1
                                                     - s V_synth_full_s)
            - (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
            + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
            - (0 # 1) * s V_synth_full_sb * max0(16 - s V_synth_full_sb)
            + (59 # 104) * s V_synth_full_sb * max0(s V_synth_full__tmp)
            - (54 # 151) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            - (31 # 104) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
            + (3 # 11) * s V_synth_full_sb * max0(s V_synth_full_s)
            + (2 # 37) * s V_synth_full_sb * max0(s V_synth_full_sb)
            + (1 # 55) * s V_synth_full_sb^2 + s V_synth_full_z
            - (7 # 116) * s V_synth_full_z * max0(-16 + s V_synth_full_sb)
            + (97 # 156) * max0(-16 + s V_synth_full_sb)
            + (87 # 142) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (58 # 111) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                                + s V_synth_full__tmp1)
            - (65 # 122) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                                + s V_synth_full__tmp1
                                                                - s V_synth_full_s)
            + (0 # 1) * max0(-16 + s V_synth_full_sb) * max0(16
                                                             - s V_synth_full_sb)
            + (1 # 85) * max0(-16 + s V_synth_full_sb) * max0(s V_synth_full_sb)
            + (7 # 116) * max0(-16 + s V_synth_full_sb) * max0(s V_synth_full_z)
            + (4 # 103) * max0(-16 + s V_synth_full_sb)^2
            - (1 # 64) * max0(-2 + s V_synth_full_sb) * max0(-1
                                                             + s V_synth_full__tmp1)
            + (1 # 103) * max0(-2 + s V_synth_full_sb) * max0(-1
                                                              + s V_synth_full_sb)
            - (0 # 1) * max0(-2 + s V_synth_full_sb) * max0(15
                                                            - s V_synth_full_sb)
            - (7 # 12) * max0(-1 + s V_synth_full__tmp)
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (8 # 93) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                               + s V_synth_full__tmp1)
            + (3 # 113) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                                - s V_synth_full_sb)
            + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(16
                                                               - s V_synth_full_sb)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (845 # 86) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (254 # 63) * max0(-1 + s V_synth_full__tmp1)
            - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                + s V_synth_full_sb)
            - (9 # 83) * max0(-1 + s V_synth_full__tmp1) * max0(16
                                                                - s V_synth_full_sb)
            - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
            + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                  - s V_synth_full_ch)
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            - (5 # 79) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
            + (8 # 185) * max0(-1 + s V_synth_full_sb)
            + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                             - s V_synth_full_ch)
            + (0 # 1) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (0 # 1) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
            + (2 # 81) * max0(15 - s V_synth_full_sb)
            + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
            + (26 # 83) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
            + (0 # 1) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            - (1 # 145) * max0(s V_synth_full__tmp)
            - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            + (0 # 1) * max0(s V_synth_full__tmp1)
            + (963 # 100) * max0(s V_synth_full_ch)
            + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (14 # 75) * max0(s V_synth_full_sb)
            - (2 # 37) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 63 => ((233 # 34) - (508 # 57) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            - (87 # 142) * s V_synth_full__tmp * max0(-16 + s V_synth_full_sb)
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            + (3 # 94) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (3 # 113) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
            - (2 # 101) * s V_synth_full__tmp * max0(16 - s V_synth_full_sb)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            + (4 # 153) * s V_synth_full__tmp * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full__tmp^2
            + (2 # 87) * s V_synth_full__tmp1 * max0(-2 + s V_synth_full_sb)
            - (8 # 93) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full__tmp)
            - (1213 # 105) * s V_synth_full__tmp1 * max0(-1
                                                         + s V_synth_full__tmp
                                                         - s V_synth_full_ch)
            - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
            - (16 # 83) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
            - (1 # 6) * s V_synth_full__tmp1 * max0(16 - s V_synth_full_sb)
            + (467 # 105) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
            - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
            + (5 # 79) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
            - (122 # 103) * s V_synth_full_ch
            + (87 # 142) * s V_synth_full_ch * max0(-16 + s V_synth_full_sb)
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            - (4 # 13) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
            + (4 # 103) * s V_synth_full_ch * max0(16 - s V_synth_full_sb)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full_ch^2
            - (58 # 111) * s V_synth_full_s * max0(-16 + s V_synth_full_sb)
            + (1213 # 105) * s V_synth_full_s * max0(-1 + s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
            + (5 # 131) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
            + (2 # 83) * s V_synth_full_s * max0(16 - s V_synth_full_sb)
            - (205 # 47) * s V_synth_full_s * max0(s V_synth_full__tmp)
            - (977 # 112) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
            + (14 # 75) * s V_synth_full_s * max0(s V_synth_full_sb)
            - (47 # 78) * s V_synth_full_sb
            - (9 # 131) * s V_synth_full_sb * max0(-16 + s V_synth_full_sb)
            + (2 # 55) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
            - (76 # 157) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
            + (121 # 147) * s V_synth_full_sb * max0(-1
                                                     + s V_synth_full__tmp1
                                                     - s V_synth_full_s)
            - (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
            + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
            - (0 # 1) * s V_synth_full_sb * max0(16 - s V_synth_full_sb)
            + (59 # 104) * s V_synth_full_sb * max0(s V_synth_full__tmp)
            - (54 # 151) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            - (31 # 104) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
            + (3 # 11) * s V_synth_full_sb * max0(s V_synth_full_s)
            + (2 # 37) * s V_synth_full_sb * max0(s V_synth_full_sb)
            + (1 # 55) * s V_synth_full_sb^2 + s V_synth_full_z
            - (7 # 116) * s V_synth_full_z * max0(-16 + s V_synth_full_sb)
            + (97 # 156) * max0(-16 + s V_synth_full_sb)
            + (87 # 142) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (58 # 111) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                                + s V_synth_full__tmp1)
            - (65 # 122) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                                + s V_synth_full__tmp1
                                                                - s V_synth_full_s)
            + (0 # 1) * max0(-16 + s V_synth_full_sb) * max0(16
                                                             - s V_synth_full_sb)
            + (1 # 85) * max0(-16 + s V_synth_full_sb) * max0(s V_synth_full_sb)
            + (7 # 116) * max0(-16 + s V_synth_full_sb) * max0(s V_synth_full_z)
            + (4 # 103) * max0(-16 + s V_synth_full_sb)^2
            - (1 # 64) * max0(-2 + s V_synth_full_sb) * max0(-1
                                                             + s V_synth_full__tmp1)
            + (1 # 103) * max0(-2 + s V_synth_full_sb) * max0(-1
                                                              + s V_synth_full_sb)
            - (0 # 1) * max0(-2 + s V_synth_full_sb) * max0(15
                                                            - s V_synth_full_sb)
            - (7 # 12) * max0(-1 + s V_synth_full__tmp)
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (8 # 93) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                               + s V_synth_full__tmp1)
            + (3 # 113) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                                - s V_synth_full_sb)
            + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(16
                                                               - s V_synth_full_sb)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (845 # 86) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (254 # 63) * max0(-1 + s V_synth_full__tmp1)
            - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                + s V_synth_full_sb)
            - (9 # 83) * max0(-1 + s V_synth_full__tmp1) * max0(16
                                                                - s V_synth_full_sb)
            - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
            + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                  - s V_synth_full_ch)
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            - (5 # 79) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
            + (8 # 185) * max0(-1 + s V_synth_full_sb)
            + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                             - s V_synth_full_ch)
            + (0 # 1) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (0 # 1) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
            + (2 # 81) * max0(15 - s V_synth_full_sb)
            + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
            + (26 # 83) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
            + (0 # 1) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            - (1 # 145) * max0(s V_synth_full__tmp)
            - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            + (0 # 1) * max0(s V_synth_full__tmp1)
            + (963 # 100) * max0(s V_synth_full_ch)
            + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (14 # 75) * max0(s V_synth_full_sb)
            - (2 # 37) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 64 => ((233 # 34) - (508 # 57) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            - (87 # 142) * s V_synth_full__tmp * max0(-16 + s V_synth_full_sb)
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            + (3 # 94) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (3 # 113) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
            - (2 # 101) * s V_synth_full__tmp * max0(16 - s V_synth_full_sb)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            + (4 # 153) * s V_synth_full__tmp * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full__tmp^2
            + (2 # 87) * s V_synth_full__tmp1 * max0(-2 + s V_synth_full_sb)
            - (8 # 93) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full__tmp)
            - (1213 # 105) * s V_synth_full__tmp1 * max0(-1
                                                         + s V_synth_full__tmp
                                                         - s V_synth_full_ch)
            - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
            - (16 # 83) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
            - (1 # 6) * s V_synth_full__tmp1 * max0(16 - s V_synth_full_sb)
            + (467 # 105) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
            - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
            + (5 # 79) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
            - (122 # 103) * s V_synth_full_ch
            + (87 # 142) * s V_synth_full_ch * max0(-16 + s V_synth_full_sb)
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            - (4 # 13) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
            + (4 # 103) * s V_synth_full_ch * max0(16 - s V_synth_full_sb)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full_ch^2
            - (58 # 111) * s V_synth_full_s * max0(-16 + s V_synth_full_sb)
            + (1213 # 105) * s V_synth_full_s * max0(-1 + s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
            + (5 # 131) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
            + (2 # 83) * s V_synth_full_s * max0(16 - s V_synth_full_sb)
            - (205 # 47) * s V_synth_full_s * max0(s V_synth_full__tmp)
            - (977 # 112) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
            + (14 # 75) * s V_synth_full_s * max0(s V_synth_full_sb)
            - (47 # 78) * s V_synth_full_sb
            - (9 # 131) * s V_synth_full_sb * max0(-16 + s V_synth_full_sb)
            + (2 # 55) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
            - (76 # 157) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
            + (121 # 147) * s V_synth_full_sb * max0(-1
                                                     + s V_synth_full__tmp1
                                                     - s V_synth_full_s)
            - (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
            + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
            - (0 # 1) * s V_synth_full_sb * max0(16 - s V_synth_full_sb)
            + (59 # 104) * s V_synth_full_sb * max0(s V_synth_full__tmp)
            - (54 # 151) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            - (31 # 104) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
            + (3 # 11) * s V_synth_full_sb * max0(s V_synth_full_s)
            + (2 # 37) * s V_synth_full_sb * max0(s V_synth_full_sb)
            + (1 # 55) * s V_synth_full_sb^2 + s V_synth_full_z
            - (7 # 116) * s V_synth_full_z * max0(-16 + s V_synth_full_sb)
            + (97 # 156) * max0(-16 + s V_synth_full_sb)
            + (87 # 142) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (58 # 111) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                                + s V_synth_full__tmp1)
            - (65 # 122) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                                + s V_synth_full__tmp1
                                                                - s V_synth_full_s)
            + (0 # 1) * max0(-16 + s V_synth_full_sb) * max0(16
                                                             - s V_synth_full_sb)
            + (1 # 85) * max0(-16 + s V_synth_full_sb) * max0(s V_synth_full_sb)
            + (7 # 116) * max0(-16 + s V_synth_full_sb) * max0(s V_synth_full_z)
            + (4 # 103) * max0(-16 + s V_synth_full_sb)^2
            - (1 # 64) * max0(-2 + s V_synth_full_sb) * max0(-1
                                                             + s V_synth_full__tmp1)
            + (1 # 103) * max0(-2 + s V_synth_full_sb) * max0(-1
                                                              + s V_synth_full_sb)
            - (0 # 1) * max0(-2 + s V_synth_full_sb) * max0(15
                                                            - s V_synth_full_sb)
            - (7 # 12) * max0(-1 + s V_synth_full__tmp)
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (8 # 93) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                               + s V_synth_full__tmp1)
            + (3 # 113) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                                - s V_synth_full_sb)
            + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(16
                                                               - s V_synth_full_sb)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (845 # 86) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (254 # 63) * max0(-1 + s V_synth_full__tmp1)
            - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                + s V_synth_full_sb)
            - (9 # 83) * max0(-1 + s V_synth_full__tmp1) * max0(16
                                                                - s V_synth_full_sb)
            - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
            + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                  - s V_synth_full_ch)
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            - (5 # 79) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
            + (8 # 185) * max0(-1 + s V_synth_full_sb)
            + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                             - s V_synth_full_ch)
            + (0 # 1) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (0 # 1) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
            + (2 # 81) * max0(15 - s V_synth_full_sb)
            + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
            + (26 # 83) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
            + (0 # 1) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            - (1 # 145) * max0(s V_synth_full__tmp)
            - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            + (0 # 1) * max0(s V_synth_full__tmp1)
            + (963 # 100) * max0(s V_synth_full_ch)
            + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (14 # 75) * max0(s V_synth_full_sb)
            - (2 # 37) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 65 => ((233 # 34) - (508 # 57) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            - (87 # 142) * s V_synth_full__tmp * max0(-16 + s V_synth_full_sb)
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (3 # 113) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
            - (2 # 101) * s V_synth_full__tmp * max0(16 - s V_synth_full_sb)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (3 # 94) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            + (4 # 153) * s V_synth_full__tmp * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full__tmp^2
            + (2 # 87) * s V_synth_full__tmp1 * max0(-2 + s V_synth_full_sb)
            - (8 # 93) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full__tmp)
            - (1213 # 105) * s V_synth_full__tmp1 * max0(-1
                                                         + s V_synth_full__tmp
                                                         - s V_synth_full_ch)
            - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
            - (16 # 83) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
            - (1 # 6) * s V_synth_full__tmp1 * max0(16 - s V_synth_full_sb)
            + (467 # 105) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
            - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
            + (5 # 79) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
            - (122 # 103) * s V_synth_full_ch
            + (87 # 142) * s V_synth_full_ch * max0(-16 + s V_synth_full_sb)
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            - (4 # 13) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
            + (4 # 103) * s V_synth_full_ch * max0(16 - s V_synth_full_sb)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full_ch^2
            - (58 # 111) * s V_synth_full_s * max0(-16 + s V_synth_full_sb)
            + (1213 # 105) * s V_synth_full_s * max0(-1 + s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
            + (5 # 131) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
            + (2 # 83) * s V_synth_full_s * max0(16 - s V_synth_full_sb)
            - (205 # 47) * s V_synth_full_s * max0(s V_synth_full__tmp)
            - (977 # 112) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
            + (14 # 75) * s V_synth_full_s * max0(s V_synth_full_sb)
            - (47 # 78) * s V_synth_full_sb
            - (9 # 131) * s V_synth_full_sb * max0(-16 + s V_synth_full_sb)
            + (2 # 55) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
            - (76 # 157) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
            + (3 # 11) * s V_synth_full_sb * max0(-1 + s V_synth_full_s)
            - (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
            + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
            - (0 # 1) * s V_synth_full_sb * max0(16 - s V_synth_full_sb)
            + (59 # 104) * s V_synth_full_sb * max0(s V_synth_full__tmp)
            - (54 # 151) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            - (31 # 104) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
            + (121 # 147) * s V_synth_full_sb * max0(s V_synth_full__tmp1
                                                     - s V_synth_full_s)
            + (2 # 37) * s V_synth_full_sb * max0(s V_synth_full_sb)
            + (1 # 55) * s V_synth_full_sb^2 + s V_synth_full_z
            - (7 # 116) * s V_synth_full_z * max0(-16 + s V_synth_full_sb)
            + (111 # 97) * max0(-16 + s V_synth_full_sb)
            + (87 # 142) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (58 # 111) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                                + s V_synth_full__tmp1)
            + (0 # 1) * max0(-16 + s V_synth_full_sb) * max0(16
                                                             - s V_synth_full_sb)
            - (65 # 122) * max0(-16 + s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                                - s V_synth_full_s)
            + (1 # 85) * max0(-16 + s V_synth_full_sb) * max0(s V_synth_full_sb)
            + (7 # 116) * max0(-16 + s V_synth_full_sb) * max0(s V_synth_full_z)
            + (4 # 103) * max0(-16 + s V_synth_full_sb)^2
            - (1 # 64) * max0(-2 + s V_synth_full_sb) * max0(-1
                                                             + s V_synth_full__tmp1)
            + (1 # 103) * max0(-2 + s V_synth_full_sb) * max0(-1
                                                              + s V_synth_full_sb)
            - (0 # 1) * max0(-2 + s V_synth_full_sb) * max0(15
                                                            - s V_synth_full_sb)
            - (7 # 12) * max0(-1 + s V_synth_full__tmp)
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (8 # 93) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                               + s V_synth_full__tmp1)
            + (3 # 113) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                                - s V_synth_full_sb)
            + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(16
                                                               - s V_synth_full_sb)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            - (259 # 150) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (254 # 63) * max0(-1 + s V_synth_full__tmp1)
            - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                + s V_synth_full_sb)
            - (9 # 83) * max0(-1 + s V_synth_full__tmp1) * max0(16
                                                                - s V_synth_full_sb)
            - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
            + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                  - s V_synth_full_ch)
            - (5 # 131) * max0(-1 + s V_synth_full_s) * max0(15
                                                             - s V_synth_full_sb)
            + (205 # 47) * max0(-1 + s V_synth_full_s) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
            + (1 # 52) * max0(-1 + s V_synth_full_sb)
            + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                             - s V_synth_full_ch)
            + (0 # 1) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (0 # 1) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
            - (1 # 74) * max0(15 - s V_synth_full_sb)
            + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            - (2 # 83) * max0(16 - s V_synth_full_sb)
            + (26 # 83) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
            + (0 # 1) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (135 # 31) * max0(s V_synth_full__tmp)
            - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (977 # 112) * max0(s V_synth_full__tmp - s V_synth_full_ch)
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (193 # 69) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
            + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            + (0 # 1) * max0(s V_synth_full__tmp1)
            - (5 # 79) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
            + (288 # 55) * max0(s V_synth_full_ch)
            + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
            - (2 # 37) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 66 => ((233 # 34) - (508 # 57) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            - (87 # 142) * s V_synth_full__tmp * max0(-16 + s V_synth_full_sb)
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (3 # 113) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
            - (2 # 101) * s V_synth_full__tmp * max0(16 - s V_synth_full_sb)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (3 # 94) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            + (4 # 153) * s V_synth_full__tmp * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full__tmp^2
            + (2 # 87) * s V_synth_full__tmp1 * max0(-2 + s V_synth_full_sb)
            - (8 # 93) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full__tmp)
            - (1213 # 105) * s V_synth_full__tmp1 * max0(-1
                                                         + s V_synth_full__tmp
                                                         - s V_synth_full_ch)
            - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
            - (16 # 83) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
            - (1 # 6) * s V_synth_full__tmp1 * max0(16 - s V_synth_full_sb)
            + (467 # 105) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
            - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
            + (5 # 79) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
            - (122 # 103) * s V_synth_full_ch
            + (87 # 142) * s V_synth_full_ch * max0(-16 + s V_synth_full_sb)
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            - (4 # 13) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
            + (4 # 103) * s V_synth_full_ch * max0(16 - s V_synth_full_sb)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full_ch^2
            - (58 # 111) * s V_synth_full_s * max0(-16 + s V_synth_full_sb)
            + (1213 # 105) * s V_synth_full_s * max0(-1 + s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
            + (5 # 131) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
            + (2 # 83) * s V_synth_full_s * max0(16 - s V_synth_full_sb)
            - (205 # 47) * s V_synth_full_s * max0(s V_synth_full__tmp)
            - (977 # 112) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
            + (14 # 75) * s V_synth_full_s * max0(s V_synth_full_sb)
            - (47 # 78) * s V_synth_full_sb
            - (9 # 131) * s V_synth_full_sb * max0(-16 + s V_synth_full_sb)
            + (2 # 55) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
            - (76 # 157) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
            + (3 # 11) * s V_synth_full_sb * max0(-1 + s V_synth_full_s)
            - (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
            + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
            - (0 # 1) * s V_synth_full_sb * max0(16 - s V_synth_full_sb)
            + (59 # 104) * s V_synth_full_sb * max0(s V_synth_full__tmp)
            - (54 # 151) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            - (31 # 104) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
            + (121 # 147) * s V_synth_full_sb * max0(s V_synth_full__tmp1
                                                     - s V_synth_full_s)
            + (2 # 37) * s V_synth_full_sb * max0(s V_synth_full_sb)
            + (1 # 55) * s V_synth_full_sb^2 + s V_synth_full_z
            - (7 # 116) * s V_synth_full_z * max0(-16 + s V_synth_full_sb)
            + (111 # 97) * max0(-16 + s V_synth_full_sb)
            + (87 # 142) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (58 # 111) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                                + s V_synth_full__tmp1)
            + (0 # 1) * max0(-16 + s V_synth_full_sb) * max0(16
                                                             - s V_synth_full_sb)
            - (65 # 122) * max0(-16 + s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                                - s V_synth_full_s)
            + (1 # 85) * max0(-16 + s V_synth_full_sb) * max0(s V_synth_full_sb)
            + (7 # 116) * max0(-16 + s V_synth_full_sb) * max0(s V_synth_full_z)
            + (4 # 103) * max0(-16 + s V_synth_full_sb)^2
            - (1 # 64) * max0(-2 + s V_synth_full_sb) * max0(-1
                                                             + s V_synth_full__tmp1)
            + (1 # 103) * max0(-2 + s V_synth_full_sb) * max0(-1
                                                              + s V_synth_full_sb)
            - (0 # 1) * max0(-2 + s V_synth_full_sb) * max0(15
                                                            - s V_synth_full_sb)
            - (7 # 12) * max0(-1 + s V_synth_full__tmp)
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (8 # 93) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                               + s V_synth_full__tmp1)
            + (3 # 113) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                                - s V_synth_full_sb)
            + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(16
                                                               - s V_synth_full_sb)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            - (259 # 150) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (254 # 63) * max0(-1 + s V_synth_full__tmp1)
            - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                + s V_synth_full_sb)
            - (9 # 83) * max0(-1 + s V_synth_full__tmp1) * max0(16
                                                                - s V_synth_full_sb)
            - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
            + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                  - s V_synth_full_ch)
            - (5 # 131) * max0(-1 + s V_synth_full_s) * max0(15
                                                             - s V_synth_full_sb)
            + (205 # 47) * max0(-1 + s V_synth_full_s) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
            + (1 # 52) * max0(-1 + s V_synth_full_sb)
            + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                             - s V_synth_full_ch)
            + (0 # 1) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (0 # 1) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
            - (1 # 74) * max0(15 - s V_synth_full_sb)
            + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            - (2 # 83) * max0(16 - s V_synth_full_sb)
            + (26 # 83) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
            + (0 # 1) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (135 # 31) * max0(s V_synth_full__tmp)
            - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (977 # 112) * max0(s V_synth_full__tmp - s V_synth_full_ch)
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (193 # 69) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
            + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            + (0 # 1) * max0(s V_synth_full__tmp1)
            - (5 # 79) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
            + (288 # 55) * max0(s V_synth_full_ch)
            + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
            - (2 # 37) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 67 => ((233 # 34) - (508 # 57) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            - (87 # 142) * s V_synth_full__tmp * max0(-16 + s V_synth_full_sb)
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (3 # 113) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
            - (2 # 101) * s V_synth_full__tmp * max0(16 - s V_synth_full_sb)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (3 # 94) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            + (4 # 153) * s V_synth_full__tmp * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full__tmp^2
            + (2 # 87) * s V_synth_full__tmp1 * max0(-2 + s V_synth_full_sb)
            - (8 # 93) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full__tmp)
            - (1213 # 105) * s V_synth_full__tmp1 * max0(-1
                                                         + s V_synth_full__tmp
                                                         - s V_synth_full_ch)
            - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
            - (16 # 83) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
            - (1 # 6) * s V_synth_full__tmp1 * max0(16 - s V_synth_full_sb)
            + (467 # 105) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
            - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
            + (5 # 79) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
            - (122 # 103) * s V_synth_full_ch
            + (87 # 142) * s V_synth_full_ch * max0(-16 + s V_synth_full_sb)
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            - (4 # 13) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
            + (4 # 103) * s V_synth_full_ch * max0(16 - s V_synth_full_sb)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full_ch^2
            - (58 # 111) * s V_synth_full_s * max0(-16 + s V_synth_full_sb)
            + (1213 # 105) * s V_synth_full_s * max0(-1 + s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
            + (5 # 131) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
            + (2 # 83) * s V_synth_full_s * max0(16 - s V_synth_full_sb)
            - (205 # 47) * s V_synth_full_s * max0(s V_synth_full__tmp)
            - (977 # 112) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
            + (14 # 75) * s V_synth_full_s * max0(s V_synth_full_sb)
            - (47 # 78) * s V_synth_full_sb
            - (9 # 131) * s V_synth_full_sb * max0(-16 + s V_synth_full_sb)
            + (2 # 55) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
            - (76 # 157) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
            + (3 # 11) * s V_synth_full_sb * max0(-1 + s V_synth_full_s)
            - (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
            + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
            - (0 # 1) * s V_synth_full_sb * max0(16 - s V_synth_full_sb)
            + (59 # 104) * s V_synth_full_sb * max0(s V_synth_full__tmp)
            - (54 # 151) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            - (31 # 104) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
            + (121 # 147) * s V_synth_full_sb * max0(s V_synth_full__tmp1
                                                     - s V_synth_full_s)
            + (2 # 37) * s V_synth_full_sb * max0(s V_synth_full_sb)
            + (1 # 55) * s V_synth_full_sb^2 + s V_synth_full_z
            - (7 # 116) * s V_synth_full_z * max0(-16 + s V_synth_full_sb)
            + (111 # 97) * max0(-16 + s V_synth_full_sb)
            + (87 # 142) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (58 # 111) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                                + s V_synth_full__tmp1)
            + (0 # 1) * max0(-16 + s V_synth_full_sb) * max0(16
                                                             - s V_synth_full_sb)
            - (65 # 122) * max0(-16 + s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                                - s V_synth_full_s)
            + (1 # 85) * max0(-16 + s V_synth_full_sb) * max0(s V_synth_full_sb)
            + (7 # 116) * max0(-16 + s V_synth_full_sb) * max0(s V_synth_full_z)
            + (4 # 103) * max0(-16 + s V_synth_full_sb)^2
            - (1 # 64) * max0(-2 + s V_synth_full_sb) * max0(-1
                                                             + s V_synth_full__tmp1)
            + (1 # 103) * max0(-2 + s V_synth_full_sb) * max0(-1
                                                              + s V_synth_full_sb)
            - (0 # 1) * max0(-2 + s V_synth_full_sb) * max0(15
                                                            - s V_synth_full_sb)
            - (7 # 12) * max0(-1 + s V_synth_full__tmp)
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (8 # 93) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                               + s V_synth_full__tmp1)
            + (3 # 113) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                                - s V_synth_full_sb)
            + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(16
                                                               - s V_synth_full_sb)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            - (259 # 150) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (254 # 63) * max0(-1 + s V_synth_full__tmp1)
            - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                + s V_synth_full_sb)
            - (9 # 83) * max0(-1 + s V_synth_full__tmp1) * max0(16
                                                                - s V_synth_full_sb)
            - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
            + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                  - s V_synth_full_ch)
            - (5 # 131) * max0(-1 + s V_synth_full_s) * max0(15
                                                             - s V_synth_full_sb)
            + (205 # 47) * max0(-1 + s V_synth_full_s) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
            + (1 # 52) * max0(-1 + s V_synth_full_sb)
            + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                             - s V_synth_full_ch)
            + (0 # 1) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (0 # 1) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
            - (1 # 74) * max0(15 - s V_synth_full_sb)
            + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            - (2 # 83) * max0(16 - s V_synth_full_sb)
            + (26 # 83) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
            + (0 # 1) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (135 # 31) * max0(s V_synth_full__tmp)
            - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (977 # 112) * max0(s V_synth_full__tmp - s V_synth_full_ch)
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (193 # 69) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
            + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            + (0 # 1) * max0(s V_synth_full__tmp1)
            - (5 # 79) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
            + (288 # 55) * max0(s V_synth_full_ch)
            + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
            - (2 # 37) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 68 => hints
     [(*-0.0364562 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-16
                                                                    + s V_synth_full_sb) (0))) (F_max0_ge_0 (-16
                                                                    + s V_synth_full_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp)) (F_check_ge (0) (0)));
      (*-0.272602 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-16
                                                                    + s V_synth_full_sb) (0))) (F_max0_ge_0 (-16
                                                                    + s V_synth_full_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full_s)) (F_check_ge (0) (0)));
      (*-0.612772 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-16
                                                                    + s V_synth_full_sb) (0))) (F_max0_ge_0 (-16
                                                                    + s V_synth_full_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp)) (F_check_ge (0) (0)));
      (*-0.823181 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-16
                                                                    + s V_synth_full_sb) (0))) (F_max0_ge_0 (-16
                                                                    + s V_synth_full_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp1
                                                                    - s V_synth_full_s)) (F_check_ge (0) (0)));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-16
                                                                    + s V_synth_full_sb) (0))) (F_max0_ge_0 (-16
                                                                    + s V_synth_full_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_s)) (F_check_ge (0) (0)));
      (*-0.060432 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-16
                                                                    + s V_synth_full_sb) (0))) (F_max0_ge_0 (-16
                                                                    + s V_synth_full_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_z)) (F_check_ge (0) (0)));
      (*-0.0571184 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-16
                                                                    + s V_synth_full_sb)) (F_check_ge (-16
                                                                    + s V_synth_full_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-16
                                                                    + s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.522602 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-16
                                                                    + 
                                                                    s V_synth_full_sb)) (F_check_ge (-16
                                                                    + s V_synth_full_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp1)) (F_check_ge (0) (0)));
      (*-0.00417526 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-16
                                                                    + s V_synth_full_sb)) (F_check_ge (-16
                                                                    + s V_synth_full_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (16
                                                                    - s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.612772 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-16
                                                                    + 
                                                                    s V_synth_full_sb)) (F_check_ge (-16
                                                                    + s V_synth_full_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_ch)) (F_check_ge (0) (0)));
      (*-0.0156994 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                    + s V_synth_full_sb) (0))) (F_max0_ge_0 (-2
                                                                    + s V_synth_full_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp1)) (F_check_ge (0) (0)));
      (*-0.000884507 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                    + s V_synth_full_sb) (0))) (F_max0_ge_0 (-2
                                                                    + s V_synth_full_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (15
                                                                    - s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.00962983 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-2
                                                                    + s V_synth_full_sb)) (F_check_ge (-2
                                                                    + s V_synth_full_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.0228975 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-2
                                                                    + s V_synth_full_sb)) (F_check_ge (-2
                                                                    + s V_synth_full_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp1)) (F_check_ge (0) (0)));
      (*-0.0859738 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + s V_synth_full__tmp)) (F_check_ge (-1
                                                                    + s V_synth_full__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp1)) (F_check_ge (0) (0)));
      (*-0.0264765 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + s V_synth_full__tmp)) (F_check_ge (-1
                                                                    + s V_synth_full__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (15
                                                                    - s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.0174763 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + s V_synth_full__tmp)) (F_check_ge (-1
                                                                    + s V_synth_full__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (16
                                                                    - s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-4.36164 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    s V_synth_full__tmp)) (F_check_ge (-1
                                                                    + s V_synth_full__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp1
                                                                    - s V_synth_full_s)) (F_check_ge (0) (0)));
      (*-4.36164 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    s V_synth_full__tmp)) (F_check_ge (-1
                                                                    + s V_synth_full__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_s)) (F_check_ge (0) (0)));
      (*-0.0364562 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + 
                                                                    s V_synth_full__tmp)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-16
                                                                    + s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-4.36164 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_synth_full__tmp
                                                                    - s V_synth_full_ch) (0))) (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp
                                                                    - s V_synth_full_ch))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp1)) (F_check_ge (0) (0)));
      (*-4.36164 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    s V_synth_full__tmp
                                                                    - 
                                                                    s V_synth_full_ch)) (F_check_ge (-1
                                                                    + s V_synth_full__tmp
                                                                    - s V_synth_full_ch) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full_s)) (F_check_ge (0) (0)));
      (*-4.36164 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    s V_synth_full__tmp
                                                                    - 
                                                                    s V_synth_full_ch)) (F_check_ge (-1
                                                                    + s V_synth_full__tmp
                                                                    - s V_synth_full_ch) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp1
                                                                    - s V_synth_full_s)) (F_check_ge (0) (0)));
      (*-0.612772 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                   + 
                                                                   s V_synth_full__tmp
                                                                   - 
                                                                   s V_synth_full_ch)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-16
                                                                    + s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.10839 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_synth_full__tmp1) (0))) (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp1))) (F_binom_monotonic 1 (F_max0_ge_0 (16
                                                                    - s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-4.36164 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    s V_synth_full__tmp1)) (F_check_ge (-1
                                                                    + s V_synth_full__tmp1) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp
                                                                    - s V_synth_full_ch)) (F_check_ge (0) (0)));
      (*0 4.36164*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_synth_full_s) (0))) (F_max0_ge_0 (-1
                                                                    + s V_synth_full_s))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp
                                                                    - s V_synth_full_ch)) (F_check_ge (0) (0)));
      (*0 0.038223*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_synth_full_s) (0))) (F_max0_ge_0 (-1
                                                                    + s V_synth_full_s))) (F_binom_monotonic 1 (F_max0_ge_0 (15
                                                                    - s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*0 4.36164*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_synth_full_s) (0))) (F_max0_ge_0 (-1
                                                                    + s V_synth_full_s))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_ch)) (F_check_ge (0) (0)));
      (*-0.272602 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    s V_synth_full_s)) (F_check_ge (-1
                                                                    + s V_synth_full_s) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-16
                                                                    + s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*0 4.36164*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                   + 
                                                                   s V_synth_full_s)) (F_check_ge (-1
                                                                    + s V_synth_full_s) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp)) (F_check_ge (0) (0)));
      (*0 4.36164*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                   + 
                                                                   s V_synth_full_s)) (F_check_ge (-1
                                                                    + s V_synth_full_s) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp
                                                                    - s V_synth_full_ch)) (F_check_ge (0) (0)));
      (*-0.0228974 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_synth_full_sb) (0))) (F_max0_ge_0 (-1
                                                                    + s V_synth_full_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp1)) (F_check_ge (0) (0)));
      (*-0.306386 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_synth_full_sb) (0))) (F_max0_ge_0 (-1
                                                                    + s V_synth_full_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_ch)) (F_check_ge (0) (0)));
      (*-0.0192597 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + s V_synth_full_sb)) (F_check_ge (-1
                                                                    + s V_synth_full_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp
                                                                    - s V_synth_full_ch)) (F_check_ge (0) (0)));
      (*-0.0241093 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + s V_synth_full_sb)) (F_check_ge (-1
                                                                    + s V_synth_full_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_s)) (F_check_ge (0) (0)));
      (*-0.060432 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                   + 
                                                                   s V_synth_full_z)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-16
                                                                    + s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.338301 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (16
                                                                    - s V_synth_full_sb) (0))) (F_max0_ge_0 (16
                                                                    - s V_synth_full_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp
                                                                    - s V_synth_full_ch)) (F_check_ge (0) (0)));
      (*-0.275189 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (16
                                                                    - s V_synth_full_sb) (0))) (F_max0_ge_0 (16
                                                                    - s V_synth_full_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp1)) (F_check_ge (0) (0)));
      (*-0.060432 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (16
                                                                    - s V_synth_full_sb) (0))) (F_max0_ge_0 (16
                                                                    - s V_synth_full_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_z)) (F_check_ge (0) (0)));
      (*-0.0241093 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (16
                                                                    - s V_synth_full_sb)) (F_check_ge (16
                                                                    - s V_synth_full_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_s)) (F_check_ge (0) (0)));
      (*-0.060432 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (16
                                                                   - 
                                                                   s V_synth_full_sb)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_z)) (F_check_ge (0) (0)));
      (*-0.0859738 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full__tmp) (0))) (F_max0_ge_0 (s V_synth_full__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp1)) (F_check_ge (0) (0)));
      (*-4.36164 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full__tmp) (0))) (F_max0_ge_0 (s V_synth_full__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full_s)) (F_check_ge (0) (0)));
      (*-0.0454564 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full__tmp) (0))) (F_max0_ge_0 (s V_synth_full__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.612772 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full__tmp)) (F_check_ge (s V_synth_full__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-16
                                                                    + s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-4.36164 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full__tmp
                                                                    - s V_synth_full_ch) (0))) (F_max0_ge_0 (s V_synth_full__tmp
                                                                    - s V_synth_full_ch))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_s)) (F_check_ge (0) (0)));
      (*-4.36164 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full__tmp
                                                                    - 
                                                                    s V_synth_full_ch)) (F_check_ge (s V_synth_full__tmp
                                                                    - s V_synth_full_ch) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp1)) (F_check_ge (0) (0)));
      (*-0.00240196 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full__tmp
                                                                    - s V_synth_full_ch)) (F_check_ge (s V_synth_full__tmp
                                                                    - s V_synth_full_ch) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (16
                                                                    - s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-2.79716 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full__tmp
                                                                    - 
                                                                    s V_synth_full_ch)) (F_check_ge (s V_synth_full__tmp
                                                                    - s V_synth_full_ch) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp1
                                                                    - s V_synth_full_s)) (F_check_ge (0) (0)));
      (*-0.0192597 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full__tmp
                                                                    - s V_synth_full_ch)) (F_check_ge (s V_synth_full__tmp
                                                                    - s V_synth_full_ch) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.649228 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp
                                                                   - 
                                                                   s V_synth_full_ch)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (16
                                                                    - s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.0228975 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full__tmp1) (0))) (F_max0_ge_0 (s V_synth_full__tmp1))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-4.44761 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full__tmp1) (0))) (F_max0_ge_0 (s V_synth_full__tmp1))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp)) (F_check_ge (0) (0)));
      (*-4.44761 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full__tmp1)) (F_check_ge (s V_synth_full__tmp1) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp)) (F_check_ge (0) (0)));
      (*-0.00121181 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full__tmp1)) (F_check_ge (s V_synth_full__tmp1) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.192753 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full__tmp1)) (F_check_ge (s V_synth_full__tmp1) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (15
                                                                    - s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.27521 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full__tmp1)) (F_check_ge (s V_synth_full__tmp1) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (16
                                                                    - s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-4.36164 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full__tmp1
                                                                    - s V_synth_full_s) (0))) (F_max0_ge_0 (s V_synth_full__tmp1
                                                                    - s V_synth_full_s))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp)) (F_check_ge (0) (0)));
      (*0 0.0632962*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full__tmp1
                                                                    - s V_synth_full_s) (0))) (F_max0_ge_0 (s V_synth_full__tmp1
                                                                    - s V_synth_full_s))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-7.19075 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full__tmp1
                                                                    - 
                                                                    s V_synth_full_s)) (F_check_ge (s V_synth_full__tmp1
                                                                    - s V_synth_full_s) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp
                                                                    - s V_synth_full_ch)) (F_check_ge (0) (0)));
      (*-4.39359 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full__tmp1
                                                                    - 
                                                                    s V_synth_full_s)) (F_check_ge (s V_synth_full__tmp1
                                                                    - s V_synth_full_s) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_ch)) (F_check_ge (0) (0)));
      (*-0.29034 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp1
                                                                  - s V_synth_full_s)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-16
                                                                    + s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.612772 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full_ch) (0))) (F_max0_ge_0 (s V_synth_full_ch))) (F_binom_monotonic 1 (F_max0_ge_0 (-16
                                                                    + s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.0364562 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full_ch) (0))) (F_max0_ge_0 (s V_synth_full_ch))) (F_binom_monotonic 1 (F_max0_ge_0 (16
                                                                    - s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-4.39359 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full_ch) (0))) (F_max0_ge_0 (s V_synth_full_ch))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp1
                                                                    - s V_synth_full_s)) (F_check_ge (0) (0)));
      (*-4.36164 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full_ch) (0))) (F_max0_ge_0 (s V_synth_full_ch))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_s)) (F_check_ge (0) (0)));
      (*-4.36164 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full_ch)) (F_check_ge (s V_synth_full_ch) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full_s)) (F_check_ge (0) (0)));
      (*-0.307451 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full_ch)) (F_check_ge (s V_synth_full_ch) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.307451 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full_ch)) (F_check_ge (s V_synth_full_ch) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.0364562 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_ch)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (16
                                                                    - s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-4.36164 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full_s) (0))) (F_max0_ge_0 (s V_synth_full_s))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp)) (F_check_ge (0) (0)));
      (*-0.0241093 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full_s) (0))) (F_max0_ge_0 (s V_synth_full_s))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*0 0.0241093*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full_s) (0))) (F_max0_ge_0 (s V_synth_full_s))) (F_binom_monotonic 1 (F_max0_ge_0 (16
                                                                    - s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full_s) (0))) (F_max0_ge_0 (s V_synth_full_s))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full_s)) (F_check_ge (s V_synth_full_s) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-16
                                                                    + s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-4.36164 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full_s)) (F_check_ge (s V_synth_full_s) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_ch)) (F_check_ge (0) (0)));
      (*0 0.306386*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full_sb) (0))) (F_max0_ge_0 (s V_synth_full_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_ch)) (F_check_ge (0) (0)));
      (*-0.0541303 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full_sb) (0))) (F_max0_ge_0 (s V_synth_full_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.011669 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full_sb)) (F_check_ge (s V_synth_full_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-16
                                                                    + s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*0 0.0454564*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full_sb)) (F_check_ge (s V_synth_full_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp)) (F_check_ge (0) (0)));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full_sb)) (F_check_ge (s V_synth_full_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_s)) (F_check_ge (0) (0)));
      (*-0.060432 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full_z)) (F_check_ge (s V_synth_full_z) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-16
                                                                    + s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-10.0968 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full__tmp)) (F_check_ge (s V_synth_full__tmp) (0));
      (*-0.0182607 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (-16
                                                                    + s V_synth_full_sb) (0))) (F_max0_ge_0 (-16
                                                                    + s V_synth_full_sb))]
     ((199 # 34) - (508 # 57) * s V_synth_full__tmp
      + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
      - (87 # 142) * s V_synth_full__tmp * max0(-16 + s V_synth_full_sb)
      + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
      + (371 # 142) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp
                                                 - s V_synth_full_ch)
      - (3 # 113) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
      - (2 # 101) * s V_synth_full__tmp * max0(16 - s V_synth_full_sb)
      - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
      + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                               - s V_synth_full_ch)
      - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
      + (3 # 94) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                              - s V_synth_full_s)
      + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
      + (4 # 153) * s V_synth_full__tmp * max0(s V_synth_full_sb)
      - (21 # 64) * s V_synth_full__tmp^2
      + (2 # 87) * s V_synth_full__tmp1 * max0(-2 + s V_synth_full_sb)
      - (8 # 93) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full__tmp)
      - (1213 # 105) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
      - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
      - (16 # 83) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
      - (1 # 6) * s V_synth_full__tmp1 * max0(16 - s V_synth_full_sb)
      + (467 # 105) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
      - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
      + (5 # 79) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
      - (122 # 103) * s V_synth_full_ch
      + (87 # 142) * s V_synth_full_ch * max0(-16 + s V_synth_full_sb)
      - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
      - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                             - s V_synth_full_ch)
      - (4 # 13) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
      + (4 # 103) * s V_synth_full_ch * max0(16 - s V_synth_full_sb)
      + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
      - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                             - s V_synth_full_ch)
      + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
      - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
      - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
      - (21 # 64) * s V_synth_full_ch^2
      - (58 # 111) * s V_synth_full_s * max0(-16 + s V_synth_full_sb)
      + (1213 # 105) * s V_synth_full_s * max0(-1 + s V_synth_full__tmp
                                               - s V_synth_full_ch)
      + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
      + (5 # 131) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
      + (2 # 83) * s V_synth_full_s * max0(16 - s V_synth_full_sb)
      - (205 # 47) * s V_synth_full_s * max0(s V_synth_full__tmp)
      - (977 # 112) * s V_synth_full_s * max0(s V_synth_full__tmp
                                              - s V_synth_full_ch)
      + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
      + (14 # 75) * s V_synth_full_s * max0(s V_synth_full_sb)
      - (47 # 78) * s V_synth_full_sb
      - (9 # 131) * s V_synth_full_sb * max0(-16 + s V_synth_full_sb)
      + (2 # 55) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
      - (76 # 157) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
      + (3 # 11) * s V_synth_full_sb * max0(-1 + s V_synth_full_s)
      - (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
      + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
      - (0 # 1) * s V_synth_full_sb * max0(16 - s V_synth_full_sb)
      + (59 # 104) * s V_synth_full_sb * max0(s V_synth_full__tmp)
      - (54 # 151) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                              - s V_synth_full_ch)
      - (31 # 104) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
      + (121 # 147) * s V_synth_full_sb * max0(s V_synth_full__tmp1
                                               - s V_synth_full_s)
      + (2 # 37) * s V_synth_full_sb * max0(s V_synth_full_sb)
      + (1 # 55) * s V_synth_full_sb^2 + s V_synth_full_z
      - (7 # 116) * s V_synth_full_z * max0(-16 + s V_synth_full_sb)
      + (100 # 83) * max0(-16 + s V_synth_full_sb)
      + (87 # 142) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                          + s V_synth_full__tmp
                                                          - s V_synth_full_ch)
      + (58 # 111) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                          + s V_synth_full__tmp1)
      + (7 # 116) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                         + s V_synth_full_z)
      + (0 # 1) * max0(-16 + s V_synth_full_sb) * max0(16 - s V_synth_full_sb)
      - (65 # 122) * max0(-16 + s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                          - s V_synth_full_s)
      + (1 # 85) * max0(-16 + s V_synth_full_sb) * max0(s V_synth_full_sb)
      + (4 # 103) * max0(-16 + s V_synth_full_sb)^2
      - (1 # 64) * max0(-2 + s V_synth_full_sb) * max0(-1
                                                       + s V_synth_full__tmp1)
      + (1 # 103) * max0(-2 + s V_synth_full_sb) * max0(-1
                                                        + s V_synth_full_sb)
      - (0 # 1) * max0(-2 + s V_synth_full_sb) * max0(15 - s V_synth_full_sb)
      - (7 # 12) * max0(-1 + s V_synth_full__tmp)
      - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                          + s V_synth_full__tmp
                                                          - s V_synth_full_ch)
      + (8 # 93) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                         + s V_synth_full__tmp1)
      + (3 # 113) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                          - s V_synth_full_sb)
      + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(16
                                                         - s V_synth_full_sb)
      + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
      + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
      + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
      - (259 # 150) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
      - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
      + (1213 # 105) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
      - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
      + (254 # 63) * max0(-1 + s V_synth_full__tmp1)
      - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                          + s V_synth_full_sb)
      - (9 # 83) * max0(-1 + s V_synth_full__tmp1) * max0(16
                                                          - s V_synth_full_sb)
      - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
      + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                            - s V_synth_full_ch)
      - (5 # 131) * max0(-1 + s V_synth_full_s) * max0(15 - s V_synth_full_sb)
      + (205 # 47) * max0(-1 + s V_synth_full_s) * max0(s V_synth_full__tmp
                                                        - s V_synth_full_ch)
      + (1 # 52) * max0(-1 + s V_synth_full_sb)
      + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
      + (0 # 1) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp1)
      + (0 # 1) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
      - (1 # 74) * max0(15 - s V_synth_full_sb)
      + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
      - (2 # 83) * max0(16 - s V_synth_full_sb)
      + (26 # 83) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                        - s V_synth_full_ch)
      + (0 # 1) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
      + (135 # 31) * max0(s V_synth_full__tmp)
      - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
      - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
      + (36 # 97) * max0(s V_synth_full__tmp)^2
      + (977 # 112) * max0(s V_synth_full__tmp - s V_synth_full_ch)
      + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
      + (193 # 69) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
      + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
      + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
      + (0 # 1) * max0(s V_synth_full__tmp1)
      - (5 # 79) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
      + (288 # 55) * max0(s V_synth_full_ch)
      + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
      - (2 # 37) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 69 => hints
     [(*0 0.00100404*) F_binom_monotonic 2 (F_max0_ge_arg (16
                                                           - s V_synth_full_sb)) (F_check_ge (16
                                                                    - s V_synth_full_sb) (0));
      (*0 0.0985466*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_synth_full__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp1
                                                                    - s V_synth_full_s)) (F_check_ge (0) (0)));
      (*0 0.0166148*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_synth_full__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.0174763 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + s V_synth_full__tmp)) (F_check_ge (-1
                                                                    + s V_synth_full__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (16
                                                                    - s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.612772 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    s V_synth_full__tmp
                                                                    - 
                                                                    s V_synth_full_ch)) (F_check_ge (-1
                                                                    + s V_synth_full__tmp
                                                                    - s V_synth_full_ch) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-16
                                                                    + s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*0 0.000426754*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + s V_synth_full__tmp
                                                                    - s V_synth_full_ch)) (F_check_ge (-1
                                                                    + s V_synth_full__tmp
                                                                    - s V_synth_full_ch) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (16
                                                                    - s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.0228975 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_synth_full__tmp1) (0))) (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp1))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*0.0457949 0.154185*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_synth_full__tmp1) (0))) (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp1))) (F_binom_monotonic 1 (F_max0_ge_0 (16
                                                                    - s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.522602 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    s V_synth_full__tmp1)) (F_check_ge (-1
                                                                    + s V_synth_full__tmp1) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-16
                                                                    + s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.522602 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_synth_full__tmp1
                                                                    - s V_synth_full_s) (0))) (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp1
                                                                    - s V_synth_full_s))) (F_binom_monotonic 1 (F_max0_ge_0 (-16
                                                                    + s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.0457949 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_synth_full__tmp1
                                                                    - s V_synth_full_s) (0))) (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp1
                                                                    - s V_synth_full_s))) (F_binom_monotonic 1 (F_max0_ge_0 (16
                                                                    - s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*0 0.0632962*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_synth_full__tmp1
                                                                    - s V_synth_full_s) (0))) (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp1
                                                                    - s V_synth_full_s))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*0 0.006608*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    s V_synth_full__tmp1
                                                                    - 
                                                                    s V_synth_full_s)) (F_check_ge (-1
                                                                    + s V_synth_full__tmp1
                                                                    - s V_synth_full_s) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (15
                                                                    - s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.306386 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_synth_full_sb) (0))) (F_max0_ge_0 (-1
                                                                    + s V_synth_full_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_ch)) (F_check_ge (0) (0)));
      (*-0.00962983 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + s V_synth_full_sb)) (F_check_ge (-1
                                                                    + s V_synth_full_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*0 0.0192597*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    s V_synth_full_sb)) (F_check_ge (-1
                                                                    + s V_synth_full_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.000884507 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (15
                                                                    - s V_synth_full_sb) (0))) (F_max0_ge_0 (15
                                                                    - s V_synth_full_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.00900018 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (15
                                                                    - s V_synth_full_sb)) (F_check_ge (15
                                                                    - s V_synth_full_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp)) (F_check_ge (0) (0)));
      (*-0.00298805 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (16
                                                                    - s V_synth_full_sb) (0))) (F_max0_ge_0 (16
                                                                    - s V_synth_full_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (-16
                                                                    + s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.0337776 -0.0327735*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (16
                                                                    - s V_synth_full_sb) (0))) (F_max0_ge_0 (16
                                                                    - s V_synth_full_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (16
                                                                    - s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*0 0.296875*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (16
                                                                    - s V_synth_full_sb) (0))) (F_max0_ge_0 (16
                                                                    - s V_synth_full_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp1)) (F_check_ge (0) (0)));
      (*-0.0391869 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (16
                                                                    - s V_synth_full_sb)) (F_check_ge (16
                                                                    - s V_synth_full_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp1
                                                                    - s V_synth_full_s)) (F_check_ge (0) (0)));
      (*0 0.0364562*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (16
                                                                    - 
                                                                    s V_synth_full_sb)) (F_check_ge (16
                                                                    - s V_synth_full_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp)) (F_check_ge (0) (0)));
      (*-0.274898 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (16
                                                                    - 
                                                                    s V_synth_full_sb)) (F_check_ge (16
                                                                    - s V_synth_full_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp
                                                                    - s V_synth_full_ch)) (F_check_ge (0) (0)));
      (*-0.0457949 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (16
                                                                    - s V_synth_full_sb)) (F_check_ge (16
                                                                    - s V_synth_full_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_s)) (F_check_ge (0) (0)));
      (*-0.00399209 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (16
                                                                    - s V_synth_full_sb)) (F_check_ge (16
                                                                    - s V_synth_full_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*0.540441 0.576897*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full__tmp) (0))) (F_max0_ge_0 (s V_synth_full__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (16
                                                                    - s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*0 0.0985466*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full__tmp) (0))) (F_max0_ge_0 (s V_synth_full__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp1
                                                                    - s V_synth_full_s)) (F_check_ge (0) (0)));
      (*-0.00702498 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full__tmp
                                                                    - s V_synth_full_ch)) (F_check_ge (s V_synth_full__tmp
                                                                    - s V_synth_full_ch) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (15
                                                                    - s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*0 0.0384314*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full__tmp
                                                                    - 
                                                                    s V_synth_full_ch)) (F_check_ge (s V_synth_full__tmp
                                                                    - s V_synth_full_ch) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (16
                                                                    - s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.296896 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full__tmp1)) (F_check_ge (s V_synth_full__tmp1) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (16
                                                                    - s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*0 0.0175013*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full__tmp1
                                                                    - 
                                                                    s V_synth_full_s)) (F_check_ge (s V_synth_full__tmp1
                                                                    - s V_synth_full_s) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (16
                                                                    - s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*0 0.0985466*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full__tmp1
                                                                    - 
                                                                    s V_synth_full_s)) (F_check_ge (s V_synth_full__tmp1
                                                                    - s V_synth_full_s) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp)) (F_check_ge (0) (0)));
      (*-0.0457949 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full_s) (0))) (F_max0_ge_0 (s V_synth_full_s))) (F_binom_monotonic 1 (F_max0_ge_0 (16
                                                                    - s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.0105143 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full_sb) (0))) (F_max0_ge_0 (s V_synth_full_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.00446806 -0.000475964*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full_sb) (0))) (F_max0_ge_0 (s V_synth_full_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (16
                                                                    - s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.0657993 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full_sb)) (F_check_ge (s V_synth_full_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-16
                                                                    + s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*0 0.0166148*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full_sb)) (F_check_ge (s V_synth_full_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp)) (F_check_ge (0) (0)));
      (*-0.060432 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full_z)) (F_check_ge (s V_synth_full_z) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-16
                                                                    + s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*0 0.11453*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full_ch) (0))) (F_max0_ge_0 (s V_synth_full_ch));
      (*-0.11453 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full__tmp)) (F_check_ge (s V_synth_full__tmp) (0))]
     ((87 # 5) - (508 # 57) * s V_synth_full__tmp
      + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
      - (87 # 142) * s V_synth_full__tmp * max0(-16 + s V_synth_full_sb)
      + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
      + (371 # 142) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp
                                                 - s V_synth_full_ch)
      - (1 # 52) * s V_synth_full__tmp * max0(-1 + s V_synth_full_sb)
      - (3 # 113) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
      - (2 # 101) * s V_synth_full__tmp * max0(16 - s V_synth_full_sb)
      - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
      + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                               - s V_synth_full_ch)
      - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
      + (557 # 63) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                - s V_synth_full_s)
      + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
      + (4 # 153) * s V_synth_full__tmp * max0(s V_synth_full_sb)
      - (21 # 64) * s V_synth_full__tmp^2
      + (2 # 87) * s V_synth_full__tmp1 * max0(-2 + s V_synth_full_sb)
      - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
      - (1 # 16) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
      - (21 # 131) * s V_synth_full__tmp1 * max0(16 - s V_synth_full_sb)
      + (2 # 37) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
      - (451 # 63) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                 - s V_synth_full_ch)
      - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
      + (8 # 129) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
      - (122 # 103) * s V_synth_full_ch
      + (87 # 142) * s V_synth_full_ch * max0(-16 + s V_synth_full_sb)
      - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
      - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                             - s V_synth_full_ch)
      - (17 # 59) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
      + (4 # 103) * s V_synth_full_ch * max0(16 - s V_synth_full_sb)
      + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
      - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                             - s V_synth_full_ch)
      + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
      - (413 # 94) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                              - s V_synth_full_s)
      - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
      - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
      - (21 # 64) * s V_synth_full_ch^2
      - (58 # 111) * s V_synth_full_s * max0(-16 + s V_synth_full_sb)
      + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
      + (3 # 67) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
      + (1 # 57) * s V_synth_full_s * max0(16 - s V_synth_full_sb)
      + (3 # 94) * s V_synth_full_s * max0(s V_synth_full__tmp)
      - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                            - s V_synth_full_ch)
      + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
      + (14 # 75) * s V_synth_full_s * max0(s V_synth_full_sb)
      - (83 # 110) * s V_synth_full_sb
      - (9 # 131) * s V_synth_full_sb * max0(-16 + s V_synth_full_sb)
      + (2 # 55) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
      + (1 # 51) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp
                                            - s V_synth_full_ch)
      - (41 # 86) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
      + (100 # 123) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1
                                               - s V_synth_full_s)
      - (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
      + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
      + (0 # 1) * s V_synth_full_sb * max0(16 - s V_synth_full_sb)
      + (59 # 104) * s V_synth_full_sb * max0(s V_synth_full__tmp)
      - (54 # 151) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                              - s V_synth_full_ch)
      - (21 # 71) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
      - (21 # 82) * s V_synth_full_sb * max0(s V_synth_full__tmp1
                                             - s V_synth_full_s)
      + (3 # 11) * s V_synth_full_sb * max0(s V_synth_full_s)
      + (1 # 95) * s V_synth_full_sb * max0(s V_synth_full_sb)
      - (0 # 1) * s V_synth_full_sb^2 + s V_synth_full_z
      - (7 # 116) * s V_synth_full_z * max0(-16 + s V_synth_full_sb)
      + (72 # 109) * max0(-16 + s V_synth_full_sb)
      + (87 # 142) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                          + s V_synth_full__tmp
                                                          - s V_synth_full_ch)
      + (58 # 111) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                          + s V_synth_full__tmp1)
      - (58 # 111) * max0(-16 + s V_synth_full_sb) * max0(-1
                                                          + s V_synth_full__tmp1
                                                          - s V_synth_full_s)
      - (0 # 1) * max0(-16 + s V_synth_full_sb) * max0(16 - s V_synth_full_sb)
      + (5 # 76) * max0(-16 + s V_synth_full_sb) * max0(s V_synth_full_sb)
      + (7 # 116) * max0(-16 + s V_synth_full_sb) * max0(s V_synth_full_z)
      - (2 # 87) * max0(-2 + s V_synth_full_sb) * max0(-1
                                                       + s V_synth_full__tmp1)
      + (1 # 103) * max0(-2 + s V_synth_full_sb) * max0(-1
                                                        + s V_synth_full_sb)
      - (0 # 1) * max0(-2 + s V_synth_full_sb) * max0(15 - s V_synth_full_sb)
      - (1 # 95) * max0(-2 + s V_synth_full_sb) * max0(s V_synth_full_sb)
      - (83 # 124) * max0(-1 + s V_synth_full__tmp)
      - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                          + s V_synth_full__tmp
                                                          - s V_synth_full_ch)
      + (3 # 113) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                          - s V_synth_full_sb)
      + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(16
                                                         - s V_synth_full_sb)
      + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
      + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
      - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                            - s V_synth_full_s)
      + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
      + (49 # 5) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
      + (0 # 1) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(16
                                                                    - s V_synth_full_sb)
      - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
      + (1213 # 105) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
      - (1213 # 105) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
      - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
      + (233 # 58) * max0(-1 + s V_synth_full__tmp1)
      - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                          + s V_synth_full_sb)
      - (13 # 95) * max0(-1 + s V_synth_full__tmp1) * max0(15
                                                           - s V_synth_full_sb)
      - (9 # 83) * max0(-1 + s V_synth_full__tmp1) * max0(16
                                                          - s V_synth_full_sb)
      - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
      + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                            - s V_synth_full_ch)
      + (19 # 116) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s)
      - (1 # 150) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(16
                                                                    - s V_synth_full_sb)
      + (3 # 94) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp)
      + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
      - (5 # 79) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
      + (5 # 97) * max0(-1 + s V_synth_full_sb)
      + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
      + (0 # 1) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
      + (1 # 90) * max0(-1 + s V_synth_full_sb)^2
      - (12 # 107) * max0(15 - s V_synth_full_sb)
      + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
      + (1 # 150) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                        - s V_synth_full_s)
      - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
      - (4 # 33) * max0(16 - s V_synth_full_sb)
      + (26 # 83) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                        - s V_synth_full_ch)
      + (0 # 1) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
      + (1 # 57) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                       - s V_synth_full_s)
      - (1 # 145) * max0(s V_synth_full__tmp)
      - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
      - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
      + (36 # 97) * max0(s V_synth_full__tmp)^2
      + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
      + (451 # 63) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
      + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
      + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
      + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
      + (0 # 1) * max0(s V_synth_full__tmp1) * max0(s V_synth_full_sb)
      - (6 # 73) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
      + (413 # 94) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_ch)
      - (1 # 60) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
      + (963 # 100) * max0(s V_synth_full_ch)
      + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
      - (3 # 46) * max0(s V_synth_full_sb)
      - (1 # 139) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 70 => ((2505 # 142) - (783 # 89) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (7 # 71) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (1 # 52) * s V_synth_full__tmp * max0(-1 + s V_synth_full_sb)
            - (2 # 103) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (883 # 101) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                       - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            + (1 # 104) * s V_synth_full__tmp * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full__tmp^2
            - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
            - (6 # 107) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
            + (9 # 59) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
            - (451 # 63) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
            - (0 # 1) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
            - (126 # 97) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
            - (1 # 141) * s V_synth_full_ch * max0(15 - s V_synth_full_sb)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (413 # 94) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full_ch^2
            + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
            + (5 # 131) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
            - (1 # 15) * s V_synth_full_s * max0(s V_synth_full__tmp)
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
            + (1 # 4) * s V_synth_full_s * max0(s V_synth_full_sb)
            - (11 # 14) * s V_synth_full_sb
            + (3 # 68) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
            + (1 # 51) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            - (41 # 86) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
            + (65 # 84) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            + (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
            + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
            + (43 # 81) * s V_synth_full_sb * max0(s V_synth_full__tmp)
            - (74 # 117) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            + (0 # 1) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
            - (21 # 82) * s V_synth_full_sb * max0(s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            - (19 # 62) * s V_synth_full_sb * max0(s V_synth_full_ch)
            + (22 # 97) * s V_synth_full_sb * max0(s V_synth_full_s)
            + (1 # 152) * s V_synth_full_sb * max0(s V_synth_full_sb)
            + s V_synth_full_z - (39 # 73) * max0(-1 + s V_synth_full__tmp)
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (7 # 71) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                               + s V_synth_full__tmp1
                                                               - s V_synth_full_s)
            + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                               - s V_synth_full_sb)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (49 # 5) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (233 # 58) * max0(-1 + s V_synth_full__tmp1)
            - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                + s V_synth_full_sb)
            - (13 # 95) * max0(-1 + s V_synth_full__tmp1) * max0(15
                                                                 - s V_synth_full_sb)
            - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
            + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                  - s V_synth_full_ch)
            + (177 # 199) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s)
            - (1 # 150) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(15
                                                                    - s V_synth_full_sb)
            + (3 # 94) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp)
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1 # 31) * max0(-1 + s V_synth_full_sb)
            + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                             - s V_synth_full_ch)
            + (4 # 13) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
            - (1 # 122) * max0(-1 + s V_synth_full_sb)^2
            - (7 # 59) * max0(15 - s V_synth_full_sb)
            - (1 # 141) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
            + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (1 # 150) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
            + (73 # 158) * max0(s V_synth_full__tmp)
            - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (519 # 118) * max0(s V_synth_full__tmp - s V_synth_full_ch)
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (451 # 63) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            - (19 # 4) * max0(s V_synth_full__tmp1)
            + (0 # 1) * max0(s V_synth_full__tmp1) * max0(s V_synth_full_sb)
            - (6 # 73) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
            + (413 # 94) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_ch)
            - (1 # 60) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
            + (593 # 59) * max0(s V_synth_full_ch)
            + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (74 # 101) * max0(s V_synth_full_s)
            + (7 # 89) * max0(s V_synth_full_sb)
            - (1 # 139) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 71 => ((2505 # 142) - (783 # 89) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (7 # 71) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (1 # 52) * s V_synth_full__tmp * max0(-1 + s V_synth_full_sb)
            - (2 # 103) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (883 # 101) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                       - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            + (1 # 104) * s V_synth_full__tmp * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full__tmp^2
            - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
            - (6 # 107) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
            + (9 # 59) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
            - (451 # 63) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
            - (0 # 1) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
            - (126 # 97) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
            - (1 # 141) * s V_synth_full_ch * max0(15 - s V_synth_full_sb)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (413 # 94) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full_ch^2
            + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
            + (5 # 131) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
            - (1 # 15) * s V_synth_full_s * max0(s V_synth_full__tmp)
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
            + (1 # 4) * s V_synth_full_s * max0(s V_synth_full_sb)
            - (11 # 14) * s V_synth_full_sb
            + (3 # 68) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
            + (1 # 51) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            - (41 # 86) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
            + (65 # 84) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            + (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
            + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
            + (43 # 81) * s V_synth_full_sb * max0(s V_synth_full__tmp)
            - (74 # 117) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            + (0 # 1) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
            - (21 # 82) * s V_synth_full_sb * max0(s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            - (19 # 62) * s V_synth_full_sb * max0(s V_synth_full_ch)
            + (22 # 97) * s V_synth_full_sb * max0(s V_synth_full_s)
            + (1 # 152) * s V_synth_full_sb * max0(s V_synth_full_sb)
            + s V_synth_full_z - (39 # 73) * max0(-1 + s V_synth_full__tmp)
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (7 # 71) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                               + s V_synth_full__tmp1
                                                               - s V_synth_full_s)
            + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                               - s V_synth_full_sb)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (49 # 5) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (233 # 58) * max0(-1 + s V_synth_full__tmp1)
            - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                + s V_synth_full_sb)
            - (13 # 95) * max0(-1 + s V_synth_full__tmp1) * max0(15
                                                                 - s V_synth_full_sb)
            - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
            + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                  - s V_synth_full_ch)
            + (177 # 199) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s)
            - (1 # 150) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(15
                                                                    - s V_synth_full_sb)
            + (3 # 94) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp)
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1 # 31) * max0(-1 + s V_synth_full_sb)
            + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                             - s V_synth_full_ch)
            + (4 # 13) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
            - (1 # 122) * max0(-1 + s V_synth_full_sb)^2
            - (7 # 59) * max0(15 - s V_synth_full_sb)
            - (1 # 141) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
            + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (1 # 150) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
            + (73 # 158) * max0(s V_synth_full__tmp)
            - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (519 # 118) * max0(s V_synth_full__tmp - s V_synth_full_ch)
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (451 # 63) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            - (19 # 4) * max0(s V_synth_full__tmp1)
            + (0 # 1) * max0(s V_synth_full__tmp1) * max0(s V_synth_full_sb)
            - (6 # 73) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
            + (413 # 94) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_ch)
            - (1 # 60) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
            + (593 # 59) * max0(s V_synth_full_ch)
            + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (74 # 101) * max0(s V_synth_full_s)
            + (7 # 89) * max0(s V_synth_full_sb)
            - (1 # 139) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 72 => ((2505 # 142) - (783 # 89) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (7 # 71) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (1 # 52) * s V_synth_full__tmp * max0(-1 + s V_synth_full_sb)
            - (2 # 103) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (883 # 101) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                       - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            + (1 # 104) * s V_synth_full__tmp * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full__tmp^2
            - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
            - (6 # 107) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
            + (9 # 59) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
            - (451 # 63) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
            - (0 # 1) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
            - (126 # 97) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
            - (1 # 141) * s V_synth_full_ch * max0(15 - s V_synth_full_sb)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (413 # 94) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full_ch^2
            + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
            + (5 # 131) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
            - (1 # 15) * s V_synth_full_s * max0(s V_synth_full__tmp)
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
            + (1 # 4) * s V_synth_full_s * max0(s V_synth_full_sb)
            - (11 # 14) * s V_synth_full_sb
            + (3 # 68) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
            + (1 # 51) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            - (41 # 86) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
            + (65 # 84) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            + (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
            + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
            + (43 # 81) * s V_synth_full_sb * max0(s V_synth_full__tmp)
            - (74 # 117) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            + (0 # 1) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
            - (21 # 82) * s V_synth_full_sb * max0(s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            - (19 # 62) * s V_synth_full_sb * max0(s V_synth_full_ch)
            + (22 # 97) * s V_synth_full_sb * max0(s V_synth_full_s)
            + (1 # 152) * s V_synth_full_sb * max0(s V_synth_full_sb)
            + s V_synth_full_z - (39 # 73) * max0(-1 + s V_synth_full__tmp)
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (7 # 71) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                               + s V_synth_full__tmp1
                                                               - s V_synth_full_s)
            + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                               - s V_synth_full_sb)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (49 # 5) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (233 # 58) * max0(-1 + s V_synth_full__tmp1)
            - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                + s V_synth_full_sb)
            - (13 # 95) * max0(-1 + s V_synth_full__tmp1) * max0(15
                                                                 - s V_synth_full_sb)
            - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
            + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                  - s V_synth_full_ch)
            + (177 # 199) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s)
            - (1 # 150) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(15
                                                                    - s V_synth_full_sb)
            + (3 # 94) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp)
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1 # 31) * max0(-1 + s V_synth_full_sb)
            + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                             - s V_synth_full_ch)
            + (4 # 13) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
            - (1 # 122) * max0(-1 + s V_synth_full_sb)^2
            - (7 # 59) * max0(15 - s V_synth_full_sb)
            - (1 # 141) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
            + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (1 # 150) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
            + (73 # 158) * max0(s V_synth_full__tmp)
            - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (519 # 118) * max0(s V_synth_full__tmp - s V_synth_full_ch)
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (451 # 63) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            - (19 # 4) * max0(s V_synth_full__tmp1)
            + (0 # 1) * max0(s V_synth_full__tmp1) * max0(s V_synth_full_sb)
            - (6 # 73) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
            + (413 # 94) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_ch)
            - (1 # 60) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
            + (593 # 59) * max0(s V_synth_full_ch)
            + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (74 # 101) * max0(s V_synth_full_s)
            + (7 # 89) * max0(s V_synth_full_sb)
            - (1 # 139) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 73 => ((2505 # 142) - (783 # 89) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (7 # 71) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (1 # 52) * s V_synth_full__tmp * max0(-1 + s V_synth_full_sb)
            - (2 # 103) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (883 # 101) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                       - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            + (1 # 104) * s V_synth_full__tmp * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full__tmp^2
            - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
            - (6 # 107) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
            + (9 # 59) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
            - (451 # 63) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
            - (0 # 1) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
            - (126 # 97) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
            - (1 # 141) * s V_synth_full_ch * max0(15 - s V_synth_full_sb)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (413 # 94) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full_ch^2
            + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
            + (5 # 131) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
            - (1 # 15) * s V_synth_full_s * max0(s V_synth_full__tmp)
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
            + (1 # 4) * s V_synth_full_s * max0(s V_synth_full_sb)
            - (11 # 14) * s V_synth_full_sb
            + (3 # 68) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
            + (1 # 51) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            - (41 # 86) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
            + (65 # 84) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            + (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
            + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
            + (43 # 81) * s V_synth_full_sb * max0(s V_synth_full__tmp)
            - (74 # 117) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            + (0 # 1) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
            - (21 # 82) * s V_synth_full_sb * max0(s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            - (19 # 62) * s V_synth_full_sb * max0(s V_synth_full_ch)
            + (22 # 97) * s V_synth_full_sb * max0(s V_synth_full_s)
            + (1 # 152) * s V_synth_full_sb * max0(s V_synth_full_sb)
            + s V_synth_full_z - (39 # 73) * max0(-1 + s V_synth_full__tmp)
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (7 # 71) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                               + s V_synth_full__tmp1
                                                               - s V_synth_full_s)
            + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                               - s V_synth_full_sb)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (49 # 5) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (233 # 58) * max0(-1 + s V_synth_full__tmp1)
            - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                + s V_synth_full_sb)
            - (13 # 95) * max0(-1 + s V_synth_full__tmp1) * max0(15
                                                                 - s V_synth_full_sb)
            - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
            + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                  - s V_synth_full_ch)
            + (177 # 199) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s)
            - (1 # 150) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(15
                                                                    - s V_synth_full_sb)
            + (3 # 94) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp)
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1 # 31) * max0(-1 + s V_synth_full_sb)
            + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                             - s V_synth_full_ch)
            + (4 # 13) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
            - (1 # 122) * max0(-1 + s V_synth_full_sb)^2
            - (7 # 59) * max0(15 - s V_synth_full_sb)
            - (1 # 141) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
            + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (1 # 150) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
            + (73 # 158) * max0(s V_synth_full__tmp)
            - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (519 # 118) * max0(s V_synth_full__tmp - s V_synth_full_ch)
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (451 # 63) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            - (19 # 4) * max0(s V_synth_full__tmp1)
            + (0 # 1) * max0(s V_synth_full__tmp1) * max0(s V_synth_full_sb)
            - (6 # 73) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
            + (413 # 94) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_ch)
            - (1 # 60) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
            + (593 # 59) * max0(s V_synth_full_ch)
            + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (74 # 101) * max0(s V_synth_full_s)
            + (7 # 89) * max0(s V_synth_full_sb)
            - (1 # 139) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 74 => ((2505 # 142) - (783 # 89) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (7 # 71) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (1 # 52) * s V_synth_full__tmp * max0(-1 + s V_synth_full_sb)
            - (2 # 103) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (883 # 101) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                       - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            + (1 # 104) * s V_synth_full__tmp * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full__tmp^2
            - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
            - (6 # 107) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
            + (9 # 59) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
            - (451 # 63) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
            - (0 # 1) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
            - (126 # 97) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
            - (1 # 141) * s V_synth_full_ch * max0(15 - s V_synth_full_sb)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (413 # 94) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full_ch^2
            + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
            + (5 # 131) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
            - (1 # 15) * s V_synth_full_s * max0(s V_synth_full__tmp)
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
            + (1 # 4) * s V_synth_full_s * max0(s V_synth_full_sb)
            - (11 # 14) * s V_synth_full_sb
            + (3 # 68) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
            + (1 # 51) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            - (41 # 86) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
            + (65 # 84) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            + (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
            + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
            + (43 # 81) * s V_synth_full_sb * max0(s V_synth_full__tmp)
            - (74 # 117) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            + (0 # 1) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
            - (21 # 82) * s V_synth_full_sb * max0(s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            - (19 # 62) * s V_synth_full_sb * max0(s V_synth_full_ch)
            + (22 # 97) * s V_synth_full_sb * max0(s V_synth_full_s)
            + (1 # 152) * s V_synth_full_sb * max0(s V_synth_full_sb)
            + s V_synth_full_z - (39 # 73) * max0(-1 + s V_synth_full__tmp)
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (7 # 71) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                               + s V_synth_full__tmp1
                                                               - s V_synth_full_s)
            + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                               - s V_synth_full_sb)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (49 # 5) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (233 # 58) * max0(-1 + s V_synth_full__tmp1)
            - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                + s V_synth_full_sb)
            - (13 # 95) * max0(-1 + s V_synth_full__tmp1) * max0(15
                                                                 - s V_synth_full_sb)
            - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
            + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                  - s V_synth_full_ch)
            + (177 # 199) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s)
            - (1 # 150) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(15
                                                                    - s V_synth_full_sb)
            + (3 # 94) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp)
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1 # 31) * max0(-1 + s V_synth_full_sb)
            + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                             - s V_synth_full_ch)
            + (4 # 13) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
            - (1 # 122) * max0(-1 + s V_synth_full_sb)^2
            - (7 # 59) * max0(15 - s V_synth_full_sb)
            - (1 # 141) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
            + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (1 # 150) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
            + (73 # 158) * max0(s V_synth_full__tmp)
            - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (519 # 118) * max0(s V_synth_full__tmp - s V_synth_full_ch)
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (451 # 63) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            - (19 # 4) * max0(s V_synth_full__tmp1)
            + (0 # 1) * max0(s V_synth_full__tmp1) * max0(s V_synth_full_sb)
            - (6 # 73) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
            + (413 # 94) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_ch)
            - (1 # 60) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
            + (593 # 59) * max0(s V_synth_full_ch)
            + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (74 # 101) * max0(s V_synth_full_s)
            + (7 # 89) * max0(s V_synth_full_sb)
            - (1 # 139) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 75 => ((2505 # 142) - (783 # 89) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (7 # 71) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (1 # 52) * s V_synth_full__tmp * max0(-1 + s V_synth_full_sb)
            - (2 # 103) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (883 # 101) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                       - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            + (1 # 104) * s V_synth_full__tmp * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full__tmp^2
            - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
            - (6 # 107) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
            + (9 # 59) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
            - (451 # 63) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
            - (0 # 1) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
            - (126 # 97) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
            - (1 # 141) * s V_synth_full_ch * max0(15 - s V_synth_full_sb)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (413 # 94) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full_ch^2
            + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
            + (5 # 131) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
            - (1 # 15) * s V_synth_full_s * max0(s V_synth_full__tmp)
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
            + (1 # 4) * s V_synth_full_s * max0(s V_synth_full_sb)
            - (11 # 14) * s V_synth_full_sb
            + (3 # 68) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
            + (1 # 51) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            - (41 # 86) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
            + (65 # 84) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            + (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
            + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
            + (43 # 81) * s V_synth_full_sb * max0(s V_synth_full__tmp)
            - (74 # 117) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            + (0 # 1) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
            - (21 # 82) * s V_synth_full_sb * max0(s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            - (19 # 62) * s V_synth_full_sb * max0(s V_synth_full_ch)
            + (22 # 97) * s V_synth_full_sb * max0(s V_synth_full_s)
            + (1 # 152) * s V_synth_full_sb * max0(s V_synth_full_sb)
            + s V_synth_full_z - (39 # 73) * max0(-1 + s V_synth_full__tmp)
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (7 # 71) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                               + s V_synth_full__tmp1
                                                               - s V_synth_full_s)
            + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                               - s V_synth_full_sb)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (49 # 5) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (233 # 58) * max0(-1 + s V_synth_full__tmp1)
            - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                + s V_synth_full_sb)
            - (13 # 95) * max0(-1 + s V_synth_full__tmp1) * max0(15
                                                                 - s V_synth_full_sb)
            - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
            + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                  - s V_synth_full_ch)
            + (177 # 199) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s)
            - (1 # 150) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(15
                                                                    - s V_synth_full_sb)
            + (3 # 94) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp)
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1 # 31) * max0(-1 + s V_synth_full_sb)
            + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                             - s V_synth_full_ch)
            + (4 # 13) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
            - (1 # 122) * max0(-1 + s V_synth_full_sb)^2
            - (7 # 59) * max0(15 - s V_synth_full_sb)
            - (1 # 141) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
            + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (1 # 150) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
            + (73 # 158) * max0(s V_synth_full__tmp)
            - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (519 # 118) * max0(s V_synth_full__tmp - s V_synth_full_ch)
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (451 # 63) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            - (19 # 4) * max0(s V_synth_full__tmp1)
            + (0 # 1) * max0(s V_synth_full__tmp1) * max0(s V_synth_full_sb)
            - (6 # 73) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
            + (413 # 94) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_ch)
            - (1 # 60) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
            + (593 # 59) * max0(s V_synth_full_ch)
            + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (74 # 101) * max0(s V_synth_full_s)
            + (7 # 89) * max0(s V_synth_full_sb)
            - (1 # 139) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 76 => ((2505 # 142) - (783 # 89) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (7 # 71) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (1 # 52) * s V_synth_full__tmp * max0(-1 + s V_synth_full_sb)
            - (2 # 103) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (883 # 101) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                       - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            + (1 # 104) * s V_synth_full__tmp * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full__tmp^2
            - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
            - (6 # 107) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
            + (9 # 59) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
            - (451 # 63) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
            - (0 # 1) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
            - (126 # 97) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
            - (1 # 141) * s V_synth_full_ch * max0(15 - s V_synth_full_sb)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (413 # 94) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full_ch^2
            + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
            + (5 # 131) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
            - (1 # 15) * s V_synth_full_s * max0(s V_synth_full__tmp)
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
            + (1 # 4) * s V_synth_full_s * max0(s V_synth_full_sb)
            - (11 # 14) * s V_synth_full_sb
            + (3 # 68) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
            + (1 # 51) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            - (41 # 86) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
            + (65 # 84) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            + (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
            + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
            + (43 # 81) * s V_synth_full_sb * max0(s V_synth_full__tmp)
            - (74 # 117) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            + (0 # 1) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
            - (21 # 82) * s V_synth_full_sb * max0(s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            - (19 # 62) * s V_synth_full_sb * max0(s V_synth_full_ch)
            + (22 # 97) * s V_synth_full_sb * max0(s V_synth_full_s)
            + (1 # 152) * s V_synth_full_sb * max0(s V_synth_full_sb)
            + s V_synth_full_z - (39 # 73) * max0(-1 + s V_synth_full__tmp)
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (7 # 71) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                               + s V_synth_full__tmp1
                                                               - s V_synth_full_s)
            + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                               - s V_synth_full_sb)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (49 # 5) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (233 # 58) * max0(-1 + s V_synth_full__tmp1)
            - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                + s V_synth_full_sb)
            - (13 # 95) * max0(-1 + s V_synth_full__tmp1) * max0(15
                                                                 - s V_synth_full_sb)
            - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
            + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                  - s V_synth_full_ch)
            + (177 # 199) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s)
            - (1 # 150) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(15
                                                                    - s V_synth_full_sb)
            + (3 # 94) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp)
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1 # 31) * max0(-1 + s V_synth_full_sb)
            + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                             - s V_synth_full_ch)
            + (4 # 13) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
            - (1 # 122) * max0(-1 + s V_synth_full_sb)^2
            - (7 # 59) * max0(15 - s V_synth_full_sb)
            - (1 # 141) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
            + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (1 # 150) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
            + (73 # 158) * max0(s V_synth_full__tmp)
            - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (519 # 118) * max0(s V_synth_full__tmp - s V_synth_full_ch)
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (451 # 63) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            - (19 # 4) * max0(s V_synth_full__tmp1)
            + (0 # 1) * max0(s V_synth_full__tmp1) * max0(s V_synth_full_sb)
            - (6 # 73) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
            + (413 # 94) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_ch)
            - (1 # 60) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
            + (593 # 59) * max0(s V_synth_full_ch)
            + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (74 # 101) * max0(s V_synth_full_s)
            + (7 # 89) * max0(s V_synth_full_sb)
            - (1 # 139) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 77 => ((2505 # 142) - (783 # 89) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (7 # 71) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (1 # 52) * s V_synth_full__tmp * max0(-1 + s V_synth_full_sb)
            - (2 # 103) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (883 # 101) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                       - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            + (1 # 104) * s V_synth_full__tmp * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full__tmp^2
            - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
            - (6 # 107) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
            + (9 # 59) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
            - (451 # 63) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
            - (0 # 1) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
            - (126 # 97) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
            - (1 # 141) * s V_synth_full_ch * max0(15 - s V_synth_full_sb)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (413 # 94) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full_ch^2
            + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
            + (5 # 131) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
            - (1 # 15) * s V_synth_full_s * max0(s V_synth_full__tmp)
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
            + (1 # 4) * s V_synth_full_s * max0(s V_synth_full_sb)
            - (11 # 14) * s V_synth_full_sb
            + (3 # 68) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
            + (1 # 51) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            - (41 # 86) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
            + (65 # 84) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            + (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
            + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
            + (43 # 81) * s V_synth_full_sb * max0(s V_synth_full__tmp)
            - (74 # 117) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            + (0 # 1) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
            - (21 # 82) * s V_synth_full_sb * max0(s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            - (19 # 62) * s V_synth_full_sb * max0(s V_synth_full_ch)
            + (22 # 97) * s V_synth_full_sb * max0(s V_synth_full_s)
            + (1 # 152) * s V_synth_full_sb * max0(s V_synth_full_sb)
            + s V_synth_full_z - (39 # 73) * max0(-1 + s V_synth_full__tmp)
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (7 # 71) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                               + s V_synth_full__tmp1
                                                               - s V_synth_full_s)
            + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                               - s V_synth_full_sb)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (49 # 5) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (233 # 58) * max0(-1 + s V_synth_full__tmp1)
            - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                + s V_synth_full_sb)
            - (13 # 95) * max0(-1 + s V_synth_full__tmp1) * max0(15
                                                                 - s V_synth_full_sb)
            - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
            + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                  - s V_synth_full_ch)
            + (177 # 199) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s)
            - (1 # 150) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(15
                                                                    - s V_synth_full_sb)
            + (3 # 94) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp)
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1 # 31) * max0(-1 + s V_synth_full_sb)
            + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                             - s V_synth_full_ch)
            + (4 # 13) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
            - (1 # 122) * max0(-1 + s V_synth_full_sb)^2
            - (7 # 59) * max0(15 - s V_synth_full_sb)
            - (1 # 141) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
            + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (1 # 150) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
            + (73 # 158) * max0(s V_synth_full__tmp)
            - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (519 # 118) * max0(s V_synth_full__tmp - s V_synth_full_ch)
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (451 # 63) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            - (19 # 4) * max0(s V_synth_full__tmp1)
            + (0 # 1) * max0(s V_synth_full__tmp1) * max0(s V_synth_full_sb)
            - (6 # 73) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
            + (413 # 94) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_ch)
            - (1 # 60) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
            + (593 # 59) * max0(s V_synth_full_ch)
            + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (74 # 101) * max0(s V_synth_full_s)
            + (7 # 89) * max0(s V_synth_full_sb)
            - (1 # 139) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 78 => ((2505 # 142) - (783 # 89) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (7 # 71) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (1 # 52) * s V_synth_full__tmp * max0(-1 + s V_synth_full_sb)
            - (2 # 103) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (883 # 101) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                       - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            + (1 # 104) * s V_synth_full__tmp * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full__tmp^2
            - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
            - (6 # 107) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
            + (9 # 59) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
            - (451 # 63) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
            - (0 # 1) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
            - (126 # 97) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
            - (1 # 141) * s V_synth_full_ch * max0(15 - s V_synth_full_sb)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (413 # 94) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full_ch^2
            + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
            + (5 # 131) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
            - (1 # 15) * s V_synth_full_s * max0(s V_synth_full__tmp)
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
            + (1 # 4) * s V_synth_full_s * max0(s V_synth_full_sb)
            - (11 # 14) * s V_synth_full_sb
            + (3 # 68) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
            + (1 # 51) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            - (41 # 86) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
            + (65 # 84) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            + (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
            + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
            + (43 # 81) * s V_synth_full_sb * max0(s V_synth_full__tmp)
            - (74 # 117) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            + (0 # 1) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
            - (21 # 82) * s V_synth_full_sb * max0(s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            - (19 # 62) * s V_synth_full_sb * max0(s V_synth_full_ch)
            + (22 # 97) * s V_synth_full_sb * max0(s V_synth_full_s)
            + (1 # 152) * s V_synth_full_sb * max0(s V_synth_full_sb)
            + s V_synth_full_z - (39 # 73) * max0(-1 + s V_synth_full__tmp)
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (7 # 71) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                               + s V_synth_full__tmp1
                                                               - s V_synth_full_s)
            + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                               - s V_synth_full_sb)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (49 # 5) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (233 # 58) * max0(-1 + s V_synth_full__tmp1)
            - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                + s V_synth_full_sb)
            - (13 # 95) * max0(-1 + s V_synth_full__tmp1) * max0(15
                                                                 - s V_synth_full_sb)
            - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
            + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                  - s V_synth_full_ch)
            + (177 # 199) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s)
            - (1 # 150) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(15
                                                                    - s V_synth_full_sb)
            + (3 # 94) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp)
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1 # 31) * max0(-1 + s V_synth_full_sb)
            + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                             - s V_synth_full_ch)
            + (4 # 13) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
            - (1 # 122) * max0(-1 + s V_synth_full_sb)^2
            - (7 # 59) * max0(15 - s V_synth_full_sb)
            - (1 # 141) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
            + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (1 # 150) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
            + (73 # 158) * max0(s V_synth_full__tmp)
            - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (519 # 118) * max0(s V_synth_full__tmp - s V_synth_full_ch)
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (451 # 63) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            - (19 # 4) * max0(s V_synth_full__tmp1)
            + (0 # 1) * max0(s V_synth_full__tmp1) * max0(s V_synth_full_sb)
            - (6 # 73) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
            + (413 # 94) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_ch)
            - (1 # 60) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
            + (593 # 59) * max0(s V_synth_full_ch)
            + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (74 # 101) * max0(s V_synth_full_s)
            + (7 # 89) * max0(s V_synth_full_sb)
            - (1 # 139) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 79 => ((2505 # 142) - (783 # 89) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (7 # 71) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (1 # 52) * s V_synth_full__tmp * max0(-1 + s V_synth_full_sb)
            - (2 # 103) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (883 # 101) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                       - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            + (1 # 104) * s V_synth_full__tmp * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full__tmp^2
            - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
            - (6 # 107) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
            + (9 # 59) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
            - (451 # 63) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
            - (0 # 1) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
            - (126 # 97) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
            - (1 # 141) * s V_synth_full_ch * max0(15 - s V_synth_full_sb)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (413 # 94) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full_ch^2
            + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
            + (5 # 131) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
            - (1 # 15) * s V_synth_full_s * max0(s V_synth_full__tmp)
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
            + (1 # 4) * s V_synth_full_s * max0(s V_synth_full_sb)
            - (11 # 14) * s V_synth_full_sb
            + (3 # 68) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
            + (1 # 51) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            - (41 # 86) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
            + (65 # 84) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            + (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
            + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
            + (43 # 81) * s V_synth_full_sb * max0(s V_synth_full__tmp)
            - (74 # 117) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            + (0 # 1) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
            - (21 # 82) * s V_synth_full_sb * max0(s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            - (19 # 62) * s V_synth_full_sb * max0(s V_synth_full_ch)
            + (22 # 97) * s V_synth_full_sb * max0(s V_synth_full_s)
            + (1 # 152) * s V_synth_full_sb * max0(s V_synth_full_sb)
            + s V_synth_full_z - (39 # 73) * max0(-1 + s V_synth_full__tmp)
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (7 # 71) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                               + s V_synth_full__tmp1
                                                               - s V_synth_full_s)
            + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                               - s V_synth_full_sb)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (49 # 5) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (233 # 58) * max0(-1 + s V_synth_full__tmp1)
            - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                + s V_synth_full_sb)
            - (13 # 95) * max0(-1 + s V_synth_full__tmp1) * max0(15
                                                                 - s V_synth_full_sb)
            - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
            + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                  - s V_synth_full_ch)
            + (177 # 199) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s)
            - (1 # 150) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(15
                                                                    - s V_synth_full_sb)
            + (3 # 94) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp)
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1 # 31) * max0(-1 + s V_synth_full_sb)
            + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                             - s V_synth_full_ch)
            + (4 # 13) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
            - (1 # 122) * max0(-1 + s V_synth_full_sb)^2
            - (7 # 59) * max0(15 - s V_synth_full_sb)
            - (1 # 141) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
            + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (1 # 150) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
            + (73 # 158) * max0(s V_synth_full__tmp)
            - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (519 # 118) * max0(s V_synth_full__tmp - s V_synth_full_ch)
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (451 # 63) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            - (19 # 4) * max0(s V_synth_full__tmp1)
            + (0 # 1) * max0(s V_synth_full__tmp1) * max0(s V_synth_full_sb)
            - (6 # 73) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
            + (413 # 94) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_ch)
            - (1 # 60) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
            + (593 # 59) * max0(s V_synth_full_ch)
            + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (74 # 101) * max0(s V_synth_full_s)
            + (7 # 89) * max0(s V_synth_full_sb)
            - (1 # 139) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 80 => ((2505 # 142) - (783 # 89) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (7 # 71) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (1 # 52) * s V_synth_full__tmp * max0(-1 + s V_synth_full_sb)
            - (2 # 103) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (883 # 101) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                       - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            + (1 # 104) * s V_synth_full__tmp * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full__tmp^2
            - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
            - (6 # 107) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
            + (9 # 59) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
            - (451 # 63) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
            - (0 # 1) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
            - (126 # 97) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
            - (1 # 141) * s V_synth_full_ch * max0(15 - s V_synth_full_sb)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (413 # 94) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full_ch^2
            + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
            + (5 # 131) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
            - (1 # 15) * s V_synth_full_s * max0(s V_synth_full__tmp)
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
            + (1 # 4) * s V_synth_full_s * max0(s V_synth_full_sb)
            - (11 # 14) * s V_synth_full_sb
            + (3 # 68) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
            + (1 # 51) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            - (41 # 86) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
            + (65 # 84) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            + (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
            + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
            + (43 # 81) * s V_synth_full_sb * max0(s V_synth_full__tmp)
            - (74 # 117) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            + (0 # 1) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
            - (21 # 82) * s V_synth_full_sb * max0(s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            - (19 # 62) * s V_synth_full_sb * max0(s V_synth_full_ch)
            + (22 # 97) * s V_synth_full_sb * max0(s V_synth_full_s)
            + (1 # 152) * s V_synth_full_sb * max0(s V_synth_full_sb)
            + s V_synth_full_z - (39 # 73) * max0(-1 + s V_synth_full__tmp)
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (7 # 71) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                               + s V_synth_full__tmp1
                                                               - s V_synth_full_s)
            + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                               - s V_synth_full_sb)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (49 # 5) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (233 # 58) * max0(-1 + s V_synth_full__tmp1)
            - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                + s V_synth_full_sb)
            - (13 # 95) * max0(-1 + s V_synth_full__tmp1) * max0(15
                                                                 - s V_synth_full_sb)
            - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
            + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                  - s V_synth_full_ch)
            + (177 # 199) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s)
            - (1 # 150) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(15
                                                                    - s V_synth_full_sb)
            + (3 # 94) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp)
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1 # 31) * max0(-1 + s V_synth_full_sb)
            + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                             - s V_synth_full_ch)
            + (4 # 13) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
            - (1 # 122) * max0(-1 + s V_synth_full_sb)^2
            - (7 # 59) * max0(15 - s V_synth_full_sb)
            - (1 # 141) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
            + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (1 # 150) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
            + (73 # 158) * max0(s V_synth_full__tmp)
            - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (519 # 118) * max0(s V_synth_full__tmp - s V_synth_full_ch)
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (451 # 63) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            - (19 # 4) * max0(s V_synth_full__tmp1)
            + (0 # 1) * max0(s V_synth_full__tmp1) * max0(s V_synth_full_sb)
            - (6 # 73) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
            + (413 # 94) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_ch)
            - (1 # 60) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
            + (593 # 59) * max0(s V_synth_full_ch)
            + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (74 # 101) * max0(s V_synth_full_s)
            + (7 # 89) * max0(s V_synth_full_sb)
            - (1 # 139) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 81 => ((2505 # 142) - (783 # 89) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (7 # 71) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (1 # 52) * s V_synth_full__tmp * max0(-1 + s V_synth_full_sb)
            - (2 # 103) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (883 # 101) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                       - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            + (1 # 104) * s V_synth_full__tmp * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full__tmp^2
            - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
            - (6 # 107) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
            + (9 # 59) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
            - (451 # 63) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
            - (0 # 1) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
            - (126 # 97) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
            - (1 # 141) * s V_synth_full_ch * max0(15 - s V_synth_full_sb)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (413 # 94) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full_ch^2
            + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
            + (5 # 131) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
            - (1 # 15) * s V_synth_full_s * max0(s V_synth_full__tmp)
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
            + (1 # 4) * s V_synth_full_s * max0(s V_synth_full_sb)
            - (11 # 14) * s V_synth_full_sb
            + (3 # 68) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
            + (1 # 51) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            - (41 # 86) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
            + (65 # 84) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            + (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
            + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
            + (43 # 81) * s V_synth_full_sb * max0(s V_synth_full__tmp)
            - (74 # 117) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            + (0 # 1) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
            - (21 # 82) * s V_synth_full_sb * max0(s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            - (19 # 62) * s V_synth_full_sb * max0(s V_synth_full_ch)
            + (22 # 97) * s V_synth_full_sb * max0(s V_synth_full_s)
            + (1 # 152) * s V_synth_full_sb * max0(s V_synth_full_sb)
            + s V_synth_full_z - (39 # 73) * max0(-1 + s V_synth_full__tmp)
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (7 # 71) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                               + s V_synth_full__tmp1
                                                               - s V_synth_full_s)
            + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                               - s V_synth_full_sb)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (49 # 5) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (233 # 58) * max0(-1 + s V_synth_full__tmp1)
            - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                + s V_synth_full_sb)
            - (13 # 95) * max0(-1 + s V_synth_full__tmp1) * max0(15
                                                                 - s V_synth_full_sb)
            - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
            + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                  - s V_synth_full_ch)
            + (177 # 199) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s)
            - (1 # 150) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(15
                                                                    - s V_synth_full_sb)
            + (3 # 94) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp)
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1 # 31) * max0(-1 + s V_synth_full_sb)
            + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                             - s V_synth_full_ch)
            + (4 # 13) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
            - (1 # 122) * max0(-1 + s V_synth_full_sb)^2
            - (7 # 59) * max0(15 - s V_synth_full_sb)
            - (1 # 141) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
            + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (1 # 150) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
            + (73 # 158) * max0(s V_synth_full__tmp)
            - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (519 # 118) * max0(s V_synth_full__tmp - s V_synth_full_ch)
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (451 # 63) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            - (19 # 4) * max0(s V_synth_full__tmp1)
            + (0 # 1) * max0(s V_synth_full__tmp1) * max0(s V_synth_full_sb)
            - (6 # 73) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
            + (413 # 94) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_ch)
            - (1 # 60) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
            + (593 # 59) * max0(s V_synth_full_ch)
            + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (74 # 101) * max0(s V_synth_full_s)
            + (7 # 89) * max0(s V_synth_full_sb)
            - (1 # 139) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 82 => ((2505 # 142) - (783 # 89) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (7 # 71) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (1 # 52) * s V_synth_full__tmp * max0(-1 + s V_synth_full_sb)
            - (2 # 103) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (883 # 101) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                       - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            + (1 # 104) * s V_synth_full__tmp * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full__tmp^2
            - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
            - (6 # 107) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
            + (9 # 59) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
            - (451 # 63) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
            - (0 # 1) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
            - (126 # 97) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
            - (1 # 141) * s V_synth_full_ch * max0(15 - s V_synth_full_sb)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (413 # 94) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full_ch^2
            + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
            + (5 # 131) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
            - (1 # 15) * s V_synth_full_s * max0(s V_synth_full__tmp)
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
            + (1 # 4) * s V_synth_full_s * max0(s V_synth_full_sb)
            - (11 # 14) * s V_synth_full_sb
            + (3 # 68) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
            + (1 # 51) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            - (41 # 86) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
            + (65 # 84) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            + (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
            + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
            + (43 # 81) * s V_synth_full_sb * max0(s V_synth_full__tmp)
            - (74 # 117) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            + (0 # 1) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
            - (21 # 82) * s V_synth_full_sb * max0(s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            - (19 # 62) * s V_synth_full_sb * max0(s V_synth_full_ch)
            + (22 # 97) * s V_synth_full_sb * max0(s V_synth_full_s)
            + (1 # 152) * s V_synth_full_sb * max0(s V_synth_full_sb)
            + s V_synth_full_z - (39 # 73) * max0(-1 + s V_synth_full__tmp)
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (7 # 71) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                               + s V_synth_full__tmp1
                                                               - s V_synth_full_s)
            + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                               - s V_synth_full_sb)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (49 # 5) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (233 # 58) * max0(-1 + s V_synth_full__tmp1)
            - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                + s V_synth_full_sb)
            - (13 # 95) * max0(-1 + s V_synth_full__tmp1) * max0(15
                                                                 - s V_synth_full_sb)
            - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
            + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                  - s V_synth_full_ch)
            + (177 # 199) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s)
            - (1 # 150) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(15
                                                                    - s V_synth_full_sb)
            + (3 # 94) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp)
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1 # 31) * max0(-1 + s V_synth_full_sb)
            + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                             - s V_synth_full_ch)
            + (4 # 13) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
            - (1 # 122) * max0(-1 + s V_synth_full_sb)^2
            - (7 # 59) * max0(15 - s V_synth_full_sb)
            - (1 # 141) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
            + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (1 # 150) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
            + (73 # 158) * max0(s V_synth_full__tmp)
            - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (519 # 118) * max0(s V_synth_full__tmp - s V_synth_full_ch)
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (451 # 63) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            - (19 # 4) * max0(s V_synth_full__tmp1)
            + (0 # 1) * max0(s V_synth_full__tmp1) * max0(s V_synth_full_sb)
            - (6 # 73) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
            + (413 # 94) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_ch)
            - (1 # 60) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
            + (593 # 59) * max0(s V_synth_full_ch)
            + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (74 # 101) * max0(s V_synth_full_s)
            + (7 # 89) * max0(s V_synth_full_sb)
            - (1 # 139) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 83 => ((2505 # 142) - (783 # 89) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (7 # 71) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (1 # 52) * s V_synth_full__tmp * max0(-1 + s V_synth_full_sb)
            - (2 # 103) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (883 # 101) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                       - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            + (1 # 104) * s V_synth_full__tmp * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full__tmp^2
            - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
            - (6 # 107) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
            + (9 # 59) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
            - (451 # 63) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
            - (0 # 1) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
            - (126 # 97) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
            - (1 # 141) * s V_synth_full_ch * max0(15 - s V_synth_full_sb)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (413 # 94) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full_ch^2
            + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
            + (5 # 131) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
            - (1 # 15) * s V_synth_full_s * max0(s V_synth_full__tmp)
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
            + (1 # 4) * s V_synth_full_s * max0(s V_synth_full_sb)
            - (11 # 14) * s V_synth_full_sb
            + (3 # 68) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
            + (1 # 51) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            - (41 # 86) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
            + (65 # 84) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            + (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
            + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
            + (43 # 81) * s V_synth_full_sb * max0(s V_synth_full__tmp)
            - (74 # 117) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            + (0 # 1) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
            - (21 # 82) * s V_synth_full_sb * max0(s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            - (19 # 62) * s V_synth_full_sb * max0(s V_synth_full_ch)
            + (22 # 97) * s V_synth_full_sb * max0(s V_synth_full_s)
            + (1 # 152) * s V_synth_full_sb * max0(s V_synth_full_sb)
            + s V_synth_full_z - (39 # 73) * max0(-1 + s V_synth_full__tmp)
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (7 # 71) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                               + s V_synth_full__tmp1
                                                               - s V_synth_full_s)
            + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                               - s V_synth_full_sb)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (49 # 5) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (233 # 58) * max0(-1 + s V_synth_full__tmp1)
            - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                + s V_synth_full_sb)
            - (13 # 95) * max0(-1 + s V_synth_full__tmp1) * max0(15
                                                                 - s V_synth_full_sb)
            - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
            + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                  - s V_synth_full_ch)
            + (177 # 199) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s)
            - (1 # 150) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(15
                                                                    - s V_synth_full_sb)
            + (3 # 94) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp)
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1 # 31) * max0(-1 + s V_synth_full_sb)
            + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                             - s V_synth_full_ch)
            + (4 # 13) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
            - (1 # 122) * max0(-1 + s V_synth_full_sb)^2
            - (7 # 59) * max0(15 - s V_synth_full_sb)
            - (1 # 141) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
            + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (1 # 150) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
            + (73 # 158) * max0(s V_synth_full__tmp)
            - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (519 # 118) * max0(s V_synth_full__tmp - s V_synth_full_ch)
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (451 # 63) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            - (19 # 4) * max0(s V_synth_full__tmp1)
            + (0 # 1) * max0(s V_synth_full__tmp1) * max0(s V_synth_full_sb)
            - (6 # 73) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
            + (413 # 94) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_ch)
            - (1 # 60) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
            + (593 # 59) * max0(s V_synth_full_ch)
            + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (74 # 101) * max0(s V_synth_full_s)
            + (7 # 89) * max0(s V_synth_full_sb)
            - (1 # 139) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 84 => ((2505 # 142) - (783 # 89) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (7 # 71) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (1 # 52) * s V_synth_full__tmp * max0(-1 + s V_synth_full_sb)
            - (2 # 103) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (883 # 101) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                       - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            + (1 # 104) * s V_synth_full__tmp * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full__tmp^2
            - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
            - (6 # 107) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
            + (9 # 59) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
            - (451 # 63) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
            - (0 # 1) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
            - (126 # 97) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
            - (1 # 141) * s V_synth_full_ch * max0(15 - s V_synth_full_sb)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (413 # 94) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full_ch^2
            + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
            + (5 # 131) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
            - (1 # 15) * s V_synth_full_s * max0(s V_synth_full__tmp)
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
            + (1 # 4) * s V_synth_full_s * max0(s V_synth_full_sb)
            - (11 # 14) * s V_synth_full_sb
            + (3 # 68) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
            + (1 # 51) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            - (41 # 86) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
            + (65 # 84) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            + (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
            + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
            + (43 # 81) * s V_synth_full_sb * max0(s V_synth_full__tmp)
            - (74 # 117) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            + (0 # 1) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
            - (21 # 82) * s V_synth_full_sb * max0(s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            - (19 # 62) * s V_synth_full_sb * max0(s V_synth_full_ch)
            + (22 # 97) * s V_synth_full_sb * max0(s V_synth_full_s)
            + (1 # 152) * s V_synth_full_sb * max0(s V_synth_full_sb)
            + s V_synth_full_z - (39 # 73) * max0(-1 + s V_synth_full__tmp)
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (7 # 71) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                               + s V_synth_full__tmp1
                                                               - s V_synth_full_s)
            + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                               - s V_synth_full_sb)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (49 # 5) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (233 # 58) * max0(-1 + s V_synth_full__tmp1)
            - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                + s V_synth_full_sb)
            - (13 # 95) * max0(-1 + s V_synth_full__tmp1) * max0(15
                                                                 - s V_synth_full_sb)
            - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
            + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                  - s V_synth_full_ch)
            + (177 # 199) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s)
            - (1 # 150) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(15
                                                                    - s V_synth_full_sb)
            + (3 # 94) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp)
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1 # 31) * max0(-1 + s V_synth_full_sb)
            + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                             - s V_synth_full_ch)
            + (4 # 13) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
            - (1 # 122) * max0(-1 + s V_synth_full_sb)^2
            - (7 # 59) * max0(15 - s V_synth_full_sb)
            - (1 # 141) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
            + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (1 # 150) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
            + (73 # 158) * max0(s V_synth_full__tmp)
            - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (519 # 118) * max0(s V_synth_full__tmp - s V_synth_full_ch)
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (451 # 63) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            - (19 # 4) * max0(s V_synth_full__tmp1)
            + (0 # 1) * max0(s V_synth_full__tmp1) * max0(s V_synth_full_sb)
            - (6 # 73) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
            + (413 # 94) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_ch)
            - (1 # 60) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
            + (593 # 59) * max0(s V_synth_full_ch)
            + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (74 # 101) * max0(s V_synth_full_s)
            + (7 # 89) * max0(s V_synth_full_sb)
            - (1 # 139) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 85 => ((2505 # 142) - (783 # 89) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (7 # 71) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (1 # 52) * s V_synth_full__tmp * max0(-1 + s V_synth_full_sb)
            - (2 # 103) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (883 # 101) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                       - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            + (1 # 104) * s V_synth_full__tmp * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full__tmp^2
            - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
            - (6 # 107) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
            + (9 # 59) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
            - (451 # 63) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
            - (0 # 1) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
            - (126 # 97) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
            - (1 # 141) * s V_synth_full_ch * max0(15 - s V_synth_full_sb)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (413 # 94) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full_ch^2
            + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
            + (5 # 131) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
            - (1 # 15) * s V_synth_full_s * max0(s V_synth_full__tmp)
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
            + (1 # 4) * s V_synth_full_s * max0(s V_synth_full_sb)
            - (11 # 14) * s V_synth_full_sb
            + (3 # 68) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
            + (1 # 51) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            - (41 # 86) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
            + (65 # 84) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            + (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
            + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
            + (43 # 81) * s V_synth_full_sb * max0(s V_synth_full__tmp)
            - (74 # 117) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            + (0 # 1) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
            - (21 # 82) * s V_synth_full_sb * max0(s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            - (19 # 62) * s V_synth_full_sb * max0(s V_synth_full_ch)
            + (22 # 97) * s V_synth_full_sb * max0(s V_synth_full_s)
            + (1 # 152) * s V_synth_full_sb * max0(s V_synth_full_sb)
            + s V_synth_full_z - (39 # 73) * max0(-1 + s V_synth_full__tmp)
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (7 # 71) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                               + s V_synth_full__tmp1
                                                               - s V_synth_full_s)
            + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                               - s V_synth_full_sb)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (49 # 5) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (233 # 58) * max0(-1 + s V_synth_full__tmp1)
            - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                + s V_synth_full_sb)
            - (13 # 95) * max0(-1 + s V_synth_full__tmp1) * max0(15
                                                                 - s V_synth_full_sb)
            - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
            + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                  - s V_synth_full_ch)
            + (177 # 199) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s)
            - (1 # 150) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(15
                                                                    - s V_synth_full_sb)
            + (3 # 94) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp)
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1 # 31) * max0(-1 + s V_synth_full_sb)
            + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                             - s V_synth_full_ch)
            + (4 # 13) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
            - (1 # 122) * max0(-1 + s V_synth_full_sb)^2
            - (7 # 59) * max0(15 - s V_synth_full_sb)
            - (1 # 141) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
            + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (1 # 150) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
            + (73 # 158) * max0(s V_synth_full__tmp)
            - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (519 # 118) * max0(s V_synth_full__tmp - s V_synth_full_ch)
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (451 # 63) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            - (19 # 4) * max0(s V_synth_full__tmp1)
            + (0 # 1) * max0(s V_synth_full__tmp1) * max0(s V_synth_full_sb)
            - (6 # 73) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
            + (413 # 94) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_ch)
            - (1 # 60) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
            + (593 # 59) * max0(s V_synth_full_ch)
            + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (74 # 101) * max0(s V_synth_full_s)
            + (7 # 89) * max0(s V_synth_full_sb)
            - (1 # 139) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 86 => ((2505 # 142) - (783 # 89) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (7 # 71) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (1 # 52) * s V_synth_full__tmp * max0(-1 + s V_synth_full_sb)
            - (2 # 103) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (883 # 101) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                       - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            + (1 # 104) * s V_synth_full__tmp * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full__tmp^2
            - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
            - (6 # 107) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
            + (9 # 59) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
            - (451 # 63) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
            - (0 # 1) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
            - (126 # 97) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
            - (1 # 141) * s V_synth_full_ch * max0(15 - s V_synth_full_sb)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (413 # 94) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full_ch^2
            + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
            + (5 # 131) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
            - (1 # 15) * s V_synth_full_s * max0(s V_synth_full__tmp)
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
            + (1 # 4) * s V_synth_full_s * max0(s V_synth_full_sb)
            - (11 # 14) * s V_synth_full_sb
            + (3 # 68) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
            + (1 # 51) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            - (41 # 86) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
            + (65 # 84) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            + (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
            + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
            + (43 # 81) * s V_synth_full_sb * max0(s V_synth_full__tmp)
            - (74 # 117) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            + (0 # 1) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
            - (21 # 82) * s V_synth_full_sb * max0(s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            - (19 # 62) * s V_synth_full_sb * max0(s V_synth_full_ch)
            + (22 # 97) * s V_synth_full_sb * max0(s V_synth_full_s)
            + (1 # 152) * s V_synth_full_sb * max0(s V_synth_full_sb)
            + s V_synth_full_z - (39 # 73) * max0(-1 + s V_synth_full__tmp)
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (7 # 71) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                               + s V_synth_full__tmp1
                                                               - s V_synth_full_s)
            + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                               - s V_synth_full_sb)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (49 # 5) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (233 # 58) * max0(-1 + s V_synth_full__tmp1)
            - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                + s V_synth_full_sb)
            - (13 # 95) * max0(-1 + s V_synth_full__tmp1) * max0(15
                                                                 - s V_synth_full_sb)
            - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
            + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                  - s V_synth_full_ch)
            + (177 # 199) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s)
            - (1 # 150) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(15
                                                                    - s V_synth_full_sb)
            + (3 # 94) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp)
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1 # 31) * max0(-1 + s V_synth_full_sb)
            + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                             - s V_synth_full_ch)
            + (4 # 13) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
            - (1 # 122) * max0(-1 + s V_synth_full_sb)^2
            - (7 # 59) * max0(15 - s V_synth_full_sb)
            - (1 # 141) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
            + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (1 # 150) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
            + (73 # 158) * max0(s V_synth_full__tmp)
            - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (519 # 118) * max0(s V_synth_full__tmp - s V_synth_full_ch)
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (451 # 63) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            - (19 # 4) * max0(s V_synth_full__tmp1)
            + (0 # 1) * max0(s V_synth_full__tmp1) * max0(s V_synth_full_sb)
            - (6 # 73) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
            + (413 # 94) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_ch)
            - (1 # 60) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
            + (593 # 59) * max0(s V_synth_full_ch)
            + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (74 # 101) * max0(s V_synth_full_s)
            + (7 # 89) * max0(s V_synth_full_sb)
            - (1 # 139) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 87 => ((2505 # 142) - (783 # 89) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (7 # 71) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (1 # 52) * s V_synth_full__tmp * max0(-1 + s V_synth_full_sb)
            - (2 # 103) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (883 # 101) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                       - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            + (1 # 104) * s V_synth_full__tmp * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full__tmp^2
            - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
            - (6 # 107) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
            + (9 # 59) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
            - (451 # 63) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
            - (0 # 1) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
            - (126 # 97) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
            - (1 # 141) * s V_synth_full_ch * max0(15 - s V_synth_full_sb)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (413 # 94) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full_ch^2
            + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
            + (5 # 131) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
            - (1 # 15) * s V_synth_full_s * max0(s V_synth_full__tmp)
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
            + (1 # 4) * s V_synth_full_s * max0(s V_synth_full_sb)
            - (11 # 14) * s V_synth_full_sb
            + (3 # 68) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
            + (1 # 51) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            - (41 # 86) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
            + (65 # 84) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            + (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
            + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
            + (43 # 81) * s V_synth_full_sb * max0(s V_synth_full__tmp)
            - (74 # 117) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            + (0 # 1) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
            - (21 # 82) * s V_synth_full_sb * max0(s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            - (19 # 62) * s V_synth_full_sb * max0(s V_synth_full_ch)
            + (22 # 97) * s V_synth_full_sb * max0(s V_synth_full_s)
            + (1 # 152) * s V_synth_full_sb * max0(s V_synth_full_sb)
            + s V_synth_full_z - (39 # 73) * max0(-1 + s V_synth_full__tmp)
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (7 # 71) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                               + s V_synth_full__tmp1
                                                               - s V_synth_full_s)
            + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                               - s V_synth_full_sb)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (49 # 5) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (233 # 58) * max0(-1 + s V_synth_full__tmp1)
            - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                + s V_synth_full_sb)
            - (13 # 95) * max0(-1 + s V_synth_full__tmp1) * max0(15
                                                                 - s V_synth_full_sb)
            - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
            + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                  - s V_synth_full_ch)
            + (177 # 199) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s)
            - (1 # 150) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(15
                                                                    - s V_synth_full_sb)
            + (3 # 94) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp)
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1 # 31) * max0(-1 + s V_synth_full_sb)
            + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                             - s V_synth_full_ch)
            + (4 # 13) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
            - (1 # 122) * max0(-1 + s V_synth_full_sb)^2
            - (7 # 59) * max0(15 - s V_synth_full_sb)
            - (1 # 141) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
            + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (1 # 150) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
            + (73 # 158) * max0(s V_synth_full__tmp)
            - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (519 # 118) * max0(s V_synth_full__tmp - s V_synth_full_ch)
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (451 # 63) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            - (19 # 4) * max0(s V_synth_full__tmp1)
            + (0 # 1) * max0(s V_synth_full__tmp1) * max0(s V_synth_full_sb)
            - (6 # 73) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
            + (413 # 94) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_ch)
            - (1 # 60) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
            + (593 # 59) * max0(s V_synth_full_ch)
            + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (74 # 101) * max0(s V_synth_full_s)
            + (7 # 89) * max0(s V_synth_full_sb)
            - (1 # 139) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 88 => ((2505 # 142) - (783 # 89) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (7 # 71) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (1 # 52) * s V_synth_full__tmp * max0(-1 + s V_synth_full_sb)
            - (2 # 103) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (883 # 101) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                       - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            + (1 # 104) * s V_synth_full__tmp * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full__tmp^2
            - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
            - (6 # 107) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
            + (9 # 59) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
            - (451 # 63) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
            - (0 # 1) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
            - (126 # 97) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
            - (1 # 141) * s V_synth_full_ch * max0(15 - s V_synth_full_sb)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (413 # 94) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full_ch^2
            + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
            + (5 # 131) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
            - (1 # 15) * s V_synth_full_s * max0(s V_synth_full__tmp)
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
            + (1 # 4) * s V_synth_full_s * max0(s V_synth_full_sb)
            - (11 # 14) * s V_synth_full_sb
            + (3 # 68) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
            + (1 # 51) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            - (41 # 86) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
            + (65 # 84) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            + (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
            + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
            + (43 # 81) * s V_synth_full_sb * max0(s V_synth_full__tmp)
            - (74 # 117) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            + (0 # 1) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
            - (21 # 82) * s V_synth_full_sb * max0(s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            - (19 # 62) * s V_synth_full_sb * max0(s V_synth_full_ch)
            + (22 # 97) * s V_synth_full_sb * max0(s V_synth_full_s)
            + (1 # 152) * s V_synth_full_sb * max0(s V_synth_full_sb)
            + s V_synth_full_z - (39 # 73) * max0(-1 + s V_synth_full__tmp)
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (7 # 71) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                               + s V_synth_full__tmp1
                                                               - s V_synth_full_s)
            + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                               - s V_synth_full_sb)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (49 # 5) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (233 # 58) * max0(-1 + s V_synth_full__tmp1)
            - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                + s V_synth_full_sb)
            - (13 # 95) * max0(-1 + s V_synth_full__tmp1) * max0(15
                                                                 - s V_synth_full_sb)
            - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
            + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                  - s V_synth_full_ch)
            + (177 # 199) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s)
            - (1 # 150) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(15
                                                                    - s V_synth_full_sb)
            + (3 # 94) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp)
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1 # 31) * max0(-1 + s V_synth_full_sb)
            + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                             - s V_synth_full_ch)
            + (4 # 13) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
            - (1 # 122) * max0(-1 + s V_synth_full_sb)^2
            - (7 # 59) * max0(15 - s V_synth_full_sb)
            - (1 # 141) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
            + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (1 # 150) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
            + (73 # 158) * max0(s V_synth_full__tmp)
            - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (519 # 118) * max0(s V_synth_full__tmp - s V_synth_full_ch)
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (451 # 63) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            - (19 # 4) * max0(s V_synth_full__tmp1)
            + (0 # 1) * max0(s V_synth_full__tmp1) * max0(s V_synth_full_sb)
            - (6 # 73) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
            + (413 # 94) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_ch)
            - (1 # 60) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
            + (593 # 59) * max0(s V_synth_full_ch)
            + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (74 # 101) * max0(s V_synth_full_s)
            + (7 # 89) * max0(s V_synth_full_sb)
            - (1 # 139) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 89 => ((2505 # 142) - (783 # 89) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (7 # 71) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (1 # 52) * s V_synth_full__tmp * max0(-1 + s V_synth_full_sb)
            - (2 # 103) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (883 # 101) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                       - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            + (1 # 104) * s V_synth_full__tmp * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full__tmp^2
            - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
            - (6 # 107) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
            + (9 # 59) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
            - (451 # 63) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
            - (0 # 1) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
            - (126 # 97) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
            - (1 # 141) * s V_synth_full_ch * max0(15 - s V_synth_full_sb)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (413 # 94) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full_ch^2
            + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
            + (5 # 131) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
            - (1 # 15) * s V_synth_full_s * max0(s V_synth_full__tmp)
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
            + (1 # 4) * s V_synth_full_s * max0(s V_synth_full_sb)
            - (11 # 14) * s V_synth_full_sb
            + (3 # 68) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
            + (1 # 51) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            - (41 # 86) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
            + (65 # 84) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            + (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
            + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
            + (43 # 81) * s V_synth_full_sb * max0(s V_synth_full__tmp)
            - (74 # 117) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            + (0 # 1) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
            - (21 # 82) * s V_synth_full_sb * max0(s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            - (19 # 62) * s V_synth_full_sb * max0(s V_synth_full_ch)
            + (22 # 97) * s V_synth_full_sb * max0(s V_synth_full_s)
            + (1 # 152) * s V_synth_full_sb * max0(s V_synth_full_sb)
            + s V_synth_full_z - (39 # 73) * max0(-1 + s V_synth_full__tmp)
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (7 # 71) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                               + s V_synth_full__tmp1
                                                               - s V_synth_full_s)
            + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                               - s V_synth_full_sb)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (49 # 5) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (233 # 58) * max0(-1 + s V_synth_full__tmp1)
            - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                + s V_synth_full_sb)
            - (13 # 95) * max0(-1 + s V_synth_full__tmp1) * max0(15
                                                                 - s V_synth_full_sb)
            - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
            + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                  - s V_synth_full_ch)
            + (177 # 199) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s)
            - (1 # 150) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(15
                                                                    - s V_synth_full_sb)
            + (3 # 94) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp)
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1 # 31) * max0(-1 + s V_synth_full_sb)
            + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                             - s V_synth_full_ch)
            + (4 # 13) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
            - (1 # 122) * max0(-1 + s V_synth_full_sb)^2
            - (7 # 59) * max0(15 - s V_synth_full_sb)
            - (1 # 141) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
            + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (1 # 150) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
            + (73 # 158) * max0(s V_synth_full__tmp)
            - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (519 # 118) * max0(s V_synth_full__tmp - s V_synth_full_ch)
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (451 # 63) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            - (19 # 4) * max0(s V_synth_full__tmp1)
            + (0 # 1) * max0(s V_synth_full__tmp1) * max0(s V_synth_full_sb)
            - (6 # 73) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
            + (413 # 94) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_ch)
            - (1 # 60) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
            + (593 # 59) * max0(s V_synth_full_ch)
            + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (74 # 101) * max0(s V_synth_full_s)
            + (7 # 89) * max0(s V_synth_full_sb)
            - (1 # 139) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 90 => ((2505 # 142) - (783 # 89) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (7 # 71) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (1 # 52) * s V_synth_full__tmp * max0(-1 + s V_synth_full_sb)
            - (2 # 103) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (883 # 101) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                       - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            + (1 # 104) * s V_synth_full__tmp * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full__tmp^2
            - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
            - (6 # 107) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
            + (9 # 59) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
            - (451 # 63) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
            - (0 # 1) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
            - (126 # 97) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
            - (1 # 141) * s V_synth_full_ch * max0(15 - s V_synth_full_sb)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (413 # 94) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full_ch^2
            + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
            + (5 # 131) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
            - (1 # 15) * s V_synth_full_s * max0(s V_synth_full__tmp)
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
            + (1 # 4) * s V_synth_full_s * max0(s V_synth_full_sb)
            - (11 # 14) * s V_synth_full_sb
            + (3 # 68) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
            + (1 # 51) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            - (41 # 86) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
            + (65 # 84) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            + (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
            + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
            + (43 # 81) * s V_synth_full_sb * max0(s V_synth_full__tmp)
            - (74 # 117) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            + (0 # 1) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
            - (21 # 82) * s V_synth_full_sb * max0(s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            - (19 # 62) * s V_synth_full_sb * max0(s V_synth_full_ch)
            + (22 # 97) * s V_synth_full_sb * max0(s V_synth_full_s)
            + (1 # 152) * s V_synth_full_sb * max0(s V_synth_full_sb)
            + s V_synth_full_z - (39 # 73) * max0(-1 + s V_synth_full__tmp)
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (7 # 71) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                               + s V_synth_full__tmp1
                                                               - s V_synth_full_s)
            + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                               - s V_synth_full_sb)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (49 # 5) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (233 # 58) * max0(-1 + s V_synth_full__tmp1)
            - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                + s V_synth_full_sb)
            - (13 # 95) * max0(-1 + s V_synth_full__tmp1) * max0(15
                                                                 - s V_synth_full_sb)
            - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
            + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                  - s V_synth_full_ch)
            + (177 # 199) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s)
            - (1 # 150) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(15
                                                                    - s V_synth_full_sb)
            + (3 # 94) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp)
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1 # 31) * max0(-1 + s V_synth_full_sb)
            + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                             - s V_synth_full_ch)
            + (4 # 13) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
            - (1 # 122) * max0(-1 + s V_synth_full_sb)^2
            - (7 # 59) * max0(15 - s V_synth_full_sb)
            - (1 # 141) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
            + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (1 # 150) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
            + (73 # 158) * max0(s V_synth_full__tmp)
            - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (519 # 118) * max0(s V_synth_full__tmp - s V_synth_full_ch)
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (451 # 63) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            - (19 # 4) * max0(s V_synth_full__tmp1)
            + (0 # 1) * max0(s V_synth_full__tmp1) * max0(s V_synth_full_sb)
            - (6 # 73) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
            + (413 # 94) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_ch)
            - (1 # 60) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
            + (593 # 59) * max0(s V_synth_full_ch)
            + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (74 # 101) * max0(s V_synth_full_s)
            + (7 # 89) * max0(s V_synth_full_sb)
            - (1 # 139) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 91 => ((2505 # 142) - (783 # 89) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (7 # 71) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (1 # 52) * s V_synth_full__tmp * max0(-1 + s V_synth_full_sb)
            - (2 # 103) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (883 # 101) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                       - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            + (1 # 104) * s V_synth_full__tmp * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full__tmp^2
            - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
            - (6 # 107) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
            + (9 # 59) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
            - (451 # 63) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
            - (0 # 1) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
            - (126 # 97) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
            - (1 # 141) * s V_synth_full_ch * max0(15 - s V_synth_full_sb)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (413 # 94) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full_ch^2
            + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
            + (5 # 131) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
            - (1 # 15) * s V_synth_full_s * max0(s V_synth_full__tmp)
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
            + (1 # 4) * s V_synth_full_s * max0(s V_synth_full_sb)
            - (11 # 14) * s V_synth_full_sb
            + (3 # 68) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
            + (1 # 51) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            - (41 # 86) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
            + (65 # 84) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            + (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
            + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
            + (43 # 81) * s V_synth_full_sb * max0(s V_synth_full__tmp)
            - (74 # 117) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            + (0 # 1) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
            - (21 # 82) * s V_synth_full_sb * max0(s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            - (19 # 62) * s V_synth_full_sb * max0(s V_synth_full_ch)
            + (22 # 97) * s V_synth_full_sb * max0(s V_synth_full_s)
            + (1 # 152) * s V_synth_full_sb * max0(s V_synth_full_sb)
            + s V_synth_full_z - (39 # 73) * max0(-1 + s V_synth_full__tmp)
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (7 # 71) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                               + s V_synth_full__tmp1
                                                               - s V_synth_full_s)
            + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                               - s V_synth_full_sb)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (49 # 5) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (233 # 58) * max0(-1 + s V_synth_full__tmp1)
            - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                + s V_synth_full_sb)
            - (13 # 95) * max0(-1 + s V_synth_full__tmp1) * max0(15
                                                                 - s V_synth_full_sb)
            - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
            + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                  - s V_synth_full_ch)
            + (177 # 199) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s)
            - (1 # 150) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(15
                                                                    - s V_synth_full_sb)
            + (3 # 94) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp)
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1 # 31) * max0(-1 + s V_synth_full_sb)
            + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                             - s V_synth_full_ch)
            + (4 # 13) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
            - (1 # 122) * max0(-1 + s V_synth_full_sb)^2
            - (7 # 59) * max0(15 - s V_synth_full_sb)
            - (1 # 141) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
            + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (1 # 150) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
            + (73 # 158) * max0(s V_synth_full__tmp)
            - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (519 # 118) * max0(s V_synth_full__tmp - s V_synth_full_ch)
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (451 # 63) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            - (19 # 4) * max0(s V_synth_full__tmp1)
            + (0 # 1) * max0(s V_synth_full__tmp1) * max0(s V_synth_full_sb)
            - (6 # 73) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
            + (413 # 94) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_ch)
            - (1 # 60) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
            + (593 # 59) * max0(s V_synth_full_ch)
            + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (74 # 101) * max0(s V_synth_full_s)
            + (7 # 89) * max0(s V_synth_full_sb)
            - (1 # 139) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 92 => ((2505 # 142) - (783 # 89) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (7 # 71) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (1 # 52) * s V_synth_full__tmp * max0(-1 + s V_synth_full_sb)
            - (2 # 103) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (883 # 101) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                       - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            + (1 # 104) * s V_synth_full__tmp * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full__tmp^2
            - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
            - (6 # 107) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
            + (9 # 59) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
            - (451 # 63) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
            - (0 # 1) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
            - (126 # 97) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
            - (1 # 141) * s V_synth_full_ch * max0(15 - s V_synth_full_sb)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (413 # 94) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full_ch^2
            + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
            + (5 # 131) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
            - (1 # 15) * s V_synth_full_s * max0(s V_synth_full__tmp)
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
            + (1 # 4) * s V_synth_full_s * max0(s V_synth_full_sb)
            - (11 # 14) * s V_synth_full_sb
            + (3 # 68) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
            + (1 # 51) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            - (41 # 86) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
            + (65 # 84) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            + (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
            + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
            + (43 # 81) * s V_synth_full_sb * max0(s V_synth_full__tmp)
            - (74 # 117) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            + (0 # 1) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
            - (21 # 82) * s V_synth_full_sb * max0(s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            - (19 # 62) * s V_synth_full_sb * max0(s V_synth_full_ch)
            + (22 # 97) * s V_synth_full_sb * max0(s V_synth_full_s)
            + (1 # 152) * s V_synth_full_sb * max0(s V_synth_full_sb)
            + s V_synth_full_z - (39 # 73) * max0(-1 + s V_synth_full__tmp)
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (7 # 71) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                               + s V_synth_full__tmp1
                                                               - s V_synth_full_s)
            + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                               - s V_synth_full_sb)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (49 # 5) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (233 # 58) * max0(-1 + s V_synth_full__tmp1)
            - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                + s V_synth_full_sb)
            - (13 # 95) * max0(-1 + s V_synth_full__tmp1) * max0(15
                                                                 - s V_synth_full_sb)
            - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
            + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                  - s V_synth_full_ch)
            + (177 # 199) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s)
            - (1 # 150) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(15
                                                                    - s V_synth_full_sb)
            + (3 # 94) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp)
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1 # 31) * max0(-1 + s V_synth_full_sb)
            + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                             - s V_synth_full_ch)
            + (4 # 13) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
            - (1 # 122) * max0(-1 + s V_synth_full_sb)^2
            - (7 # 59) * max0(15 - s V_synth_full_sb)
            - (1 # 141) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
            + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (1 # 150) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
            + (73 # 158) * max0(s V_synth_full__tmp)
            - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (519 # 118) * max0(s V_synth_full__tmp - s V_synth_full_ch)
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (451 # 63) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            - (19 # 4) * max0(s V_synth_full__tmp1)
            + (0 # 1) * max0(s V_synth_full__tmp1) * max0(s V_synth_full_sb)
            - (6 # 73) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
            + (413 # 94) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_ch)
            - (1 # 60) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
            + (593 # 59) * max0(s V_synth_full_ch)
            + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (74 # 101) * max0(s V_synth_full_s)
            + (7 # 89) * max0(s V_synth_full_sb)
            - (1 # 139) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 93 => ((2505 # 142) - (783 # 89) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (7 # 71) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (1 # 52) * s V_synth_full__tmp * max0(-1 + s V_synth_full_sb)
            - (2 # 103) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (883 # 101) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                       - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            + (1 # 104) * s V_synth_full__tmp * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full__tmp^2
            - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
            - (6 # 107) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
            + (9 # 59) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
            - (451 # 63) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
            - (0 # 1) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
            - (126 # 97) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
            - (1 # 141) * s V_synth_full_ch * max0(15 - s V_synth_full_sb)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (413 # 94) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full_ch^2
            + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
            + (5 # 131) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
            - (1 # 15) * s V_synth_full_s * max0(s V_synth_full__tmp)
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
            + (1 # 4) * s V_synth_full_s * max0(s V_synth_full_sb)
            - (11 # 14) * s V_synth_full_sb
            + (3 # 68) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
            + (1 # 51) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            - (41 # 86) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
            + (65 # 84) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            + (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
            + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
            + (43 # 81) * s V_synth_full_sb * max0(s V_synth_full__tmp)
            - (74 # 117) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            + (0 # 1) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
            - (21 # 82) * s V_synth_full_sb * max0(s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            - (19 # 62) * s V_synth_full_sb * max0(s V_synth_full_ch)
            + (22 # 97) * s V_synth_full_sb * max0(s V_synth_full_s)
            + (1 # 152) * s V_synth_full_sb * max0(s V_synth_full_sb)
            + s V_synth_full_z - (39 # 73) * max0(-1 + s V_synth_full__tmp)
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (7 # 71) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                               + s V_synth_full__tmp1
                                                               - s V_synth_full_s)
            + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                               - s V_synth_full_sb)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (49 # 5) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (233 # 58) * max0(-1 + s V_synth_full__tmp1)
            - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                + s V_synth_full_sb)
            - (13 # 95) * max0(-1 + s V_synth_full__tmp1) * max0(15
                                                                 - s V_synth_full_sb)
            - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
            + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                  - s V_synth_full_ch)
            + (177 # 199) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s)
            - (1 # 150) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(15
                                                                    - s V_synth_full_sb)
            + (3 # 94) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp)
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1 # 31) * max0(-1 + s V_synth_full_sb)
            + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                             - s V_synth_full_ch)
            + (4 # 13) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
            - (1 # 122) * max0(-1 + s V_synth_full_sb)^2
            - (7 # 59) * max0(15 - s V_synth_full_sb)
            - (1 # 141) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
            + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (1 # 150) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
            + (73 # 158) * max0(s V_synth_full__tmp)
            - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (519 # 118) * max0(s V_synth_full__tmp - s V_synth_full_ch)
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (451 # 63) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            - (19 # 4) * max0(s V_synth_full__tmp1)
            + (0 # 1) * max0(s V_synth_full__tmp1) * max0(s V_synth_full_sb)
            - (6 # 73) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
            + (413 # 94) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_ch)
            - (1 # 60) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
            + (593 # 59) * max0(s V_synth_full_ch)
            + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (74 # 101) * max0(s V_synth_full_s)
            + (7 # 89) * max0(s V_synth_full_sb)
            - (1 # 139) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 94 => ((2505 # 142) - (783 # 89) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (7 # 71) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (1 # 52) * s V_synth_full__tmp * max0(-1 + s V_synth_full_sb)
            - (2 # 103) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (883 # 101) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                       - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            + (1 # 104) * s V_synth_full__tmp * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full__tmp^2
            - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
            - (6 # 107) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
            + (9 # 59) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
            - (451 # 63) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
            - (0 # 1) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
            - (126 # 97) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
            - (1 # 141) * s V_synth_full_ch * max0(15 - s V_synth_full_sb)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (413 # 94) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full_ch^2
            + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
            + (5 # 131) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
            - (1 # 15) * s V_synth_full_s * max0(s V_synth_full__tmp)
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
            + (1 # 4) * s V_synth_full_s * max0(s V_synth_full_sb)
            - (11 # 14) * s V_synth_full_sb
            + (3 # 68) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
            + (1 # 51) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            - (41 # 86) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
            + (65 # 84) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            + (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
            + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
            + (43 # 81) * s V_synth_full_sb * max0(s V_synth_full__tmp)
            - (74 # 117) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            + (0 # 1) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
            - (21 # 82) * s V_synth_full_sb * max0(s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            - (19 # 62) * s V_synth_full_sb * max0(s V_synth_full_ch)
            + (22 # 97) * s V_synth_full_sb * max0(s V_synth_full_s)
            + (1 # 152) * s V_synth_full_sb * max0(s V_synth_full_sb)
            + s V_synth_full_z - (39 # 73) * max0(-1 + s V_synth_full__tmp)
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (7 # 71) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                               + s V_synth_full__tmp1
                                                               - s V_synth_full_s)
            + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                               - s V_synth_full_sb)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (49 # 5) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (233 # 58) * max0(-1 + s V_synth_full__tmp1)
            - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                + s V_synth_full_sb)
            - (13 # 95) * max0(-1 + s V_synth_full__tmp1) * max0(15
                                                                 - s V_synth_full_sb)
            - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
            + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                  - s V_synth_full_ch)
            + (177 # 199) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s)
            - (1 # 150) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(15
                                                                    - s V_synth_full_sb)
            + (3 # 94) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp)
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1 # 31) * max0(-1 + s V_synth_full_sb)
            + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                             - s V_synth_full_ch)
            + (4 # 13) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
            - (1 # 122) * max0(-1 + s V_synth_full_sb)^2
            - (7 # 59) * max0(15 - s V_synth_full_sb)
            - (1 # 141) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
            + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (1 # 150) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
            + (73 # 158) * max0(s V_synth_full__tmp)
            - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (519 # 118) * max0(s V_synth_full__tmp - s V_synth_full_ch)
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (451 # 63) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            - (19 # 4) * max0(s V_synth_full__tmp1)
            + (0 # 1) * max0(s V_synth_full__tmp1) * max0(s V_synth_full_sb)
            - (6 # 73) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
            + (413 # 94) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_ch)
            - (1 # 60) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
            + (593 # 59) * max0(s V_synth_full_ch)
            + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (74 # 101) * max0(s V_synth_full_s)
            + (7 # 89) * max0(s V_synth_full_sb)
            - (1 # 139) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 95 => ((2505 # 142) - (783 # 89) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (7 # 71) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (1 # 52) * s V_synth_full__tmp * max0(-1 + s V_synth_full_sb)
            - (2 # 103) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (883 # 101) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                       - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            + (1 # 104) * s V_synth_full__tmp * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full__tmp^2
            - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
            - (6 # 107) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
            + (9 # 59) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
            - (451 # 63) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
            - (0 # 1) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
            - (126 # 97) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
            - (1 # 141) * s V_synth_full_ch * max0(15 - s V_synth_full_sb)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (413 # 94) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full_ch^2
            + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
            + (5 # 131) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
            - (1 # 15) * s V_synth_full_s * max0(s V_synth_full__tmp)
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
            + (1 # 4) * s V_synth_full_s * max0(s V_synth_full_sb)
            - (11 # 14) * s V_synth_full_sb
            + (3 # 68) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
            + (1 # 51) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            - (41 # 86) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
            + (65 # 84) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            + (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
            + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
            + (43 # 81) * s V_synth_full_sb * max0(s V_synth_full__tmp)
            - (74 # 117) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            + (0 # 1) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
            - (21 # 82) * s V_synth_full_sb * max0(s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            - (19 # 62) * s V_synth_full_sb * max0(s V_synth_full_ch)
            + (22 # 97) * s V_synth_full_sb * max0(s V_synth_full_s)
            + (1 # 152) * s V_synth_full_sb * max0(s V_synth_full_sb)
            + s V_synth_full_z - (39 # 73) * max0(-1 + s V_synth_full__tmp)
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (7 # 71) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                               + s V_synth_full__tmp1
                                                               - s V_synth_full_s)
            + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                               - s V_synth_full_sb)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (49 # 5) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (233 # 58) * max0(-1 + s V_synth_full__tmp1)
            - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                + s V_synth_full_sb)
            - (13 # 95) * max0(-1 + s V_synth_full__tmp1) * max0(15
                                                                 - s V_synth_full_sb)
            - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
            + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                  - s V_synth_full_ch)
            + (177 # 199) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s)
            - (1 # 150) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(15
                                                                    - s V_synth_full_sb)
            + (3 # 94) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp)
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1 # 31) * max0(-1 + s V_synth_full_sb)
            + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                             - s V_synth_full_ch)
            + (4 # 13) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
            - (1 # 122) * max0(-1 + s V_synth_full_sb)^2
            - (7 # 59) * max0(15 - s V_synth_full_sb)
            - (1 # 141) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
            + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (1 # 150) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
            + (73 # 158) * max0(s V_synth_full__tmp)
            - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (519 # 118) * max0(s V_synth_full__tmp - s V_synth_full_ch)
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (451 # 63) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            - (19 # 4) * max0(s V_synth_full__tmp1)
            + (0 # 1) * max0(s V_synth_full__tmp1) * max0(s V_synth_full_sb)
            - (6 # 73) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
            + (413 # 94) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_ch)
            - (1 # 60) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
            + (593 # 59) * max0(s V_synth_full_ch)
            + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (74 # 101) * max0(s V_synth_full_s)
            + (7 # 89) * max0(s V_synth_full_sb)
            - (1 # 139) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 96 => ((2505 # 142) - (783 # 89) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (7 # 71) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (1 # 52) * s V_synth_full__tmp * max0(-1 + s V_synth_full_sb)
            - (2 # 103) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (883 # 101) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                       - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            + (1 # 104) * s V_synth_full__tmp * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full__tmp^2
            - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
            - (6 # 107) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
            + (9 # 59) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
            - (451 # 63) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
            - (0 # 1) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
            - (126 # 97) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
            - (1 # 141) * s V_synth_full_ch * max0(15 - s V_synth_full_sb)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (413 # 94) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full_ch^2
            + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
            + (5 # 131) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
            - (1 # 15) * s V_synth_full_s * max0(s V_synth_full__tmp)
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
            + (1 # 4) * s V_synth_full_s * max0(s V_synth_full_sb)
            - (11 # 14) * s V_synth_full_sb
            + (3 # 68) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
            + (1 # 51) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            - (41 # 86) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
            + (65 # 84) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            + (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
            + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
            + (43 # 81) * s V_synth_full_sb * max0(s V_synth_full__tmp)
            - (74 # 117) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            + (0 # 1) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
            - (21 # 82) * s V_synth_full_sb * max0(s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            - (19 # 62) * s V_synth_full_sb * max0(s V_synth_full_ch)
            + (22 # 97) * s V_synth_full_sb * max0(s V_synth_full_s)
            + (1 # 152) * s V_synth_full_sb * max0(s V_synth_full_sb)
            + s V_synth_full_z - (39 # 73) * max0(-1 + s V_synth_full__tmp)
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (7 # 71) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                               + s V_synth_full__tmp1
                                                               - s V_synth_full_s)
            + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                               - s V_synth_full_sb)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (49 # 5) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (233 # 58) * max0(-1 + s V_synth_full__tmp1)
            - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                + s V_synth_full_sb)
            - (13 # 95) * max0(-1 + s V_synth_full__tmp1) * max0(15
                                                                 - s V_synth_full_sb)
            - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
            + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                  - s V_synth_full_ch)
            + (177 # 199) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s)
            - (1 # 150) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(15
                                                                    - s V_synth_full_sb)
            + (3 # 94) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp)
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1 # 31) * max0(-1 + s V_synth_full_sb)
            + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                             - s V_synth_full_ch)
            + (4 # 13) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
            - (1 # 122) * max0(-1 + s V_synth_full_sb)^2
            - (7 # 59) * max0(15 - s V_synth_full_sb)
            - (1 # 141) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
            + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (1 # 150) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
            + (73 # 158) * max0(s V_synth_full__tmp)
            - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (519 # 118) * max0(s V_synth_full__tmp - s V_synth_full_ch)
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (451 # 63) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            - (19 # 4) * max0(s V_synth_full__tmp1)
            + (0 # 1) * max0(s V_synth_full__tmp1) * max0(s V_synth_full_sb)
            - (6 # 73) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
            + (413 # 94) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_ch)
            - (1 # 60) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
            + (593 # 59) * max0(s V_synth_full_ch)
            + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (74 # 101) * max0(s V_synth_full_s)
            + (7 # 89) * max0(s V_synth_full_sb)
            - (1 # 139) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 97 => ((2505 # 142) - (783 # 89) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (7 # 71) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (1 # 52) * s V_synth_full__tmp * max0(-1 + s V_synth_full_sb)
            - (2 # 103) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (883 # 101) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                       - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            + (1 # 104) * s V_synth_full__tmp * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full__tmp^2
            - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
            - (6 # 107) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
            + (9 # 59) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
            - (451 # 63) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
            - (0 # 1) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
            - (126 # 97) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
            - (1 # 141) * s V_synth_full_ch * max0(15 - s V_synth_full_sb)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (413 # 94) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full_ch^2
            + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
            + (5 # 131) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
            - (1 # 15) * s V_synth_full_s * max0(s V_synth_full__tmp)
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
            + (1 # 4) * s V_synth_full_s * max0(s V_synth_full_sb)
            - (11 # 14) * s V_synth_full_sb
            + (3 # 68) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
            + (1 # 51) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            - (41 # 86) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
            + (65 # 84) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            + (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
            + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
            + (43 # 81) * s V_synth_full_sb * max0(s V_synth_full__tmp)
            - (74 # 117) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            + (0 # 1) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
            - (21 # 82) * s V_synth_full_sb * max0(s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            - (19 # 62) * s V_synth_full_sb * max0(s V_synth_full_ch)
            + (22 # 97) * s V_synth_full_sb * max0(s V_synth_full_s)
            + (1 # 152) * s V_synth_full_sb * max0(s V_synth_full_sb)
            + s V_synth_full_z - (39 # 73) * max0(-1 + s V_synth_full__tmp)
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (7 # 71) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                               + s V_synth_full__tmp1
                                                               - s V_synth_full_s)
            + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                               - s V_synth_full_sb)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (49 # 5) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (233 # 58) * max0(-1 + s V_synth_full__tmp1)
            - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                + s V_synth_full_sb)
            - (13 # 95) * max0(-1 + s V_synth_full__tmp1) * max0(15
                                                                 - s V_synth_full_sb)
            - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
            + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                  - s V_synth_full_ch)
            + (177 # 199) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s)
            - (1 # 150) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(15
                                                                    - s V_synth_full_sb)
            + (3 # 94) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp)
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1 # 31) * max0(-1 + s V_synth_full_sb)
            + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                             - s V_synth_full_ch)
            + (4 # 13) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
            - (1 # 122) * max0(-1 + s V_synth_full_sb)^2
            - (7 # 59) * max0(15 - s V_synth_full_sb)
            - (1 # 141) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
            + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (1 # 150) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
            + (73 # 158) * max0(s V_synth_full__tmp)
            - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (519 # 118) * max0(s V_synth_full__tmp - s V_synth_full_ch)
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (451 # 63) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            - (19 # 4) * max0(s V_synth_full__tmp1)
            + (0 # 1) * max0(s V_synth_full__tmp1) * max0(s V_synth_full_sb)
            - (6 # 73) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
            + (413 # 94) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_ch)
            - (1 # 60) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
            + (593 # 59) * max0(s V_synth_full_ch)
            + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (74 # 101) * max0(s V_synth_full_s)
            + (7 # 89) * max0(s V_synth_full_sb)
            - (1 # 139) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 98 => ((2505 # 142) - (783 # 89) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (7 # 71) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (1 # 52) * s V_synth_full__tmp * max0(-1 + s V_synth_full_sb)
            - (2 # 103) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (883 # 101) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                       - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            + (1 # 104) * s V_synth_full__tmp * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full__tmp^2
            - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
            - (6 # 107) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
            + (9 # 59) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
            - (451 # 63) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
            - (0 # 1) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
            - (126 # 97) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
            - (1 # 141) * s V_synth_full_ch * max0(15 - s V_synth_full_sb)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (413 # 94) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full_ch^2
            + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
            + (5 # 131) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
            - (1 # 15) * s V_synth_full_s * max0(s V_synth_full__tmp)
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
            + (1 # 4) * s V_synth_full_s * max0(s V_synth_full_sb)
            - (11 # 14) * s V_synth_full_sb
            + (3 # 68) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
            + (1 # 51) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            - (41 # 86) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
            + (65 # 84) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            + (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
            + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
            + (43 # 81) * s V_synth_full_sb * max0(s V_synth_full__tmp)
            - (74 # 117) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            + (0 # 1) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
            - (21 # 82) * s V_synth_full_sb * max0(s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            - (19 # 62) * s V_synth_full_sb * max0(s V_synth_full_ch)
            + (22 # 97) * s V_synth_full_sb * max0(s V_synth_full_s)
            + (1 # 152) * s V_synth_full_sb * max0(s V_synth_full_sb)
            + s V_synth_full_z - (39 # 73) * max0(-1 + s V_synth_full__tmp)
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (7 # 71) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                               + s V_synth_full__tmp1
                                                               - s V_synth_full_s)
            + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                               - s V_synth_full_sb)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (49 # 5) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (233 # 58) * max0(-1 + s V_synth_full__tmp1)
            - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                + s V_synth_full_sb)
            - (13 # 95) * max0(-1 + s V_synth_full__tmp1) * max0(15
                                                                 - s V_synth_full_sb)
            - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
            + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                  - s V_synth_full_ch)
            + (177 # 199) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s)
            - (1 # 150) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(15
                                                                    - s V_synth_full_sb)
            + (3 # 94) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp)
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1 # 31) * max0(-1 + s V_synth_full_sb)
            + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                             - s V_synth_full_ch)
            + (4 # 13) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
            - (1 # 122) * max0(-1 + s V_synth_full_sb)^2
            - (7 # 59) * max0(15 - s V_synth_full_sb)
            - (1 # 141) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
            + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (1 # 150) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
            + (73 # 158) * max0(s V_synth_full__tmp)
            - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (519 # 118) * max0(s V_synth_full__tmp - s V_synth_full_ch)
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (451 # 63) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            - (19 # 4) * max0(s V_synth_full__tmp1)
            + (0 # 1) * max0(s V_synth_full__tmp1) * max0(s V_synth_full_sb)
            - (6 # 73) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
            + (413 # 94) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_ch)
            - (1 # 60) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
            + (593 # 59) * max0(s V_synth_full_ch)
            + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (74 # 101) * max0(s V_synth_full_s)
            + (7 # 89) * max0(s V_synth_full_sb)
            - (1 # 139) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 99 => ((2505 # 142) - (783 # 89) * s V_synth_full__tmp
            + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
            + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
            + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                       + s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (7 # 71) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (1 # 52) * s V_synth_full__tmp * max0(-1 + s V_synth_full_sb)
            - (2 # 103) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
            - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
            + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
            - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
            + (883 # 101) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                       - s V_synth_full_s)
            + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
            + (1 # 104) * s V_synth_full__tmp * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full__tmp^2
            - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
            - (6 # 107) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
            + (9 # 59) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
            - (451 # 63) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
            - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
            - (0 # 1) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
            - (126 # 97) * s V_synth_full_ch
            - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
            - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
            - (1 # 141) * s V_synth_full_ch * max0(15 - s V_synth_full_sb)
            + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
            - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
            + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
            - (413 # 94) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
            - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
            - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
            - (21 # 64) * s V_synth_full_ch^2
            + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
            + (5 # 131) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
            - (1 # 15) * s V_synth_full_s * max0(s V_synth_full__tmp)
            - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
            + (1 # 4) * s V_synth_full_s * max0(s V_synth_full_sb)
            - (11 # 14) * s V_synth_full_sb
            + (3 # 68) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
            + (1 # 51) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp
                                                  - s V_synth_full_ch)
            - (41 # 86) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
            + (65 # 84) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            + (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
            + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
            + (43 # 81) * s V_synth_full_sb * max0(s V_synth_full__tmp)
            - (74 # 117) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
            + (0 # 1) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
            - (21 # 82) * s V_synth_full_sb * max0(s V_synth_full__tmp1
                                                   - s V_synth_full_s)
            - (19 # 62) * s V_synth_full_sb * max0(s V_synth_full_ch)
            + (22 # 97) * s V_synth_full_sb * max0(s V_synth_full_s)
            + (1 # 152) * s V_synth_full_sb * max0(s V_synth_full_sb)
            + s V_synth_full_z - (39 # 73) * max0(-1 + s V_synth_full__tmp)
            - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp
                                                                - s V_synth_full_ch)
            + (7 # 71) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                               + s V_synth_full__tmp1
                                                               - s V_synth_full_s)
            + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                               - s V_synth_full_sb)
            + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
            + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                  - s V_synth_full_s)
            + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (49 # 5) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                  - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
            + (233 # 58) * max0(-1 + s V_synth_full__tmp1)
            - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                + s V_synth_full_sb)
            - (13 # 95) * max0(-1 + s V_synth_full__tmp1) * max0(15
                                                                 - s V_synth_full_sb)
            - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
            + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                  - s V_synth_full_ch)
            + (177 # 199) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s)
            - (1 # 150) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(15
                                                                    - s V_synth_full_sb)
            + (3 # 94) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp)
            + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
            + (1 # 31) * max0(-1 + s V_synth_full_sb)
            + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                             - s V_synth_full_ch)
            + (4 # 13) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
            - (1 # 122) * max0(-1 + s V_synth_full_sb)^2
            - (7 # 59) * max0(15 - s V_synth_full_sb)
            - (1 # 141) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
            + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
            + (1 # 150) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
            - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
            + (73 # 158) * max0(s V_synth_full__tmp)
            - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
            - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
            + (36 # 97) * max0(s V_synth_full__tmp)^2
            + (519 # 118) * max0(s V_synth_full__tmp - s V_synth_full_ch)
            + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
            + (451 # 63) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
            + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
            + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
            - (19 # 4) * max0(s V_synth_full__tmp1)
            + (0 # 1) * max0(s V_synth_full__tmp1) * max0(s V_synth_full_sb)
            - (6 # 73) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
            + (413 # 94) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_ch)
            - (1 # 60) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
            + (593 # 59) * max0(s V_synth_full_ch)
            + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
            + (74 # 101) * max0(s V_synth_full_s)
            + (7 # 89) * max0(s V_synth_full_sb)
            - (1 # 139) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 100 => ((2505 # 142) - (783 # 89) * s V_synth_full__tmp
             + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
             + (36 # 97) * s V_synth_full__tmp * max0(-1
                                                      + s V_synth_full__tmp)
             + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                        + s V_synth_full__tmp
                                                        - s V_synth_full_ch)
             - (7 # 71) * s V_synth_full__tmp * max0(-1
                                                     + s V_synth_full__tmp1
                                                     - s V_synth_full_s)
             - (1 # 52) * s V_synth_full__tmp * max0(-1 + s V_synth_full_sb)
             - (2 # 103) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
             - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
             + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                      - s V_synth_full_ch)
             - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
             + (883 # 101) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                        - s V_synth_full_s)
             + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
             + (1 # 104) * s V_synth_full__tmp * max0(s V_synth_full_sb)
             - (21 # 64) * s V_synth_full__tmp^2
             - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
             - (6 # 107) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
             + (9 # 59) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
             - (451 # 63) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                        - s V_synth_full_ch)
             - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
             - (0 # 1) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
             - (126 # 97) * s V_synth_full_ch
             - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
             - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                    - s V_synth_full_ch)
             - (17 # 59) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
             - (1 # 141) * s V_synth_full_ch * max0(15 - s V_synth_full_sb)
             + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
             - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
             + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
             - (413 # 94) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                     - s V_synth_full_s)
             - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
             - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
             - (21 # 64) * s V_synth_full_ch^2
             + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
             + (5 # 131) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
             - (1 # 15) * s V_synth_full_s * max0(s V_synth_full__tmp)
             - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
             + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
             + (1 # 4) * s V_synth_full_s * max0(s V_synth_full_sb)
             - (11 # 14) * s V_synth_full_sb
             + (3 # 68) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
             + (1 # 51) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
             - (41 # 86) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
             + (65 # 84) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1
                                                    - s V_synth_full_s)
             + (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
             + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
             + (43 # 81) * s V_synth_full_sb * max0(s V_synth_full__tmp)
             - (74 # 117) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
             + (0 # 1) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
             - (21 # 82) * s V_synth_full_sb * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
             - (19 # 62) * s V_synth_full_sb * max0(s V_synth_full_ch)
             + (22 # 97) * s V_synth_full_sb * max0(s V_synth_full_s)
             + (1 # 152) * s V_synth_full_sb * max0(s V_synth_full_sb)
             + s V_synth_full_z - (39 # 73) * max0(-1 + s V_synth_full__tmp)
             - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                 + s V_synth_full__tmp
                                                                 - s V_synth_full_ch)
             + (7 # 71) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp1
                                                                - s V_synth_full_s)
             + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                                - s V_synth_full_sb)
             + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
             + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
             - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                   - 
                                                                   s V_synth_full_s)
             + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
             + (49 # 5) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
             - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
             + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                   - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
             - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                   - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                               - s V_synth_full_s)
             - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
             + (233 # 58) * max0(-1 + s V_synth_full__tmp1)
             - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                 + s V_synth_full_sb)
             - (13 # 95) * max0(-1 + s V_synth_full__tmp1) * max0(15
                                                                  - s V_synth_full_sb)
             - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
             + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                   - 
                                                                   s V_synth_full_ch)
             + (177 # 199) * max0(-1 + s V_synth_full__tmp1
                                  - s V_synth_full_s)
             - (1 # 150) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(15
                                                                    - s V_synth_full_sb)
             + (3 # 94) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp)
             + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
             + (1 # 31) * max0(-1 + s V_synth_full_sb)
             + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
             + (4 # 13) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
             - (1 # 122) * max0(-1 + s V_synth_full_sb)^2
             - (7 # 59) * max0(15 - s V_synth_full_sb)
             - (1 # 141) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                               - s V_synth_full_ch)
             + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
             + (1 # 150) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                               - s V_synth_full_s)
             - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
             + (73 # 158) * max0(s V_synth_full__tmp)
             - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
             - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
             + (36 # 97) * max0(s V_synth_full__tmp)^2
             + (519 # 118) * max0(s V_synth_full__tmp - s V_synth_full_ch)
             + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
             + (451 # 63) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
             + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
             + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
             + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
             - (19 # 4) * max0(s V_synth_full__tmp1)
             + (0 # 1) * max0(s V_synth_full__tmp1) * max0(s V_synth_full_sb)
             - (6 # 73) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
             + (413 # 94) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_ch)
             - (1 # 60) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
             + (593 # 59) * max0(s V_synth_full_ch)
             + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
             + (74 # 101) * max0(s V_synth_full_s)
             + (7 # 89) * max0(s V_synth_full_sb)
             - (1 # 139) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 101 => ((2505 # 142) - (783 # 89) * s V_synth_full__tmp
             + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
             + (36 # 97) * s V_synth_full__tmp * max0(-1
                                                      + s V_synth_full__tmp)
             + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                        + s V_synth_full__tmp
                                                        - s V_synth_full_ch)
             - (7 # 71) * s V_synth_full__tmp * max0(-1
                                                     + s V_synth_full__tmp1
                                                     - s V_synth_full_s)
             - (1 # 52) * s V_synth_full__tmp * max0(-1 + s V_synth_full_sb)
             - (2 # 103) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
             - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
             + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                      - s V_synth_full_ch)
             - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
             + (883 # 101) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                        - s V_synth_full_s)
             + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
             + (1 # 104) * s V_synth_full__tmp * max0(s V_synth_full_sb)
             - (21 # 64) * s V_synth_full__tmp^2
             - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
             - (6 # 107) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
             + (9 # 59) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
             - (451 # 63) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                        - s V_synth_full_ch)
             - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
             - (0 # 1) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
             - (126 # 97) * s V_synth_full_ch
             - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
             - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                    - s V_synth_full_ch)
             - (17 # 59) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
             - (1 # 141) * s V_synth_full_ch * max0(15 - s V_synth_full_sb)
             + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
             - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
             + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
             - (413 # 94) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                     - s V_synth_full_s)
             - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
             - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
             - (21 # 64) * s V_synth_full_ch^2
             + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
             + (5 # 131) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
             - (1 # 15) * s V_synth_full_s * max0(s V_synth_full__tmp)
             - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
             + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
             + (1 # 4) * s V_synth_full_s * max0(s V_synth_full_sb)
             - (11 # 14) * s V_synth_full_sb
             + (3 # 68) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
             + (1 # 51) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
             - (41 # 86) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
             + (65 # 84) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1
                                                    - s V_synth_full_s)
             + (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
             + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
             + (43 # 81) * s V_synth_full_sb * max0(s V_synth_full__tmp)
             - (74 # 117) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
             + (0 # 1) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
             - (21 # 82) * s V_synth_full_sb * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
             - (19 # 62) * s V_synth_full_sb * max0(s V_synth_full_ch)
             + (22 # 97) * s V_synth_full_sb * max0(s V_synth_full_s)
             + (1 # 152) * s V_synth_full_sb * max0(s V_synth_full_sb)
             + s V_synth_full_z - (39 # 73) * max0(-1 + s V_synth_full__tmp)
             - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                 + s V_synth_full__tmp
                                                                 - s V_synth_full_ch)
             + (7 # 71) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp1
                                                                - s V_synth_full_s)
             + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                                - s V_synth_full_sb)
             + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
             + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
             - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                   - 
                                                                   s V_synth_full_s)
             + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
             + (49 # 5) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
             - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
             + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                   - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
             - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                   - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                               - s V_synth_full_s)
             - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
             + (233 # 58) * max0(-1 + s V_synth_full__tmp1)
             - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                 + s V_synth_full_sb)
             - (13 # 95) * max0(-1 + s V_synth_full__tmp1) * max0(15
                                                                  - s V_synth_full_sb)
             - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
             + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                   - 
                                                                   s V_synth_full_ch)
             + (177 # 199) * max0(-1 + s V_synth_full__tmp1
                                  - s V_synth_full_s)
             - (1 # 150) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(15
                                                                    - s V_synth_full_sb)
             + (3 # 94) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp)
             + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
             + (1 # 31) * max0(-1 + s V_synth_full_sb)
             + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
             + (4 # 13) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
             - (1 # 122) * max0(-1 + s V_synth_full_sb)^2
             - (7 # 59) * max0(15 - s V_synth_full_sb)
             - (1 # 141) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                               - s V_synth_full_ch)
             + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
             + (1 # 150) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                               - s V_synth_full_s)
             - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
             + (73 # 158) * max0(s V_synth_full__tmp)
             - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
             - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
             + (36 # 97) * max0(s V_synth_full__tmp)^2
             + (519 # 118) * max0(s V_synth_full__tmp - s V_synth_full_ch)
             + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
             + (451 # 63) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
             + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
             + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
             + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
             - (19 # 4) * max0(s V_synth_full__tmp1)
             + (0 # 1) * max0(s V_synth_full__tmp1) * max0(s V_synth_full_sb)
             - (6 # 73) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
             + (413 # 94) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_ch)
             - (1 # 60) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
             + (593 # 59) * max0(s V_synth_full_ch)
             + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
             + (74 # 101) * max0(s V_synth_full_s)
             + (7 # 89) * max0(s V_synth_full_sb)
             - (1 # 139) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 102 => ((2505 # 142) - (783 # 89) * s V_synth_full__tmp
             + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
             + (36 # 97) * s V_synth_full__tmp * max0(-1
                                                      + s V_synth_full__tmp)
             + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                        + s V_synth_full__tmp
                                                        - s V_synth_full_ch)
             - (7 # 71) * s V_synth_full__tmp * max0(-1
                                                     + s V_synth_full__tmp1
                                                     - s V_synth_full_s)
             - (1 # 52) * s V_synth_full__tmp * max0(-1 + s V_synth_full_sb)
             - (2 # 103) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
             - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
             + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                      - s V_synth_full_ch)
             - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
             + (883 # 101) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                        - s V_synth_full_s)
             + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
             + (1 # 104) * s V_synth_full__tmp * max0(s V_synth_full_sb)
             - (21 # 64) * s V_synth_full__tmp^2
             - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
             - (6 # 107) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
             + (9 # 59) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
             - (451 # 63) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                        - s V_synth_full_ch)
             - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
             - (0 # 1) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
             - (126 # 97) * s V_synth_full_ch
             - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
             - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                    - s V_synth_full_ch)
             - (17 # 59) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
             - (1 # 141) * s V_synth_full_ch * max0(15 - s V_synth_full_sb)
             + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
             - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
             + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
             - (413 # 94) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                     - s V_synth_full_s)
             - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
             - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
             - (21 # 64) * s V_synth_full_ch^2
             + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
             + (5 # 131) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
             - (1 # 15) * s V_synth_full_s * max0(s V_synth_full__tmp)
             - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
             + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
             + (1 # 4) * s V_synth_full_s * max0(s V_synth_full_sb)
             - (11 # 14) * s V_synth_full_sb
             + (3 # 68) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
             + (1 # 51) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
             - (41 # 86) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
             + (65 # 84) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1
                                                    - s V_synth_full_s)
             + (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
             + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
             + (43 # 81) * s V_synth_full_sb * max0(s V_synth_full__tmp)
             - (74 # 117) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
             + (0 # 1) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
             - (21 # 82) * s V_synth_full_sb * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
             - (19 # 62) * s V_synth_full_sb * max0(s V_synth_full_ch)
             + (22 # 97) * s V_synth_full_sb * max0(s V_synth_full_s)
             + (1 # 152) * s V_synth_full_sb * max0(s V_synth_full_sb)
             + s V_synth_full_z - (39 # 73) * max0(-1 + s V_synth_full__tmp)
             - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                 + s V_synth_full__tmp
                                                                 - s V_synth_full_ch)
             + (7 # 71) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp1
                                                                - s V_synth_full_s)
             + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                                - s V_synth_full_sb)
             + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
             + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
             - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                   - 
                                                                   s V_synth_full_s)
             + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
             + (49 # 5) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
             - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
             + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                   - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
             - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                   - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                               - s V_synth_full_s)
             - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
             + (233 # 58) * max0(-1 + s V_synth_full__tmp1)
             - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                 + s V_synth_full_sb)
             - (13 # 95) * max0(-1 + s V_synth_full__tmp1) * max0(15
                                                                  - s V_synth_full_sb)
             - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
             + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                   - 
                                                                   s V_synth_full_ch)
             + (177 # 199) * max0(-1 + s V_synth_full__tmp1
                                  - s V_synth_full_s)
             - (1 # 150) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(15
                                                                    - s V_synth_full_sb)
             + (3 # 94) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp)
             + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
             + (1 # 31) * max0(-1 + s V_synth_full_sb)
             + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
             + (4 # 13) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
             - (1 # 122) * max0(-1 + s V_synth_full_sb)^2
             - (7 # 59) * max0(15 - s V_synth_full_sb)
             - (1 # 141) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                               - s V_synth_full_ch)
             + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
             + (1 # 150) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                               - s V_synth_full_s)
             - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
             + (73 # 158) * max0(s V_synth_full__tmp)
             - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
             - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
             + (36 # 97) * max0(s V_synth_full__tmp)^2
             + (519 # 118) * max0(s V_synth_full__tmp - s V_synth_full_ch)
             + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
             + (451 # 63) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
             + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
             + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
             + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
             - (19 # 4) * max0(s V_synth_full__tmp1)
             + (0 # 1) * max0(s V_synth_full__tmp1) * max0(s V_synth_full_sb)
             - (6 # 73) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
             + (413 # 94) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_ch)
             - (1 # 60) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
             + (593 # 59) * max0(s V_synth_full_ch)
             + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
             + (74 # 101) * max0(s V_synth_full_s)
             + (7 # 89) * max0(s V_synth_full_sb)
             - (1 # 139) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 103 => ((2505 # 142) - (783 # 89) * s V_synth_full__tmp
             + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
             + (36 # 97) * s V_synth_full__tmp * max0(-1
                                                      + s V_synth_full__tmp)
             + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                        + s V_synth_full__tmp
                                                        - s V_synth_full_ch)
             - (7 # 71) * s V_synth_full__tmp * max0(-1
                                                     + s V_synth_full__tmp1
                                                     - s V_synth_full_s)
             - (1 # 52) * s V_synth_full__tmp * max0(-1 + s V_synth_full_sb)
             - (2 # 103) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
             - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
             + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                      - s V_synth_full_ch)
             - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
             + (883 # 101) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                        - s V_synth_full_s)
             + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
             + (1 # 104) * s V_synth_full__tmp * max0(s V_synth_full_sb)
             - (21 # 64) * s V_synth_full__tmp^2
             - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
             - (6 # 107) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
             + (9 # 59) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
             - (451 # 63) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                        - s V_synth_full_ch)
             - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
             - (0 # 1) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
             - (126 # 97) * s V_synth_full_ch
             - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
             - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                    - s V_synth_full_ch)
             - (17 # 59) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
             - (1 # 141) * s V_synth_full_ch * max0(15 - s V_synth_full_sb)
             + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
             - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
             + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
             - (413 # 94) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                     - s V_synth_full_s)
             - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
             - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
             - (21 # 64) * s V_synth_full_ch^2
             + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
             + (5 # 131) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
             - (1 # 15) * s V_synth_full_s * max0(s V_synth_full__tmp)
             - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
             + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
             + (1 # 4) * s V_synth_full_s * max0(s V_synth_full_sb)
             - (11 # 14) * s V_synth_full_sb
             + (3 # 68) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
             + (1 # 51) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
             - (41 # 86) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
             + (65 # 84) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1
                                                    - s V_synth_full_s)
             + (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
             + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
             + (43 # 81) * s V_synth_full_sb * max0(s V_synth_full__tmp)
             - (74 # 117) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
             + (0 # 1) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
             - (21 # 82) * s V_synth_full_sb * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
             - (19 # 62) * s V_synth_full_sb * max0(s V_synth_full_ch)
             + (22 # 97) * s V_synth_full_sb * max0(s V_synth_full_s)
             + (1 # 152) * s V_synth_full_sb * max0(s V_synth_full_sb)
             + s V_synth_full_z - (39 # 73) * max0(-1 + s V_synth_full__tmp)
             - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                 + s V_synth_full__tmp
                                                                 - s V_synth_full_ch)
             + (7 # 71) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp1
                                                                - s V_synth_full_s)
             + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                                - s V_synth_full_sb)
             + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
             + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
             - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                   - 
                                                                   s V_synth_full_s)
             + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
             + (49 # 5) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
             - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
             + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                   - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
             - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                   - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                               - s V_synth_full_s)
             - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
             + (233 # 58) * max0(-1 + s V_synth_full__tmp1)
             - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                 + s V_synth_full_sb)
             - (13 # 95) * max0(-1 + s V_synth_full__tmp1) * max0(15
                                                                  - s V_synth_full_sb)
             - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
             + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                   - 
                                                                   s V_synth_full_ch)
             + (177 # 199) * max0(-1 + s V_synth_full__tmp1
                                  - s V_synth_full_s)
             - (1 # 150) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(15
                                                                    - s V_synth_full_sb)
             + (3 # 94) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp)
             + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
             + (1 # 31) * max0(-1 + s V_synth_full_sb)
             + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
             + (4 # 13) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
             - (1 # 122) * max0(-1 + s V_synth_full_sb)^2
             - (7 # 59) * max0(15 - s V_synth_full_sb)
             - (1 # 141) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                               - s V_synth_full_ch)
             + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
             + (1 # 150) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                               - s V_synth_full_s)
             - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
             + (73 # 158) * max0(s V_synth_full__tmp)
             - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
             - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
             + (36 # 97) * max0(s V_synth_full__tmp)^2
             + (519 # 118) * max0(s V_synth_full__tmp - s V_synth_full_ch)
             + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
             + (451 # 63) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
             + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
             + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
             + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
             - (19 # 4) * max0(s V_synth_full__tmp1)
             + (0 # 1) * max0(s V_synth_full__tmp1) * max0(s V_synth_full_sb)
             - (6 # 73) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
             + (413 # 94) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_ch)
             - (1 # 60) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
             + (593 # 59) * max0(s V_synth_full_ch)
             + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
             + (74 # 101) * max0(s V_synth_full_s)
             + (7 # 89) * max0(s V_synth_full_sb)
             - (1 # 139) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 104 => ((2505 # 142) - (783 # 89) * s V_synth_full__tmp
             + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
             + (36 # 97) * s V_synth_full__tmp * max0(-1
                                                      + s V_synth_full__tmp)
             + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                        + s V_synth_full__tmp
                                                        - s V_synth_full_ch)
             - (7 # 71) * s V_synth_full__tmp * max0(-1
                                                     + s V_synth_full__tmp1
                                                     - s V_synth_full_s)
             - (1 # 52) * s V_synth_full__tmp * max0(-1 + s V_synth_full_sb)
             - (2 # 103) * s V_synth_full__tmp * max0(15 - s V_synth_full_sb)
             - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
             + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                      - s V_synth_full_ch)
             - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
             + (883 # 101) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                        - s V_synth_full_s)
             + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
             + (1 # 104) * s V_synth_full__tmp * max0(s V_synth_full_sb)
             - (21 # 64) * s V_synth_full__tmp^2
             - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
             - (6 # 107) * s V_synth_full__tmp1 * max0(15 - s V_synth_full_sb)
             + (9 # 59) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
             - (451 # 63) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                        - s V_synth_full_ch)
             - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
             - (0 # 1) * s V_synth_full__tmp1 * max0(s V_synth_full_sb)
             - (126 # 97) * s V_synth_full_ch
             - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
             - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                    - s V_synth_full_ch)
             - (17 # 59) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
             - (1 # 141) * s V_synth_full_ch * max0(15 - s V_synth_full_sb)
             + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
             - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
             + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
             - (413 # 94) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                     - s V_synth_full_s)
             - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
             - (17 # 59) * s V_synth_full_ch * max0(s V_synth_full_sb)
             - (21 # 64) * s V_synth_full_ch^2
             + (2 # 83) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
             + (5 # 131) * s V_synth_full_s * max0(15 - s V_synth_full_sb)
             - (1 # 15) * s V_synth_full_s * max0(s V_synth_full__tmp)
             - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
             + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
             + (1 # 4) * s V_synth_full_s * max0(s V_synth_full_sb)
             - (11 # 14) * s V_synth_full_sb
             + (3 # 68) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
             + (1 # 51) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
             - (41 # 86) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
             + (65 # 84) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1
                                                    - s V_synth_full_s)
             + (1 # 103) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
             + (0 # 1) * s V_synth_full_sb * max0(15 - s V_synth_full_sb)
             + (43 # 81) * s V_synth_full_sb * max0(s V_synth_full__tmp)
             - (74 # 117) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
             + (0 # 1) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
             - (21 # 82) * s V_synth_full_sb * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
             - (19 # 62) * s V_synth_full_sb * max0(s V_synth_full_ch)
             + (22 # 97) * s V_synth_full_sb * max0(s V_synth_full_s)
             + (1 # 152) * s V_synth_full_sb * max0(s V_synth_full_sb)
             + s V_synth_full_z - (39 # 73) * max0(-1 + s V_synth_full__tmp)
             - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                 + s V_synth_full__tmp
                                                                 - s V_synth_full_ch)
             + (7 # 71) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp1
                                                                - s V_synth_full_s)
             + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(15
                                                                - s V_synth_full_sb)
             + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
             + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
             - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                   - 
                                                                   s V_synth_full_s)
             + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
             + (49 # 5) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
             - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
             + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                   - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
             - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                   - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                               - s V_synth_full_s)
             - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
             + (233 # 58) * max0(-1 + s V_synth_full__tmp1)
             - (2 # 87) * max0(-1 + s V_synth_full__tmp1) * max0(-1
                                                                 + s V_synth_full_sb)
             - (13 # 95) * max0(-1 + s V_synth_full__tmp1) * max0(15
                                                                  - s V_synth_full_sb)
             - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
             + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                   - 
                                                                   s V_synth_full_ch)
             + (177 # 199) * max0(-1 + s V_synth_full__tmp1
                                  - s V_synth_full_s)
             - (1 # 150) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(15
                                                                    - s V_synth_full_sb)
             + (3 # 94) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp)
             + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
             + (1 # 31) * max0(-1 + s V_synth_full_sb)
             + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
             + (4 # 13) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
             - (1 # 122) * max0(-1 + s V_synth_full_sb)^2
             - (7 # 59) * max0(15 - s V_synth_full_sb)
             - (1 # 141) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                               - s V_synth_full_ch)
             + (16 # 83) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
             + (1 # 150) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                               - s V_synth_full_s)
             - (5 # 131) * max0(15 - s V_synth_full_sb) * max0(s V_synth_full_s)
             + (73 # 158) * max0(s V_synth_full__tmp)
             - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
             - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
             + (36 # 97) * max0(s V_synth_full__tmp)^2
             + (519 # 118) * max0(s V_synth_full__tmp - s V_synth_full_ch)
             + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
             + (451 # 63) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
             + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
             + (1 # 52) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_sb)
             + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
             - (19 # 4) * max0(s V_synth_full__tmp1)
             + (0 # 1) * max0(s V_synth_full__tmp1) * max0(s V_synth_full_sb)
             - (6 # 73) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
             + (413 # 94) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_ch)
             - (1 # 60) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_sb)
             + (593 # 59) * max0(s V_synth_full_ch)
             + (0 # 1) * max0(s V_synth_full_ch) * max0(s V_synth_full_sb)
             + (74 # 101) * max0(s V_synth_full_s)
             + (7 # 89) * max0(s V_synth_full_sb)
             - (1 # 139) * max0(s V_synth_full_sb)^2 <= z)%Q
   | 105 => ((1382 # 75) - (783 # 89) * s V_synth_full__tmp
             + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
             - (1 # 52) * s V_synth_full__tmp * max0(-2 + s V_synth_full_sb)
             + (36 # 97) * s V_synth_full__tmp * max0(-1
                                                      + s V_synth_full__tmp)
             + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                        + s V_synth_full__tmp
                                                        - s V_synth_full_ch)
             - (7 # 71) * s V_synth_full__tmp * max0(-1
                                                     + s V_synth_full__tmp1
                                                     - s V_synth_full_s)
             + (1 # 104) * s V_synth_full__tmp * max0(-1 + s V_synth_full_sb)
             - (2 # 103) * s V_synth_full__tmp * max0(16 - s V_synth_full_sb)
             - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
             + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                      - s V_synth_full_ch)
             - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
             + (883 # 101) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                        - s V_synth_full_s)
             + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
             - (21 # 64) * s V_synth_full__tmp^2
             - (0 # 1) * s V_synth_full__tmp1 * max0(-2 + s V_synth_full_sb)
             - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
             - (6 # 107) * s V_synth_full__tmp1 * max0(16 - s V_synth_full_sb)
             + (9 # 59) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
             - (451 # 63) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                        - s V_synth_full_ch)
             - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
             - (126 # 97) * s V_synth_full_ch
             - (17 # 59) * s V_synth_full_ch * max0(-2 + s V_synth_full_sb)
             - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
             - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                    - s V_synth_full_ch)
             - (17 # 59) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
             - (1 # 141) * s V_synth_full_ch * max0(16 - s V_synth_full_sb)
             + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
             - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
             + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
             - (413 # 94) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                     - s V_synth_full_s)
             - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
             - (21 # 64) * s V_synth_full_ch^2
             + (2 # 83) * s V_synth_full_s * max0(-2 + s V_synth_full_sb)
             + (1 # 4) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
             + (5 # 131) * s V_synth_full_s * max0(16 - s V_synth_full_sb)
             - (1 # 15) * s V_synth_full_s * max0(s V_synth_full__tmp)
             - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
             + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
             - (11 # 14) * s V_synth_full_sb
             + (1 # 103) * s V_synth_full_sb * max0(-2 + s V_synth_full_sb)
             + (3 # 68) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
             + (1 # 51) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
             - (41 # 86) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
             + (65 # 84) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1
                                                    - s V_synth_full_s)
             + (1 # 152) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
             + (0 # 1) * s V_synth_full_sb * max0(16 - s V_synth_full_sb)
             + (43 # 81) * s V_synth_full_sb * max0(s V_synth_full__tmp)
             - (74 # 117) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
             + (0 # 1) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
             - (21 # 82) * s V_synth_full_sb * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
             - (19 # 62) * s V_synth_full_sb * max0(s V_synth_full_ch)
             + (22 # 97) * s V_synth_full_sb * max0(s V_synth_full_s)
             + s V_synth_full_z + (3 # 133) * max0(-2 + s V_synth_full_sb)
             - (2 # 87) * max0(-2 + s V_synth_full_sb) * max0(-1
                                                              + s V_synth_full__tmp1)
             + (1 # 52) * max0(-2 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
             + (4 # 13) * max0(-2 + s V_synth_full_sb) * max0(s V_synth_full_ch)
             - (1 # 122) * max0(-2 + s V_synth_full_sb)^2
             - (48 # 83) * max0(-1 + s V_synth_full__tmp)
             - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                 + s V_synth_full__tmp
                                                                 - s V_synth_full_ch)
             + (7 # 71) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp1
                                                                - s V_synth_full_s)
             + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(16
                                                                - s V_synth_full_sb)
             + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
             + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
             - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                   - 
                                                                   s V_synth_full_s)
             + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
             + (1066 # 109) * max0(-1 + s V_synth_full__tmp
                                   - s V_synth_full_ch)
             - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
             + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                   - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
             - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                   - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                               - s V_synth_full_s)
             - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
             + (382 # 85) * max0(-1 + s V_synth_full__tmp1)
             - (13 # 95) * max0(-1 + s V_synth_full__tmp1) * max0(16
                                                                  - s V_synth_full_sb)
             - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
             + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                   - 
                                                                   s V_synth_full_ch)
             + (17 # 147) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s)
             - (1 # 150) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(16
                                                                    - s V_synth_full_sb)
             + (3 # 94) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp)
             + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
             + (7 # 97) * max0(-1 + s V_synth_full_sb)
             + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
             + (0 # 1) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp1)
             - (1 # 60) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
             + (0 # 1) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
             - (1 # 139) * max0(-1 + s V_synth_full_sb)^2
             - (11 # 92) * max0(16 - s V_synth_full_sb)
             - (1 # 141) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                               - s V_synth_full_ch)
             + (16 # 83) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
             + (1 # 150) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                               - s V_synth_full_s)
             - (5 # 131) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full_s)
             - (2 # 29) * max0(s V_synth_full__tmp)
             - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
             - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
             + (36 # 97) * max0(s V_synth_full__tmp)^2
             + (327 # 65) * max0(s V_synth_full__tmp - s V_synth_full_ch)
             + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
             + (451 # 63) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
             + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
             + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
             - (19 # 4) * max0(s V_synth_full__tmp1)
             + (4 # 23) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
             + (413 # 94) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_ch)
             + (145 # 14) * max0(s V_synth_full_ch)
             + (43 # 85) * max0(s V_synth_full_s) <= z)%Q
   | 106 => ((1382 # 75) - (783 # 89) * s V_synth_full__tmp
             + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
             - (1 # 52) * s V_synth_full__tmp * max0(-2 + s V_synth_full_sb)
             + (36 # 97) * s V_synth_full__tmp * max0(-1
                                                      + s V_synth_full__tmp)
             + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                        + s V_synth_full__tmp
                                                        - s V_synth_full_ch)
             - (7 # 71) * s V_synth_full__tmp * max0(-1
                                                     + s V_synth_full__tmp1
                                                     - s V_synth_full_s)
             + (1 # 104) * s V_synth_full__tmp * max0(-1 + s V_synth_full_sb)
             - (2 # 103) * s V_synth_full__tmp * max0(16 - s V_synth_full_sb)
             - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
             + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                      - s V_synth_full_ch)
             - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
             + (883 # 101) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                        - s V_synth_full_s)
             + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
             - (21 # 64) * s V_synth_full__tmp^2
             - (0 # 1) * s V_synth_full__tmp1 * max0(-2 + s V_synth_full_sb)
             - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
             - (6 # 107) * s V_synth_full__tmp1 * max0(16 - s V_synth_full_sb)
             + (9 # 59) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
             - (451 # 63) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                        - s V_synth_full_ch)
             - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
             - (126 # 97) * s V_synth_full_ch
             - (17 # 59) * s V_synth_full_ch * max0(-2 + s V_synth_full_sb)
             - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
             - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                    - s V_synth_full_ch)
             - (17 # 59) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
             - (1 # 141) * s V_synth_full_ch * max0(16 - s V_synth_full_sb)
             + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
             - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
             + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
             - (413 # 94) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                     - s V_synth_full_s)
             - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
             - (21 # 64) * s V_synth_full_ch^2
             + (2 # 83) * s V_synth_full_s * max0(-2 + s V_synth_full_sb)
             + (1 # 4) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
             + (5 # 131) * s V_synth_full_s * max0(16 - s V_synth_full_sb)
             - (1 # 15) * s V_synth_full_s * max0(s V_synth_full__tmp)
             - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
             + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
             - (11 # 14) * s V_synth_full_sb
             + (1 # 103) * s V_synth_full_sb * max0(-2 + s V_synth_full_sb)
             + (3 # 68) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
             + (1 # 51) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
             - (41 # 86) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
             + (65 # 84) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1
                                                    - s V_synth_full_s)
             + (1 # 152) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
             + (0 # 1) * s V_synth_full_sb * max0(16 - s V_synth_full_sb)
             + (43 # 81) * s V_synth_full_sb * max0(s V_synth_full__tmp)
             - (74 # 117) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
             + (0 # 1) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
             - (21 # 82) * s V_synth_full_sb * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
             - (19 # 62) * s V_synth_full_sb * max0(s V_synth_full_ch)
             + (22 # 97) * s V_synth_full_sb * max0(s V_synth_full_s)
             + s V_synth_full_z + (3 # 133) * max0(-2 + s V_synth_full_sb)
             - (2 # 87) * max0(-2 + s V_synth_full_sb) * max0(-1
                                                              + s V_synth_full__tmp1)
             + (1 # 52) * max0(-2 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
             + (4 # 13) * max0(-2 + s V_synth_full_sb) * max0(s V_synth_full_ch)
             - (1 # 122) * max0(-2 + s V_synth_full_sb)^2
             - (48 # 83) * max0(-1 + s V_synth_full__tmp)
             - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                 + s V_synth_full__tmp
                                                                 - s V_synth_full_ch)
             + (7 # 71) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp1
                                                                - s V_synth_full_s)
             + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(16
                                                                - s V_synth_full_sb)
             + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
             + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
             - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                   - 
                                                                   s V_synth_full_s)
             + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
             + (1066 # 109) * max0(-1 + s V_synth_full__tmp
                                   - s V_synth_full_ch)
             - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
             + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                   - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
             - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                   - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                               - s V_synth_full_s)
             - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
             + (382 # 85) * max0(-1 + s V_synth_full__tmp1)
             - (13 # 95) * max0(-1 + s V_synth_full__tmp1) * max0(16
                                                                  - s V_synth_full_sb)
             - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
             + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                   - 
                                                                   s V_synth_full_ch)
             + (17 # 147) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s)
             - (1 # 150) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(16
                                                                    - s V_synth_full_sb)
             + (3 # 94) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp)
             + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
             + (7 # 97) * max0(-1 + s V_synth_full_sb)
             + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
             + (0 # 1) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp1)
             - (1 # 60) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
             + (0 # 1) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
             - (1 # 139) * max0(-1 + s V_synth_full_sb)^2
             - (11 # 92) * max0(16 - s V_synth_full_sb)
             - (1 # 141) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                               - s V_synth_full_ch)
             + (16 # 83) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
             + (1 # 150) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                               - s V_synth_full_s)
             - (5 # 131) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full_s)
             - (2 # 29) * max0(s V_synth_full__tmp)
             - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
             - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
             + (36 # 97) * max0(s V_synth_full__tmp)^2
             + (327 # 65) * max0(s V_synth_full__tmp - s V_synth_full_ch)
             + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
             + (451 # 63) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
             + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
             + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
             - (19 # 4) * max0(s V_synth_full__tmp1)
             + (4 # 23) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
             + (413 # 94) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_ch)
             + (145 # 14) * max0(s V_synth_full_ch)
             + (43 # 85) * max0(s V_synth_full_s) <= z)%Q
   | 107 => ((1382 # 75) - (783 # 89) * s V_synth_full__tmp
             + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
             - (1 # 52) * s V_synth_full__tmp * max0(-2 + s V_synth_full_sb)
             + (36 # 97) * s V_synth_full__tmp * max0(-1
                                                      + s V_synth_full__tmp)
             + (371 # 142) * s V_synth_full__tmp * max0(-1
                                                        + s V_synth_full__tmp
                                                        - s V_synth_full_ch)
             - (7 # 71) * s V_synth_full__tmp * max0(-1
                                                     + s V_synth_full__tmp1
                                                     - s V_synth_full_s)
             + (1 # 104) * s V_synth_full__tmp * max0(-1 + s V_synth_full_sb)
             - (2 # 103) * s V_synth_full__tmp * max0(16 - s V_synth_full_sb)
             - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
             + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                                      - s V_synth_full_ch)
             - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
             + (883 # 101) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                        - s V_synth_full_s)
             + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
             - (21 # 64) * s V_synth_full__tmp^2
             - (0 # 1) * s V_synth_full__tmp1 * max0(-2 + s V_synth_full_sb)
             - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
             - (6 # 107) * s V_synth_full__tmp1 * max0(16 - s V_synth_full_sb)
             + (9 # 59) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
             - (451 # 63) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                        - s V_synth_full_ch)
             - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
             - (126 # 97) * s V_synth_full_ch
             - (17 # 59) * s V_synth_full_ch * max0(-2 + s V_synth_full_sb)
             - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
             - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                                    - s V_synth_full_ch)
             - (17 # 59) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
             - (1 # 141) * s V_synth_full_ch * max0(16 - s V_synth_full_sb)
             + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
             - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                                    - s V_synth_full_ch)
             + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
             - (413 # 94) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                                     - s V_synth_full_s)
             - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
             - (21 # 64) * s V_synth_full_ch^2
             + (2 # 83) * s V_synth_full_s * max0(-2 + s V_synth_full_sb)
             + (1 # 4) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
             + (5 # 131) * s V_synth_full_s * max0(16 - s V_synth_full_sb)
             - (1 # 15) * s V_synth_full_s * max0(s V_synth_full__tmp)
             - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                                   - s V_synth_full_ch)
             + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
             - (11 # 14) * s V_synth_full_sb
             + (1 # 103) * s V_synth_full_sb * max0(-2 + s V_synth_full_sb)
             + (3 # 68) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
             + (1 # 51) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp
                                                   - s V_synth_full_ch)
             - (41 # 86) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
             + (65 # 84) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1
                                                    - s V_synth_full_s)
             + (1 # 152) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
             + (0 # 1) * s V_synth_full_sb * max0(16 - s V_synth_full_sb)
             + (43 # 81) * s V_synth_full_sb * max0(s V_synth_full__tmp)
             - (74 # 117) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                                     - s V_synth_full_ch)
             + (0 # 1) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
             - (21 # 82) * s V_synth_full_sb * max0(s V_synth_full__tmp1
                                                    - s V_synth_full_s)
             - (19 # 62) * s V_synth_full_sb * max0(s V_synth_full_ch)
             + (22 # 97) * s V_synth_full_sb * max0(s V_synth_full_s)
             + s V_synth_full_z + (3 # 133) * max0(-2 + s V_synth_full_sb)
             - (2 # 87) * max0(-2 + s V_synth_full_sb) * max0(-1
                                                              + s V_synth_full__tmp1)
             + (1 # 52) * max0(-2 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
             + (4 # 13) * max0(-2 + s V_synth_full_sb) * max0(s V_synth_full_ch)
             - (1 # 122) * max0(-2 + s V_synth_full_sb)^2
             - (48 # 83) * max0(-1 + s V_synth_full__tmp)
             - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                 + s V_synth_full__tmp
                                                                 - s V_synth_full_ch)
             + (7 # 71) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                                + s V_synth_full__tmp1
                                                                - s V_synth_full_s)
             + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(16
                                                                - s V_synth_full_sb)
             + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
             + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
             - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                                   - 
                                                                   s V_synth_full_s)
             + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
             + (1066 # 109) * max0(-1 + s V_synth_full__tmp
                                   - s V_synth_full_ch)
             - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
             + (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                   - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
             - (1213 # 105) * max0(-1 + s V_synth_full__tmp
                                   - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                               - s V_synth_full_s)
             - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
             + (382 # 85) * max0(-1 + s V_synth_full__tmp1)
             - (13 # 95) * max0(-1 + s V_synth_full__tmp1) * max0(16
                                                                  - s V_synth_full_sb)
             - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
             + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                                   - 
                                                                   s V_synth_full_ch)
             + (17 # 147) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s)
             - (1 # 150) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(16
                                                                    - s V_synth_full_sb)
             + (3 # 94) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp)
             + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
             + (7 # 97) * max0(-1 + s V_synth_full_sb)
             + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                              - s V_synth_full_ch)
             + (0 # 1) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp1)
             - (1 # 60) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                              - s V_synth_full_s)
             + (0 # 1) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
             - (1 # 139) * max0(-1 + s V_synth_full_sb)^2
             - (11 # 92) * max0(16 - s V_synth_full_sb)
             - (1 # 141) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                               - s V_synth_full_ch)
             + (16 # 83) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
             + (1 # 150) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                               - s V_synth_full_s)
             - (5 # 131) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full_s)
             - (2 # 29) * max0(s V_synth_full__tmp)
             - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
             - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
             + (36 # 97) * max0(s V_synth_full__tmp)^2
             + (327 # 65) * max0(s V_synth_full__tmp - s V_synth_full_ch)
             + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
             + (451 # 63) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
             + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
             + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
             - (19 # 4) * max0(s V_synth_full__tmp1)
             + (4 # 23) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
             + (413 # 94) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_ch)
             + (145 # 14) * max0(s V_synth_full_ch)
             + (43 # 85) * max0(s V_synth_full_s) <= z)%Q
   | 108 => hints
     [(*0 0.00813581*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (-2
                                                                    + s V_synth_full_sb) (0))) (F_max0_ge_0 (-2
                                                                    + s V_synth_full_sb));
      (*-0.00713176 0*) F_binom_monotonic 2 (F_max0_ge_arg (s V_synth_full_sb)) (F_check_ge (s V_synth_full_sb) (0));
      (*-0.0241093 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                    + s V_synth_full_sb) (0))) (F_max0_ge_0 (-2
                                                                    + s V_synth_full_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp1
                                                                    - s V_synth_full_s)) (F_check_ge (0) (0)));
      (*-0.000884505 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-2
                                                                    + s V_synth_full_sb)) (F_check_ge (-2
                                                                    + s V_synth_full_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (15
                                                                    - s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.0105143 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-2
                                                                    + s V_synth_full_sb)) (F_check_ge (-2
                                                                    + s V_synth_full_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.0264765 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_synth_full__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (15
                                                                    - s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.00761459 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + s V_synth_full__tmp)) (F_check_ge (-1
                                                                    + s V_synth_full__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.0985466 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + s V_synth_full__tmp)) (F_check_ge (-1
                                                                    + s V_synth_full__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp1
                                                                    - s V_synth_full_s)) (F_check_ge (0) (0)));
      (*-0.612772 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_synth_full__tmp
                                                                    - s V_synth_full_ch) (0))) (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp
                                                                    - s V_synth_full_ch))) (F_binom_monotonic 1 (F_max0_ge_0 (-16
                                                                    + s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*0 0.000426754*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_synth_full__tmp
                                                                    - s V_synth_full_ch) (0))) (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp
                                                                    - s V_synth_full_ch))) (F_binom_monotonic 1 (F_max0_ge_0 (16
                                                                    - s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.522602 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_synth_full__tmp1) (0))) (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp1))) (F_binom_monotonic 1 (F_max0_ge_0 (-16
                                                                    + s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.136759 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    s V_synth_full__tmp1)) (F_check_ge (-1
                                                                    + s V_synth_full__tmp1) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (15
                                                                    - s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.0512668 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + s V_synth_full__tmp1)) (F_check_ge (-1
                                                                    + s V_synth_full__tmp1) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.0985466 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_synth_full__tmp1
                                                                    - s V_synth_full_s) (0))) (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp1
                                                                    - s V_synth_full_s))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp)) (F_check_ge (0) (0)));
      (*-0.522602 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    s V_synth_full__tmp1
                                                                    - 
                                                                    s V_synth_full_s)) (F_check_ge (-1
                                                                    + s V_synth_full__tmp1
                                                                    - s V_synth_full_s) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-16
                                                                    + s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.0241093 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + s V_synth_full__tmp1
                                                                    - s V_synth_full_s)) (F_check_ge (-1
                                                                    + s V_synth_full__tmp1
                                                                    - s V_synth_full_s) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.0985466 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + s V_synth_full__tmp1
                                                                    - s V_synth_full_s)) (F_check_ge (-1
                                                                    + s V_synth_full__tmp1
                                                                    - s V_synth_full_s) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp)) (F_check_ge (0) (0)));
      (*0 0.00962983*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_synth_full_sb) (0))) (F_max0_ge_0 (-1
                                                                    + s V_synth_full_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.00761459 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_synth_full_sb) (0))) (F_max0_ge_0 (-1
                                                                    + s V_synth_full_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp)) (F_check_ge (0) (0)));
      (*0 0.0182556*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_synth_full_sb) (0))) (F_max0_ge_0 (-1
                                                                    + s V_synth_full_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.0166922 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_synth_full_sb) (0))) (F_max0_ge_0 (-1
                                                                    + s V_synth_full_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp1
                                                                    - s V_synth_full_s)) (F_check_ge (0) (0)));
      (*-0.0228975 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + s V_synth_full_sb)) (F_check_ge (-1
                                                                    + s V_synth_full_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp1)) (F_check_ge (0) (0)));
      (*0 0.00210354*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + s V_synth_full_sb)) (F_check_ge (-1
                                                                    + s V_synth_full_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (16
                                                                    - s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.0364562 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + s V_synth_full_sb)) (F_check_ge (-1
                                                                    + s V_synth_full_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp)) (F_check_ge (0) (0)));
      (*-7.57381e-05 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + s V_synth_full_sb)) (F_check_ge (-1
                                                                    + s V_synth_full_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp1)) (F_check_ge (0) (0)));
      (*-0.225891 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    s V_synth_full_sb)) (F_check_ge (-1
                                                                    + s V_synth_full_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_s)) (F_check_ge (0) (0)));
      (*-0.0283694 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (16
                                                                    - s V_synth_full_sb) (0))) (F_max0_ge_0 (16
                                                                    - s V_synth_full_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp1)) (F_check_ge (0) (0)));
      (*-0.00210354 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (16
                                                                    - s V_synth_full_sb) (0))) (F_max0_ge_0 (16
                                                                    - s V_synth_full_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.320354 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (16
                                                                    - s V_synth_full_sb) (0))) (F_max0_ge_0 (16
                                                                    - s V_synth_full_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp
                                                                    - s V_synth_full_ch)) (F_check_ge (0) (0)));
      (*-0.0108933 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (16
                                                                    - s V_synth_full_sb) (0))) (F_max0_ge_0 (16
                                                                    - s V_synth_full_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp1
                                                                    - s V_synth_full_s)) (F_check_ge (0) (0)));
      (*-0.0454564 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (16
                                                                    - s V_synth_full_sb) (0))) (F_max0_ge_0 (16
                                                                    - s V_synth_full_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_ch)) (F_check_ge (0) (0)));
      (*-0.0175013 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (16
                                                                    - s V_synth_full_sb) (0))) (F_max0_ge_0 (16
                                                                    - s V_synth_full_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_s)) (F_check_ge (0) (0)));
      (*-0.00298805 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (16
                                                                    - s V_synth_full_sb)) (F_check_ge (16
                                                                    - s V_synth_full_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-16
                                                                    + s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.296951 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (16
                                                                    - 
                                                                    s V_synth_full_sb)) (F_check_ge (16
                                                                    - s V_synth_full_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp1)) (F_check_ge (0) (0)));
      (*-0.0364562 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full__tmp) (0))) (F_max0_ge_0 (s V_synth_full__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.0985466 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full__tmp)) (F_check_ge (s V_synth_full__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp1
                                                                    - s V_synth_full_s)) (F_check_ge (0) (0)));
      (*-0.0192597 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full__tmp
                                                                    - s V_synth_full_ch)) (F_check_ge (s V_synth_full__tmp
                                                                    - s V_synth_full_ch) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.0261967 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full__tmp
                                                                    - s V_synth_full_ch)) (F_check_ge (s V_synth_full__tmp
                                                                    - s V_synth_full_ch) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.192753 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full__tmp1) (0))) (F_max0_ge_0 (s V_synth_full__tmp1))) (F_binom_monotonic 1 (F_max0_ge_0 (15
                                                                    - s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.104219 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full__tmp1) (0))) (F_max0_ge_0 (s V_synth_full__tmp1))) (F_binom_monotonic 1 (F_max0_ge_0 (16
                                                                    - s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-7.57381e-05 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full__tmp1) (0))) (F_max0_ge_0 (s V_synth_full__tmp1))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.0985466 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full__tmp1
                                                                    - s V_synth_full_s) (0))) (F_max0_ge_0 (s V_synth_full__tmp1
                                                                    - s V_synth_full_s))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp)) (F_check_ge (0) (0)));
      (*-0.006608 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full__tmp1
                                                                    - s V_synth_full_s) (0))) (F_max0_ge_0 (s V_synth_full__tmp1
                                                                    - s V_synth_full_s))) (F_binom_monotonic 1 (F_max0_ge_0 (15
                                                                    - s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.0108933 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full__tmp1
                                                                    - s V_synth_full_s)) (F_check_ge (s V_synth_full__tmp1
                                                                    - s V_synth_full_s) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.261995 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full_ch) (0))) (F_max0_ge_0 (s V_synth_full_ch))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.307451 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full_ch)) (F_check_ge (s V_synth_full_ch) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.0454564 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full_ch)) (F_check_ge (s V_synth_full_ch) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (16
                                                                    - s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.225891 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full_s) (0))) (F_max0_ge_0 (s V_synth_full_s))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.0207217 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full_s) (0))) (F_max0_ge_0 (s V_synth_full_s))) (F_binom_monotonic 1 (F_max0_ge_0 (16
                                                                    - s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.038223 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full_s)) (F_check_ge (s V_synth_full_s) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (15
                                                                    - s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.197597 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full_s)) (F_check_ge (s V_synth_full_s) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.0657993 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full_sb) (0))) (F_max0_ge_0 (s V_synth_full_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (-16
                                                                    + s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.0512668 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full_sb) (0))) (F_max0_ge_0 (s V_synth_full_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp1)) (F_check_ge (0) (0)));
      (*-0.0454564 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full_sb) (0))) (F_max0_ge_0 (s V_synth_full_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp
                                                                    - s V_synth_full_ch)) (F_check_ge (0) (0)));
      (*-0.197597 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full_sb) (0))) (F_max0_ge_0 (s V_synth_full_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_s)) (F_check_ge (0) (0)));
      (*-0.0632962 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full_sb)) (F_check_ge (s V_synth_full_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp1
                                                                    - s V_synth_full_s)) (F_check_ge (0) (0)));
      (*0 0.00579891*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full_sb)) (F_check_ge (s V_synth_full_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full__tmp1
                                                                    - s V_synth_full_s)) (F_check_ge (0) (0)));
      (*-0.26093 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_full_sb)) (F_check_ge (s V_synth_full_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_full_ch)) (F_check_ge (0) (0)));
      (*-0.060432 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full_z) (0))) (F_max0_ge_0 (s V_synth_full_z))) (F_binom_monotonic 1 (F_max0_ge_0 (-16
                                                                    + s V_synth_full_sb)) (F_check_ge (0) (0)));
      (*-0.0948434 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_full__tmp
                                                                    - s V_synth_full_ch) (0))) (F_max0_ge_0 (s V_synth_full__tmp
                                                                    - s V_synth_full_ch));
      (*-0.00242194 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_synth_full_sb) (0))) (F_max0_ge_0 (-1
                                                                    + s V_synth_full_sb));
      (*0 0.0196864*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_synth_full__tmp
                                                                    - s V_synth_full_ch) (0))) (F_max0_ge_0 (-1
                                                                    + s V_synth_full__tmp
                                                                    - s V_synth_full_ch))]
     ((1307 # 75) - (783 # 89) * s V_synth_full__tmp
      + (21 # 32) * s V_synth_full__tmp * s V_synth_full_ch
      - (1 # 52) * s V_synth_full__tmp * max0(-2 + s V_synth_full_sb)
      + (36 # 97) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp)
      + (371 # 142) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp
                                                 - s V_synth_full_ch)
      - (7 # 71) * s V_synth_full__tmp * max0(-1 + s V_synth_full__tmp1
                                              - s V_synth_full_s)
      + (1 # 104) * s V_synth_full__tmp * max0(-1 + s V_synth_full_sb)
      - (2 # 103) * s V_synth_full__tmp * max0(16 - s V_synth_full_sb)
      - (92 # 113) * s V_synth_full__tmp * max0(s V_synth_full__tmp)
      + (72 # 97) * s V_synth_full__tmp * max0(s V_synth_full__tmp
                                               - s V_synth_full_ch)
      - (1213 # 105) * s V_synth_full__tmp * max0(s V_synth_full__tmp1)
      + (883 # 101) * s V_synth_full__tmp * max0(s V_synth_full__tmp1
                                                 - s V_synth_full_s)
      + (13 # 57) * s V_synth_full__tmp * max0(s V_synth_full_ch)
      - (21 # 64) * s V_synth_full__tmp^2
      - (0 # 1) * s V_synth_full__tmp1 * max0(-2 + s V_synth_full_sb)
      - (0 # 1) * s V_synth_full__tmp1 * max0(-1 + s V_synth_full_sb)
      - (6 # 107) * s V_synth_full__tmp1 * max0(16 - s V_synth_full_sb)
      + (9 # 59) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp)
      - (451 # 63) * s V_synth_full__tmp1 * max0(s V_synth_full__tmp
                                                 - s V_synth_full_ch)
      - (413 # 94) * s V_synth_full__tmp1 * max0(s V_synth_full_ch)
      - (126 # 97) * s V_synth_full_ch
      - (17 # 59) * s V_synth_full_ch * max0(-2 + s V_synth_full_sb)
      - (76 # 71) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp)
      - (87 # 67) * s V_synth_full_ch * max0(-1 + s V_synth_full__tmp
                                             - s V_synth_full_ch)
      - (17 # 59) * s V_synth_full_ch * max0(-1 + s V_synth_full_sb)
      - (1 # 141) * s V_synth_full_ch * max0(16 - s V_synth_full_sb)
      + (72 # 97) * s V_synth_full_ch * max0(s V_synth_full__tmp)
      - (76 # 71) * s V_synth_full_ch * max0(s V_synth_full__tmp
                                             - s V_synth_full_ch)
      + (1213 # 105) * s V_synth_full_ch * max0(s V_synth_full__tmp1)
      - (413 # 94) * s V_synth_full_ch * max0(s V_synth_full__tmp1
                                              - s V_synth_full_s)
      - (13 # 57) * s V_synth_full_ch * max0(s V_synth_full_ch)
      - (21 # 64) * s V_synth_full_ch^2
      + (2 # 83) * s V_synth_full_s * max0(-2 + s V_synth_full_sb)
      + (1 # 4) * s V_synth_full_s * max0(-1 + s V_synth_full_sb)
      + (5 # 131) * s V_synth_full_s * max0(16 - s V_synth_full_sb)
      - (1 # 15) * s V_synth_full_s * max0(s V_synth_full__tmp)
      - (97 # 62) * s V_synth_full_s * max0(s V_synth_full__tmp
                                            - s V_synth_full_ch)
      + (413 # 94) * s V_synth_full_s * max0(s V_synth_full_ch)
      - (11 # 14) * s V_synth_full_sb
      + (1 # 103) * s V_synth_full_sb * max0(-2 + s V_synth_full_sb)
      + (3 # 68) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp)
      + (1 # 51) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp
                                            - s V_synth_full_ch)
      - (41 # 86) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1)
      + (65 # 84) * s V_synth_full_sb * max0(-1 + s V_synth_full__tmp1
                                             - s V_synth_full_s)
      + (1 # 152) * s V_synth_full_sb * max0(-1 + s V_synth_full_sb)
      + (0 # 1) * s V_synth_full_sb * max0(16 - s V_synth_full_sb)
      + (43 # 81) * s V_synth_full_sb * max0(s V_synth_full__tmp)
      - (74 # 117) * s V_synth_full_sb * max0(s V_synth_full__tmp
                                              - s V_synth_full_ch)
      + (0 # 1) * s V_synth_full_sb * max0(s V_synth_full__tmp1)
      - (21 # 82) * s V_synth_full_sb * max0(s V_synth_full__tmp1
                                             - s V_synth_full_s)
      - (19 # 62) * s V_synth_full_sb * max0(s V_synth_full_ch)
      + (22 # 97) * s V_synth_full_sb * max0(s V_synth_full_s)
      + s V_synth_full_z + (3 # 133) * max0(-2 + s V_synth_full_sb)
      - (2 # 87) * max0(-2 + s V_synth_full_sb) * max0(-1
                                                       + s V_synth_full__tmp1)
      + (1 # 52) * max0(-2 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
      + (4 # 13) * max0(-2 + s V_synth_full_sb) * max0(s V_synth_full_ch)
      - (1 # 122) * max0(-2 + s V_synth_full_sb)^2
      - (48 # 83) * max0(-1 + s V_synth_full__tmp)
      - (91 # 59) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                          + s V_synth_full__tmp
                                                          - s V_synth_full_ch)
      + (7 # 71) * max0(-1 + s V_synth_full__tmp) * max0(-1
                                                         + s V_synth_full__tmp1
                                                         - s V_synth_full_s)
      + (1 # 57) * max0(-1 + s V_synth_full__tmp) * max0(16
                                                         - s V_synth_full_sb)
      + (2 # 5) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp)
      + (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
      - (467 # 105) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full__tmp1
                                                            - s V_synth_full_s)
      + (21 # 64) * max0(-1 + s V_synth_full__tmp) * max0(s V_synth_full_ch)
      + (1066 # 109) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)
      - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
      + (1213 # 105) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
      - (1213 # 105) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
      - (76 # 71) * max0(-1 + s V_synth_full__tmp - s V_synth_full_ch)^2
      + (382 # 85) * max0(-1 + s V_synth_full__tmp1)
      - (13 # 95) * max0(-1 + s V_synth_full__tmp1) * max0(16
                                                           - s V_synth_full_sb)
      - (8 # 93) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp)
      + (205 # 47) * max0(-1 + s V_synth_full__tmp1) * max0(s V_synth_full__tmp
                                                            - s V_synth_full_ch)
      + (17 # 147) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s)
      - (1 # 150) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(16
                                                                    - s V_synth_full_sb)
      + (3 # 94) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp)
      + (193 # 69) * max0(-1 + s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full__tmp
                                                                    - s V_synth_full_ch)
      + (7 # 97) * max0(-1 + s V_synth_full_sb)
      + (1 # 52) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                       - s V_synth_full_ch)
      + (0 # 1) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp1)
      - (1 # 60) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                       - s V_synth_full_s)
      + (0 # 1) * max0(-1 + s V_synth_full_sb) * max0(s V_synth_full_ch)
      - (1 # 139) * max0(-1 + s V_synth_full_sb)^2
      - (11 # 92) * max0(16 - s V_synth_full_sb)
      - (1 # 141) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp
                                                        - s V_synth_full_ch)
      + (16 # 83) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp1)
      + (1 # 150) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full__tmp1
                                                        - s V_synth_full_s)
      - (5 # 131) * max0(16 - s V_synth_full_sb) * max0(s V_synth_full_s)
      - (2 # 29) * max0(s V_synth_full__tmp)
      - (467 # 105) * max0(s V_synth_full__tmp) * max0(s V_synth_full__tmp1)
      - (21 # 64) * max0(s V_synth_full__tmp) * max0(s V_synth_full_ch)
      + (36 # 97) * max0(s V_synth_full__tmp)^2
      + (327 # 65) * max0(s V_synth_full__tmp - s V_synth_full_ch)
      + (16 # 1) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1)
      + (451 # 63) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full__tmp1
                                                                    - s V_synth_full_s)
      + (205 # 47) * max0(s V_synth_full__tmp - s V_synth_full_ch) * max0(s V_synth_full_s)
      + (21 # 64) * max0(s V_synth_full__tmp - s V_synth_full_ch)^2
      - (19 # 4) * max0(s V_synth_full__tmp1)
      + (4 # 23) * max0(s V_synth_full__tmp1 - s V_synth_full_s)
      + (413 # 94) * max0(s V_synth_full__tmp1 - s V_synth_full_s) * max0(s V_synth_full_ch)
      + (145 # 14) * max0(s V_synth_full_ch)
      + (43 # 85) * max0(s V_synth_full_s) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_synth_full =>
    [mkPA Q (fun n z s => ai_synth_full n s /\ annot0_synth_full n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_synth_full (proc_start P_synth_full) s1 (proc_end P_synth_full) s2 ->
    (s2 V_synth_full_z <= max0(s1 V_synth_full_nch)
                          + (16 # 1) * max0(s1 V_synth_full_nch) * max0(s1 V_synth_full_ns))%Q.
Proof.
  prove_bound ipa admissible_ipa P_synth_full.
Qed.
