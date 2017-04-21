Require Import pasta.Pasta.

Inductive proc: Type :=
  P_RPE_grid_selection.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_RPE_grid_selection_z := 1%positive.
Notation V_RPE_grid_selection_EM := 2%positive.
Notation V_RPE_grid_selection_L_common_0_3 := 3%positive.
Notation V_RPE_grid_selection_L_result := 4%positive.
Notation V_RPE_grid_selection_L_temp := 5%positive.
Notation V_RPE_grid_selection_Mc := 6%positive.
Notation V_RPE_grid_selection_Mc_out_dref := 7%positive.
Notation V_RPE_grid_selection_i := 8%positive.
Notation V_RPE_grid_selection_Mc_out := 9%positive.
Notation V_RPE_grid_selection_x := 10%positive.
Notation V_RPE_grid_selection_xM := 11%positive.
Definition Pedges_RPE_grid_selection: list (edge proc) :=
  (EA 1 (AAssign V_RPE_grid_selection_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_RPE_grid_selection_EM (Some (ENum (0)))) 3)::(EA 3 (AAssign
  V_RPE_grid_selection_Mc (Some (ENum (0)))) 4)::(EA 4 (AAssign
  V_RPE_grid_selection_L_result (Some (ENum (0)))) 5)::(EA 5 (AAssign
  V_RPE_grid_selection_L_temp None) 6)::(EA 6 (AAssign
  V_RPE_grid_selection_L_result
  (Some (EAdd (EVar V_RPE_grid_selection_L_result)
  (EMul (EVar V_RPE_grid_selection_L_temp)
  (EVar V_RPE_grid_selection_L_temp))))) 7)::(EA 7 (AAssign
  V_RPE_grid_selection_L_temp None) 8)::(EA 8 (AAssign
  V_RPE_grid_selection_L_result
  (Some (EAdd (EVar V_RPE_grid_selection_L_result)
  (EMul (EVar V_RPE_grid_selection_L_temp)
  (EVar V_RPE_grid_selection_L_temp))))) 9)::(EA 9 (AAssign
  V_RPE_grid_selection_L_temp None) 10)::(EA 10 (AAssign
  V_RPE_grid_selection_L_result
  (Some (EAdd (EVar V_RPE_grid_selection_L_result)
  (EMul (EVar V_RPE_grid_selection_L_temp)
  (EVar V_RPE_grid_selection_L_temp))))) 11)::(EA 11 (AAssign
  V_RPE_grid_selection_L_temp None) 12)::(EA 12 (AAssign
  V_RPE_grid_selection_L_result
  (Some (EAdd (EVar V_RPE_grid_selection_L_result)
  (EMul (EVar V_RPE_grid_selection_L_temp)
  (EVar V_RPE_grid_selection_L_temp))))) 13)::(EA 13 (AAssign
  V_RPE_grid_selection_L_temp None) 14)::(EA 14 (AAssign
  V_RPE_grid_selection_L_result
  (Some (EAdd (EVar V_RPE_grid_selection_L_result)
  (EMul (EVar V_RPE_grid_selection_L_temp)
  (EVar V_RPE_grid_selection_L_temp))))) 15)::(EA 15 (AAssign
  V_RPE_grid_selection_L_temp None) 16)::(EA 16 (AAssign
  V_RPE_grid_selection_L_result
  (Some (EAdd (EVar V_RPE_grid_selection_L_result)
  (EMul (EVar V_RPE_grid_selection_L_temp)
  (EVar V_RPE_grid_selection_L_temp))))) 17)::(EA 17 (AAssign
  V_RPE_grid_selection_L_temp None) 18)::(EA 18 (AAssign
  V_RPE_grid_selection_L_result
  (Some (EAdd (EVar V_RPE_grid_selection_L_result)
  (EMul (EVar V_RPE_grid_selection_L_temp)
  (EVar V_RPE_grid_selection_L_temp))))) 19)::(EA 19 (AAssign
  V_RPE_grid_selection_L_temp None) 20)::(EA 20 (AAssign
  V_RPE_grid_selection_L_result
  (Some (EAdd (EVar V_RPE_grid_selection_L_result)
  (EMul (EVar V_RPE_grid_selection_L_temp)
  (EVar V_RPE_grid_selection_L_temp))))) 21)::(EA 21 (AAssign
  V_RPE_grid_selection_L_temp None) 22)::(EA 22 (AAssign
  V_RPE_grid_selection_L_result
  (Some (EAdd (EVar V_RPE_grid_selection_L_result)
  (EMul (EVar V_RPE_grid_selection_L_temp)
  (EVar V_RPE_grid_selection_L_temp))))) 23)::(EA 23 (AAssign
  V_RPE_grid_selection_L_temp None) 24)::(EA 24 (AAssign
  V_RPE_grid_selection_L_result
  (Some (EAdd (EVar V_RPE_grid_selection_L_result)
  (EMul (EVar V_RPE_grid_selection_L_temp)
  (EVar V_RPE_grid_selection_L_temp))))) 25)::(EA 25 (AAssign
  V_RPE_grid_selection_L_temp None) 26)::(EA 26 (AAssign
  V_RPE_grid_selection_L_result
  (Some (EAdd (EVar V_RPE_grid_selection_L_result)
  (EMul (EVar V_RPE_grid_selection_L_temp)
  (EVar V_RPE_grid_selection_L_temp))))) 27)::(EA 27 (AAssign
  V_RPE_grid_selection_L_temp None) 28)::(EA 28 (AAssign
  V_RPE_grid_selection_L_result
  (Some (EAdd (EVar V_RPE_grid_selection_L_result)
  (EMul (EVar V_RPE_grid_selection_L_temp)
  (EVar V_RPE_grid_selection_L_temp))))) 29)::(EA 29 (AAssign
  V_RPE_grid_selection_L_common_0_3
  (Some (EVar V_RPE_grid_selection_L_result))) 30)::(EA 30 (AAssign
  V_RPE_grid_selection_L_temp None) 31)::(EA 31 (AAssign
  V_RPE_grid_selection_L_result
  (Some (EAdd (EVar V_RPE_grid_selection_L_result)
  (EMul (EVar V_RPE_grid_selection_L_temp)
  (EVar V_RPE_grid_selection_L_temp))))) 32)::(EA 32 (AAssign
  V_RPE_grid_selection_L_result None) 33)::(EA 33 (AAssign
  V_RPE_grid_selection_EM (Some (EVar V_RPE_grid_selection_L_result))) 34)::
  (EA 34 (AAssign V_RPE_grid_selection_L_result (Some (ENum (0)))) 35)::
  (EA 35 (AAssign V_RPE_grid_selection_L_temp None) 36)::(EA 36 (AAssign
  V_RPE_grid_selection_L_result
  (Some (EAdd (EVar V_RPE_grid_selection_L_result)
  (EMul (EVar V_RPE_grid_selection_L_temp)
  (EVar V_RPE_grid_selection_L_temp))))) 37)::(EA 37 (AAssign
  V_RPE_grid_selection_L_temp None) 38)::(EA 38 (AAssign
  V_RPE_grid_selection_L_result
  (Some (EAdd (EVar V_RPE_grid_selection_L_result)
  (EMul (EVar V_RPE_grid_selection_L_temp)
  (EVar V_RPE_grid_selection_L_temp))))) 39)::(EA 39 (AAssign
  V_RPE_grid_selection_L_temp None) 40)::(EA 40 (AAssign
  V_RPE_grid_selection_L_result
  (Some (EAdd (EVar V_RPE_grid_selection_L_result)
  (EMul (EVar V_RPE_grid_selection_L_temp)
  (EVar V_RPE_grid_selection_L_temp))))) 41)::(EA 41 (AAssign
  V_RPE_grid_selection_L_temp None) 42)::(EA 42 (AAssign
  V_RPE_grid_selection_L_result
  (Some (EAdd (EVar V_RPE_grid_selection_L_result)
  (EMul (EVar V_RPE_grid_selection_L_temp)
  (EVar V_RPE_grid_selection_L_temp))))) 43)::(EA 43 (AAssign
  V_RPE_grid_selection_L_temp None) 44)::(EA 44 (AAssign
  V_RPE_grid_selection_L_result
  (Some (EAdd (EVar V_RPE_grid_selection_L_result)
  (EMul (EVar V_RPE_grid_selection_L_temp)
  (EVar V_RPE_grid_selection_L_temp))))) 45)::(EA 45 (AAssign
  V_RPE_grid_selection_L_temp None) 46)::(EA 46 (AAssign
  V_RPE_grid_selection_L_result
  (Some (EAdd (EVar V_RPE_grid_selection_L_result)
  (EMul (EVar V_RPE_grid_selection_L_temp)
  (EVar V_RPE_grid_selection_L_temp))))) 47)::(EA 47 (AAssign
  V_RPE_grid_selection_L_temp None) 48)::(EA 48 (AAssign
  V_RPE_grid_selection_L_result
  (Some (EAdd (EVar V_RPE_grid_selection_L_result)
  (EMul (EVar V_RPE_grid_selection_L_temp)
  (EVar V_RPE_grid_selection_L_temp))))) 49)::(EA 49 (AAssign
  V_RPE_grid_selection_L_temp None) 50)::(EA 50 (AAssign
  V_RPE_grid_selection_L_result
  (Some (EAdd (EVar V_RPE_grid_selection_L_result)
  (EMul (EVar V_RPE_grid_selection_L_temp)
  (EVar V_RPE_grid_selection_L_temp))))) 51)::(EA 51 (AAssign
  V_RPE_grid_selection_L_temp None) 52)::(EA 52 (AAssign
  V_RPE_grid_selection_L_result
  (Some (EAdd (EVar V_RPE_grid_selection_L_result)
  (EMul (EVar V_RPE_grid_selection_L_temp)
  (EVar V_RPE_grid_selection_L_temp))))) 53)::(EA 53 (AAssign
  V_RPE_grid_selection_L_temp None) 54)::(EA 54 (AAssign
  V_RPE_grid_selection_L_result
  (Some (EAdd (EVar V_RPE_grid_selection_L_result)
  (EMul (EVar V_RPE_grid_selection_L_temp)
  (EVar V_RPE_grid_selection_L_temp))))) 55)::(EA 55 (AAssign
  V_RPE_grid_selection_L_temp None) 56)::(EA 56 (AAssign
  V_RPE_grid_selection_L_result
  (Some (EAdd (EVar V_RPE_grid_selection_L_result)
  (EMul (EVar V_RPE_grid_selection_L_temp)
  (EVar V_RPE_grid_selection_L_temp))))) 57)::(EA 57 (AAssign
  V_RPE_grid_selection_L_temp None) 58)::(EA 58 (AAssign
  V_RPE_grid_selection_L_result
  (Some (EAdd (EVar V_RPE_grid_selection_L_result)
  (EMul (EVar V_RPE_grid_selection_L_temp)
  (EVar V_RPE_grid_selection_L_temp))))) 59)::(EA 59 (AAssign
  V_RPE_grid_selection_L_temp None) 60)::(EA 60 (AAssign
  V_RPE_grid_selection_L_result
  (Some (EAdd (EVar V_RPE_grid_selection_L_result)
  (EMul (EVar V_RPE_grid_selection_L_temp)
  (EVar V_RPE_grid_selection_L_temp))))) 61)::(EA 61 (AAssign
  V_RPE_grid_selection_L_result None) 62)::(EA 62 AWeaken 63)::(EA 63 (AGuard
  (fun s => ((eval (EVar V_RPE_grid_selection_L_result) s) >
  (eval (EVar V_RPE_grid_selection_EM) s))%Z)) 65)::(EA 63 (AGuard
  (fun s => ((eval (EVar V_RPE_grid_selection_L_result) s) <=
  (eval (EVar V_RPE_grid_selection_EM) s))%Z)) 64)::(EA 64 AWeaken 69)::
  (EA 65 AWeaken 66)::(EA 66 (AAssign V_RPE_grid_selection_Mc
  (Some (ENum (1)))) 67)::(EA 67 (AAssign V_RPE_grid_selection_EM
  (Some (EVar V_RPE_grid_selection_L_result))) 68)::(EA 68 ANone 69)::
  (EA 69 (AAssign V_RPE_grid_selection_L_result (Some (ENum (0)))) 70)::
  (EA 70 (AAssign V_RPE_grid_selection_L_temp None) 71)::(EA 71 (AAssign
  V_RPE_grid_selection_L_result
  (Some (EAdd (EVar V_RPE_grid_selection_L_result)
  (EMul (EVar V_RPE_grid_selection_L_temp)
  (EVar V_RPE_grid_selection_L_temp))))) 72)::(EA 72 (AAssign
  V_RPE_grid_selection_L_temp None) 73)::(EA 73 (AAssign
  V_RPE_grid_selection_L_result
  (Some (EAdd (EVar V_RPE_grid_selection_L_result)
  (EMul (EVar V_RPE_grid_selection_L_temp)
  (EVar V_RPE_grid_selection_L_temp))))) 74)::(EA 74 (AAssign
  V_RPE_grid_selection_L_temp None) 75)::(EA 75 (AAssign
  V_RPE_grid_selection_L_result
  (Some (EAdd (EVar V_RPE_grid_selection_L_result)
  (EMul (EVar V_RPE_grid_selection_L_temp)
  (EVar V_RPE_grid_selection_L_temp))))) 76)::(EA 76 (AAssign
  V_RPE_grid_selection_L_temp None) 77)::(EA 77 (AAssign
  V_RPE_grid_selection_L_result
  (Some (EAdd (EVar V_RPE_grid_selection_L_result)
  (EMul (EVar V_RPE_grid_selection_L_temp)
  (EVar V_RPE_grid_selection_L_temp))))) 78)::(EA 78 (AAssign
  V_RPE_grid_selection_L_temp None) 79)::(EA 79 (AAssign
  V_RPE_grid_selection_L_result
  (Some (EAdd (EVar V_RPE_grid_selection_L_result)
  (EMul (EVar V_RPE_grid_selection_L_temp)
  (EVar V_RPE_grid_selection_L_temp))))) 80)::(EA 80 (AAssign
  V_RPE_grid_selection_L_temp None) 81)::(EA 81 (AAssign
  V_RPE_grid_selection_L_result
  (Some (EAdd (EVar V_RPE_grid_selection_L_result)
  (EMul (EVar V_RPE_grid_selection_L_temp)
  (EVar V_RPE_grid_selection_L_temp))))) 82)::(EA 82 (AAssign
  V_RPE_grid_selection_L_temp None) 83)::(EA 83 (AAssign
  V_RPE_grid_selection_L_result
  (Some (EAdd (EVar V_RPE_grid_selection_L_result)
  (EMul (EVar V_RPE_grid_selection_L_temp)
  (EVar V_RPE_grid_selection_L_temp))))) 84)::(EA 84 (AAssign
  V_RPE_grid_selection_L_temp None) 85)::(EA 85 (AAssign
  V_RPE_grid_selection_L_result
  (Some (EAdd (EVar V_RPE_grid_selection_L_result)
  (EMul (EVar V_RPE_grid_selection_L_temp)
  (EVar V_RPE_grid_selection_L_temp))))) 86)::(EA 86 (AAssign
  V_RPE_grid_selection_L_temp None) 87)::(EA 87 (AAssign
  V_RPE_grid_selection_L_result
  (Some (EAdd (EVar V_RPE_grid_selection_L_result)
  (EMul (EVar V_RPE_grid_selection_L_temp)
  (EVar V_RPE_grid_selection_L_temp))))) 88)::(EA 88 (AAssign
  V_RPE_grid_selection_L_temp None) 89)::(EA 89 (AAssign
  V_RPE_grid_selection_L_result
  (Some (EAdd (EVar V_RPE_grid_selection_L_result)
  (EMul (EVar V_RPE_grid_selection_L_temp)
  (EVar V_RPE_grid_selection_L_temp))))) 90)::(EA 90 (AAssign
  V_RPE_grid_selection_L_temp None) 91)::(EA 91 (AAssign
  V_RPE_grid_selection_L_result
  (Some (EAdd (EVar V_RPE_grid_selection_L_result)
  (EMul (EVar V_RPE_grid_selection_L_temp)
  (EVar V_RPE_grid_selection_L_temp))))) 92)::(EA 92 (AAssign
  V_RPE_grid_selection_L_temp None) 93)::(EA 93 (AAssign
  V_RPE_grid_selection_L_result
  (Some (EAdd (EVar V_RPE_grid_selection_L_result)
  (EMul (EVar V_RPE_grid_selection_L_temp)
  (EVar V_RPE_grid_selection_L_temp))))) 94)::(EA 94 (AAssign
  V_RPE_grid_selection_L_temp None) 95)::(EA 95 (AAssign
  V_RPE_grid_selection_L_result
  (Some (EAdd (EVar V_RPE_grid_selection_L_result)
  (EMul (EVar V_RPE_grid_selection_L_temp)
  (EVar V_RPE_grid_selection_L_temp))))) 96)::(EA 96 (AAssign
  V_RPE_grid_selection_L_result None) 97)::(EA 97 AWeaken 98)::(EA 98 (AGuard
  (fun s => ((eval (EVar V_RPE_grid_selection_L_result) s) >
  (eval (EVar V_RPE_grid_selection_EM) s))%Z)) 100)::(EA 98 (AGuard
  (fun s => ((eval (EVar V_RPE_grid_selection_L_result) s) <=
  (eval (EVar V_RPE_grid_selection_EM) s))%Z)) 99)::(EA 99 AWeaken 104)::
  (EA 100 AWeaken 101)::(EA 101 (AAssign V_RPE_grid_selection_Mc
  (Some (ENum (2)))) 102)::(EA 102 (AAssign V_RPE_grid_selection_EM
  (Some (EVar V_RPE_grid_selection_L_result))) 103)::(EA 103 ANone 104)::
  (EA 104 (AAssign V_RPE_grid_selection_L_result
  (Some (EVar V_RPE_grid_selection_L_common_0_3))) 105)::(EA 105 (AAssign
  V_RPE_grid_selection_L_temp None) 106)::(EA 106 (AAssign
  V_RPE_grid_selection_L_result
  (Some (EAdd (EVar V_RPE_grid_selection_L_result)
  (EMul (EVar V_RPE_grid_selection_L_temp)
  (EVar V_RPE_grid_selection_L_temp))))) 107)::(EA 107 (AAssign
  V_RPE_grid_selection_L_result None) 108)::(EA 108 AWeaken 109)::
  (EA 109 (AGuard (fun s => ((eval (EVar V_RPE_grid_selection_L_result) s) >
  (eval (EVar V_RPE_grid_selection_EM) s))%Z)) 111)::(EA 109 (AGuard
  (fun s => ((eval (EVar V_RPE_grid_selection_L_result) s) <=
  (eval (EVar V_RPE_grid_selection_EM) s))%Z)) 110)::(EA 110 AWeaken 115)::
  (EA 111 AWeaken 112)::(EA 112 (AAssign V_RPE_grid_selection_Mc
  (Some (ENum (3)))) 113)::(EA 113 (AAssign V_RPE_grid_selection_EM
  (Some (EVar V_RPE_grid_selection_L_result))) 114)::(EA 114 ANone 115)::
  (EA 115 (AAssign V_RPE_grid_selection_i (Some (ENum (0)))) 116)::
  (EA 116 ANone 117)::(EA 117 AWeaken 118)::(EA 118 (AGuard
  (fun s => ((eval (EVar V_RPE_grid_selection_i) s) <= (eval (ENum (12))
  s))%Z)) 123)::(EA 118 (AGuard
  (fun s => ((eval (EVar V_RPE_grid_selection_i) s) > (eval (ENum (12))
  s))%Z)) 119)::(EA 119 AWeaken 120)::(EA 120 (AAssign
  V_RPE_grid_selection_Mc_out_dref
  (Some (EVar V_RPE_grid_selection_Mc))) 121)::(EA 121 AWeaken 122)::
  (EA 123 AWeaken 124)::(EA 124 ANone 125)::(EA 125 (AAssign
  V_RPE_grid_selection_i (Some (EAdd (EVar V_RPE_grid_selection_i)
  (ENum (1))))) 126)::(EA 126 ANone 127)::(EA 127 ANone 128)::
  (EA 128 (AAssign V_RPE_grid_selection_z (Some (EAdd (ENum (1))
  (EVar V_RPE_grid_selection_z)))) 129)::(EA 129 AWeaken 118)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_RPE_grid_selection => Pedges_RPE_grid_selection
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_RPE_grid_selection => 122
     end)%positive;
  var_global := var_global
}.

Definition ai_RPE_grid_selection (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0)%Z
   | 3 => (-1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_EM <= 0 /\ -1 * s V_RPE_grid_selection_EM <= 0)%Z
   | 4 => (-1 * s V_RPE_grid_selection_EM <= 0 /\ 1 * s V_RPE_grid_selection_EM <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0)%Z
   | 5 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_EM <= 0 /\ -1 * s V_RPE_grid_selection_EM <= 0 /\ 1 * s V_RPE_grid_selection_L_result <= 0 /\ -1 * s V_RPE_grid_selection_L_result <= 0)%Z
   | 6 => (-1 * s V_RPE_grid_selection_L_result <= 0 /\ 1 * s V_RPE_grid_selection_L_result <= 0 /\ -1 * s V_RPE_grid_selection_EM <= 0 /\ 1 * s V_RPE_grid_selection_EM <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0)%Z
   | 7 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_EM <= 0 /\ -1 * s V_RPE_grid_selection_EM <= 0)%Z
   | 8 => (-1 * s V_RPE_grid_selection_EM <= 0 /\ 1 * s V_RPE_grid_selection_EM <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0)%Z
   | 9 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_EM <= 0 /\ -1 * s V_RPE_grid_selection_EM <= 0)%Z
   | 10 => (-1 * s V_RPE_grid_selection_EM <= 0 /\ 1 * s V_RPE_grid_selection_EM <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0)%Z
   | 11 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_EM <= 0 /\ -1 * s V_RPE_grid_selection_EM <= 0)%Z
   | 12 => (-1 * s V_RPE_grid_selection_EM <= 0 /\ 1 * s V_RPE_grid_selection_EM <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0)%Z
   | 13 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_EM <= 0 /\ -1 * s V_RPE_grid_selection_EM <= 0)%Z
   | 14 => (-1 * s V_RPE_grid_selection_EM <= 0 /\ 1 * s V_RPE_grid_selection_EM <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0)%Z
   | 15 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_EM <= 0 /\ -1 * s V_RPE_grid_selection_EM <= 0)%Z
   | 16 => (-1 * s V_RPE_grid_selection_EM <= 0 /\ 1 * s V_RPE_grid_selection_EM <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0)%Z
   | 17 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_EM <= 0 /\ -1 * s V_RPE_grid_selection_EM <= 0)%Z
   | 18 => (-1 * s V_RPE_grid_selection_EM <= 0 /\ 1 * s V_RPE_grid_selection_EM <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0)%Z
   | 19 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_EM <= 0 /\ -1 * s V_RPE_grid_selection_EM <= 0)%Z
   | 20 => (-1 * s V_RPE_grid_selection_EM <= 0 /\ 1 * s V_RPE_grid_selection_EM <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0)%Z
   | 21 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_EM <= 0 /\ -1 * s V_RPE_grid_selection_EM <= 0)%Z
   | 22 => (-1 * s V_RPE_grid_selection_EM <= 0 /\ 1 * s V_RPE_grid_selection_EM <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0)%Z
   | 23 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_EM <= 0 /\ -1 * s V_RPE_grid_selection_EM <= 0)%Z
   | 24 => (-1 * s V_RPE_grid_selection_EM <= 0 /\ 1 * s V_RPE_grid_selection_EM <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0)%Z
   | 25 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_EM <= 0 /\ -1 * s V_RPE_grid_selection_EM <= 0)%Z
   | 26 => (-1 * s V_RPE_grid_selection_EM <= 0 /\ 1 * s V_RPE_grid_selection_EM <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0)%Z
   | 27 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_EM <= 0 /\ -1 * s V_RPE_grid_selection_EM <= 0)%Z
   | 28 => (-1 * s V_RPE_grid_selection_EM <= 0 /\ 1 * s V_RPE_grid_selection_EM <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0)%Z
   | 29 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_EM <= 0 /\ -1 * s V_RPE_grid_selection_EM <= 0)%Z
   | 30 => (-1 * s V_RPE_grid_selection_EM <= 0 /\ 1 * s V_RPE_grid_selection_EM <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0)%Z
   | 31 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_EM <= 0 /\ -1 * s V_RPE_grid_selection_EM <= 0)%Z
   | 32 => (-1 * s V_RPE_grid_selection_EM <= 0 /\ 1 * s V_RPE_grid_selection_EM <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0)%Z
   | 33 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_EM <= 0 /\ -1 * s V_RPE_grid_selection_EM <= 0)%Z
   | 34 => (1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0)%Z
   | 35 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_L_result <= 0 /\ -1 * s V_RPE_grid_selection_L_result <= 0)%Z
   | 36 => (-1 * s V_RPE_grid_selection_L_result <= 0 /\ 1 * s V_RPE_grid_selection_L_result <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0)%Z
   | 37 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0)%Z
   | 38 => (1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0)%Z
   | 39 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0)%Z
   | 40 => (1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0)%Z
   | 41 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0)%Z
   | 42 => (1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0)%Z
   | 43 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0)%Z
   | 44 => (1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0)%Z
   | 45 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0)%Z
   | 46 => (1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0)%Z
   | 47 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0)%Z
   | 48 => (1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0)%Z
   | 49 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0)%Z
   | 50 => (1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0)%Z
   | 51 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0)%Z
   | 52 => (1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0)%Z
   | 53 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0)%Z
   | 54 => (1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0)%Z
   | 55 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0)%Z
   | 56 => (1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0)%Z
   | 57 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0)%Z
   | 58 => (1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0)%Z
   | 59 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0)%Z
   | 60 => (1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0)%Z
   | 61 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0)%Z
   | 62 => (1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0)%Z
   | 63 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0)%Z
   | 64 => (1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_EM+ 1 * s V_RPE_grid_selection_L_result <= 0)%Z
   | 65 => (1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_EM+ -1 * s V_RPE_grid_selection_L_result + 1 <= 0)%Z
   | 66 => (1 * s V_RPE_grid_selection_EM+ -1 * s V_RPE_grid_selection_L_result + 1 <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0)%Z
   | 67 => (1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_EM+ -1 * s V_RPE_grid_selection_L_result + 1 <= 0 /\ 1 * s V_RPE_grid_selection_Mc + -1 <= 0 /\ -1 * s V_RPE_grid_selection_Mc + 1 <= 0)%Z
   | 68 => (-1 * s V_RPE_grid_selection_Mc + 1 <= 0 /\ 1 * s V_RPE_grid_selection_Mc + -1 <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0)%Z
   | 69 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc + -1 <= 0)%Z
   | 70 => (1 * s V_RPE_grid_selection_Mc + -1 <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_L_result <= 0 /\ -1 * s V_RPE_grid_selection_L_result <= 0)%Z
   | 71 => (-1 * s V_RPE_grid_selection_L_result <= 0 /\ 1 * s V_RPE_grid_selection_L_result <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc + -1 <= 0)%Z
   | 72 => (1 * s V_RPE_grid_selection_Mc + -1 <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0)%Z
   | 73 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc + -1 <= 0)%Z
   | 74 => (1 * s V_RPE_grid_selection_Mc + -1 <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0)%Z
   | 75 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc + -1 <= 0)%Z
   | 76 => (1 * s V_RPE_grid_selection_Mc + -1 <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0)%Z
   | 77 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc + -1 <= 0)%Z
   | 78 => (1 * s V_RPE_grid_selection_Mc + -1 <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0)%Z
   | 79 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc + -1 <= 0)%Z
   | 80 => (1 * s V_RPE_grid_selection_Mc + -1 <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0)%Z
   | 81 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc + -1 <= 0)%Z
   | 82 => (1 * s V_RPE_grid_selection_Mc + -1 <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0)%Z
   | 83 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc + -1 <= 0)%Z
   | 84 => (1 * s V_RPE_grid_selection_Mc + -1 <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0)%Z
   | 85 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc + -1 <= 0)%Z
   | 86 => (1 * s V_RPE_grid_selection_Mc + -1 <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0)%Z
   | 87 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc + -1 <= 0)%Z
   | 88 => (1 * s V_RPE_grid_selection_Mc + -1 <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0)%Z
   | 89 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc + -1 <= 0)%Z
   | 90 => (1 * s V_RPE_grid_selection_Mc + -1 <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0)%Z
   | 91 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc + -1 <= 0)%Z
   | 92 => (1 * s V_RPE_grid_selection_Mc + -1 <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0)%Z
   | 93 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc + -1 <= 0)%Z
   | 94 => (1 * s V_RPE_grid_selection_Mc + -1 <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0)%Z
   | 95 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc + -1 <= 0)%Z
   | 96 => (1 * s V_RPE_grid_selection_Mc + -1 <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0)%Z
   | 97 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc + -1 <= 0)%Z
   | 98 => (1 * s V_RPE_grid_selection_Mc + -1 <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0)%Z
   | 99 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc + -1 <= 0 /\ -1 * s V_RPE_grid_selection_EM+ 1 * s V_RPE_grid_selection_L_result <= 0)%Z
   | 100 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc + -1 <= 0 /\ 1 * s V_RPE_grid_selection_EM+ -1 * s V_RPE_grid_selection_L_result + 1 <= 0)%Z
   | 101 => (1 * s V_RPE_grid_selection_EM+ -1 * s V_RPE_grid_selection_L_result + 1 <= 0 /\ 1 * s V_RPE_grid_selection_Mc + -1 <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0)%Z
   | 102 => (1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_EM+ -1 * s V_RPE_grid_selection_L_result + 1 <= 0 /\ 1 * s V_RPE_grid_selection_Mc + -2 <= 0 /\ -1 * s V_RPE_grid_selection_Mc + 2 <= 0)%Z
   | 103 => (-1 * s V_RPE_grid_selection_Mc + 2 <= 0 /\ 1 * s V_RPE_grid_selection_Mc + -2 <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0)%Z
   | 104 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc + -2 <= 0)%Z
   | 105 => (1 * s V_RPE_grid_selection_Mc + -2 <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0)%Z
   | 106 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc + -2 <= 0)%Z
   | 107 => (1 * s V_RPE_grid_selection_Mc + -2 <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0)%Z
   | 108 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc + -2 <= 0)%Z
   | 109 => (1 * s V_RPE_grid_selection_Mc + -2 <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0)%Z
   | 110 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc + -2 <= 0 /\ -1 * s V_RPE_grid_selection_EM+ 1 * s V_RPE_grid_selection_L_result <= 0)%Z
   | 111 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc + -2 <= 0 /\ 1 * s V_RPE_grid_selection_EM+ -1 * s V_RPE_grid_selection_L_result + 1 <= 0)%Z
   | 112 => (1 * s V_RPE_grid_selection_EM+ -1 * s V_RPE_grid_selection_L_result + 1 <= 0 /\ 1 * s V_RPE_grid_selection_Mc + -2 <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0)%Z
   | 113 => (1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_EM+ -1 * s V_RPE_grid_selection_L_result + 1 <= 0 /\ 1 * s V_RPE_grid_selection_Mc + -3 <= 0 /\ -1 * s V_RPE_grid_selection_Mc + 3 <= 0)%Z
   | 114 => (-1 * s V_RPE_grid_selection_Mc + 3 <= 0 /\ 1 * s V_RPE_grid_selection_Mc + -3 <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0)%Z
   | 115 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc + -3 <= 0)%Z
   | 116 => (1 * s V_RPE_grid_selection_Mc + -3 <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_i <= 0 /\ -1 * s V_RPE_grid_selection_i <= 0)%Z
   | 117 => (-1 * s V_RPE_grid_selection_i <= 0 /\ 1 * s V_RPE_grid_selection_i <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc + -3 <= 0)%Z
   | 118 => (-1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_i <= 0 /\ 1 * s V_RPE_grid_selection_Mc + -3 <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_i + -13 <= 0)%Z
   | 119 => (1 * s V_RPE_grid_selection_i + -13 <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_Mc + -3 <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_i + 13 <= 0)%Z
   | 120 => (-1 * s V_RPE_grid_selection_i + 13 <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc + -3 <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_i + -13 <= 0)%Z
   | 121 => (1 * s V_RPE_grid_selection_i + -13 <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_Mc + -3 <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_i + 13 <= 0 /\ 1 * s V_RPE_grid_selection_Mc_out_dref + -3 <= 0 /\ -1 * s V_RPE_grid_selection_Mc_out_dref <= 0)%Z
   | 122 => (-1 * s V_RPE_grid_selection_Mc_out_dref <= 0 /\ 1 * s V_RPE_grid_selection_Mc_out_dref + -3 <= 0 /\ -1 * s V_RPE_grid_selection_i + 13 <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc + -3 <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_i + -13 <= 0)%Z
   | 123 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_Mc + -3 <= 0 /\ -1 * s V_RPE_grid_selection_i <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_i + -12 <= 0)%Z
   | 124 => (1 * s V_RPE_grid_selection_i + -12 <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ -1 * s V_RPE_grid_selection_i <= 0 /\ 1 * s V_RPE_grid_selection_Mc + -3 <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0)%Z
   | 125 => (-1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_Mc + -3 <= 0 /\ -1 * s V_RPE_grid_selection_i <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_i + -12 <= 0)%Z
   | 126 => (-1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc + -3 <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_i + 1 <= 0 /\ 1 * s V_RPE_grid_selection_i + -13 <= 0)%Z
   | 127 => (1 * s V_RPE_grid_selection_i + -13 <= 0 /\ -1 * s V_RPE_grid_selection_i + 1 <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_Mc + -3 <= 0 /\ -1 * s V_RPE_grid_selection_z <= 0)%Z
   | 128 => (-1 * s V_RPE_grid_selection_z <= 0 /\ 1 * s V_RPE_grid_selection_Mc + -3 <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0 /\ -1 * s V_RPE_grid_selection_i + 1 <= 0 /\ 1 * s V_RPE_grid_selection_i + -13 <= 0)%Z
   | 129 => (1 * s V_RPE_grid_selection_i + -13 <= 0 /\ -1 * s V_RPE_grid_selection_i + 1 <= 0 /\ -1 * s V_RPE_grid_selection_Mc <= 0 /\ 1 * s V_RPE_grid_selection_Mc + -3 <= 0 /\ -1 * s V_RPE_grid_selection_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_RPE_grid_selection (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((13 # 1) <= z)%Q
   | 2 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 3 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 4 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 5 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 6 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 7 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 8 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 9 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 10 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 11 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 12 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 13 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 14 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 15 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 16 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 17 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 18 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 19 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 20 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 21 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 22 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 23 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 24 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 25 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 26 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 27 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 28 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 29 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 30 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 31 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 32 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 33 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 34 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 35 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 36 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 37 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 38 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 39 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 40 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 41 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 42 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 43 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 44 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 45 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 46 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 47 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 48 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 49 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 50 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 51 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 52 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 53 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 54 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 55 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 56 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 57 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 58 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 59 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 60 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 61 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 62 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 63 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 64 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 65 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 66 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 67 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 68 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 69 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 70 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 71 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 72 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 73 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 74 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 75 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 76 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 77 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 78 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 79 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 80 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 81 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 82 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 83 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 84 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 85 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 86 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 87 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 88 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 89 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 90 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 91 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 92 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 93 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 94 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 95 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 96 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 97 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 98 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 99 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 100 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 101 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 102 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 103 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 104 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 105 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 106 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 107 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 108 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 109 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 110 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 111 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 112 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 113 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 114 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 115 => ((13 # 1) + s V_RPE_grid_selection_z <= z)%Q
   | 116 => ((13 # 1) - s V_RPE_grid_selection_i + s V_RPE_grid_selection_z <= z)%Q
   | 117 => ((13 # 1) - s V_RPE_grid_selection_i + s V_RPE_grid_selection_z <= z)%Q
   | 118 => ((13 # 1) - s V_RPE_grid_selection_i + s V_RPE_grid_selection_z <= z)%Q
   | 119 => ((13 # 1) - s V_RPE_grid_selection_i + s V_RPE_grid_selection_z <= z)%Q
   | 120 => ((13 # 1) - s V_RPE_grid_selection_i + s V_RPE_grid_selection_z <= z)%Q
   | 121 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (13 - s V_RPE_grid_selection_i) (12
                                                                    - s V_RPE_grid_selection_i));
      (*-1 0*) F_max0_ge_0 (12 - s V_RPE_grid_selection_i);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (13
                                                               - s V_RPE_grid_selection_i) (0))) (F_max0_ge_0 (13
                                                                    - s V_RPE_grid_selection_i))]
     ((13 # 1) - s V_RPE_grid_selection_i + s V_RPE_grid_selection_z <= z)%Q
   | 122 => (s V_RPE_grid_selection_z <= z)%Q
   | 123 => ((13 # 1) - s V_RPE_grid_selection_i + s V_RPE_grid_selection_z <= z)%Q
   | 124 => ((13 # 1) - s V_RPE_grid_selection_i + s V_RPE_grid_selection_z <= z)%Q
   | 125 => ((13 # 1) - s V_RPE_grid_selection_i + s V_RPE_grid_selection_z <= z)%Q
   | 126 => ((14 # 1) - s V_RPE_grid_selection_i + s V_RPE_grid_selection_z <= z)%Q
   | 127 => ((14 # 1) - s V_RPE_grid_selection_i + s V_RPE_grid_selection_z <= z)%Q
   | 128 => ((14 # 1) - s V_RPE_grid_selection_i + s V_RPE_grid_selection_z <= z)%Q
   | 129 => ((13 # 1) - s V_RPE_grid_selection_i + s V_RPE_grid_selection_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_RPE_grid_selection =>
    [mkPA Q (fun n z s => ai_RPE_grid_selection n s /\ annot0_RPE_grid_selection n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_RPE_grid_selection (proc_start P_RPE_grid_selection) s1 (proc_end P_RPE_grid_selection) s2 ->
    (s2 V_RPE_grid_selection_z <= (13 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_RPE_grid_selection.
Qed.
