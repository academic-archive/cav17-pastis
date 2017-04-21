Require Import pasta.Pasta.

Inductive proc: Type :=
  P_bits_fill_rectangle.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_bits_fill_rectangle_z := 1%positive.
Notation V_bits_fill_rectangle__tmp := 2%positive.
Notation V_bits_fill_rectangle__tmp1 := 3%positive.
Notation V_bits_fill_rectangle__tmp2 := 4%positive.
Notation V_bits_fill_rectangle__tmp3 := 5%positive.
Notation V_bits_fill_rectangle__tmp4 := 6%positive.
Notation V_bits_fill_rectangle_bit := 7%positive.
Notation V_bits_fill_rectangle_byte_count := 8%positive.
Notation V_bits_fill_rectangle_last := 9%positive.
Notation V_bits_fill_rectangle_last_bit := 10%positive.
Notation V_bits_fill_rectangle_line_count := 11%positive.
Notation V_bits_fill_rectangle_line_count1 := 12%positive.
Notation V_bits_fill_rectangle_line_count11 := 13%positive.
Notation V_bits_fill_rectangle_line_count13 := 14%positive.
Notation V_bits_fill_rectangle_line_count15 := 15%positive.
Notation V_bits_fill_rectangle_line_count17 := 16%positive.
Notation V_bits_fill_rectangle_line_count19 := 17%positive.
Notation V_bits_fill_rectangle_line_count21 := 18%positive.
Notation V_bits_fill_rectangle_line_count3 := 19%positive.
Notation V_bits_fill_rectangle_line_count5 := 20%positive.
Notation V_bits_fill_rectangle_line_count7 := 21%positive.
Notation V_bits_fill_rectangle_line_count9 := 22%positive.
Notation V_bits_fill_rectangle_mask := 23%positive.
Notation V_bits_fill_rectangle_right_mask := 24%positive.
Notation V_bits_fill_rectangle_dest := 25%positive.
Notation V_bits_fill_rectangle_dest_bit := 26%positive.
Notation V_bits_fill_rectangle_draster := 27%positive.
Notation V_bits_fill_rectangle_height := 28%positive.
Notation V_bits_fill_rectangle_pattern := 29%positive.
Notation V_bits_fill_rectangle_width_bits := 30%positive.
Definition Pedges_bits_fill_rectangle: list (edge proc) :=
  (EA 1 (AAssign V_bits_fill_rectangle_z (Some (ENum (0)))) 2)::
  (EA 2 (AAssign V_bits_fill_rectangle__tmp4
  (Some (EVar V_bits_fill_rectangle_dest_bit))) 3)::(EA 3 (AAssign
  V_bits_fill_rectangle__tmp3
  (Some (EVar V_bits_fill_rectangle_draster))) 4)::(EA 4 (AAssign
  V_bits_fill_rectangle__tmp2
  (Some (EVar V_bits_fill_rectangle_pattern))) 5)::(EA 5 (AAssign
  V_bits_fill_rectangle__tmp1
  (Some (EVar V_bits_fill_rectangle_width_bits))) 6)::(EA 6 (AAssign
  V_bits_fill_rectangle__tmp (Some (EVar V_bits_fill_rectangle_height))) 7)::
  (EA 7 (AAssign V_bits_fill_rectangle_bit None) 8)::(EA 8 (AAssign
  V_bits_fill_rectangle_last_bit
  (Some (ESub (EAdd (EVar V_bits_fill_rectangle__tmp1)
  (EVar V_bits_fill_rectangle_bit)) (ENum (33))))) 9)::(EA 9 AWeaken 10)::
  (EA 10 (AGuard (fun s => ((eval (EVar V_bits_fill_rectangle_last_bit) s) <
  (eval (ENum (0)) s))%Z)) 128)::(EA 10 (AGuard
  (fun s => ((eval (EVar V_bits_fill_rectangle_last_bit) s) >=
  (eval (ENum (0)) s))%Z)) 11)::(EA 11 AWeaken 12)::(EA 12 (AAssign
  V_bits_fill_rectangle_last None) 13)::(EA 13 (AAssign
  V_bits_fill_rectangle_mask None) 14)::(EA 14 (AAssign
  V_bits_fill_rectangle_right_mask None) 15)::(EA 15 AWeaken 16)::
  (EA 16 ANone 89)::(EA 16 ANone 53)::(EA 16 ANone 17)::(EA 17 AWeaken 18)::
  (EA 18 ANone 41)::(EA 18 ANone 30)::(EA 18 ANone 19)::(EA 19 (AAssign
  V_bits_fill_rectangle_line_count13
  (Some (EVar V_bits_fill_rectangle__tmp))) 20)::(EA 20 ANone 21)::
  (EA 21 ANone 22)::(EA 22 (AAssign V_bits_fill_rectangle_line_count13
  (Some (EAdd (EVar V_bits_fill_rectangle_line_count13) (ENum (-1))))) 23)::
  (EA 23 AWeaken 24)::(EA 24 (AGuard
  (fun s => ((eval (EAdd (EVar V_bits_fill_rectangle_line_count13)
  (ENum (-1))) s) <> (eval (ENum (0)) s))%Z)) 27)::(EA 24 (AGuard
  (fun s => ((eval (EAdd (EVar V_bits_fill_rectangle_line_count13)
  (ENum (-1))) s) = (eval (ENum (0)) s))%Z)) 25)::(EA 25 AWeaken 26)::
  (EA 26 ANone 49)::(EA 27 AWeaken 28)::(EA 28 ANone 29)::(EA 29 (AAssign
  V_bits_fill_rectangle_z (Some (EAdd (ENum (1))
  (EVar V_bits_fill_rectangle_z)))) 21)::(EA 30 (AAssign
  V_bits_fill_rectangle_line_count11
  (Some (EVar V_bits_fill_rectangle__tmp))) 31)::(EA 31 ANone 32)::
  (EA 32 ANone 33)::(EA 33 (AAssign V_bits_fill_rectangle_line_count11
  (Some (EAdd (EVar V_bits_fill_rectangle_line_count11) (ENum (-1))))) 34)::
  (EA 34 AWeaken 35)::(EA 35 (AGuard
  (fun s => ((eval (EAdd (EVar V_bits_fill_rectangle_line_count11)
  (ENum (-1))) s) <> (eval (ENum (0)) s))%Z)) 38)::(EA 35 (AGuard
  (fun s => ((eval (EAdd (EVar V_bits_fill_rectangle_line_count11)
  (ENum (-1))) s) = (eval (ENum (0)) s))%Z)) 36)::(EA 36 AWeaken 37)::
  (EA 37 ANone 49)::(EA 38 AWeaken 39)::(EA 39 ANone 40)::(EA 40 (AAssign
  V_bits_fill_rectangle_z (Some (EAdd (ENum (1))
  (EVar V_bits_fill_rectangle_z)))) 32)::(EA 41 (AAssign
  V_bits_fill_rectangle_line_count15
  (Some (EVar V_bits_fill_rectangle__tmp))) 42)::(EA 42 ANone 43)::
  (EA 43 ANone 44)::(EA 44 (AAssign V_bits_fill_rectangle_line_count15
  (Some (EAdd (EVar V_bits_fill_rectangle_line_count15) (ENum (-1))))) 45)::
  (EA 45 AWeaken 46)::(EA 46 (AGuard
  (fun s => ((eval (EAdd (EVar V_bits_fill_rectangle_line_count15)
  (ENum (-1))) s) <> (eval (ENum (0)) s))%Z)) 50)::(EA 46 (AGuard
  (fun s => ((eval (EAdd (EVar V_bits_fill_rectangle_line_count15)
  (ENum (-1))) s) = (eval (ENum (0)) s))%Z)) 47)::(EA 47 AWeaken 48)::
  (EA 48 ANone 49)::(EA 49 ANone 123)::(EA 50 AWeaken 51)::(EA 51 ANone 52)::
  (EA 52 (AAssign V_bits_fill_rectangle_z (Some (EAdd (ENum (1))
  (EVar V_bits_fill_rectangle_z)))) 43)::(EA 53 AWeaken 54)::
  (EA 54 ANone 77)::(EA 54 ANone 66)::(EA 54 ANone 55)::(EA 55 (AAssign
  V_bits_fill_rectangle_line_count7
  (Some (EVar V_bits_fill_rectangle__tmp))) 56)::(EA 56 ANone 57)::
  (EA 57 ANone 58)::(EA 58 (AAssign V_bits_fill_rectangle_line_count7
  (Some (EAdd (EVar V_bits_fill_rectangle_line_count7) (ENum (-1))))) 59)::
  (EA 59 AWeaken 60)::(EA 60 (AGuard
  (fun s => ((eval (EAdd (EVar V_bits_fill_rectangle_line_count7)
  (ENum (-1))) s) <> (eval (ENum (0)) s))%Z)) 63)::(EA 60 (AGuard
  (fun s => ((eval (EAdd (EVar V_bits_fill_rectangle_line_count7)
  (ENum (-1))) s) = (eval (ENum (0)) s))%Z)) 61)::(EA 61 AWeaken 62)::
  (EA 62 ANone 85)::(EA 63 AWeaken 64)::(EA 64 ANone 65)::(EA 65 (AAssign
  V_bits_fill_rectangle_z (Some (EAdd (ENum (1))
  (EVar V_bits_fill_rectangle_z)))) 57)::(EA 66 (AAssign
  V_bits_fill_rectangle_line_count5
  (Some (EVar V_bits_fill_rectangle__tmp))) 67)::(EA 67 ANone 68)::
  (EA 68 ANone 69)::(EA 69 (AAssign V_bits_fill_rectangle_line_count5
  (Some (EAdd (EVar V_bits_fill_rectangle_line_count5) (ENum (-1))))) 70)::
  (EA 70 AWeaken 71)::(EA 71 (AGuard
  (fun s => ((eval (EAdd (EVar V_bits_fill_rectangle_line_count5)
  (ENum (-1))) s) <> (eval (ENum (0)) s))%Z)) 74)::(EA 71 (AGuard
  (fun s => ((eval (EAdd (EVar V_bits_fill_rectangle_line_count5)
  (ENum (-1))) s) = (eval (ENum (0)) s))%Z)) 72)::(EA 72 AWeaken 73)::
  (EA 73 ANone 85)::(EA 74 AWeaken 75)::(EA 75 ANone 76)::(EA 76 (AAssign
  V_bits_fill_rectangle_z (Some (EAdd (ENum (1))
  (EVar V_bits_fill_rectangle_z)))) 68)::(EA 77 (AAssign
  V_bits_fill_rectangle_line_count9
  (Some (EVar V_bits_fill_rectangle__tmp))) 78)::(EA 78 ANone 79)::
  (EA 79 ANone 80)::(EA 80 (AAssign V_bits_fill_rectangle_line_count9
  (Some (EAdd (EVar V_bits_fill_rectangle_line_count9) (ENum (-1))))) 81)::
  (EA 81 AWeaken 82)::(EA 82 (AGuard
  (fun s => ((eval (EAdd (EVar V_bits_fill_rectangle_line_count9)
  (ENum (-1))) s) <> (eval (ENum (0)) s))%Z)) 86)::(EA 82 (AGuard
  (fun s => ((eval (EAdd (EVar V_bits_fill_rectangle_line_count9)
  (ENum (-1))) s) = (eval (ENum (0)) s))%Z)) 83)::(EA 83 AWeaken 84)::
  (EA 84 ANone 85)::(EA 85 ANone 123)::(EA 86 AWeaken 87)::(EA 87 ANone 88)::
  (EA 88 (AAssign V_bits_fill_rectangle_z (Some (EAdd (ENum (1))
  (EVar V_bits_fill_rectangle_z)))) 79)::(EA 89 (AAssign
  V_bits_fill_rectangle_byte_count None) 90)::(EA 90 AWeaken 91)::
  (EA 91 ANone 114)::(EA 91 ANone 103)::(EA 91 ANone 92)::(EA 92 (AAssign
  V_bits_fill_rectangle_line_count19
  (Some (EVar V_bits_fill_rectangle__tmp))) 93)::(EA 93 ANone 94)::
  (EA 94 ANone 95)::(EA 95 (AAssign V_bits_fill_rectangle_line_count19
  (Some (EAdd (EVar V_bits_fill_rectangle_line_count19) (ENum (-1))))) 96)::
  (EA 96 AWeaken 97)::(EA 97 (AGuard
  (fun s => ((eval (EAdd (EVar V_bits_fill_rectangle_line_count19)
  (ENum (-1))) s) <> (eval (ENum (0)) s))%Z)) 100)::(EA 97 (AGuard
  (fun s => ((eval (EAdd (EVar V_bits_fill_rectangle_line_count19)
  (ENum (-1))) s) = (eval (ENum (0)) s))%Z)) 98)::(EA 98 AWeaken 99)::
  (EA 99 ANone 122)::(EA 100 AWeaken 101)::(EA 101 ANone 102)::
  (EA 102 (AAssign V_bits_fill_rectangle_z (Some (EAdd (ENum (1))
  (EVar V_bits_fill_rectangle_z)))) 94)::(EA 103 (AAssign
  V_bits_fill_rectangle_line_count17
  (Some (EVar V_bits_fill_rectangle__tmp))) 104)::(EA 104 ANone 105)::
  (EA 105 ANone 106)::(EA 106 (AAssign V_bits_fill_rectangle_line_count17
  (Some (EAdd (EVar V_bits_fill_rectangle_line_count17) (ENum (-1))))) 107)::
  (EA 107 AWeaken 108)::(EA 108 (AGuard
  (fun s => ((eval (EAdd (EVar V_bits_fill_rectangle_line_count17)
  (ENum (-1))) s) <> (eval (ENum (0)) s))%Z)) 111)::(EA 108 (AGuard
  (fun s => ((eval (EAdd (EVar V_bits_fill_rectangle_line_count17)
  (ENum (-1))) s) = (eval (ENum (0)) s))%Z)) 109)::(EA 109 AWeaken 110)::
  (EA 110 ANone 122)::(EA 111 AWeaken 112)::(EA 112 ANone 113)::
  (EA 113 (AAssign V_bits_fill_rectangle_z (Some (EAdd (ENum (1))
  (EVar V_bits_fill_rectangle_z)))) 105)::(EA 114 (AAssign
  V_bits_fill_rectangle_line_count21
  (Some (EVar V_bits_fill_rectangle__tmp))) 115)::(EA 115 ANone 116)::
  (EA 116 ANone 117)::(EA 117 (AAssign V_bits_fill_rectangle_line_count21
  (Some (EAdd (EVar V_bits_fill_rectangle_line_count21) (ENum (-1))))) 118)::
  (EA 118 AWeaken 119)::(EA 119 (AGuard
  (fun s => ((eval (EAdd (EVar V_bits_fill_rectangle_line_count21)
  (ENum (-1))) s) <> (eval (ENum (0)) s))%Z)) 125)::(EA 119 (AGuard
  (fun s => ((eval (EAdd (EVar V_bits_fill_rectangle_line_count21)
  (ENum (-1))) s) = (eval (ENum (0)) s))%Z)) 120)::(EA 120 AWeaken 121)::
  (EA 121 ANone 122)::(EA 122 ANone 123)::(EA 123 ANone 124)::
  (EA 124 AWeaken 164)::(EA 125 AWeaken 126)::(EA 126 ANone 127)::
  (EA 127 (AAssign V_bits_fill_rectangle_z (Some (EAdd (ENum (1))
  (EVar V_bits_fill_rectangle_z)))) 116)::(EA 128 AWeaken 129)::
  (EA 129 (AAssign V_bits_fill_rectangle_right_mask None) 130)::
  (EA 130 AWeaken 131)::(EA 131 ANone 154)::(EA 131 ANone 143)::
  (EA 131 ANone 132)::(EA 132 (AAssign V_bits_fill_rectangle_line_count1
  (Some (EVar V_bits_fill_rectangle__tmp))) 133)::(EA 133 ANone 134)::
  (EA 134 ANone 135)::(EA 135 (AAssign V_bits_fill_rectangle_line_count1
  (Some (EAdd (EVar V_bits_fill_rectangle_line_count1) (ENum (-1))))) 136)::
  (EA 136 AWeaken 137)::(EA 137 (AGuard
  (fun s => ((eval (EAdd (EVar V_bits_fill_rectangle_line_count1)
  (ENum (-1))) s) <> (eval (ENum (0)) s))%Z)) 140)::(EA 137 (AGuard
  (fun s => ((eval (EAdd (EVar V_bits_fill_rectangle_line_count1)
  (ENum (-1))) s) = (eval (ENum (0)) s))%Z)) 138)::(EA 138 AWeaken 139)::
  (EA 139 ANone 162)::(EA 140 AWeaken 141)::(EA 141 ANone 142)::
  (EA 142 (AAssign V_bits_fill_rectangle_z (Some (EAdd (ENum (1))
  (EVar V_bits_fill_rectangle_z)))) 134)::(EA 143 (AAssign
  V_bits_fill_rectangle_line_count
  (Some (EVar V_bits_fill_rectangle__tmp))) 144)::(EA 144 ANone 145)::
  (EA 145 ANone 146)::(EA 146 (AAssign V_bits_fill_rectangle_line_count
  (Some (EAdd (EVar V_bits_fill_rectangle_line_count) (ENum (-1))))) 147)::
  (EA 147 AWeaken 148)::(EA 148 (AGuard
  (fun s => ((eval (EAdd (EVar V_bits_fill_rectangle_line_count) (ENum (-1)))
  s) <> (eval (ENum (0)) s))%Z)) 151)::(EA 148 (AGuard
  (fun s => ((eval (EAdd (EVar V_bits_fill_rectangle_line_count) (ENum (-1)))
  s) = (eval (ENum (0)) s))%Z)) 149)::(EA 149 AWeaken 150)::
  (EA 150 ANone 162)::(EA 151 AWeaken 152)::(EA 152 ANone 153)::
  (EA 153 (AAssign V_bits_fill_rectangle_z (Some (EAdd (ENum (1))
  (EVar V_bits_fill_rectangle_z)))) 145)::(EA 154 (AAssign
  V_bits_fill_rectangle_line_count3
  (Some (EVar V_bits_fill_rectangle__tmp))) 155)::(EA 155 ANone 156)::
  (EA 156 ANone 157)::(EA 157 (AAssign V_bits_fill_rectangle_line_count3
  (Some (EAdd (EVar V_bits_fill_rectangle_line_count3) (ENum (-1))))) 158)::
  (EA 158 AWeaken 159)::(EA 159 (AGuard
  (fun s => ((eval (EAdd (EVar V_bits_fill_rectangle_line_count3)
  (ENum (-1))) s) <> (eval (ENum (0)) s))%Z)) 165)::(EA 159 (AGuard
  (fun s => ((eval (EAdd (EVar V_bits_fill_rectangle_line_count3)
  (ENum (-1))) s) = (eval (ENum (0)) s))%Z)) 160)::(EA 160 AWeaken 161)::
  (EA 161 ANone 162)::(EA 162 ANone 163)::(EA 163 AWeaken 164)::
  (EA 165 AWeaken 166)::(EA 166 ANone 167)::(EA 167 (AAssign
  V_bits_fill_rectangle_z (Some (EAdd (ENum (1))
  (EVar V_bits_fill_rectangle_z)))) 156)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_bits_fill_rectangle => Pedges_bits_fill_rectangle
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_bits_fill_rectangle => 164
     end)%positive;
  var_global := var_global
}.

Definition ai_bits_fill_rectangle (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 3 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ 1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 4 => (1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 5 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ 1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 6 => (1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 7 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ 1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 8 => (1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 9 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ 1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 10 => (1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 11 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ 1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 12 => (-1 * s V_bits_fill_rectangle_last_bit <= 0 /\ 1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 13 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ 1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 14 => (-1 * s V_bits_fill_rectangle_last_bit <= 0 /\ 1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 15 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ 1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 16 => (-1 * s V_bits_fill_rectangle_last_bit <= 0 /\ 1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 17 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ 1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 18 => (-1 * s V_bits_fill_rectangle_last_bit <= 0 /\ 1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 19 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ 1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 20 => (-1 * s V_bits_fill_rectangle_last_bit <= 0 /\ 1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 21 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 22 => (-1 * s V_bits_fill_rectangle_last_bit <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 23 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 24 => (-1 * s V_bits_fill_rectangle_last_bit <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 25 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0 /\ 1 * s V_bits_fill_rectangle_line_count13 + -1 <= 0 /\ -1 * s V_bits_fill_rectangle_line_count13 + 1 <= 0)%Z
   | 26 => (-1 * s V_bits_fill_rectangle_line_count13 + 1 <= 0 /\ 1 * s V_bits_fill_rectangle_line_count13 + -1 <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 27 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 28 => (-1 * s V_bits_fill_rectangle_last_bit <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 29 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 30 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ 1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 31 => (-1 * s V_bits_fill_rectangle_last_bit <= 0 /\ 1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 32 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 33 => (-1 * s V_bits_fill_rectangle_last_bit <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 34 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 35 => (-1 * s V_bits_fill_rectangle_last_bit <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 36 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0 /\ 1 * s V_bits_fill_rectangle_line_count11 + -1 <= 0 /\ -1 * s V_bits_fill_rectangle_line_count11 + 1 <= 0)%Z
   | 37 => (-1 * s V_bits_fill_rectangle_line_count11 + 1 <= 0 /\ 1 * s V_bits_fill_rectangle_line_count11 + -1 <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 38 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 39 => (-1 * s V_bits_fill_rectangle_last_bit <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 40 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 41 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ 1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 42 => (-1 * s V_bits_fill_rectangle_last_bit <= 0 /\ 1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 43 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 44 => (-1 * s V_bits_fill_rectangle_last_bit <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 45 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 46 => (-1 * s V_bits_fill_rectangle_last_bit <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 47 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0 /\ 1 * s V_bits_fill_rectangle_line_count15 + -1 <= 0 /\ -1 * s V_bits_fill_rectangle_line_count15 + 1 <= 0)%Z
   | 48 => (-1 * s V_bits_fill_rectangle_line_count15 + 1 <= 0 /\ 1 * s V_bits_fill_rectangle_line_count15 + -1 <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 49 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 50 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 51 => (-1 * s V_bits_fill_rectangle_last_bit <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 52 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 53 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ 1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 54 => (-1 * s V_bits_fill_rectangle_last_bit <= 0 /\ 1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 55 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ 1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 56 => (-1 * s V_bits_fill_rectangle_last_bit <= 0 /\ 1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 57 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 58 => (-1 * s V_bits_fill_rectangle_last_bit <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 59 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 60 => (-1 * s V_bits_fill_rectangle_last_bit <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 61 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0 /\ 1 * s V_bits_fill_rectangle_line_count7 + -1 <= 0 /\ -1 * s V_bits_fill_rectangle_line_count7 + 1 <= 0)%Z
   | 62 => (-1 * s V_bits_fill_rectangle_line_count7 + 1 <= 0 /\ 1 * s V_bits_fill_rectangle_line_count7 + -1 <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 63 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 64 => (-1 * s V_bits_fill_rectangle_last_bit <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 65 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 66 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ 1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 67 => (-1 * s V_bits_fill_rectangle_last_bit <= 0 /\ 1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 68 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 69 => (-1 * s V_bits_fill_rectangle_last_bit <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 70 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 71 => (-1 * s V_bits_fill_rectangle_last_bit <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 72 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0 /\ 1 * s V_bits_fill_rectangle_line_count5 + -1 <= 0 /\ -1 * s V_bits_fill_rectangle_line_count5 + 1 <= 0)%Z
   | 73 => (-1 * s V_bits_fill_rectangle_line_count5 + 1 <= 0 /\ 1 * s V_bits_fill_rectangle_line_count5 + -1 <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 74 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 75 => (-1 * s V_bits_fill_rectangle_last_bit <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 76 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 77 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ 1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 78 => (-1 * s V_bits_fill_rectangle_last_bit <= 0 /\ 1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 79 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 80 => (-1 * s V_bits_fill_rectangle_last_bit <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 81 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 82 => (-1 * s V_bits_fill_rectangle_last_bit <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 83 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0 /\ 1 * s V_bits_fill_rectangle_line_count9 + -1 <= 0 /\ -1 * s V_bits_fill_rectangle_line_count9 + 1 <= 0)%Z
   | 84 => (-1 * s V_bits_fill_rectangle_line_count9 + 1 <= 0 /\ 1 * s V_bits_fill_rectangle_line_count9 + -1 <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 85 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 86 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 87 => (-1 * s V_bits_fill_rectangle_last_bit <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 88 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 89 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ 1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 90 => (-1 * s V_bits_fill_rectangle_last_bit <= 0 /\ 1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 91 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ 1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 92 => (-1 * s V_bits_fill_rectangle_last_bit <= 0 /\ 1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 93 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ 1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 94 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 95 => (-1 * s V_bits_fill_rectangle_last_bit <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 96 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 97 => (-1 * s V_bits_fill_rectangle_last_bit <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 98 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0 /\ 1 * s V_bits_fill_rectangle_line_count19 + -1 <= 0 /\ -1 * s V_bits_fill_rectangle_line_count19 + 1 <= 0)%Z
   | 99 => (-1 * s V_bits_fill_rectangle_line_count19 + 1 <= 0 /\ 1 * s V_bits_fill_rectangle_line_count19 + -1 <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 100 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 101 => (-1 * s V_bits_fill_rectangle_last_bit <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 102 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 103 => (-1 * s V_bits_fill_rectangle_last_bit <= 0 /\ 1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 104 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ 1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 105 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 106 => (-1 * s V_bits_fill_rectangle_last_bit <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 107 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 108 => (-1 * s V_bits_fill_rectangle_last_bit <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 109 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0 /\ 1 * s V_bits_fill_rectangle_line_count17 + -1 <= 0 /\ -1 * s V_bits_fill_rectangle_line_count17 + 1 <= 0)%Z
   | 110 => (-1 * s V_bits_fill_rectangle_line_count17 + 1 <= 0 /\ 1 * s V_bits_fill_rectangle_line_count17 + -1 <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 111 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 112 => (-1 * s V_bits_fill_rectangle_last_bit <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 113 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 114 => (-1 * s V_bits_fill_rectangle_last_bit <= 0 /\ 1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 115 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ 1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 116 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 117 => (-1 * s V_bits_fill_rectangle_last_bit <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 118 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 119 => (-1 * s V_bits_fill_rectangle_last_bit <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 120 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0 /\ 1 * s V_bits_fill_rectangle_line_count21 + -1 <= 0 /\ -1 * s V_bits_fill_rectangle_line_count21 + 1 <= 0)%Z
   | 121 => (-1 * s V_bits_fill_rectangle_line_count21 + 1 <= 0 /\ 1 * s V_bits_fill_rectangle_line_count21 + -1 <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 122 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 123 => (-1 * s V_bits_fill_rectangle_last_bit <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 124 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 125 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 126 => (-1 * s V_bits_fill_rectangle_last_bit <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 127 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_last_bit <= 0)%Z
   | 128 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ 1 * s V_bits_fill_rectangle_z <= 0 /\ 1 * s V_bits_fill_rectangle_last_bit + 1 <= 0)%Z
   | 129 => (1 * s V_bits_fill_rectangle_last_bit + 1 <= 0 /\ 1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 130 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ 1 * s V_bits_fill_rectangle_z <= 0 /\ 1 * s V_bits_fill_rectangle_last_bit + 1 <= 0)%Z
   | 131 => (1 * s V_bits_fill_rectangle_last_bit + 1 <= 0 /\ 1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 132 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ 1 * s V_bits_fill_rectangle_z <= 0 /\ 1 * s V_bits_fill_rectangle_last_bit + 1 <= 0)%Z
   | 133 => (1 * s V_bits_fill_rectangle_last_bit + 1 <= 0 /\ 1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 134 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ 1 * s V_bits_fill_rectangle_last_bit + 1 <= 0)%Z
   | 135 => (1 * s V_bits_fill_rectangle_last_bit + 1 <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 136 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ 1 * s V_bits_fill_rectangle_last_bit + 1 <= 0)%Z
   | 137 => (1 * s V_bits_fill_rectangle_last_bit + 1 <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 138 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ 1 * s V_bits_fill_rectangle_last_bit + 1 <= 0 /\ 1 * s V_bits_fill_rectangle_line_count1 + -1 <= 0 /\ -1 * s V_bits_fill_rectangle_line_count1 + 1 <= 0)%Z
   | 139 => (-1 * s V_bits_fill_rectangle_line_count1 + 1 <= 0 /\ 1 * s V_bits_fill_rectangle_line_count1 + -1 <= 0 /\ 1 * s V_bits_fill_rectangle_last_bit + 1 <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 140 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ 1 * s V_bits_fill_rectangle_last_bit + 1 <= 0)%Z
   | 141 => (1 * s V_bits_fill_rectangle_last_bit + 1 <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 142 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ 1 * s V_bits_fill_rectangle_last_bit + 1 <= 0)%Z
   | 143 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ 1 * s V_bits_fill_rectangle_z <= 0 /\ 1 * s V_bits_fill_rectangle_last_bit + 1 <= 0)%Z
   | 144 => (1 * s V_bits_fill_rectangle_last_bit + 1 <= 0 /\ 1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 145 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ 1 * s V_bits_fill_rectangle_last_bit + 1 <= 0)%Z
   | 146 => (1 * s V_bits_fill_rectangle_last_bit + 1 <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 147 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ 1 * s V_bits_fill_rectangle_last_bit + 1 <= 0)%Z
   | 148 => (1 * s V_bits_fill_rectangle_last_bit + 1 <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 149 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ 1 * s V_bits_fill_rectangle_last_bit + 1 <= 0 /\ 1 * s V_bits_fill_rectangle_line_count + -1 <= 0 /\ -1 * s V_bits_fill_rectangle_line_count + 1 <= 0)%Z
   | 150 => (-1 * s V_bits_fill_rectangle_line_count + 1 <= 0 /\ 1 * s V_bits_fill_rectangle_line_count + -1 <= 0 /\ 1 * s V_bits_fill_rectangle_last_bit + 1 <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 151 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ 1 * s V_bits_fill_rectangle_last_bit + 1 <= 0)%Z
   | 152 => (1 * s V_bits_fill_rectangle_last_bit + 1 <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 153 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ 1 * s V_bits_fill_rectangle_last_bit + 1 <= 0)%Z
   | 154 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ 1 * s V_bits_fill_rectangle_z <= 0 /\ 1 * s V_bits_fill_rectangle_last_bit + 1 <= 0)%Z
   | 155 => (1 * s V_bits_fill_rectangle_last_bit + 1 <= 0 /\ 1 * s V_bits_fill_rectangle_z <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 156 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ 1 * s V_bits_fill_rectangle_last_bit + 1 <= 0)%Z
   | 157 => (1 * s V_bits_fill_rectangle_last_bit + 1 <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 158 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ 1 * s V_bits_fill_rectangle_last_bit + 1 <= 0)%Z
   | 159 => (1 * s V_bits_fill_rectangle_last_bit + 1 <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 160 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ 1 * s V_bits_fill_rectangle_last_bit + 1 <= 0 /\ 1 * s V_bits_fill_rectangle_line_count3 + -1 <= 0 /\ -1 * s V_bits_fill_rectangle_line_count3 + 1 <= 0)%Z
   | 161 => (-1 * s V_bits_fill_rectangle_line_count3 + 1 <= 0 /\ 1 * s V_bits_fill_rectangle_line_count3 + -1 <= 0 /\ 1 * s V_bits_fill_rectangle_last_bit + 1 <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 162 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ 1 * s V_bits_fill_rectangle_last_bit + 1 <= 0)%Z
   | 163 => (1 * s V_bits_fill_rectangle_last_bit + 1 <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 164 => (-1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 165 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ 1 * s V_bits_fill_rectangle_last_bit + 1 <= 0)%Z
   | 166 => (1 * s V_bits_fill_rectangle_last_bit + 1 <= 0 /\ -1 * s V_bits_fill_rectangle_z <= 0)%Z
   | 167 => (-1 * s V_bits_fill_rectangle_z <= 0 /\ 1 * s V_bits_fill_rectangle_last_bit + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_bits_fill_rectangle (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (s V_bits_fill_rectangle_height <= z)%Q
   | 2 => (s V_bits_fill_rectangle_height <= z)%Q
   | 3 => (s V_bits_fill_rectangle_height <= z)%Q
   | 4 => (s V_bits_fill_rectangle_height <= z)%Q
   | 5 => (s V_bits_fill_rectangle_height <= z)%Q
   | 6 => (s V_bits_fill_rectangle_height <= z)%Q
   | 7 => (s V_bits_fill_rectangle__tmp <= z)%Q
   | 8 => (s V_bits_fill_rectangle__tmp <= z)%Q
   | 9 => (s V_bits_fill_rectangle__tmp <= z)%Q
   | 10 => (s V_bits_fill_rectangle__tmp <= z)%Q
   | 11 => (s V_bits_fill_rectangle__tmp <= z)%Q
   | 12 => (s V_bits_fill_rectangle__tmp <= z)%Q
   | 13 => (s V_bits_fill_rectangle__tmp <= z)%Q
   | 14 => (s V_bits_fill_rectangle__tmp <= z)%Q
   | 15 => (s V_bits_fill_rectangle__tmp <= z)%Q
   | 16 => (s V_bits_fill_rectangle__tmp <= z)%Q
   | 17 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_bits_fill_rectangle_z)) (F_check_ge (0) (0));
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_bits_fill_rectangle_z) (0))) (F_max0_ge_0 (-
                                                                    s V_bits_fill_rectangle_z))]
     (s V_bits_fill_rectangle__tmp <= z)%Q
   | 18 => (-(1 # 1) + s V_bits_fill_rectangle__tmp
            + s V_bits_fill_rectangle_z <= z)%Q
   | 19 => (-(1 # 1) + s V_bits_fill_rectangle__tmp
            + s V_bits_fill_rectangle_z <= z)%Q
   | 20 => (-(1 # 1) + s V_bits_fill_rectangle_line_count13
            + s V_bits_fill_rectangle_z <= z)%Q
   | 21 => (-(1 # 1) + s V_bits_fill_rectangle_line_count13
            + s V_bits_fill_rectangle_z <= z)%Q
   | 22 => (-(1 # 1) + s V_bits_fill_rectangle_line_count13
            + s V_bits_fill_rectangle_z <= z)%Q
   | 23 => (s V_bits_fill_rectangle_line_count13 + s V_bits_fill_rectangle_z <= z)%Q
   | 24 => (s V_bits_fill_rectangle_line_count13 + s V_bits_fill_rectangle_z <= z)%Q
   | 25 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                 + s V_bits_fill_rectangle_line_count13)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_bits_fill_rectangle_line_count13) (0))) (F_max0_ge_0 (-1
                                                                    + s V_bits_fill_rectangle_line_count13))]
     (s V_bits_fill_rectangle_line_count13 + s V_bits_fill_rectangle_z <= z)%Q
   | 26 => (s V_bits_fill_rectangle_z <= z)%Q
   | 27 => (s V_bits_fill_rectangle_line_count13 + s V_bits_fill_rectangle_z <= z)%Q
   | 28 => (s V_bits_fill_rectangle_line_count13 + s V_bits_fill_rectangle_z <= z)%Q
   | 29 => (s V_bits_fill_rectangle_line_count13 + s V_bits_fill_rectangle_z <= z)%Q
   | 30 => (-(1 # 1) + s V_bits_fill_rectangle__tmp
            + s V_bits_fill_rectangle_z <= z)%Q
   | 31 => (-(1 # 1) + s V_bits_fill_rectangle_line_count11
            + s V_bits_fill_rectangle_z <= z)%Q
   | 32 => (-(1 # 1) + s V_bits_fill_rectangle_line_count11
            + s V_bits_fill_rectangle_z <= z)%Q
   | 33 => (-(1 # 1) + s V_bits_fill_rectangle_line_count11
            + s V_bits_fill_rectangle_z <= z)%Q
   | 34 => (s V_bits_fill_rectangle_line_count11 + s V_bits_fill_rectangle_z <= z)%Q
   | 35 => (s V_bits_fill_rectangle_line_count11 + s V_bits_fill_rectangle_z <= z)%Q
   | 36 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                 + s V_bits_fill_rectangle_line_count11)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_bits_fill_rectangle_line_count11) (0))) (F_max0_ge_0 (-1
                                                                    + s V_bits_fill_rectangle_line_count11))]
     (s V_bits_fill_rectangle_line_count11 + s V_bits_fill_rectangle_z <= z)%Q
   | 37 => (s V_bits_fill_rectangle_z <= z)%Q
   | 38 => (s V_bits_fill_rectangle_line_count11 + s V_bits_fill_rectangle_z <= z)%Q
   | 39 => (s V_bits_fill_rectangle_line_count11 + s V_bits_fill_rectangle_z <= z)%Q
   | 40 => (s V_bits_fill_rectangle_line_count11 + s V_bits_fill_rectangle_z <= z)%Q
   | 41 => (-(1 # 1) + s V_bits_fill_rectangle__tmp
            + s V_bits_fill_rectangle_z <= z)%Q
   | 42 => (-(1 # 1) + s V_bits_fill_rectangle_line_count15
            + s V_bits_fill_rectangle_z <= z)%Q
   | 43 => (-(1 # 1) + s V_bits_fill_rectangle_line_count15
            + s V_bits_fill_rectangle_z <= z)%Q
   | 44 => (-(1 # 1) + s V_bits_fill_rectangle_line_count15
            + s V_bits_fill_rectangle_z <= z)%Q
   | 45 => (s V_bits_fill_rectangle_line_count15 + s V_bits_fill_rectangle_z <= z)%Q
   | 46 => (s V_bits_fill_rectangle_line_count15 + s V_bits_fill_rectangle_z <= z)%Q
   | 47 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                 + s V_bits_fill_rectangle_line_count15)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_bits_fill_rectangle_line_count15) (0))) (F_max0_ge_0 (-1
                                                                    + s V_bits_fill_rectangle_line_count15))]
     (s V_bits_fill_rectangle_line_count15 + s V_bits_fill_rectangle_z <= z)%Q
   | 48 => (s V_bits_fill_rectangle_z <= z)%Q
   | 49 => (s V_bits_fill_rectangle_z <= z)%Q
   | 50 => (s V_bits_fill_rectangle_line_count15 + s V_bits_fill_rectangle_z <= z)%Q
   | 51 => (s V_bits_fill_rectangle_line_count15 + s V_bits_fill_rectangle_z <= z)%Q
   | 52 => (s V_bits_fill_rectangle_line_count15 + s V_bits_fill_rectangle_z <= z)%Q
   | 53 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_bits_fill_rectangle_z)) (F_check_ge (0) (0));
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_bits_fill_rectangle_z) (0))) (F_max0_ge_0 (-
                                                                    s V_bits_fill_rectangle_z))]
     (s V_bits_fill_rectangle__tmp <= z)%Q
   | 54 => (-(1 # 1) + s V_bits_fill_rectangle__tmp
            + s V_bits_fill_rectangle_z <= z)%Q
   | 55 => (-(1 # 1) + s V_bits_fill_rectangle__tmp
            + s V_bits_fill_rectangle_z <= z)%Q
   | 56 => (-(1 # 1) + s V_bits_fill_rectangle_line_count7
            + s V_bits_fill_rectangle_z <= z)%Q
   | 57 => (-(1 # 1) + s V_bits_fill_rectangle_line_count7
            + s V_bits_fill_rectangle_z <= z)%Q
   | 58 => (-(1 # 1) + s V_bits_fill_rectangle_line_count7
            + s V_bits_fill_rectangle_z <= z)%Q
   | 59 => (s V_bits_fill_rectangle_line_count7 + s V_bits_fill_rectangle_z <= z)%Q
   | 60 => (s V_bits_fill_rectangle_line_count7 + s V_bits_fill_rectangle_z <= z)%Q
   | 61 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                 + s V_bits_fill_rectangle_line_count7)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_bits_fill_rectangle_line_count7) (0))) (F_max0_ge_0 (-1
                                                                    + s V_bits_fill_rectangle_line_count7))]
     (s V_bits_fill_rectangle_line_count7 + s V_bits_fill_rectangle_z <= z)%Q
   | 62 => (s V_bits_fill_rectangle_z <= z)%Q
   | 63 => (s V_bits_fill_rectangle_line_count7 + s V_bits_fill_rectangle_z <= z)%Q
   | 64 => (s V_bits_fill_rectangle_line_count7 + s V_bits_fill_rectangle_z <= z)%Q
   | 65 => (s V_bits_fill_rectangle_line_count7 + s V_bits_fill_rectangle_z <= z)%Q
   | 66 => (-(1 # 1) + s V_bits_fill_rectangle__tmp
            + s V_bits_fill_rectangle_z <= z)%Q
   | 67 => (-(1 # 1) + s V_bits_fill_rectangle_line_count5
            + s V_bits_fill_rectangle_z <= z)%Q
   | 68 => (-(1 # 1) + s V_bits_fill_rectangle_line_count5
            + s V_bits_fill_rectangle_z <= z)%Q
   | 69 => (-(1 # 1) + s V_bits_fill_rectangle_line_count5
            + s V_bits_fill_rectangle_z <= z)%Q
   | 70 => (s V_bits_fill_rectangle_line_count5 + s V_bits_fill_rectangle_z <= z)%Q
   | 71 => (s V_bits_fill_rectangle_line_count5 + s V_bits_fill_rectangle_z <= z)%Q
   | 72 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                 + s V_bits_fill_rectangle_line_count5)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_bits_fill_rectangle_line_count5) (0))) (F_max0_ge_0 (-1
                                                                    + s V_bits_fill_rectangle_line_count5))]
     (s V_bits_fill_rectangle_line_count5 + s V_bits_fill_rectangle_z <= z)%Q
   | 73 => (s V_bits_fill_rectangle_z <= z)%Q
   | 74 => (s V_bits_fill_rectangle_line_count5 + s V_bits_fill_rectangle_z <= z)%Q
   | 75 => (s V_bits_fill_rectangle_line_count5 + s V_bits_fill_rectangle_z <= z)%Q
   | 76 => (s V_bits_fill_rectangle_line_count5 + s V_bits_fill_rectangle_z <= z)%Q
   | 77 => (-(1 # 1) + s V_bits_fill_rectangle__tmp
            + s V_bits_fill_rectangle_z <= z)%Q
   | 78 => (-(1 # 1) + s V_bits_fill_rectangle_line_count9
            + s V_bits_fill_rectangle_z <= z)%Q
   | 79 => (-(1 # 1) + s V_bits_fill_rectangle_line_count9
            + s V_bits_fill_rectangle_z <= z)%Q
   | 80 => (-(1 # 1) + s V_bits_fill_rectangle_line_count9
            + s V_bits_fill_rectangle_z <= z)%Q
   | 81 => (s V_bits_fill_rectangle_line_count9 + s V_bits_fill_rectangle_z <= z)%Q
   | 82 => (s V_bits_fill_rectangle_line_count9 + s V_bits_fill_rectangle_z <= z)%Q
   | 83 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                 + s V_bits_fill_rectangle_line_count9)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_bits_fill_rectangle_line_count9) (0))) (F_max0_ge_0 (-1
                                                                    + s V_bits_fill_rectangle_line_count9))]
     (s V_bits_fill_rectangle_line_count9 + s V_bits_fill_rectangle_z <= z)%Q
   | 84 => (s V_bits_fill_rectangle_z <= z)%Q
   | 85 => (s V_bits_fill_rectangle_z <= z)%Q
   | 86 => (s V_bits_fill_rectangle_line_count9 + s V_bits_fill_rectangle_z <= z)%Q
   | 87 => (s V_bits_fill_rectangle_line_count9 + s V_bits_fill_rectangle_z <= z)%Q
   | 88 => (s V_bits_fill_rectangle_line_count9 + s V_bits_fill_rectangle_z <= z)%Q
   | 89 => (s V_bits_fill_rectangle__tmp <= z)%Q
   | 90 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_bits_fill_rectangle_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_bits_fill_rectangle_z) (0))) (F_max0_ge_0 (-
                                                                    s V_bits_fill_rectangle_z))]
     (s V_bits_fill_rectangle__tmp <= z)%Q
   | 91 => (-(1 # 1) + s V_bits_fill_rectangle__tmp
            + s V_bits_fill_rectangle_z <= z)%Q
   | 92 => (-(1 # 1) + s V_bits_fill_rectangle__tmp
            + s V_bits_fill_rectangle_z <= z)%Q
   | 93 => (-(1 # 1) + s V_bits_fill_rectangle_line_count19
            + s V_bits_fill_rectangle_z <= z)%Q
   | 94 => (-(1 # 1) + s V_bits_fill_rectangle_line_count19
            + s V_bits_fill_rectangle_z <= z)%Q
   | 95 => (-(1 # 1) + s V_bits_fill_rectangle_line_count19
            + s V_bits_fill_rectangle_z <= z)%Q
   | 96 => (s V_bits_fill_rectangle_line_count19 + s V_bits_fill_rectangle_z <= z)%Q
   | 97 => (s V_bits_fill_rectangle_line_count19 + s V_bits_fill_rectangle_z <= z)%Q
   | 98 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                 + s V_bits_fill_rectangle_line_count19)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_bits_fill_rectangle_line_count19) (0))) (F_max0_ge_0 (-1
                                                                    + s V_bits_fill_rectangle_line_count19))]
     (s V_bits_fill_rectangle_line_count19 + s V_bits_fill_rectangle_z <= z)%Q
   | 99 => (s V_bits_fill_rectangle_z <= z)%Q
   | 100 => (s V_bits_fill_rectangle_line_count19 + s V_bits_fill_rectangle_z <= z)%Q
   | 101 => (s V_bits_fill_rectangle_line_count19 + s V_bits_fill_rectangle_z <= z)%Q
   | 102 => (s V_bits_fill_rectangle_line_count19 + s V_bits_fill_rectangle_z <= z)%Q
   | 103 => (-(1 # 1) + s V_bits_fill_rectangle__tmp
             + s V_bits_fill_rectangle_z <= z)%Q
   | 104 => (-(1 # 1) + s V_bits_fill_rectangle_line_count17
             + s V_bits_fill_rectangle_z <= z)%Q
   | 105 => (-(1 # 1) + s V_bits_fill_rectangle_line_count17
             + s V_bits_fill_rectangle_z <= z)%Q
   | 106 => (-(1 # 1) + s V_bits_fill_rectangle_line_count17
             + s V_bits_fill_rectangle_z <= z)%Q
   | 107 => (s V_bits_fill_rectangle_line_count17 + s V_bits_fill_rectangle_z <= z)%Q
   | 108 => (s V_bits_fill_rectangle_line_count17 + s V_bits_fill_rectangle_z <= z)%Q
   | 109 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                 + s V_bits_fill_rectangle_line_count17)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_bits_fill_rectangle_line_count17) (0))) (F_max0_ge_0 (-1
                                                                    + s V_bits_fill_rectangle_line_count17))]
     (s V_bits_fill_rectangle_line_count17 + s V_bits_fill_rectangle_z <= z)%Q
   | 110 => (s V_bits_fill_rectangle_z <= z)%Q
   | 111 => (s V_bits_fill_rectangle_line_count17 + s V_bits_fill_rectangle_z <= z)%Q
   | 112 => (s V_bits_fill_rectangle_line_count17 + s V_bits_fill_rectangle_z <= z)%Q
   | 113 => (s V_bits_fill_rectangle_line_count17 + s V_bits_fill_rectangle_z <= z)%Q
   | 114 => (-(1 # 1) + s V_bits_fill_rectangle__tmp
             + s V_bits_fill_rectangle_z <= z)%Q
   | 115 => (-(1 # 1) + s V_bits_fill_rectangle_line_count21
             + s V_bits_fill_rectangle_z <= z)%Q
   | 116 => (-(1 # 1) + s V_bits_fill_rectangle_line_count21
             + s V_bits_fill_rectangle_z <= z)%Q
   | 117 => (-(1 # 1) + s V_bits_fill_rectangle_line_count21
             + s V_bits_fill_rectangle_z <= z)%Q
   | 118 => (s V_bits_fill_rectangle_line_count21 + s V_bits_fill_rectangle_z <= z)%Q
   | 119 => (s V_bits_fill_rectangle_line_count21 + s V_bits_fill_rectangle_z <= z)%Q
   | 120 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                 + s V_bits_fill_rectangle_line_count21)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_bits_fill_rectangle_line_count21) (0))) (F_max0_ge_0 (-1
                                                                    + s V_bits_fill_rectangle_line_count21))]
     (s V_bits_fill_rectangle_line_count21 + s V_bits_fill_rectangle_z <= z)%Q
   | 121 => (s V_bits_fill_rectangle_z <= z)%Q
   | 122 => (s V_bits_fill_rectangle_z <= z)%Q
   | 123 => (s V_bits_fill_rectangle_z <= z)%Q
   | 124 => (s V_bits_fill_rectangle_z <= z)%Q
   | 125 => (s V_bits_fill_rectangle_line_count21 + s V_bits_fill_rectangle_z <= z)%Q
   | 126 => (s V_bits_fill_rectangle_line_count21 + s V_bits_fill_rectangle_z <= z)%Q
   | 127 => (s V_bits_fill_rectangle_line_count21 + s V_bits_fill_rectangle_z <= z)%Q
   | 128 => (s V_bits_fill_rectangle__tmp <= z)%Q
   | 129 => (s V_bits_fill_rectangle__tmp <= z)%Q
   | 130 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_bits_fill_rectangle_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_bits_fill_rectangle_z) (0))) (F_max0_ge_0 (-
                                                                    s V_bits_fill_rectangle_z))]
     (s V_bits_fill_rectangle__tmp <= z)%Q
   | 131 => (-(1 # 1) + s V_bits_fill_rectangle__tmp
             + s V_bits_fill_rectangle_z <= z)%Q
   | 132 => (-(1 # 1) + s V_bits_fill_rectangle__tmp
             + s V_bits_fill_rectangle_z <= z)%Q
   | 133 => (-(1 # 1) + s V_bits_fill_rectangle_line_count1
             + s V_bits_fill_rectangle_z <= z)%Q
   | 134 => (-(1 # 1) + s V_bits_fill_rectangle_line_count1
             + s V_bits_fill_rectangle_z <= z)%Q
   | 135 => (-(1 # 1) + s V_bits_fill_rectangle_line_count1
             + s V_bits_fill_rectangle_z <= z)%Q
   | 136 => (s V_bits_fill_rectangle_line_count1 + s V_bits_fill_rectangle_z <= z)%Q
   | 137 => (s V_bits_fill_rectangle_line_count1 + s V_bits_fill_rectangle_z <= z)%Q
   | 138 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                 + s V_bits_fill_rectangle_line_count1)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_bits_fill_rectangle_line_count1) (0))) (F_max0_ge_0 (-1
                                                                    + s V_bits_fill_rectangle_line_count1))]
     (s V_bits_fill_rectangle_line_count1 + s V_bits_fill_rectangle_z <= z)%Q
   | 139 => (s V_bits_fill_rectangle_z <= z)%Q
   | 140 => (s V_bits_fill_rectangle_line_count1 + s V_bits_fill_rectangle_z <= z)%Q
   | 141 => (s V_bits_fill_rectangle_line_count1 + s V_bits_fill_rectangle_z <= z)%Q
   | 142 => (s V_bits_fill_rectangle_line_count1 + s V_bits_fill_rectangle_z <= z)%Q
   | 143 => (-(1 # 1) + s V_bits_fill_rectangle__tmp
             + s V_bits_fill_rectangle_z <= z)%Q
   | 144 => (-(1 # 1) + s V_bits_fill_rectangle_line_count
             + s V_bits_fill_rectangle_z <= z)%Q
   | 145 => (-(1 # 1) + s V_bits_fill_rectangle_line_count
             + s V_bits_fill_rectangle_z <= z)%Q
   | 146 => (-(1 # 1) + s V_bits_fill_rectangle_line_count
             + s V_bits_fill_rectangle_z <= z)%Q
   | 147 => (s V_bits_fill_rectangle_line_count + s V_bits_fill_rectangle_z <= z)%Q
   | 148 => (s V_bits_fill_rectangle_line_count + s V_bits_fill_rectangle_z <= z)%Q
   | 149 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                 + s V_bits_fill_rectangle_line_count)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_bits_fill_rectangle_line_count) (0))) (F_max0_ge_0 (-1
                                                                    + s V_bits_fill_rectangle_line_count))]
     (s V_bits_fill_rectangle_line_count + s V_bits_fill_rectangle_z <= z)%Q
   | 150 => (s V_bits_fill_rectangle_z <= z)%Q
   | 151 => (s V_bits_fill_rectangle_line_count + s V_bits_fill_rectangle_z <= z)%Q
   | 152 => (s V_bits_fill_rectangle_line_count + s V_bits_fill_rectangle_z <= z)%Q
   | 153 => (s V_bits_fill_rectangle_line_count + s V_bits_fill_rectangle_z <= z)%Q
   | 154 => (-(1 # 1) + s V_bits_fill_rectangle__tmp
             + s V_bits_fill_rectangle_z <= z)%Q
   | 155 => (-(1 # 1) + s V_bits_fill_rectangle_line_count3
             + s V_bits_fill_rectangle_z <= z)%Q
   | 156 => (-(1 # 1) + s V_bits_fill_rectangle_line_count3
             + s V_bits_fill_rectangle_z <= z)%Q
   | 157 => (-(1 # 1) + s V_bits_fill_rectangle_line_count3
             + s V_bits_fill_rectangle_z <= z)%Q
   | 158 => (s V_bits_fill_rectangle_line_count3 + s V_bits_fill_rectangle_z <= z)%Q
   | 159 => (s V_bits_fill_rectangle_line_count3 + s V_bits_fill_rectangle_z <= z)%Q
   | 160 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                 + s V_bits_fill_rectangle_line_count3)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_bits_fill_rectangle_line_count3) (0))) (F_max0_ge_0 (-1
                                                                    + s V_bits_fill_rectangle_line_count3))]
     (s V_bits_fill_rectangle_line_count3 + s V_bits_fill_rectangle_z <= z)%Q
   | 161 => (s V_bits_fill_rectangle_z <= z)%Q
   | 162 => (s V_bits_fill_rectangle_z <= z)%Q
   | 163 => (s V_bits_fill_rectangle_z <= z)%Q
   | 164 => (s V_bits_fill_rectangle_z <= z)%Q
   | 165 => (s V_bits_fill_rectangle_line_count3 + s V_bits_fill_rectangle_z <= z)%Q
   | 166 => (s V_bits_fill_rectangle_line_count3 + s V_bits_fill_rectangle_z <= z)%Q
   | 167 => (s V_bits_fill_rectangle_line_count3 + s V_bits_fill_rectangle_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_bits_fill_rectangle =>
    [mkPA Q (fun n z s => ai_bits_fill_rectangle n s /\ annot0_bits_fill_rectangle n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_bits_fill_rectangle (proc_start P_bits_fill_rectangle) s1 (proc_end P_bits_fill_rectangle) s2 ->
    (s2 V_bits_fill_rectangle_z <= s1 V_bits_fill_rectangle_height)%Q.
Proof.
  prove_bound ipa admissible_ipa P_bits_fill_rectangle.
Qed.
