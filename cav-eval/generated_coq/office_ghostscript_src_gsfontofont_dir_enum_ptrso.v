Require Import pasta.Pasta.

Inductive proc: Type :=
  P_font_dir_enum_ptrs.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_font_dir_enum_ptrs_z := 1%positive.
Notation V_font_dir_enum_ptrs__tmp := 2%positive.
Notation V_font_dir_enum_ptrs__tmp1 := 3%positive.
Notation V_font_dir_enum_ptrs_cci := 4%positive.
Notation V_font_dir_enum_ptrs_count := 5%positive.
Notation V_font_dir_enum_ptrs_offset := 6%positive.
Notation V_font_dir_enum_ptrs_tmask := 7%positive.
Notation V_font_dir_enum_ptrs_vptr_dref_off128 := 8%positive.
Notation V_font_dir_enum_ptrs_vptr_dref_off132 := 9%positive.
Notation V_font_dir_enum_ptrs_vptr_dref_off48_off40 := 10%positive.
Notation V_font_dir_enum_ptrs_index := 11%positive.
Notation V_font_dir_enum_ptrs_pep := 12%positive.
Notation V_font_dir_enum_ptrs_size := 13%positive.
Notation V_font_dir_enum_ptrs_vptr := 14%positive.
Definition Pedges_font_dir_enum_ptrs: list (edge proc) :=
  (EA 1 (AAssign V_font_dir_enum_ptrs_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_font_dir_enum_ptrs_tmask) s) >= (eval (ENum (0))
  s))%Z)) 3)::(EA 3 (AGuard
  (fun s => ((eval (EVar V_font_dir_enum_ptrs_offset) s) >= (eval (ENum (0))
  s))%Z)) 4)::(EA 4 AWeaken 5)::(EA 5 (AAssign V_font_dir_enum_ptrs__tmp1
  (Some (EVar V_font_dir_enum_ptrs_size))) 6)::(EA 6 (AAssign
  V_font_dir_enum_ptrs__tmp (Some (EVar V_font_dir_enum_ptrs_index))) 7)::
  (EA 7 AWeaken 8)::(EA 8 ANone 21)::(EA 8 ANone 18)::(EA 8 ANone 15)::
  (EA 8 ANone 12)::(EA 8 ANone 9)::(EA 9 ANone 10)::(EA 10 ANone 11)::
  (EA 11 AWeaken 57)::(EA 12 ANone 13)::(EA 13 ANone 14)::
  (EA 14 AWeaken 57)::(EA 15 ANone 16)::(EA 16 ANone 17)::
  (EA 17 AWeaken 57)::(EA 18 ANone 19)::(EA 19 ANone 20)::
  (EA 20 AWeaken 57)::(EA 21 (AAssign V_font_dir_enum_ptrs_cci
  (Some (ESub (EVar V_font_dir_enum_ptrs__tmp) (ENum (4))))) 22)::
  (EA 22 (AAssign V_font_dir_enum_ptrs_tmask
  (Some (EVar V_font_dir_enum_ptrs_vptr_dref_off48_off40))) 23)::
  (EA 23 AWeaken 24)::(EA 24 (AGuard
  (fun s => ((eval (EVar V_font_dir_enum_ptrs_cci) s) = (eval (ENum (0))
  s))%Z)) 36)::(EA 24 (AGuard
  (fun s => ((eval (EVar V_font_dir_enum_ptrs_cci) s) <> (eval (ENum (0))
  s))%Z)) 25)::(EA 25 AWeaken 26)::(EA 26 (AGuard
  (fun s => ((eval (EVar V_font_dir_enum_ptrs_cci) s) =
  (eval (EAdd (EVar V_font_dir_enum_ptrs_vptr_dref_off128) (ENum (1)))
  s))%Z)) 31)::(EA 26 (AGuard
  (fun s => ((eval (EVar V_font_dir_enum_ptrs_cci) s) <>
  (eval (EAdd (EVar V_font_dir_enum_ptrs_vptr_dref_off128) (ENum (1)))
  s))%Z)) 27)::(EA 27 AWeaken 28)::(EA 28 (AAssign
  V_font_dir_enum_ptrs_offset (Some (ENum (0)))) 29)::(EA 29 (AAssign
  V_font_dir_enum_ptrs_count (Some (EVar V_font_dir_enum_ptrs_cci))) 30)::
  (EA 30 ANone 35)::(EA 31 AWeaken 32)::(EA 32 (AAssign
  V_font_dir_enum_ptrs_offset
  (Some (EAdd (EVar V_font_dir_enum_ptrs_vptr_dref_off132)
  (ENum (1))))) 33)::(EA 33 (AAssign V_font_dir_enum_ptrs_count
  (Some (ENum (1)))) 34)::(EA 34 ANone 35)::(EA 35 ANone 40)::
  (EA 36 AWeaken 37)::(EA 37 (AAssign V_font_dir_enum_ptrs_offset
  (Some (ENum (0)))) 38)::(EA 38 (AAssign V_font_dir_enum_ptrs_count
  (Some (ENum (1)))) 39)::(EA 39 ANone 40)::(EA 40 ANone 41)::
  (EA 41 AWeaken 42)::(EA 42 (AGuard
  (fun s => ((eval (EVar V_font_dir_enum_ptrs_offset) s) <=
  (eval (EVar V_font_dir_enum_ptrs_tmask) s))%Z)) 46)::(EA 42 (AGuard
  (fun s => ((eval (EVar V_font_dir_enum_ptrs_offset) s) >
  (eval (EVar V_font_dir_enum_ptrs_tmask) s))%Z)) 43)::(EA 43 AWeaken 44)::
  (EA 44 ANone 45)::(EA 45 AWeaken 57)::(EA 46 AWeaken 47)::
  (EA 47 ANone 48)::(EA 47 ANone 59)::(EA 48 (AAssign
  V_font_dir_enum_ptrs_count (Some (EAdd (EVar V_font_dir_enum_ptrs_count)
  (ENum (-1))))) 49)::(EA 49 AWeaken 50)::(EA 50 (AGuard
  (fun s => ((eval (EAdd (EVar V_font_dir_enum_ptrs_count) (ENum (-1))) s) <>
  (eval (ENum (0)) s))%Z)) 58)::(EA 50 (AGuard
  (fun s => ((eval (EAdd (EVar V_font_dir_enum_ptrs_count) (ENum (-1))) s) =
  (eval (ENum (0)) s))%Z)) 51)::(EA 51 AWeaken 52)::(EA 52 (AAssign
  V_font_dir_enum_ptrs_vptr_dref_off128
  (Some (EVar V_font_dir_enum_ptrs_cci))) 53)::(EA 53 (AAssign
  V_font_dir_enum_ptrs_vptr_dref_off132
  (Some (EVar V_font_dir_enum_ptrs_offset))) 54)::(EA 54 ANone 55)::
  (EA 55 ANone 56)::(EA 56 AWeaken 57)::(EA 58 AWeaken 59)::
  (EA 59 ANone 60)::(EA 60 (AAssign V_font_dir_enum_ptrs_offset
  (Some (EAdd (EVar V_font_dir_enum_ptrs_offset) (ENum (1))))) 61)::
  (EA 61 ANone 62)::(EA 62 ANone 63)::(EA 63 (AAssign V_font_dir_enum_ptrs_z
  (Some (EAdd (ENum (1)) (EVar V_font_dir_enum_ptrs_z)))) 64)::
  (EA 64 AWeaken 42)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_font_dir_enum_ptrs => Pedges_font_dir_enum_ptrs
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_font_dir_enum_ptrs => 57
     end)%positive;
  var_global := var_global
}.

Definition ai_font_dir_enum_ptrs (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_font_dir_enum_ptrs_z <= 0 /\ -1 * s V_font_dir_enum_ptrs_z <= 0)%Z
   | 3 => (-1 * s V_font_dir_enum_ptrs_z <= 0 /\ 1 * s V_font_dir_enum_ptrs_z <= 0 /\ -1 * s V_font_dir_enum_ptrs_tmask <= 0)%Z
   | 4 => (-1 * s V_font_dir_enum_ptrs_tmask <= 0 /\ 1 * s V_font_dir_enum_ptrs_z <= 0 /\ -1 * s V_font_dir_enum_ptrs_z <= 0 /\ -1 * s V_font_dir_enum_ptrs_offset <= 0)%Z
   | 5 => (-1 * s V_font_dir_enum_ptrs_offset <= 0 /\ -1 * s V_font_dir_enum_ptrs_z <= 0 /\ 1 * s V_font_dir_enum_ptrs_z <= 0 /\ -1 * s V_font_dir_enum_ptrs_tmask <= 0)%Z
   | 6 => (-1 * s V_font_dir_enum_ptrs_tmask <= 0 /\ 1 * s V_font_dir_enum_ptrs_z <= 0 /\ -1 * s V_font_dir_enum_ptrs_z <= 0 /\ -1 * s V_font_dir_enum_ptrs_offset <= 0)%Z
   | 7 => (-1 * s V_font_dir_enum_ptrs_offset <= 0 /\ -1 * s V_font_dir_enum_ptrs_z <= 0 /\ 1 * s V_font_dir_enum_ptrs_z <= 0 /\ -1 * s V_font_dir_enum_ptrs_tmask <= 0)%Z
   | 8 => (-1 * s V_font_dir_enum_ptrs_tmask <= 0 /\ 1 * s V_font_dir_enum_ptrs_z <= 0 /\ -1 * s V_font_dir_enum_ptrs_z <= 0 /\ -1 * s V_font_dir_enum_ptrs_offset <= 0)%Z
   | 9 => (-1 * s V_font_dir_enum_ptrs_offset <= 0 /\ -1 * s V_font_dir_enum_ptrs_z <= 0 /\ 1 * s V_font_dir_enum_ptrs_z <= 0 /\ -1 * s V_font_dir_enum_ptrs_tmask <= 0)%Z
   | 10 => (-1 * s V_font_dir_enum_ptrs_tmask <= 0 /\ 1 * s V_font_dir_enum_ptrs_z <= 0 /\ -1 * s V_font_dir_enum_ptrs_z <= 0 /\ -1 * s V_font_dir_enum_ptrs_offset <= 0)%Z
   | 11 => (-1 * s V_font_dir_enum_ptrs_offset <= 0 /\ -1 * s V_font_dir_enum_ptrs_z <= 0 /\ 1 * s V_font_dir_enum_ptrs_z <= 0 /\ -1 * s V_font_dir_enum_ptrs_tmask <= 0)%Z
   | 12 => (-1 * s V_font_dir_enum_ptrs_offset <= 0 /\ -1 * s V_font_dir_enum_ptrs_z <= 0 /\ 1 * s V_font_dir_enum_ptrs_z <= 0 /\ -1 * s V_font_dir_enum_ptrs_tmask <= 0)%Z
   | 13 => (-1 * s V_font_dir_enum_ptrs_tmask <= 0 /\ 1 * s V_font_dir_enum_ptrs_z <= 0 /\ -1 * s V_font_dir_enum_ptrs_z <= 0 /\ -1 * s V_font_dir_enum_ptrs_offset <= 0)%Z
   | 14 => (-1 * s V_font_dir_enum_ptrs_offset <= 0 /\ -1 * s V_font_dir_enum_ptrs_z <= 0 /\ 1 * s V_font_dir_enum_ptrs_z <= 0 /\ -1 * s V_font_dir_enum_ptrs_tmask <= 0)%Z
   | 15 => (-1 * s V_font_dir_enum_ptrs_offset <= 0 /\ -1 * s V_font_dir_enum_ptrs_z <= 0 /\ 1 * s V_font_dir_enum_ptrs_z <= 0 /\ -1 * s V_font_dir_enum_ptrs_tmask <= 0)%Z
   | 16 => (-1 * s V_font_dir_enum_ptrs_tmask <= 0 /\ 1 * s V_font_dir_enum_ptrs_z <= 0 /\ -1 * s V_font_dir_enum_ptrs_z <= 0 /\ -1 * s V_font_dir_enum_ptrs_offset <= 0)%Z
   | 17 => (-1 * s V_font_dir_enum_ptrs_offset <= 0 /\ -1 * s V_font_dir_enum_ptrs_z <= 0 /\ 1 * s V_font_dir_enum_ptrs_z <= 0 /\ -1 * s V_font_dir_enum_ptrs_tmask <= 0)%Z
   | 18 => (-1 * s V_font_dir_enum_ptrs_offset <= 0 /\ -1 * s V_font_dir_enum_ptrs_z <= 0 /\ 1 * s V_font_dir_enum_ptrs_z <= 0 /\ -1 * s V_font_dir_enum_ptrs_tmask <= 0)%Z
   | 19 => (-1 * s V_font_dir_enum_ptrs_tmask <= 0 /\ 1 * s V_font_dir_enum_ptrs_z <= 0 /\ -1 * s V_font_dir_enum_ptrs_z <= 0 /\ -1 * s V_font_dir_enum_ptrs_offset <= 0)%Z
   | 20 => (-1 * s V_font_dir_enum_ptrs_offset <= 0 /\ -1 * s V_font_dir_enum_ptrs_z <= 0 /\ 1 * s V_font_dir_enum_ptrs_z <= 0 /\ -1 * s V_font_dir_enum_ptrs_tmask <= 0)%Z
   | 21 => (-1 * s V_font_dir_enum_ptrs_offset <= 0 /\ -1 * s V_font_dir_enum_ptrs_z <= 0 /\ 1 * s V_font_dir_enum_ptrs_z <= 0 /\ -1 * s V_font_dir_enum_ptrs_tmask <= 0)%Z
   | 22 => (-1 * s V_font_dir_enum_ptrs_tmask <= 0 /\ 1 * s V_font_dir_enum_ptrs_z <= 0 /\ -1 * s V_font_dir_enum_ptrs_z <= 0 /\ -1 * s V_font_dir_enum_ptrs_offset <= 0)%Z
   | 23 => (-1 * s V_font_dir_enum_ptrs_offset <= 0 /\ -1 * s V_font_dir_enum_ptrs_z <= 0 /\ 1 * s V_font_dir_enum_ptrs_z <= 0)%Z
   | 24 => (1 * s V_font_dir_enum_ptrs_z <= 0 /\ -1 * s V_font_dir_enum_ptrs_z <= 0 /\ -1 * s V_font_dir_enum_ptrs_offset <= 0)%Z
   | 25 => (-1 * s V_font_dir_enum_ptrs_offset <= 0 /\ -1 * s V_font_dir_enum_ptrs_z <= 0 /\ 1 * s V_font_dir_enum_ptrs_z <= 0)%Z
   | 26 => (1 * s V_font_dir_enum_ptrs_z <= 0 /\ -1 * s V_font_dir_enum_ptrs_z <= 0 /\ -1 * s V_font_dir_enum_ptrs_offset <= 0)%Z
   | 27 => (-1 * s V_font_dir_enum_ptrs_offset <= 0 /\ -1 * s V_font_dir_enum_ptrs_z <= 0 /\ 1 * s V_font_dir_enum_ptrs_z <= 0)%Z
   | 28 => (1 * s V_font_dir_enum_ptrs_z <= 0 /\ -1 * s V_font_dir_enum_ptrs_z <= 0 /\ -1 * s V_font_dir_enum_ptrs_offset <= 0)%Z
   | 29 => (-1 * s V_font_dir_enum_ptrs_z <= 0 /\ 1 * s V_font_dir_enum_ptrs_z <= 0 /\ 1 * s V_font_dir_enum_ptrs_offset <= 0 /\ -1 * s V_font_dir_enum_ptrs_offset <= 0)%Z
   | 30 => (-1 * s V_font_dir_enum_ptrs_offset <= 0 /\ 1 * s V_font_dir_enum_ptrs_offset <= 0 /\ 1 * s V_font_dir_enum_ptrs_z <= 0 /\ -1 * s V_font_dir_enum_ptrs_z <= 0)%Z
   | 31 => (-1 * s V_font_dir_enum_ptrs_offset <= 0 /\ -1 * s V_font_dir_enum_ptrs_z <= 0 /\ 1 * s V_font_dir_enum_ptrs_z <= 0 /\ 1 * s V_font_dir_enum_ptrs_cci+ -1 * s V_font_dir_enum_ptrs_vptr_dref_off128 + -1 <= 0 /\ -1 * s V_font_dir_enum_ptrs_cci+ 1 * s V_font_dir_enum_ptrs_vptr_dref_off128 + 1 <= 0)%Z
   | 32 => (-1 * s V_font_dir_enum_ptrs_cci+ 1 * s V_font_dir_enum_ptrs_vptr_dref_off128 + 1 <= 0 /\ 1 * s V_font_dir_enum_ptrs_cci+ -1 * s V_font_dir_enum_ptrs_vptr_dref_off128 + -1 <= 0 /\ 1 * s V_font_dir_enum_ptrs_z <= 0 /\ -1 * s V_font_dir_enum_ptrs_z <= 0 /\ -1 * s V_font_dir_enum_ptrs_offset <= 0)%Z
   | 33 => (-1 * s V_font_dir_enum_ptrs_z <= 0 /\ 1 * s V_font_dir_enum_ptrs_z <= 0 /\ 1 * s V_font_dir_enum_ptrs_cci+ -1 * s V_font_dir_enum_ptrs_vptr_dref_off128 + -1 <= 0 /\ -1 * s V_font_dir_enum_ptrs_cci+ 1 * s V_font_dir_enum_ptrs_vptr_dref_off128 + 1 <= 0)%Z
   | 34 => (-1 * s V_font_dir_enum_ptrs_cci+ 1 * s V_font_dir_enum_ptrs_vptr_dref_off128 + 1 <= 0 /\ 1 * s V_font_dir_enum_ptrs_cci+ -1 * s V_font_dir_enum_ptrs_vptr_dref_off128 + -1 <= 0 /\ 1 * s V_font_dir_enum_ptrs_z <= 0 /\ -1 * s V_font_dir_enum_ptrs_z <= 0 /\ 1 * s V_font_dir_enum_ptrs_count + -1 <= 0 /\ -1 * s V_font_dir_enum_ptrs_count + 1 <= 0)%Z
   | 35 => (-1 * s V_font_dir_enum_ptrs_z <= 0 /\ 1 * s V_font_dir_enum_ptrs_z <= 0)%Z
   | 36 => (-1 * s V_font_dir_enum_ptrs_offset <= 0 /\ -1 * s V_font_dir_enum_ptrs_z <= 0 /\ 1 * s V_font_dir_enum_ptrs_z <= 0 /\ 1 * s V_font_dir_enum_ptrs_cci <= 0 /\ -1 * s V_font_dir_enum_ptrs_cci <= 0)%Z
   | 37 => (-1 * s V_font_dir_enum_ptrs_cci <= 0 /\ 1 * s V_font_dir_enum_ptrs_cci <= 0 /\ 1 * s V_font_dir_enum_ptrs_z <= 0 /\ -1 * s V_font_dir_enum_ptrs_z <= 0 /\ -1 * s V_font_dir_enum_ptrs_offset <= 0)%Z
   | 38 => (-1 * s V_font_dir_enum_ptrs_z <= 0 /\ 1 * s V_font_dir_enum_ptrs_z <= 0 /\ 1 * s V_font_dir_enum_ptrs_cci <= 0 /\ -1 * s V_font_dir_enum_ptrs_cci <= 0 /\ 1 * s V_font_dir_enum_ptrs_offset <= 0 /\ -1 * s V_font_dir_enum_ptrs_offset <= 0)%Z
   | 39 => (-1 * s V_font_dir_enum_ptrs_offset <= 0 /\ 1 * s V_font_dir_enum_ptrs_offset <= 0 /\ -1 * s V_font_dir_enum_ptrs_cci <= 0 /\ 1 * s V_font_dir_enum_ptrs_cci <= 0 /\ 1 * s V_font_dir_enum_ptrs_z <= 0 /\ -1 * s V_font_dir_enum_ptrs_z <= 0 /\ 1 * s V_font_dir_enum_ptrs_count + -1 <= 0 /\ -1 * s V_font_dir_enum_ptrs_count + 1 <= 0)%Z
   | 40 => (-1 * s V_font_dir_enum_ptrs_z <= 0 /\ 1 * s V_font_dir_enum_ptrs_z <= 0)%Z
   | 41 => (1 * s V_font_dir_enum_ptrs_z <= 0 /\ -1 * s V_font_dir_enum_ptrs_z <= 0)%Z
   | 42 => (-1 * s V_font_dir_enum_ptrs_z <= 0)%Z
   | 43 => (-1 * s V_font_dir_enum_ptrs_z <= 0 /\ -1 * s V_font_dir_enum_ptrs_offset+ 1 * s V_font_dir_enum_ptrs_tmask + 1 <= 0)%Z
   | 44 => (-1 * s V_font_dir_enum_ptrs_offset+ 1 * s V_font_dir_enum_ptrs_tmask + 1 <= 0 /\ -1 * s V_font_dir_enum_ptrs_z <= 0)%Z
   | 45 => (-1 * s V_font_dir_enum_ptrs_z <= 0 /\ -1 * s V_font_dir_enum_ptrs_offset+ 1 * s V_font_dir_enum_ptrs_tmask + 1 <= 0)%Z
   | 46 => (-1 * s V_font_dir_enum_ptrs_z <= 0 /\ 1 * s V_font_dir_enum_ptrs_offset+ -1 * s V_font_dir_enum_ptrs_tmask <= 0)%Z
   | 47 => (1 * s V_font_dir_enum_ptrs_offset+ -1 * s V_font_dir_enum_ptrs_tmask <= 0 /\ -1 * s V_font_dir_enum_ptrs_z <= 0)%Z
   | 48 => (-1 * s V_font_dir_enum_ptrs_z <= 0 /\ 1 * s V_font_dir_enum_ptrs_offset+ -1 * s V_font_dir_enum_ptrs_tmask <= 0)%Z
   | 49 => (1 * s V_font_dir_enum_ptrs_offset+ -1 * s V_font_dir_enum_ptrs_tmask <= 0 /\ -1 * s V_font_dir_enum_ptrs_z <= 0)%Z
   | 50 => (-1 * s V_font_dir_enum_ptrs_z <= 0 /\ 1 * s V_font_dir_enum_ptrs_offset+ -1 * s V_font_dir_enum_ptrs_tmask <= 0)%Z
   | 51 => (1 * s V_font_dir_enum_ptrs_offset+ -1 * s V_font_dir_enum_ptrs_tmask <= 0 /\ -1 * s V_font_dir_enum_ptrs_z <= 0 /\ 1 * s V_font_dir_enum_ptrs_count + -1 <= 0 /\ -1 * s V_font_dir_enum_ptrs_count + 1 <= 0)%Z
   | 52 => (-1 * s V_font_dir_enum_ptrs_count + 1 <= 0 /\ 1 * s V_font_dir_enum_ptrs_count + -1 <= 0 /\ -1 * s V_font_dir_enum_ptrs_z <= 0 /\ 1 * s V_font_dir_enum_ptrs_offset+ -1 * s V_font_dir_enum_ptrs_tmask <= 0)%Z
   | 53 => (1 * s V_font_dir_enum_ptrs_offset+ -1 * s V_font_dir_enum_ptrs_tmask <= 0 /\ -1 * s V_font_dir_enum_ptrs_z <= 0 /\ 1 * s V_font_dir_enum_ptrs_count + -1 <= 0 /\ -1 * s V_font_dir_enum_ptrs_count + 1 <= 0)%Z
   | 54 => (-1 * s V_font_dir_enum_ptrs_count + 1 <= 0 /\ 1 * s V_font_dir_enum_ptrs_count + -1 <= 0 /\ -1 * s V_font_dir_enum_ptrs_z <= 0 /\ 1 * s V_font_dir_enum_ptrs_offset+ -1 * s V_font_dir_enum_ptrs_tmask <= 0 /\ -1 * s V_font_dir_enum_ptrs_tmask+ 1 * s V_font_dir_enum_ptrs_vptr_dref_off132 <= 0)%Z
   | 55 => (-1 * s V_font_dir_enum_ptrs_tmask+ 1 * s V_font_dir_enum_ptrs_vptr_dref_off132 <= 0 /\ 1 * s V_font_dir_enum_ptrs_offset+ -1 * s V_font_dir_enum_ptrs_tmask <= 0 /\ -1 * s V_font_dir_enum_ptrs_z <= 0 /\ 1 * s V_font_dir_enum_ptrs_count + -1 <= 0 /\ -1 * s V_font_dir_enum_ptrs_count + 1 <= 0)%Z
   | 56 => (-1 * s V_font_dir_enum_ptrs_count + 1 <= 0 /\ 1 * s V_font_dir_enum_ptrs_count + -1 <= 0 /\ -1 * s V_font_dir_enum_ptrs_z <= 0 /\ 1 * s V_font_dir_enum_ptrs_offset+ -1 * s V_font_dir_enum_ptrs_tmask <= 0 /\ -1 * s V_font_dir_enum_ptrs_tmask+ 1 * s V_font_dir_enum_ptrs_vptr_dref_off132 <= 0)%Z
   | 57 => (-1 * s V_font_dir_enum_ptrs_z <= 0)%Z
   | 58 => (1 * s V_font_dir_enum_ptrs_offset+ -1 * s V_font_dir_enum_ptrs_tmask <= 0 /\ -1 * s V_font_dir_enum_ptrs_z <= 0)%Z
   | 59 => (-1 * s V_font_dir_enum_ptrs_z <= 0 /\ 1 * s V_font_dir_enum_ptrs_offset+ -1 * s V_font_dir_enum_ptrs_tmask <= 0)%Z
   | 60 => (1 * s V_font_dir_enum_ptrs_offset+ -1 * s V_font_dir_enum_ptrs_tmask <= 0 /\ -1 * s V_font_dir_enum_ptrs_z <= 0)%Z
   | 61 => (-1 * s V_font_dir_enum_ptrs_z <= 0 /\ 1 * s V_font_dir_enum_ptrs_offset+ -1 * s V_font_dir_enum_ptrs_tmask + -1 <= 0)%Z
   | 62 => (1 * s V_font_dir_enum_ptrs_offset+ -1 * s V_font_dir_enum_ptrs_tmask + -1 <= 0 /\ -1 * s V_font_dir_enum_ptrs_z <= 0)%Z
   | 63 => (-1 * s V_font_dir_enum_ptrs_z <= 0 /\ 1 * s V_font_dir_enum_ptrs_offset+ -1 * s V_font_dir_enum_ptrs_tmask + -1 <= 0)%Z
   | 64 => (1 * s V_font_dir_enum_ptrs_offset+ -1 * s V_font_dir_enum_ptrs_tmask + -1 <= 0 /\ -1 * s V_font_dir_enum_ptrs_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_font_dir_enum_ptrs (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(1 + s V_font_dir_enum_ptrs_vptr_dref_off48_off40)
           + max0(-s V_font_dir_enum_ptrs_vptr_dref_off132
                  + s V_font_dir_enum_ptrs_vptr_dref_off48_off40) <= z)%Q
   | 2 => (s V_font_dir_enum_ptrs_z
           + max0(1 + s V_font_dir_enum_ptrs_vptr_dref_off48_off40)
           + max0(-s V_font_dir_enum_ptrs_vptr_dref_off132
                  + s V_font_dir_enum_ptrs_vptr_dref_off48_off40) <= z)%Q
   | 3 => (s V_font_dir_enum_ptrs_z
           + max0(1 + s V_font_dir_enum_ptrs_vptr_dref_off48_off40)
           + max0(-s V_font_dir_enum_ptrs_vptr_dref_off132
                  + s V_font_dir_enum_ptrs_vptr_dref_off48_off40) <= z)%Q
   | 4 => (s V_font_dir_enum_ptrs_z
           + max0(1 + s V_font_dir_enum_ptrs_vptr_dref_off48_off40)
           + max0(-s V_font_dir_enum_ptrs_vptr_dref_off132
                  + s V_font_dir_enum_ptrs_vptr_dref_off48_off40) <= z)%Q
   | 5 => (s V_font_dir_enum_ptrs_z
           + max0(1 + s V_font_dir_enum_ptrs_vptr_dref_off48_off40)
           + max0(-s V_font_dir_enum_ptrs_vptr_dref_off132
                  + s V_font_dir_enum_ptrs_vptr_dref_off48_off40) <= z)%Q
   | 6 => (s V_font_dir_enum_ptrs_z
           + max0(1 + s V_font_dir_enum_ptrs_vptr_dref_off48_off40)
           + max0(-s V_font_dir_enum_ptrs_vptr_dref_off132
                  + s V_font_dir_enum_ptrs_vptr_dref_off48_off40) <= z)%Q
   | 7 => (s V_font_dir_enum_ptrs_z
           + max0(1 + s V_font_dir_enum_ptrs_vptr_dref_off48_off40)
           + max0(-s V_font_dir_enum_ptrs_vptr_dref_off132
                  + s V_font_dir_enum_ptrs_vptr_dref_off48_off40) <= z)%Q
   | 8 => (s V_font_dir_enum_ptrs_z
           + max0(1 + s V_font_dir_enum_ptrs_vptr_dref_off48_off40)
           + max0(-s V_font_dir_enum_ptrs_vptr_dref_off132
                  + s V_font_dir_enum_ptrs_vptr_dref_off48_off40) <= z)%Q
   | 9 => (s V_font_dir_enum_ptrs_z
           + max0(1 + s V_font_dir_enum_ptrs_vptr_dref_off48_off40)
           + max0(-s V_font_dir_enum_ptrs_vptr_dref_off132
                  + s V_font_dir_enum_ptrs_vptr_dref_off48_off40) <= z)%Q
   | 10 => (s V_font_dir_enum_ptrs_z
            + max0(1 + s V_font_dir_enum_ptrs_vptr_dref_off48_off40)
            + max0(-s V_font_dir_enum_ptrs_vptr_dref_off132
                   + s V_font_dir_enum_ptrs_vptr_dref_off48_off40) <= z)%Q
   | 11 => hints
     [(*-1 0*) F_max0_ge_0 (1 + s V_font_dir_enum_ptrs_vptr_dref_off48_off40);
      (*-1 0*) F_max0_ge_0 (-s V_font_dir_enum_ptrs_vptr_dref_off132
                            + s V_font_dir_enum_ptrs_vptr_dref_off48_off40)]
     (s V_font_dir_enum_ptrs_z
      + max0(1 + s V_font_dir_enum_ptrs_vptr_dref_off48_off40)
      + max0(-s V_font_dir_enum_ptrs_vptr_dref_off132
             + s V_font_dir_enum_ptrs_vptr_dref_off48_off40) <= z)%Q
   | 12 => (s V_font_dir_enum_ptrs_z
            + max0(1 + s V_font_dir_enum_ptrs_vptr_dref_off48_off40)
            + max0(-s V_font_dir_enum_ptrs_vptr_dref_off132
                   + s V_font_dir_enum_ptrs_vptr_dref_off48_off40) <= z)%Q
   | 13 => (s V_font_dir_enum_ptrs_z
            + max0(1 + s V_font_dir_enum_ptrs_vptr_dref_off48_off40)
            + max0(-s V_font_dir_enum_ptrs_vptr_dref_off132
                   + s V_font_dir_enum_ptrs_vptr_dref_off48_off40) <= z)%Q
   | 14 => hints
     [(*-1 0*) F_max0_ge_0 (1 + s V_font_dir_enum_ptrs_vptr_dref_off48_off40);
      (*-1 0*) F_max0_ge_0 (-s V_font_dir_enum_ptrs_vptr_dref_off132
                            + s V_font_dir_enum_ptrs_vptr_dref_off48_off40)]
     (s V_font_dir_enum_ptrs_z
      + max0(1 + s V_font_dir_enum_ptrs_vptr_dref_off48_off40)
      + max0(-s V_font_dir_enum_ptrs_vptr_dref_off132
             + s V_font_dir_enum_ptrs_vptr_dref_off48_off40) <= z)%Q
   | 15 => (s V_font_dir_enum_ptrs_z
            + max0(1 + s V_font_dir_enum_ptrs_vptr_dref_off48_off40)
            + max0(-s V_font_dir_enum_ptrs_vptr_dref_off132
                   + s V_font_dir_enum_ptrs_vptr_dref_off48_off40) <= z)%Q
   | 16 => (s V_font_dir_enum_ptrs_z
            + max0(1 + s V_font_dir_enum_ptrs_vptr_dref_off48_off40)
            + max0(-s V_font_dir_enum_ptrs_vptr_dref_off132
                   + s V_font_dir_enum_ptrs_vptr_dref_off48_off40) <= z)%Q
   | 17 => hints
     [(*-1 0*) F_max0_ge_0 (1 + s V_font_dir_enum_ptrs_vptr_dref_off48_off40);
      (*-1 0*) F_max0_ge_0 (-s V_font_dir_enum_ptrs_vptr_dref_off132
                            + s V_font_dir_enum_ptrs_vptr_dref_off48_off40)]
     (s V_font_dir_enum_ptrs_z
      + max0(1 + s V_font_dir_enum_ptrs_vptr_dref_off48_off40)
      + max0(-s V_font_dir_enum_ptrs_vptr_dref_off132
             + s V_font_dir_enum_ptrs_vptr_dref_off48_off40) <= z)%Q
   | 18 => (s V_font_dir_enum_ptrs_z
            + max0(1 + s V_font_dir_enum_ptrs_vptr_dref_off48_off40)
            + max0(-s V_font_dir_enum_ptrs_vptr_dref_off132
                   + s V_font_dir_enum_ptrs_vptr_dref_off48_off40) <= z)%Q
   | 19 => (s V_font_dir_enum_ptrs_z
            + max0(1 + s V_font_dir_enum_ptrs_vptr_dref_off48_off40)
            + max0(-s V_font_dir_enum_ptrs_vptr_dref_off132
                   + s V_font_dir_enum_ptrs_vptr_dref_off48_off40) <= z)%Q
   | 20 => hints
     [(*-1 0*) F_max0_ge_0 (1 + s V_font_dir_enum_ptrs_vptr_dref_off48_off40);
      (*-1 0*) F_max0_ge_0 (-s V_font_dir_enum_ptrs_vptr_dref_off132
                            + s V_font_dir_enum_ptrs_vptr_dref_off48_off40)]
     (s V_font_dir_enum_ptrs_z
      + max0(1 + s V_font_dir_enum_ptrs_vptr_dref_off48_off40)
      + max0(-s V_font_dir_enum_ptrs_vptr_dref_off132
             + s V_font_dir_enum_ptrs_vptr_dref_off48_off40) <= z)%Q
   | 21 => (s V_font_dir_enum_ptrs_z
            + max0(1 + s V_font_dir_enum_ptrs_vptr_dref_off48_off40)
            + max0(-s V_font_dir_enum_ptrs_vptr_dref_off132
                   + s V_font_dir_enum_ptrs_vptr_dref_off48_off40) <= z)%Q
   | 22 => (s V_font_dir_enum_ptrs_z
            + max0(1 + s V_font_dir_enum_ptrs_vptr_dref_off48_off40)
            + max0(-s V_font_dir_enum_ptrs_vptr_dref_off132
                   + s V_font_dir_enum_ptrs_vptr_dref_off48_off40) <= z)%Q
   | 23 => (s V_font_dir_enum_ptrs_z + max0(1 + s V_font_dir_enum_ptrs_tmask)
            + max0(s V_font_dir_enum_ptrs_tmask
                   - s V_font_dir_enum_ptrs_vptr_dref_off132) <= z)%Q
   | 24 => (s V_font_dir_enum_ptrs_z + max0(1 + s V_font_dir_enum_ptrs_tmask)
            + max0(s V_font_dir_enum_ptrs_tmask
                   - s V_font_dir_enum_ptrs_vptr_dref_off132) <= z)%Q
   | 25 => (s V_font_dir_enum_ptrs_z + max0(1 + s V_font_dir_enum_ptrs_tmask)
            + max0(s V_font_dir_enum_ptrs_tmask
                   - s V_font_dir_enum_ptrs_vptr_dref_off132) <= z)%Q
   | 26 => (s V_font_dir_enum_ptrs_z + max0(1 + s V_font_dir_enum_ptrs_tmask)
            + max0(s V_font_dir_enum_ptrs_tmask
                   - s V_font_dir_enum_ptrs_vptr_dref_off132) <= z)%Q
   | 27 => hints
     [(*0 1*) F_max0_ge_0 (s V_font_dir_enum_ptrs_tmask
                           - s V_font_dir_enum_ptrs_vptr_dref_off132)]
     (s V_font_dir_enum_ptrs_z + max0(1 + s V_font_dir_enum_ptrs_tmask)
      + max0(s V_font_dir_enum_ptrs_tmask
             - s V_font_dir_enum_ptrs_vptr_dref_off132) <= z)%Q
   | 28 => (s V_font_dir_enum_ptrs_z + max0(1 + s V_font_dir_enum_ptrs_tmask) <= z)%Q
   | 29 => (s V_font_dir_enum_ptrs_z
            + max0(1 - s V_font_dir_enum_ptrs_offset
                   + s V_font_dir_enum_ptrs_tmask) <= z)%Q
   | 30 => (s V_font_dir_enum_ptrs_z
            + max0(1 - s V_font_dir_enum_ptrs_offset
                   + s V_font_dir_enum_ptrs_tmask) <= z)%Q
   | 31 => hints
     [(*-1 0*) F_max0_ge_0 (1 + s V_font_dir_enum_ptrs_tmask)]
     (s V_font_dir_enum_ptrs_z + max0(1 + s V_font_dir_enum_ptrs_tmask)
      + max0(s V_font_dir_enum_ptrs_tmask
             - s V_font_dir_enum_ptrs_vptr_dref_off132) <= z)%Q
   | 32 => (s V_font_dir_enum_ptrs_z
            + max0(s V_font_dir_enum_ptrs_tmask
                   - s V_font_dir_enum_ptrs_vptr_dref_off132) <= z)%Q
   | 33 => (s V_font_dir_enum_ptrs_z
            + max0(1 - s V_font_dir_enum_ptrs_offset
                   + s V_font_dir_enum_ptrs_tmask) <= z)%Q
   | 34 => (s V_font_dir_enum_ptrs_z
            + max0(1 - s V_font_dir_enum_ptrs_offset
                   + s V_font_dir_enum_ptrs_tmask) <= z)%Q
   | 35 => (s V_font_dir_enum_ptrs_z
            + max0(1 - s V_font_dir_enum_ptrs_offset
                   + s V_font_dir_enum_ptrs_tmask) <= z)%Q
   | 36 => hints
     [(*-1 0*) F_max0_ge_0 (s V_font_dir_enum_ptrs_tmask
                            - s V_font_dir_enum_ptrs_vptr_dref_off132)]
     (s V_font_dir_enum_ptrs_z + max0(1 + s V_font_dir_enum_ptrs_tmask)
      + max0(s V_font_dir_enum_ptrs_tmask
             - s V_font_dir_enum_ptrs_vptr_dref_off132) <= z)%Q
   | 37 => (s V_font_dir_enum_ptrs_z + max0(1 + s V_font_dir_enum_ptrs_tmask) <= z)%Q
   | 38 => (s V_font_dir_enum_ptrs_z
            + max0(1 - s V_font_dir_enum_ptrs_offset
                   + s V_font_dir_enum_ptrs_tmask) <= z)%Q
   | 39 => (s V_font_dir_enum_ptrs_z
            + max0(1 - s V_font_dir_enum_ptrs_offset
                   + s V_font_dir_enum_ptrs_tmask) <= z)%Q
   | 40 => (s V_font_dir_enum_ptrs_z
            + max0(1 - s V_font_dir_enum_ptrs_offset
                   + s V_font_dir_enum_ptrs_tmask) <= z)%Q
   | 41 => (s V_font_dir_enum_ptrs_z
            + max0(1 - s V_font_dir_enum_ptrs_offset
                   + s V_font_dir_enum_ptrs_tmask) <= z)%Q
   | 42 => (s V_font_dir_enum_ptrs_z
            + max0(1 - s V_font_dir_enum_ptrs_offset
                   + s V_font_dir_enum_ptrs_tmask) <= z)%Q
   | 43 => (s V_font_dir_enum_ptrs_z
            + max0(1 - s V_font_dir_enum_ptrs_offset
                   + s V_font_dir_enum_ptrs_tmask) <= z)%Q
   | 44 => (s V_font_dir_enum_ptrs_z
            + max0(1 - s V_font_dir_enum_ptrs_offset
                   + s V_font_dir_enum_ptrs_tmask) <= z)%Q
   | 45 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (1
                                             - s V_font_dir_enum_ptrs_offset
                                             + s V_font_dir_enum_ptrs_tmask) (-
                                                                    s V_font_dir_enum_ptrs_offset
                                                                    + s V_font_dir_enum_ptrs_tmask));
      (*-1 0*) F_max0_ge_0 (-s V_font_dir_enum_ptrs_offset
                            + s V_font_dir_enum_ptrs_tmask)]
     (s V_font_dir_enum_ptrs_z
      + max0(1 - s V_font_dir_enum_ptrs_offset + s V_font_dir_enum_ptrs_tmask) <= z)%Q
   | 46 => hints
     [(*0 1*) F_max0_pre_decrement 1 (1 - s V_font_dir_enum_ptrs_offset
                                      + s V_font_dir_enum_ptrs_tmask) (1)]
     (s V_font_dir_enum_ptrs_z
      + max0(1 - s V_font_dir_enum_ptrs_offset + s V_font_dir_enum_ptrs_tmask) <= z)%Q
   | 47 => ((1 # 1) + s V_font_dir_enum_ptrs_z
            + max0(-s V_font_dir_enum_ptrs_offset
                   + s V_font_dir_enum_ptrs_tmask) <= z)%Q
   | 48 => ((1 # 1) + s V_font_dir_enum_ptrs_z
            + max0(-s V_font_dir_enum_ptrs_offset
                   + s V_font_dir_enum_ptrs_tmask) <= z)%Q
   | 49 => ((1 # 1) + s V_font_dir_enum_ptrs_z
            + max0(-s V_font_dir_enum_ptrs_offset
                   + s V_font_dir_enum_ptrs_tmask) <= z)%Q
   | 50 => ((1 # 1) + s V_font_dir_enum_ptrs_z
            + max0(-s V_font_dir_enum_ptrs_offset
                   + s V_font_dir_enum_ptrs_tmask) <= z)%Q
   | 51 => ((1 # 1) + s V_font_dir_enum_ptrs_z
            + max0(-s V_font_dir_enum_ptrs_offset
                   + s V_font_dir_enum_ptrs_tmask) <= z)%Q
   | 52 => ((1 # 1) + s V_font_dir_enum_ptrs_z
            + max0(-s V_font_dir_enum_ptrs_offset
                   + s V_font_dir_enum_ptrs_tmask) <= z)%Q
   | 53 => (s V_font_dir_enum_ptrs_z
            + max0(1 - s V_font_dir_enum_ptrs_cci
                   + s V_font_dir_enum_ptrs_vptr_dref_off128)
            + max0(-s V_font_dir_enum_ptrs_offset
                   + s V_font_dir_enum_ptrs_tmask) <= z)%Q
   | 54 => (s V_font_dir_enum_ptrs_z
            + max0(1 - s V_font_dir_enum_ptrs_cci
                   + s V_font_dir_enum_ptrs_vptr_dref_off128)
            + max0(-s V_font_dir_enum_ptrs_offset
                   + s V_font_dir_enum_ptrs_tmask) <= z)%Q
   | 55 => (s V_font_dir_enum_ptrs_z
            + max0(1 - s V_font_dir_enum_ptrs_cci
                   + s V_font_dir_enum_ptrs_vptr_dref_off128)
            + max0(-s V_font_dir_enum_ptrs_offset
                   + s V_font_dir_enum_ptrs_tmask) <= z)%Q
   | 56 => hints
     [(*-1 0*) F_max0_ge_0 (-s V_font_dir_enum_ptrs_offset
                            + s V_font_dir_enum_ptrs_tmask);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (1
                                                 - s V_font_dir_enum_ptrs_cci
                                                 + s V_font_dir_enum_ptrs_vptr_dref_off128)) (F_check_ge (0) (0))]
     (s V_font_dir_enum_ptrs_z
      + max0(1 - s V_font_dir_enum_ptrs_cci
             + s V_font_dir_enum_ptrs_vptr_dref_off128)
      + max0(-s V_font_dir_enum_ptrs_offset + s V_font_dir_enum_ptrs_tmask) <= z)%Q
   | 57 => (s V_font_dir_enum_ptrs_z <= z)%Q
   | 58 => ((1 # 1) + s V_font_dir_enum_ptrs_z
            + max0(-s V_font_dir_enum_ptrs_offset
                   + s V_font_dir_enum_ptrs_tmask) <= z)%Q
   | 59 => ((1 # 1) + s V_font_dir_enum_ptrs_z
            + max0(-s V_font_dir_enum_ptrs_offset
                   + s V_font_dir_enum_ptrs_tmask) <= z)%Q
   | 60 => ((1 # 1) + s V_font_dir_enum_ptrs_z
            + max0(-s V_font_dir_enum_ptrs_offset
                   + s V_font_dir_enum_ptrs_tmask) <= z)%Q
   | 61 => ((1 # 1) + s V_font_dir_enum_ptrs_z
            + max0(1 - s V_font_dir_enum_ptrs_offset
                   + s V_font_dir_enum_ptrs_tmask) <= z)%Q
   | 62 => ((1 # 1) + s V_font_dir_enum_ptrs_z
            + max0(1 - s V_font_dir_enum_ptrs_offset
                   + s V_font_dir_enum_ptrs_tmask) <= z)%Q
   | 63 => ((1 # 1) + s V_font_dir_enum_ptrs_z
            + max0(1 - s V_font_dir_enum_ptrs_offset
                   + s V_font_dir_enum_ptrs_tmask) <= z)%Q
   | 64 => (s V_font_dir_enum_ptrs_z
            + max0(1 - s V_font_dir_enum_ptrs_offset
                   + s V_font_dir_enum_ptrs_tmask) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_font_dir_enum_ptrs =>
    [mkPA Q (fun n z s => ai_font_dir_enum_ptrs n s /\ annot0_font_dir_enum_ptrs n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_font_dir_enum_ptrs (proc_start P_font_dir_enum_ptrs) s1 (proc_end P_font_dir_enum_ptrs) s2 ->
    (s2 V_font_dir_enum_ptrs_z <= max0(1
                                       + s1 V_font_dir_enum_ptrs_vptr_dref_off48_off40)
                                  + max0(-s1 V_font_dir_enum_ptrs_vptr_dref_off132
                                         + s1 V_font_dir_enum_ptrs_vptr_dref_off48_off40))%Q.
Proof.
  prove_bound ipa admissible_ipa P_font_dir_enum_ptrs.
Qed.
