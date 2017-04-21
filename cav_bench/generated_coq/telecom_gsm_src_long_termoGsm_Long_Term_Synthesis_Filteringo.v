Require Import pasta.Pasta.

Inductive proc: Type :=
  P_Gsm_Long_Term_Synthesis_Filtering.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_Gsm_Long_Term_Synthesis_Filtering_z := 1%positive.
Notation V_Gsm_Long_Term_Synthesis_Filtering_Nr := 2%positive.
Notation V_Gsm_Long_Term_Synthesis_Filtering_S_dref_off630 := 3%positive.
Notation V_Gsm_Long_Term_Synthesis_Filtering__tmp := 4%positive.
Notation V_Gsm_Long_Term_Synthesis_Filtering__tmp1 := 5%positive.
Notation V_Gsm_Long_Term_Synthesis_Filtering_brp := 6%positive.
Notation V_Gsm_Long_Term_Synthesis_Filtering_drpp := 7%positive.
Notation V_Gsm_Long_Term_Synthesis_Filtering_k := 8%positive.
Notation V_Gsm_Long_Term_Synthesis_Filtering_ltmp := 9%positive.
Notation V_Gsm_Long_Term_Synthesis_Filtering_Ncr := 10%positive.
Notation V_Gsm_Long_Term_Synthesis_Filtering_S := 11%positive.
Notation V_Gsm_Long_Term_Synthesis_Filtering_bcr := 12%positive.
Notation V_Gsm_Long_Term_Synthesis_Filtering_drp := 13%positive.
Notation V_Gsm_Long_Term_Synthesis_Filtering_erp := 14%positive.
Definition Pedges_Gsm_Long_Term_Synthesis_Filtering: list (edge proc) :=
  (EA 1 (AAssign V_Gsm_Long_Term_Synthesis_Filtering_z
  (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_Gsm_Long_Term_Synthesis_Filtering__tmp
  (Some (EVar V_Gsm_Long_Term_Synthesis_Filtering_Ncr))) 3)::(EA 3 (AAssign
  V_Gsm_Long_Term_Synthesis_Filtering__tmp1
  (Some (EVar V_Gsm_Long_Term_Synthesis_Filtering_bcr))) 4)::
  (EA 4 AWeaken 5)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_Gsm_Long_Term_Synthesis_Filtering__tmp) s) <
  (eval (ENum (40)) s))%Z)) 11)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_Gsm_Long_Term_Synthesis_Filtering__tmp) s) >=
  (eval (ENum (40)) s))%Z)) 6)::(EA 6 AWeaken 7)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_Gsm_Long_Term_Synthesis_Filtering__tmp) s) >
  (eval (ENum (120)) s))%Z)) 10)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_Gsm_Long_Term_Synthesis_Filtering__tmp) s) <=
  (eval (ENum (120)) s))%Z)) 8)::(EA 8 AWeaken 9)::(EA 9 ANone 13)::
  (EA 10 AWeaken 12)::(EA 11 AWeaken 12)::(EA 12 ANone 13)::(EA 13 (AAssign
  V_Gsm_Long_Term_Synthesis_Filtering_Nr None) 14)::(EA 14 (AAssign
  V_Gsm_Long_Term_Synthesis_Filtering_S_dref_off630
  (Some (EVar V_Gsm_Long_Term_Synthesis_Filtering_Nr))) 15)::
  (EA 15 AWeaken 16)::(EA 16 (AGuard
  (fun s => ((eval (EVar V_Gsm_Long_Term_Synthesis_Filtering_Nr) s) >=
  (eval (ENum (40)) s))%Z)) 18)::(EA 16 (AGuard
  (fun s => ((eval (EVar V_Gsm_Long_Term_Synthesis_Filtering_Nr) s) <
  (eval (ENum (40)) s))%Z)) 17)::(EA 17 AWeaken 21)::(EA 18 AWeaken 19)::
  (EA 19 (AGuard
  (fun s => ((eval (EVar V_Gsm_Long_Term_Synthesis_Filtering_Nr) s) <=
  (eval (ENum (120)) s))%Z)) 23)::(EA 19 (AGuard
  (fun s => ((eval (EVar V_Gsm_Long_Term_Synthesis_Filtering_Nr) s) >
  (eval (ENum (120)) s))%Z)) 20)::(EA 20 AWeaken 21)::(EA 21 ANone 22)::
  (EA 22 AWeaken 41)::(EA 23 AWeaken 24)::(EA 24 ANone 25)::(EA 25 (AAssign
  V_Gsm_Long_Term_Synthesis_Filtering_brp None) 26)::(EA 26 AWeaken 27)::
  (EA 27 ANone 30)::(EA 27 ANone 28)::(EA 28 ANone 29)::(EA 29 AWeaken 41)::
  (EA 30 ANone 31)::(EA 31 (AAssign V_Gsm_Long_Term_Synthesis_Filtering_k
  (Some (ENum (0)))) 32)::(EA 32 ANone 33)::(EA 33 AWeaken 34)::
  (EA 34 (AGuard
  (fun s => ((eval (EVar V_Gsm_Long_Term_Synthesis_Filtering_k) s) <=
  (eval (ENum (39)) s))%Z)) 49)::(EA 34 (AGuard
  (fun s => ((eval (EVar V_Gsm_Long_Term_Synthesis_Filtering_k) s) >
  (eval (ENum (39)) s))%Z)) 35)::(EA 35 AWeaken 36)::(EA 36 (AAssign
  V_Gsm_Long_Term_Synthesis_Filtering_k (Some (ENum (0)))) 37)::
  (EA 37 ANone 38)::(EA 38 AWeaken 39)::(EA 39 (AGuard
  (fun s => ((eval (EVar V_Gsm_Long_Term_Synthesis_Filtering_k) s) <=
  (eval (ENum (119)) s))%Z)) 42)::(EA 39 (AGuard
  (fun s => ((eval (EVar V_Gsm_Long_Term_Synthesis_Filtering_k) s) >
  (eval (ENum (119)) s))%Z)) 40)::(EA 40 AWeaken 41)::(EA 42 AWeaken 43)::
  (EA 43 ANone 44)::(EA 44 (AAssign V_Gsm_Long_Term_Synthesis_Filtering_k
  (Some (EAdd (EVar V_Gsm_Long_Term_Synthesis_Filtering_k)
  (ENum (1))))) 45)::(EA 45 ANone 46)::(EA 46 ANone 47)::(EA 47 (AAssign
  V_Gsm_Long_Term_Synthesis_Filtering_z (Some (EAdd (ENum (1))
  (EVar V_Gsm_Long_Term_Synthesis_Filtering_z)))) 48)::(EA 48 AWeaken 39)::
  (EA 49 AWeaken 50)::(EA 50 (AAssign
  V_Gsm_Long_Term_Synthesis_Filtering_drpp None) 51)::(EA 51 (AAssign
  V_Gsm_Long_Term_Synthesis_Filtering_ltmp None) 52)::(EA 52 AWeaken 53)::
  (EA 53 ANone 55)::(EA 53 ANone 54)::(EA 54 ANone 56)::(EA 55 ANone 56)::
  (EA 56 ANone 57)::(EA 57 (AAssign V_Gsm_Long_Term_Synthesis_Filtering_k
  (Some (EAdd (EVar V_Gsm_Long_Term_Synthesis_Filtering_k)
  (ENum (1))))) 58)::(EA 58 ANone 59)::(EA 59 ANone 60)::(EA 60 (AAssign
  V_Gsm_Long_Term_Synthesis_Filtering_z (Some (EAdd (ENum (1))
  (EVar V_Gsm_Long_Term_Synthesis_Filtering_z)))) 61)::(EA 61 AWeaken 34)::
  nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_Gsm_Long_Term_Synthesis_Filtering => Pedges_Gsm_Long_Term_Synthesis_Filtering
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_Gsm_Long_Term_Synthesis_Filtering => 41
     end)%positive;
  var_global := var_global
}.

Definition ai_Gsm_Long_Term_Synthesis_Filtering (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0)%Z
   | 3 => (-1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0)%Z
   | 4 => (1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0)%Z
   | 5 => (-1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0)%Z
   | 6 => (1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering__tmp + 40 <= 0)%Z
   | 7 => (-1 * s V_Gsm_Long_Term_Synthesis_Filtering__tmp + 40 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0)%Z
   | 8 => (1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering__tmp + 40 <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering__tmp + -120 <= 0)%Z
   | 9 => (1 * s V_Gsm_Long_Term_Synthesis_Filtering__tmp + -120 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering__tmp + 40 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0)%Z
   | 10 => (1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering__tmp + 121 <= 0)%Z
   | 11 => (1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering__tmp + -39 <= 0)%Z
   | 12 => (-1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0)%Z
   | 13 => (1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0)%Z
   | 14 => (-1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0)%Z
   | 15 => (1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0)%Z
   | 16 => (-1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0)%Z
   | 17 => (1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + -39 <= 0)%Z
   | 18 => (1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + 40 <= 0)%Z
   | 19 => (-1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + 40 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0)%Z
   | 20 => (1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + 121 <= 0)%Z
   | 21 => (-1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0)%Z
   | 22 => (1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0)%Z
   | 23 => (1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + 40 <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + -120 <= 0)%Z
   | 24 => (1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + -120 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + 40 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0)%Z
   | 25 => (1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + 40 <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + -120 <= 0)%Z
   | 26 => (1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + -120 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + 40 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0)%Z
   | 27 => (1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + 40 <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + -120 <= 0)%Z
   | 28 => (1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + -120 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + 40 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0)%Z
   | 29 => (1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + 40 <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + -120 <= 0)%Z
   | 30 => (1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + -120 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + 40 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0)%Z
   | 31 => (1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + 40 <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + -120 <= 0)%Z
   | 32 => (1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + -120 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + 40 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_k <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_k <= 0)%Z
   | 33 => (-1 * s V_Gsm_Long_Term_Synthesis_Filtering_k <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_k <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + 40 <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + -120 <= 0)%Z
   | 34 => (-1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_k <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + 40 <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + -120 <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_k + -40 <= 0)%Z
   | 35 => (1 * s V_Gsm_Long_Term_Synthesis_Filtering_k + -40 <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + -120 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + 40 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_k + 40 <= 0)%Z
   | 36 => (-1 * s V_Gsm_Long_Term_Synthesis_Filtering_k + 40 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + 40 <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + -120 <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_k + -40 <= 0)%Z
   | 37 => (1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + -120 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + 40 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_k <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_k <= 0)%Z
   | 38 => (-1 * s V_Gsm_Long_Term_Synthesis_Filtering_k <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_k <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + 40 <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + -120 <= 0)%Z
   | 39 => (-1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_k <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + -120 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + 40 <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_k + -120 <= 0)%Z
   | 40 => (1 * s V_Gsm_Long_Term_Synthesis_Filtering_k + -120 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + 40 <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + -120 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_k + 120 <= 0)%Z
   | 41 => (-1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0)%Z
   | 42 => (-1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + 40 <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + -120 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_k <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_k + -119 <= 0)%Z
   | 43 => (1 * s V_Gsm_Long_Term_Synthesis_Filtering_k + -119 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_k <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + -120 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + 40 <= 0)%Z
   | 44 => (-1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + 40 <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + -120 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_k <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_k + -119 <= 0)%Z
   | 45 => (-1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + -120 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + 40 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_k + 1 <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_k + -120 <= 0)%Z
   | 46 => (1 * s V_Gsm_Long_Term_Synthesis_Filtering_k + -120 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_k + 1 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + 40 <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + -120 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0)%Z
   | 47 => (-1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + -120 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + 40 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_k + 1 <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_k + -120 <= 0)%Z
   | 48 => (1 * s V_Gsm_Long_Term_Synthesis_Filtering_k + -120 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_k + 1 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + 40 <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + -120 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_z + 1 <= 0)%Z
   | 49 => (1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + -120 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + 40 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_k <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_k + -39 <= 0)%Z
   | 50 => (1 * s V_Gsm_Long_Term_Synthesis_Filtering_k + -39 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_k <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + 40 <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + -120 <= 0)%Z
   | 51 => (1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + -120 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + 40 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_k <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_k + -39 <= 0)%Z
   | 52 => (1 * s V_Gsm_Long_Term_Synthesis_Filtering_k + -39 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_k <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + 40 <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + -120 <= 0)%Z
   | 53 => (1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + -120 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + 40 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_k <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_k + -39 <= 0)%Z
   | 54 => (1 * s V_Gsm_Long_Term_Synthesis_Filtering_k + -39 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_k <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + 40 <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + -120 <= 0)%Z
   | 55 => (1 * s V_Gsm_Long_Term_Synthesis_Filtering_k + -39 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_k <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + 40 <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + -120 <= 0)%Z
   | 56 => (1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + -120 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + 40 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_k <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_k + -39 <= 0)%Z
   | 57 => (1 * s V_Gsm_Long_Term_Synthesis_Filtering_k + -39 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_k <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + 40 <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + -120 <= 0)%Z
   | 58 => (1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + -120 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + 40 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_k + -40 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_k + 1 <= 0)%Z
   | 59 => (-1 * s V_Gsm_Long_Term_Synthesis_Filtering_k + 1 <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_k + -40 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + 40 <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + -120 <= 0)%Z
   | 60 => (1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + -120 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + 40 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_z <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_k + -40 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_k + 1 <= 0)%Z
   | 61 => (-1 * s V_Gsm_Long_Term_Synthesis_Filtering_k + 1 <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_k + -40 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + 40 <= 0 /\ 1 * s V_Gsm_Long_Term_Synthesis_Filtering_Nr + -120 <= 0 /\ -1 * s V_Gsm_Long_Term_Synthesis_Filtering_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_Gsm_Long_Term_Synthesis_Filtering (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((2 # 1) * max0(-40 + s V_Gsm_Long_Term_Synthesis_Filtering_Ncr)
           + (2 # 1) * max0(120 - s V_Gsm_Long_Term_Synthesis_Filtering_Ncr) <= z)%Q
   | 2 => (s V_Gsm_Long_Term_Synthesis_Filtering_z
           + (2 # 1) * max0(-40 + s V_Gsm_Long_Term_Synthesis_Filtering_Ncr)
           + (2 # 1) * max0(120 - s V_Gsm_Long_Term_Synthesis_Filtering_Ncr) <= z)%Q
   | 3 => (s V_Gsm_Long_Term_Synthesis_Filtering_z
           + (2 # 1) * max0(-40 + s V_Gsm_Long_Term_Synthesis_Filtering__tmp)
           + (2 # 1) * max0(120 - s V_Gsm_Long_Term_Synthesis_Filtering__tmp) <= z)%Q
   | 4 => (s V_Gsm_Long_Term_Synthesis_Filtering_z
           + (2 # 1) * max0(-40 + s V_Gsm_Long_Term_Synthesis_Filtering__tmp)
           + (2 # 1) * max0(120 - s V_Gsm_Long_Term_Synthesis_Filtering__tmp) <= z)%Q
   | 5 => (s V_Gsm_Long_Term_Synthesis_Filtering_z
           + (2 # 1) * max0(-40 + s V_Gsm_Long_Term_Synthesis_Filtering__tmp)
           + (2 # 1) * max0(120 - s V_Gsm_Long_Term_Synthesis_Filtering__tmp) <= z)%Q
   | 6 => (s V_Gsm_Long_Term_Synthesis_Filtering_z
           + (2 # 1) * max0(-40 + s V_Gsm_Long_Term_Synthesis_Filtering__tmp)
           + (2 # 1) * max0(120 - s V_Gsm_Long_Term_Synthesis_Filtering__tmp) <= z)%Q
   | 7 => (s V_Gsm_Long_Term_Synthesis_Filtering_z
           + (2 # 1) * max0(-40 + s V_Gsm_Long_Term_Synthesis_Filtering__tmp)
           + (2 # 1) * max0(120 - s V_Gsm_Long_Term_Synthesis_Filtering__tmp) <= z)%Q
   | 8 => hints
     [(*-2 0*) F_binom_monotonic 1 (F_max0_ge_arg (120
                                                   - s V_Gsm_Long_Term_Synthesis_Filtering__tmp)) (F_check_ge (120
                                                                    - s V_Gsm_Long_Term_Synthesis_Filtering__tmp) (0));
      (*0 2*) F_binom_monotonic 1 (F_max0_ge_arg (-40
                                                  + s V_Gsm_Long_Term_Synthesis_Filtering__tmp)) (F_check_ge (-40
                                                                    + s V_Gsm_Long_Term_Synthesis_Filtering__tmp) (0))]
     (s V_Gsm_Long_Term_Synthesis_Filtering_z
      + (2 # 1) * max0(-40 + s V_Gsm_Long_Term_Synthesis_Filtering__tmp)
      + (2 # 1) * max0(120 - s V_Gsm_Long_Term_Synthesis_Filtering__tmp) <= z)%Q
   | 9 => ((160 # 1) + s V_Gsm_Long_Term_Synthesis_Filtering_z <= z)%Q
   | 10 => hints
     [(*-2 0*) F_binom_monotonic 1 (F_max0_ge_0 (120
                                                 - s V_Gsm_Long_Term_Synthesis_Filtering__tmp)) (F_check_ge (0) (0));
      (*-0.0246914 0*) F_binom_monotonic 1 (F_max0_ge_0 (-40
                                                         + s V_Gsm_Long_Term_Synthesis_Filtering__tmp)) (F_check_ge (0) (0));
      (*-1.97531 0*) F_binom_monotonic 1 (F_max0_ge_arg (-40
                                                         + s V_Gsm_Long_Term_Synthesis_Filtering__tmp)) (F_check_ge (-40
                                                                    + s V_Gsm_Long_Term_Synthesis_Filtering__tmp) (0));
      (*-1.97531 0*) F_binom_monotonic 1 (F_max0_ge_0 (-121
                                                       + s V_Gsm_Long_Term_Synthesis_Filtering__tmp)) (F_check_ge (0) (0));
      (*-1.97531 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-121
                                                                    + 
                                                                    s V_Gsm_Long_Term_Synthesis_Filtering__tmp) (0))) (F_max0_ge_0 (-121
                                                                    + s V_Gsm_Long_Term_Synthesis_Filtering__tmp))]
     (s V_Gsm_Long_Term_Synthesis_Filtering_z
      + (2 # 1) * max0(-40 + s V_Gsm_Long_Term_Synthesis_Filtering__tmp)
      + (2 # 1) * max0(120 - s V_Gsm_Long_Term_Synthesis_Filtering__tmp) <= z)%Q
   | 11 => hints
     [(*-0.0246914 0*) F_binom_monotonic 1 (F_max0_ge_0 (120
                                                         - s V_Gsm_Long_Term_Synthesis_Filtering__tmp)) (F_check_ge (0) (0));
      (*-1.97531 0*) F_binom_monotonic 1 (F_max0_ge_arg (120
                                                         - s V_Gsm_Long_Term_Synthesis_Filtering__tmp)) (F_check_ge (120
                                                                    - s V_Gsm_Long_Term_Synthesis_Filtering__tmp) (0));
      (*-1.97531 0*) F_binom_monotonic 1 (F_max0_ge_0 (39
                                                       - s V_Gsm_Long_Term_Synthesis_Filtering__tmp)) (F_check_ge (0) (0));
      (*-1.97531 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (39
                                                                    - 
                                                                    s V_Gsm_Long_Term_Synthesis_Filtering__tmp) (0))) (F_max0_ge_0 (39
                                                                    - s V_Gsm_Long_Term_Synthesis_Filtering__tmp));
      (*-2 0*) F_binom_monotonic 1 (F_max0_ge_0 (-40
                                                 + s V_Gsm_Long_Term_Synthesis_Filtering__tmp)) (F_check_ge (0) (0))]
     (s V_Gsm_Long_Term_Synthesis_Filtering_z
      + (2 # 1) * max0(-40 + s V_Gsm_Long_Term_Synthesis_Filtering__tmp)
      + (2 # 1) * max0(120 - s V_Gsm_Long_Term_Synthesis_Filtering__tmp) <= z)%Q
   | 12 => ((160 # 1) + s V_Gsm_Long_Term_Synthesis_Filtering_z <= z)%Q
   | 13 => ((160 # 1) + s V_Gsm_Long_Term_Synthesis_Filtering_z <= z)%Q
   | 14 => ((160 # 1) + s V_Gsm_Long_Term_Synthesis_Filtering_z <= z)%Q
   | 15 => ((160 # 1) + s V_Gsm_Long_Term_Synthesis_Filtering_z <= z)%Q
   | 16 => ((160 # 1) + s V_Gsm_Long_Term_Synthesis_Filtering_z <= z)%Q
   | 17 => hints
     [(*0 160*) F_one]
     ((160 # 1) + s V_Gsm_Long_Term_Synthesis_Filtering_z <= z)%Q
   | 18 => ((160 # 1) + s V_Gsm_Long_Term_Synthesis_Filtering_z <= z)%Q
   | 19 => ((160 # 1) + s V_Gsm_Long_Term_Synthesis_Filtering_z <= z)%Q
   | 20 => hints
     [(*-160 0*) F_one]
     ((160 # 1) + s V_Gsm_Long_Term_Synthesis_Filtering_z <= z)%Q
   | 21 => (s V_Gsm_Long_Term_Synthesis_Filtering_z <= z)%Q
   | 22 => (s V_Gsm_Long_Term_Synthesis_Filtering_z <= z)%Q
   | 23 => ((160 # 1) + s V_Gsm_Long_Term_Synthesis_Filtering_z <= z)%Q
   | 24 => ((160 # 1) + s V_Gsm_Long_Term_Synthesis_Filtering_z <= z)%Q
   | 25 => ((160 # 1) + s V_Gsm_Long_Term_Synthesis_Filtering_z <= z)%Q
   | 26 => ((160 # 1) + s V_Gsm_Long_Term_Synthesis_Filtering_z <= z)%Q
   | 27 => ((160 # 1) + s V_Gsm_Long_Term_Synthesis_Filtering_z <= z)%Q
   | 28 => ((160 # 1) + s V_Gsm_Long_Term_Synthesis_Filtering_z <= z)%Q
   | 29 => hints
     [(*-160 0*) F_one]
     ((160 # 1) + s V_Gsm_Long_Term_Synthesis_Filtering_z <= z)%Q
   | 30 => ((160 # 1) + s V_Gsm_Long_Term_Synthesis_Filtering_z <= z)%Q
   | 31 => ((160 # 1) + s V_Gsm_Long_Term_Synthesis_Filtering_z <= z)%Q
   | 32 => ((120 # 1) + s V_Gsm_Long_Term_Synthesis_Filtering_z
            + max0(40 - s V_Gsm_Long_Term_Synthesis_Filtering_k) <= z)%Q
   | 33 => ((120 # 1) + s V_Gsm_Long_Term_Synthesis_Filtering_z
            + max0(40 - s V_Gsm_Long_Term_Synthesis_Filtering_k) <= z)%Q
   | 34 => ((120 # 1) + s V_Gsm_Long_Term_Synthesis_Filtering_z
            + max0(40 - s V_Gsm_Long_Term_Synthesis_Filtering_k) <= z)%Q
   | 35 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (40
                                             - s V_Gsm_Long_Term_Synthesis_Filtering_k) (39
                                                                    - s V_Gsm_Long_Term_Synthesis_Filtering_k));
      (*-1 0*) F_max0_ge_0 (39 - s V_Gsm_Long_Term_Synthesis_Filtering_k)]
     ((120 # 1) + s V_Gsm_Long_Term_Synthesis_Filtering_z
      + max0(40 - s V_Gsm_Long_Term_Synthesis_Filtering_k) <= z)%Q
   | 36 => ((120 # 1) + s V_Gsm_Long_Term_Synthesis_Filtering_z <= z)%Q
   | 37 => (s V_Gsm_Long_Term_Synthesis_Filtering_z
            + max0(120 - s V_Gsm_Long_Term_Synthesis_Filtering_k) <= z)%Q
   | 38 => (s V_Gsm_Long_Term_Synthesis_Filtering_z
            + max0(120 - s V_Gsm_Long_Term_Synthesis_Filtering_k) <= z)%Q
   | 39 => (s V_Gsm_Long_Term_Synthesis_Filtering_z
            + max0(120 - s V_Gsm_Long_Term_Synthesis_Filtering_k) <= z)%Q
   | 40 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (120
                                             - s V_Gsm_Long_Term_Synthesis_Filtering_k) (119
                                                                    - s V_Gsm_Long_Term_Synthesis_Filtering_k));
      (*-1 0*) F_max0_ge_0 (119 - s V_Gsm_Long_Term_Synthesis_Filtering_k)]
     (s V_Gsm_Long_Term_Synthesis_Filtering_z
      + max0(120 - s V_Gsm_Long_Term_Synthesis_Filtering_k) <= z)%Q
   | 41 => (s V_Gsm_Long_Term_Synthesis_Filtering_z <= z)%Q
   | 42 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (120
                                       - s V_Gsm_Long_Term_Synthesis_Filtering_k) (1)]
     (s V_Gsm_Long_Term_Synthesis_Filtering_z
      + max0(120 - s V_Gsm_Long_Term_Synthesis_Filtering_k) <= z)%Q
   | 43 => ((1 # 1) + s V_Gsm_Long_Term_Synthesis_Filtering_z
            + max0(119 - s V_Gsm_Long_Term_Synthesis_Filtering_k) <= z)%Q
   | 44 => ((1 # 1) + s V_Gsm_Long_Term_Synthesis_Filtering_z
            + max0(119 - s V_Gsm_Long_Term_Synthesis_Filtering_k) <= z)%Q
   | 45 => ((1 # 1) + s V_Gsm_Long_Term_Synthesis_Filtering_z
            + max0(120 - s V_Gsm_Long_Term_Synthesis_Filtering_k) <= z)%Q
   | 46 => ((1 # 1) + s V_Gsm_Long_Term_Synthesis_Filtering_z
            + max0(120 - s V_Gsm_Long_Term_Synthesis_Filtering_k) <= z)%Q
   | 47 => ((1 # 1) + s V_Gsm_Long_Term_Synthesis_Filtering_z
            + max0(120 - s V_Gsm_Long_Term_Synthesis_Filtering_k) <= z)%Q
   | 48 => (s V_Gsm_Long_Term_Synthesis_Filtering_z
            + max0(120 - s V_Gsm_Long_Term_Synthesis_Filtering_k) <= z)%Q
   | 49 => ((120 # 1) + s V_Gsm_Long_Term_Synthesis_Filtering_z
            + max0(40 - s V_Gsm_Long_Term_Synthesis_Filtering_k) <= z)%Q
   | 50 => ((120 # 1) + s V_Gsm_Long_Term_Synthesis_Filtering_z
            + max0(40 - s V_Gsm_Long_Term_Synthesis_Filtering_k) <= z)%Q
   | 51 => ((120 # 1) + s V_Gsm_Long_Term_Synthesis_Filtering_z
            + max0(40 - s V_Gsm_Long_Term_Synthesis_Filtering_k) <= z)%Q
   | 52 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (40
                                       - s V_Gsm_Long_Term_Synthesis_Filtering_k) (1)]
     ((120 # 1) + s V_Gsm_Long_Term_Synthesis_Filtering_z
      + max0(40 - s V_Gsm_Long_Term_Synthesis_Filtering_k) <= z)%Q
   | 53 => ((121 # 1) + s V_Gsm_Long_Term_Synthesis_Filtering_z
            + max0(39 - s V_Gsm_Long_Term_Synthesis_Filtering_k) <= z)%Q
   | 54 => ((121 # 1) + s V_Gsm_Long_Term_Synthesis_Filtering_z
            + max0(39 - s V_Gsm_Long_Term_Synthesis_Filtering_k) <= z)%Q
   | 55 => ((121 # 1) + s V_Gsm_Long_Term_Synthesis_Filtering_z
            + max0(39 - s V_Gsm_Long_Term_Synthesis_Filtering_k) <= z)%Q
   | 56 => ((121 # 1) + s V_Gsm_Long_Term_Synthesis_Filtering_z
            + max0(39 - s V_Gsm_Long_Term_Synthesis_Filtering_k) <= z)%Q
   | 57 => ((121 # 1) + s V_Gsm_Long_Term_Synthesis_Filtering_z
            + max0(39 - s V_Gsm_Long_Term_Synthesis_Filtering_k) <= z)%Q
   | 58 => ((121 # 1) + s V_Gsm_Long_Term_Synthesis_Filtering_z
            + max0(40 - s V_Gsm_Long_Term_Synthesis_Filtering_k) <= z)%Q
   | 59 => ((121 # 1) + s V_Gsm_Long_Term_Synthesis_Filtering_z
            + max0(40 - s V_Gsm_Long_Term_Synthesis_Filtering_k) <= z)%Q
   | 60 => ((121 # 1) + s V_Gsm_Long_Term_Synthesis_Filtering_z
            + max0(40 - s V_Gsm_Long_Term_Synthesis_Filtering_k) <= z)%Q
   | 61 => ((120 # 1) + s V_Gsm_Long_Term_Synthesis_Filtering_z
            + max0(40 - s V_Gsm_Long_Term_Synthesis_Filtering_k) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_Gsm_Long_Term_Synthesis_Filtering =>
    [mkPA Q (fun n z s => ai_Gsm_Long_Term_Synthesis_Filtering n s /\ annot0_Gsm_Long_Term_Synthesis_Filtering n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_Gsm_Long_Term_Synthesis_Filtering (proc_start P_Gsm_Long_Term_Synthesis_Filtering) s1 (proc_end P_Gsm_Long_Term_Synthesis_Filtering) s2 ->
    (s2 V_Gsm_Long_Term_Synthesis_Filtering_z <= (2 # 1) * max0(-40
                                                                + s1 V_Gsm_Long_Term_Synthesis_Filtering_Ncr)
                                                 + (2 # 1) * max0(120
                                                                  - s1 V_Gsm_Long_Term_Synthesis_Filtering_Ncr))%Q.
Proof.
  prove_bound ipa admissible_ipa P_Gsm_Long_Term_Synthesis_Filtering.
Qed.
