open Polynom

let _ =
  let zero = Poly.zero () in
  let x15 = Poly.of_monom (Monom.of_factor (Factor.Var "x") 15) 1. in
  let z = Poly.of_monom (Monom.of_factor (Factor.Var "z") 1) 1. in
  let ym1 =
    Poly.add (Poly.of_monom (Monom.of_factor (Factor.Var "y") 1) 1.)
             (Poly.of_monom (Monom.of_factor (Factor.Max z) 1) (-0.5)) in
  begin
    Format.printf "::@.%a@." Poly.print zero;
    Format.printf "::@.%a@." Poly.print x15;
    Format.printf "::@.%a@." Poly.print ym1;
    Format.printf "::@.%a@." Poly.print (poly_subst "x" ym1 x15);
  end
