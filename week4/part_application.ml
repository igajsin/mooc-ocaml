let ccr a b c s =
  let a1 = 0.5 *. a and
      b1 = 0.5 *. b and
      c1 = 0.5 *. c and
      s1 = 0.125 *. s in
  s1 /. cos c1 /. cos b1 /. cos a1;;
