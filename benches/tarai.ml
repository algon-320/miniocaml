let rec tarai x y z =
  if x <= y then
    y
  else
    tarai (tarai (x-1) y z) (tarai (y-1) z x) (tarai (z-1) x y)
in
let x = read_int () in
let y = read_int () in
let z = read_int () in
print_endline @@ string_of_int @@ tarai x y z
