open Gg

let vpad s l =
  let w =
    List.fold_right
      (fun e -> max e#width)
      l 0.
  in
  let h =
    List.fold_right
      (fun e -> (+.) (s +. e#height))
      l s
  in
  ignore (List.fold_left
    (fun y e ->
      e#shift (P2.v 0. (y+.s+.e#height/.2.));
      y+.s +. e#height
    ) (-.h/.2.) l);
  object
    method width = w
    method height = h -. 2.*. s
    method shift v = List.iter (fun e -> e#shift v) l
  end

let hpad s l =
  let h =
    List.fold_right
      (fun e -> max e#height)
      l 0.
  in
  let w =
    List.fold_right
      (fun e -> (+.) (s +. e#width))
      l s
  in
  ignore(List.fold_left
    (fun x e ->
      e#shift (P2.v (x+.s+.e#width/.2.) 0.);
      x+.s +. e#width
    ) (-.w/.2.) l);
  object
    method width = w -. 2.*. s
    method height = h
    method shift v = List.iter (fun e -> e#shift v) l
  end
