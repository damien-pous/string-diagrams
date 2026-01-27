type 'a t = ('a list * 'a * 'a list) ref

let past h = let (l,_,_) = !h in l
let present h = let (_,x,_) = !h in x
let future h = let (_,_,l) = !h in l

let create x = ref ([],x,[])
let save ?cmp h x =
  if x<>present h then
    match cmp with
    | Some cmp when cmp x (present h) -> h := (past h, x, future h)
    | _ -> h := (present h::past h, x, [])
            
let clear h = h := ([],present h,[]) 
let undo h =  
  match past h with
  | [] -> None
  | x::past -> h := (past, x, present h::future h); Some x
let redo h =  
  match future h with
  | [] -> None
  | x::future -> h := (present h::past h, x, future); Some x


open Misc
open Messages

class virtual mk =
  object(self)

    method private virtual on_reset: unit

    val hist = create (capture())

    method private checkpoint =
      save hist (capture())

    method private reset s =
      restore s; self#on_reset      

    method private abort =
      self#reset (present hist)

    method private clear_history =
      self#checkpoint;
      clear hist
    
    method undo () =
      match undo hist with
      | Some s -> self#reset s
      | None -> warning "no more undos"

    method redo () =
      match redo hist with
      | Some s -> self#reset s
      | None -> warning "no more redos"

    initializer
      self#clear_history
      
  end
