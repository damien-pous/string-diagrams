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


open Messages

class virtual ['a] mk serialize deserialize =
  object(self)

    method private virtual state: 'a
    method private virtual set_state: 'a -> unit

    method private virtual read: string -> 'a
    method private virtual write: string -> 'a -> unit
    
    val hist = create ""

    method private set_sstate s =
      self#set_state (deserialize s)

    method private checkpoint =
      save hist (serialize self#state) 

    method private abort =
      let s = present hist in
      self#set_sstate s
    
    method undo () =
      match undo hist with
      | Some s -> self#set_sstate s
      | None -> temporary#msg "no more undos"

    method redo () =
      match redo hist with
      | Some s -> self#set_sstate s
      | None -> temporary#msg "no more redos"

    method load' (s: 'a) =
      self#set_state s;
      self#checkpoint;
      clear hist

    method load f =
      self#load' (self#read f)

    method save f =
      self#write f self#state

    initializer
      self#checkpoint;
      clear hist
      
  end
