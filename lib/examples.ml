let mu2 =
  "m: M⊗M -> M
e: 1 -> M
n: N⊗N -> N
f: 1 -> N
x: N⊗M -> M⊗N
mn<color=orange> := id·x·id ; m·n
ef<color=orange> := e·f
mm: m·id ; m = id·m ; m
em: e·id ; m = id
me: id·e ; m = id
nn: n·id ; n = id·n ; n
fn: f·id ; n = id
nf: id·f ; n = id
mx: id·m ; x = x·id ; id·x ; m·id
ex: id·e ; x = e·id
nx: n·id ; x = id·x ; x·id ; id·n
fx: f·id ; x = id·f"

let mu3 =
  "m: M^2 -> M
n: N^2-> N
o: O^2-> O
x: N⊗M -> M⊗N
y: O⊗N -> N⊗O
z: O⊗M -> M⊗O
mno<color=turquoise> := id·id·z·id·id ; id·x·y·id ; m·n·o
mm: m·id ; m = id·m ; m
nn: n·id ; n = id·n ; n
oo: o·id ; o = id·o ; o
mx: id·m ; x = x·id ; id·x ; m·id
nx: n·id ; x = id·x ; x·id ; id·n
ny: id·n ; y = y·id ; id·y ; n·id
oy: o·id ; y = id·y ; y·id ; id·o
mz: id·m ; z = z·id ; id·z ; m·id
oz: o·id ; z = id·z ; z·id ; id·o
xyz: y·id ; id·z ; x·id = id·x ; z·id ; id·y"

let list = [
    
"exchange",
"i: I -> I'
j: J -> J'",
"i·j";

"bifunctoriality",
"f: A -> B
f': A' -> B'
g: B -> C
g': B' -> C'",
"f·f' ; g·g'";

"associativity of composite monad",
mu2,
"mn·M·N ; mn = M·N·mn ; mn";

"left-unitality of composite monad",
mu2,
"ef·M·N ; mn = M·N";

"right-unitality of composite monad",
mu2,
"M·N·ef ; mn = M·N";

"composing three monads (just associativity of multiplication)",
mu3,
"mno·id·id·id ; mno = id·id·id·mno ; mno";
  ]

let list' = [
"thanks!",
"M<color=gray>
N<color=blue>
T<shape=rect;color=yellow>: 1 -> M
h<color=orange>: 1 -> M⊗M
a<color=red>: 1 -> M⊗M
n<color=turquoise>: M⊗M -> 1
k<color=blue>: M⊗M -> 1
s!<color=violet>: M -> 1",
"T·h·a ; n·k·s!"
  ]
