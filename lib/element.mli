open Types

class proxy: #element -> element

class rectangle: typs -> typs -> ?pos:point -> size:size -> name:name -> kvl -> element
class polygon: (typ*(point*vector)) list -> (typ*(point*vector)) list -> Types.polygon -> element

val mk: typs -> typs -> name:name -> kvl -> element
