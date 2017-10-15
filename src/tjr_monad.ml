(* a generic-ish monad *)


module Public = struct
  type ('a,'m) m = 'm

  type ('e,'m) monad_ops = {
    return: 'a 'm. 'a -> ('a,'m)m;
    bind: 'a 'b 'm 'n 'o. ('a,'m)m -> ('a -> ('b,'n)m) -> ('b,'o)m;
    err: 'a 'm. 'e -> ('a,'m)m
  }
end

module Private : sig
  type ('a,'m) m
  type ('e,'m) monad_ops = {
    return: 'a 'm. 'a -> ('a,'m)m;
    bind: 'a 'b 'm 'n 'o. ('a,'m)m -> ('a -> ('b,'n)m) -> ('b,'o)m;
    err: 'a 'm. 'e -> ('a,'m)m
  }
  val reveal: ('a,'m) m -> ('a,'m)Public.m
end = struct
  include Public
  let reveal x = x
end
  
