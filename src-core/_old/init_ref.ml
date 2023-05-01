(** An init-ref is a ref which can be set once during initialization,
   and read post-init. *)

type epoch = Init | Post_init

module Internal : sig 
  type 'a init_ref
  val create: 'a -> 'a init_ref
  val get: 'a init_ref -> 'a
  val set: 'a init_ref -> 'a -> unit

  (** Set the "epoch" to be post-init (time is initially init) *)
  val set_post_init: unit -> unit

  val get_epoch: unit -> epoch
end = struct

  let epoch = ref Init 

  type 'a init_ref = 'a ref

  let create a =
    assert(!epoch = Init);
    ref a

  let get r = 
    assert(!epoch = Post_init);
    !r

  let set r a =
    assert(!epoch = Init);
    r:=a

  let set_post_init () = epoch:=Post_init

  let get_epoch () = !epoch

end
include Internal

(** NOTE because this syntax overrides Stdlib, be careful when
   opening this module *)
let (!) = Internal.get

let (:=) = Internal.set

