(** The type of singleton types. 

Use as:

Make(struct type t = int val witness = -1 end)

to get a new type sng_t and a value sealed_witness of type (sng_t,int)sng

Useful for reflecting type dependencies on values into the type.

*)

module Internal : 
sig
  type ('a, 'b) sng
  val dest_sng : ('a,'b) sng -> 'b
  module Make :
    functor (S : sig type t val witness : t end) ->
      sig type sng_t val sealed_witness : (sng_t, S.t) sng end
end
 = struct

  type ('a,'b)sng = Singleton of 'b

  let dest_sng : ('a,'b)sng -> 'b = function Singleton b -> b

  module Make(S:sig type t val witness:t end) = struct
    type sng_t
    let sealed_witness : (sng_t,S.t) sng = Singleton S.witness
  end

end

include Internal
  
