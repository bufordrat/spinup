type error = Global_error_intf.error
type t = Global_error_intf.t

module T : sig
  val with_error :
    [< error] -> ('a, t) result -> ('a, t) result

  val new_list : [< error] -> t
  val new_error : [< error] -> ('a, t) result
end

val expose : ('a, t) result -> ('a, error list) result

module Specialize : functor
  (E : sig
     type t
   end)
  -> sig
  module type S = sig
    val with_error : E.t -> ('a, t) result -> ('a, t) result
    val new_list : E.t -> t
    val new_error : E.t -> ('a, t) result
  end
end

val error_to_string : error -> string
val to_string : t -> string
val print : t -> unit
