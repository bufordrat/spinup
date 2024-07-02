type error = Global_error_intf.error
type t = Global_error_intf.t

module Smart : sig
  val template_err : error
  val is_template_err : error -> bool
  val filesystem_err : error
  val is_filesystem_err : error -> bool
  val config_err : error
  val is_config_err : error -> bool

  val refer_error :
    Action_error.DataSource.t -> int * string -> error

  val is_refer_error : error -> bool
  val bad_crunch_path : string -> Lineinfo.t -> error
  val is_bad_crunch_path : error -> bool
  val file_read_error : string -> error
  val is_file_read_error : error -> bool

  val already_exists :
    string ->
    Filesystem_error.dir_or_file ->
    string ->
    error

  val is_already_exists : error -> bool
  val bad_syntax_record : Lineinfo.t -> string -> error
  val is_bad_syntax_record : error -> bool

  val tint_syntax_error :
    string -> string * string * string list -> error

  val is_tint_syntax_error : error -> bool
  val template_crunch : string -> error
  val is_template_crunch : error -> bool
end

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
val print_res : ('a, t) result -> unit
