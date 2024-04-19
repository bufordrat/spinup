module BottomLevel = struct
  type t =
    [Config_error.t | Filesystem_error.t | Template_error.t]
end

module TopLevel = struct
  type t = [ | Action_error.t]
end

type global_error = [TopLevel.t | BottomLevel.t]

module Errlist = struct
  type t = global_error list
end

type t = Errlist.t

module type TRACE = sig
  type 'a trace

  val new_error : [< global_error] -> 'a trace
  val with_error : [< global_error] -> 'a trace -> 'a trace
end
