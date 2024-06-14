module BottomLevel = struct
  type t =
    [Config_error.t | Filesystem_error.t | Template_error.t]
end

module TopLevel = struct
  type t = [ | Action_error.t]
end

type error = [TopLevel.t | BottomLevel.t]

module Errlist = struct
  type t = error list
end

type t = Errlist.t

module type TRACE = sig
  type 'a trace

  val new_error : [< error] -> 'a trace
  val with_error : [< error] -> 'a trace -> 'a trace
end
