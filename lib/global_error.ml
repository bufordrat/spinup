type t = Global_error_intf.global_error

(* module Errlist = Trace_intf.Errlist
 * 
 * module type TRACE = Trace_intf.TRACE
 * 
 * module T =
 *   struct
 *     type 'a trace = ('a, Trace_intf.Errlist.t) result
 *     let coerce e = (e : [< Trace_intf.global_error ]
 *                         :> Trace_intf.global_error)
 *     let new_error e =
 *       Error [coerce e]
 *     let trycatch e = function
 *       | Ok _ as o -> o
 *       | Error lst -> Error (coerce e :: lst)
 *   end
 * 
 * module Specialize (E : sig type error end) = struct
 *   type 'a trycatch = E.error ->
 *                      ('a, Errlist.t) result ->
 *                      ('a, Errlist.t) result
 *   type 'a new_error = E.error ->
 *                       ('a, Errlist.t) result
 *   end
 * 
 * let export = Fun.id *)
