module Path : sig
  val path : string
end

module Engine : sig
  val expand_template : template:string ->
                         context:(string * string) list ->
                         (string, string) result

  val expand_crunched : template:string ->
                         context:(string * string) list ->
                         (string, string) result
end

module Processed : sig
  type t = { write_path : string ;
             data : string ;
             vmessage : string }

  val write : t -> unit
end

module Unprocessed : sig
  type t = { template_filename : string ;
             output_filename : string ;
             template_path : string ;
             output_path : string ;
             context : (string * string) list ;
             umessage : string ;
           }

  val process : t -> (Processed.t, string) result
end
