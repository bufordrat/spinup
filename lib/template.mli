module Engine : sig
  val expand_string :
    context:(string * string) list -> string -> (string, string) result

  val expand_crunched :
    template:string ->
    context:(string * string) list ->
    (string, string) result
end

module Processed : sig
  type t = { write_path : string; data : string; vmessage : string }

  val write : t -> unit
end

module Unprocessed : sig
  type t =
    { template_filename : string;
      output_filename : string;
      template_path : string;
      output_path : string;
      context : (string * string) list;
      umessage : string
    }

  val expand_filenames : t -> (t, string) result

  val process : t -> (Processed.t, string) result
end
