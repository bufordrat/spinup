module Engine : sig
  val expand_string :
    path:string ->
    ?syntax:string ->
    context:(string * string) list ->
    string ->
    (string, Global_error_intf.t) result

  val expand_crunched :
    path:string ->
    context:(string * string) list ->
    (string, Global_error_intf.t) result
end

module Processed : sig
  type t =
    { write_path : string;
      data : string;
      vmessage : string
    }

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

  val expand_filenames :
    t -> (t, Global_error_intf.t) result

  val process :
    t -> (Processed.t, Global_error_intf.t) result
end
