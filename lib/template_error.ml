type tint_syntax =
  { path : string;
    tint_info : string * string * string list
  }

type t =
  [ `BadSyntaxRecord of Lineinfo.t * string
  | `TintSyntaxError of tint_syntax
  | `TemplateCrunch of string ]

module Smart = struct
  let bad_syntax_record li s = `BadSyntaxRecord (li, s)

  let is_bad_syntax_record = function
    | `BadSyntaxRecord _ -> true
    | _ -> false

  let tint_syntax_error path tint_info =
    `TintSyntaxError { path; tint_info }

  let is_tint_syntax_error = function
    | `TintSyntaxError _ -> true
    | _ -> false

  let template_crunch s = `TemplateCrunch s

  let is_template_crunch = function
    | `TemplateCrunch _ -> true
    | _ -> false
end
