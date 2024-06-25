type tint_syntax =
  { path : string;
    tint_info : string * string * string list
  }

type t =
  [ `BadSyntaxRecord of Lineinfo.t * string
  | `BadSyntaxString of Lineinfo.t * string
  | `TintSyntax of tint_syntax
  | `TemplateCrunch of string ]

module Smart = struct
  let bad_syntax_record li s = `BadSyntaxRecord (li, s)

  let is_bad_syntax_record = function
    | `BadSyntaxRecord _ -> true
    | _ -> false

  let bad_syntax_string li s = `BadSyntaxString (li, s)

  let is_bad_syntax_string = function
    | `BadSyntaxString _ -> true
    | _ -> false

  let tint_syntax path tint_info =
    `TintSyntax { path; tint_info }

  let is_tint_syntax = function
    | `TintSyntax _ -> true
    | _ -> false

  let template_crunch s = `TemplateCrunch s

  let is_template_crunch = function
    | `TemplateCrunch _ -> true
    | _ -> false
end
