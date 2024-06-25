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
  let bad_syntax_string li s = `BadSyntaxString (li, s)

  let tint_syntax path tint_info =
    `TintSyntax { path; tint_info }

  let template_crunch s = `TemplateCrunch s
end
