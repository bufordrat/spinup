type t =
  [ `ConstructSyntax of Lineinfo.t * string
  | `BadSyntaxString of Lineinfo.t * string
  | `TintSyntax of string * string * string list
  | `TemplateCrunch of string ]

module Smart = struct
  let construct_syntax li s = `ConstructSyntax (li, s)
  let bad_syntax_string li s = `BadSyntaxString (li, s)
  let tint_syntax tup = `TintSyntax tup
  let template_crunch s = `TemplateCrunch s
end
