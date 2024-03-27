type t =
  [ `SyntaxString of string
  | `TintSyntax of string
  | `TemplateCrunch of string ]

type t' =
  [ `SyntaxString of string
  | `TintSyntax of string * string * string list
  | `TemplateCrunch of string ]

module Smart = struct
  let syntax_string s = `SyntaxString s
  let tint_syntax tup = `TintSyntax tup
  let template_crunch s = `TemplateCrunch s
end
