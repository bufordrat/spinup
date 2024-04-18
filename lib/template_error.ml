type t =
  [ (* this case can't be reached due to a glitch in TINT *)
    `SyntaxString of
    string
  | `TintSyntax of string * string * string list
  | `TemplateCrunch of string ]

module Smart = struct
  let syntax_string s = `SyntaxString s
  let tint_syntax tup = `TintSyntax tup
  let template_crunch s = `TemplateCrunch s
end
