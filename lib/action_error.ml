type t = [`TemplateErr | `FilesystemErr]

module DataSource = struct
  type t = FromCrunch | FromAFile of string
end

module Smart = struct
  let template_err = `TemplateErr
  let filesystem_err = `FilesystemErr
end
