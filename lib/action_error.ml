type t = [`TemplateErr | `FilesystemErr | `ConfigErr ]

module DataSource = struct
  type t = FromCrunch of string | FromAFile of string
end

module Smart = struct
  let template_err = `TemplateErr
  let filesystem_err = `FilesystemErr
  let config_err = `ConfigErr
end
