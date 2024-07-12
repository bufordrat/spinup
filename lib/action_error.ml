type t = [`TemplateErr | `FilesystemErr | `ConfigErr]

module DataSource = struct
  type t = FromCrunch of string | FromAFile of string
end

module Smart = struct
  let template_err = `TemplateErr

  let is_template_err = function
    | `TemplateErr -> true
    | _ -> false

  let filesystem_err = `FilesystemErr

  let is_filesystem_err = function
    | `FilesystemErr -> true
    | _ -> false

  let config_err = `ConfigErr

  let is_config_err = function
    | `ConfigErr -> true
    | _ -> false
end
