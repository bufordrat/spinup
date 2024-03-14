type t = [`TemplateErr | `FilesystemErr]

module Smart = struct
  let template_err = `TemplateErr
  let filesystem_err = `FilesystemErr
end
