type error = [`TemplateErr | `FilesystemErr]

module Smart = struct
  let template_err x = `TemplateError x
  let filesystem_err x = `FilesystemErr x
end
