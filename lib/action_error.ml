type error = [`TemplateErr | `FilesystemErr]

module Smart = struct
  let template_err x = `TemplateErr x
  let filesystem_err x = `FilesystemErr x
end
