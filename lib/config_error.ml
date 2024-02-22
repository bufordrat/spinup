type t =
  [`ReferParsing of int * string | `ConfigCrunch of string]

module Smart = struct
  let refer_parsing (i, s) = `ReferParsing (i, s)
  let config_crunch path = `ConfigCrunch path
end
