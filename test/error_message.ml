module Examples = struct
  let refer_error_crunch =
    let open Action_error.DataSource in
    [ `ConfigErr;
      `ReferError
        ( FromCrunch ".spinuprc",
          1,
          "continuation line at beginning of record: \" \
           %dune_version 3.13\"" )
    ]

  let refer_error_file =
    let open Action_error.DataSource in
    [ `ConfigErr;
      `ReferError
        ( FromAFile "/home/teichman/.spinuprc",
          1,
          "continuation line at beginning of record: \" \
           %dune_version 3.13\"" )
    ]

  let bad_crunch_path =
    let open Lineinfo in
    [ `ConfigErr;
      `BadCrunchPath
        ( ".spinuprcXXX",
          { line = 20; filename = "lib/config.ml" } )
    ]

  let file_read_error =
    [ `ConfigErr;
      `FileReadError
        "/home/teichman/.spinuprc: Permission denied"
    ]

  let already_exists =
    [ `FilesystemErr;
      `AlreadyExists
        ( "/home/teichman/Code/GitHub/spinup/lib",
          Filesystem_error.Dir,
          "keith" )
    ]

  let bad_syntax =
    let open Lineinfo in
    [ `TemplateErr;
      `BadSyntaxRecord
        ( { line = 9; filename = "lib/template.ml" },
          "invalid syntax: 7 chars <> 4" )
    ]

  let tint_syntax =
    [ `TemplateErr;
      `TintSyntaxError
        { Template_error.path = "/dune-project";
          tint_info =
            ( "nathan",
              "no such function",
              [ "cl"; "dune_version" ] )
        }
    ]
end
