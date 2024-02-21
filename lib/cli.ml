module type TERM =
  Etude.Endofunctors_intf.Applicative.AUGMENTED

module Term : TERM with type 'a t = 'a Cmdliner.Term.t =
struct
  module type BASIC =
    Etude.Endofunctors_intf.Applicative.BASIC

  module BasicApp :
    BASIC with type 'a t = 'a Cmdliner.Term.t = struct
    open Cmdliner.Term

    type 'a t = 'a Cmdliner.Term.t

    let map f x = const f $ x

    let product ax ay = map (fun x y -> (x, y)) ax $ ay

    let pure = const

    let unit = pure ()
  end

  open Etude.Endofunctors
  include Applicative.Make (BasicApp)
end

module Arguments = struct
  let dry_run =
    let open Cmdliner.Arg in
    let doc =
      "Print description of what would happen if $(tname) \
       were run in normal mode."
    in
    let docv = "D" in
    let inf = info [ "d"; "dry-run" ] ~doc ~docv in
    let arg_type = flag in
    value (arg_type inf)

  let print_config =
    let open Cmdliner.Arg in
    let doc =
      "Print the default configuration to stdout so that \
       it can be redirected to a config file for the user \
       to customize."
    in
    let docv = "P" in
    let inf = info [ "p"; "print-config" ] ~doc ~docv in
    let arg_type = flag in
    value (arg_type inf)

  let project_name =
    let open Cmdliner.Arg in
    let doc = "Name of project to be spun up." in
    let docv = "PROJECT_NAME" in
    let inf = info [] ~doc ~docv in
    let arg_type = pos ~rev:true 0 (some string) None in
    value (arg_type inf)
end

module Command = struct
  let main_term exe print_conf dry_run project_name =
    let open Term in
    let+ main = pure exe
    and+ pc = print_conf
    and+ dr = dry_run
    and+ pn = project_name in
    main pc dr pn

  let manpage_info =
    let description =
      "$(tname) creates a skeleton for a \
       library-executable OCaml project that assumes the \
       UChicago Library's opam repository and standard \
       libraries."
    in
    let man =
      Cmdliner.[ `S Manpage.s_description; `P description ]
    in
    let doc = "Spins up an OCaml project skeleton." in
    Cmdliner.Cmd.info "spinup" ~doc ~man

  let command exe print_conf dry_run project_name =
    let term =
      main_term exe print_conf dry_run project_name
    in
    Cmdliner.Cmd.v manpage_info term

  let to_exe exe print_conf dry_run project_name =
    let open Cmdliner.Cmd in
    command exe print_conf dry_run project_name
    |> eval |> exit
end
