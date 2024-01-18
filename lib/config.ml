type t = { description : string ;
           synopsis : string ;
           maintainers : string list ;
           authors : string list ;
           homepage : string ;
           bug_reports : string ;
         }

let default =
  let description = "Insert project description here." in
  let synopsis = "Insert project synopsis, which is supposedly \
                  different, here" in
  let maintainers = [ "Your Name <youremail@gmail.com>" ] in
  let authors = [ "Your Name <youremail@gmail.com>" ] in
  let homepage = "https://your.website.here" in
  let bug_reports = "https://your.website.here" in
  { description ;
    synopsis ;
    maintainers ;
    authors ;
    homepage ;
    bug_reports }

let default_context ?(config=default) pname =
  let open Prelude.String in
  [ "pname", pname ;
    "description", config.description ;
    "synopsis", config.synopsis ;
    "maintainers", join ~sep:" " config.maintainers ;
    "authors", join ~sep:" " config.authors ;
    "homepage", config.homepage ;
    "bug_reports", config.bug_reports ; ]


