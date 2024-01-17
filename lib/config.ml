type t = { description : string ;
           synopsis : string ;
           homepage : string ;
         }

let default_config =
  let description = "Insert project description here." in
  let synopsis = "Insert project synopsis, which is supposedly \
                  different, here" in
  let homepage = "https://your.website.here" in
  { description ; synopsis ; homepage }

let default_context ?(config=default_config) pname =
  [ "pname", pname ;
    "description", config.description ;
    "synopsis", config.synopsis ;
    "homepage", config.homepage ; ]
