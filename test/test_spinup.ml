let dune_project_is_there () =
  let open Alcotest in
  let dp = Lib.Crunched.read "dune-project" in
  (check (neg (option string)))
    "crunched file is present"
    dp
    None

let gnumakefile_is_there () =
  let open Alcotest in
  let dp = Lib.Crunched.read "GNUmakefile" in
  (check (neg (option string)))
    "crunched file is present"
    dp
    None

let () =
  let open Alcotest in 
  run "Spinup Tests" [
      "check crunched dir", [ test_case "dune-project is present" `Quick dune_project_is_there ;
                              test_case "GNUmakefile is present" `Quick gnumakefile_is_there ; ] ;
    ]
