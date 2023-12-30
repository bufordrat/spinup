let dune_project_is_there () =
  let open Alcotest in
  (check (neg (option string)))
    (Some "hello")
    None

let dude () =
  let open Alcotest in
  (check string) "same string" "dude" "dude"

let man () =
  let open Alcotest in
  (check string) "same string" "man" "man"

let () =
  let open Alcotest in 
  run "Spinup Tests" [
      "check crunched dir", [ test_case "dune-project present" `Quick dune_project_is_there ; ] ;
    ]
