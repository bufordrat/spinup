let dune_project_is_there () =
  let open Alcotest in
  let dp = Lib.Crunched_templates.read "dune-project" in
  (check (neg (option string)))
    "crunched file is present" dp None

let gnumakefile_is_there () =
  let open Alcotest in
  let dp = Lib.Crunched_templates.read "GNUmakefile" in
  (check (neg (option string)))
    "crunched file is present" dp None

let random_string () =
  let rec random_string' acc n =
    let next = Char.chr (Random.int 26 + 65) in
    if n = 0
    then acc
    else random_string' (next :: acc) (n - 1)
  in
  let lst = random_string' [] 10 in
  Prelude.String.implode lst

let runargs = Lib.Command.runargs

(* let trigger_already_exists () = *)

(* runargs ["mkdir" *)

let () =
  let open Alcotest in
  run "Spinup Tests"
    [ ( "check crunched dir",
        [ test_case "dune-project is present" `Quick
            dune_project_is_there;
          test_case "GNUmakefile is present" `Quick
            gnumakefile_is_there
        ] )
    ]
