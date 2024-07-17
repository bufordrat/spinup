type t = Cleanup of { setup : Lib.Command.t;
                      test_action : Lib.Action.t;
                      teardown : Lib.Command.t }

let temp_dir dirname pname =
  { setup = { args = [ "mkdir" ; dirname ] ;
              cmessage = "creating directory called 'project'" ;
              verbosity = Quiet } ;
    test_action = Lib.Main.action ;
    teardown = { args [ "rm"; "-rf"; dirname ] ;
                 cmessage = "removing directory called 'project'" ;
                 verbosity = Quiet } }

(* let run { setup; test_action; teardown } = *)
(*   let open Lib.Action in *)
(*   run (Run setup) ; *)
(*    = run test_action in *)
(*   run (Run teardown) ; *)
  

