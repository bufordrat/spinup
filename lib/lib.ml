open Prelude

module Functor = struct
  module type FUNCTOR = sig
    type 'a t
    val map : ('a -> 'b) -> 'a t -> 'b t
  end
  
  module Goodies (F : FUNCTOR) = struct
    let (let+) x f = F.map f x
    let (>>|) = (let+)
    let (<&>) = (let+)
    let (>|=) = (let+)
    let (<$>) = F.map
  end

  module Compose (F1 : FUNCTOR) (F2 : FUNCTOR) = struct
    type 'a t = 'a F2.t F1.t
    let map f composed = F1.map (F2.map f) composed
  end
end
module type FUNCTOR = Functor.FUNCTOR

module Applicative = struct
  module type APPLICATIVE = sig
    include FUNCTOR
    val product : 'a t -> 'b t -> ('a * 'b) t
  end

  module Goodies (A : APPLICATIVE) = struct
    include Functor.Goodies (A)
    let (and+) = A.product
    let apply af ax =
      A.map
        (fun (f, x) -> f x)
        (A.product af ax)
    let (<*>) = apply
  end
end
module type APPLICATIVE = Applicative.APPLICATIVE

module Monad = struct
  module type MONAD = sig
    type 'a t
    val pure : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
  end
  
  module Goodies (M : MONAD) = struct
    let pure = M.pure
    let bind = M.bind
    let (>>=) = bind
    let (let*) = bind
    let (>=>) mf mg x = mf x >>= mg
    let (<=<) mf mg x = mg x >>= mf
    let (>>) mx my = mx >>= fun _ -> my
    end

    module ToApplicative (M : MONAD) = struct
      include Goodies (M)

      (* reduction of applicative interface to monadic interface *)
      let map f mx = let* x = mx in
                     pure (f x)
      let product ax ay = let* x = ax in
                          let* y = ay in
                          pure (x,y)

      module I = struct
        type 'a t = 'a M.t
        let map = map
        let product = product
      end

      include Applicative.Goodies (I)
      open Fun
      let ( <* ) ax ay = pure const <*> ax <*> ay
      let ( *> ) ax ay = pure (flip const) <*> ax <*> ay
    end
end
module type MONAD = Monad.MONAD

module Option = struct

  (* unwraps the Somes; throws the None-s out *)
  include Stdlib.Option
  include Prelude.Option
  let rec cat_options = function
    | [] -> []
    | Some x :: xs -> x :: cat_options xs
    | None :: xs -> cat_options xs

  (* for auto-generating monad and applicative stuff *)
  module OptionMonad = struct
    type 'a t = 'a option
    let pure = some
    let bind = (>>=)
  end

  include Monad.ToApplicative (OptionMonad)
end

module type ERROR = sig
  type t
end
              
module Result = struct
  (* module functor for building a Result module with cool extra stuff
     in it; takes a module containing the error type as an input *) 
  module Make (E : ERROR) = struct
    include Stdlib.Result
    include Prelude.Result
    
    (* for auto-generating monad and applicative stuff *)
    module ResultMonad = struct
      type 'a t = ('a, E.t) result
      let pure = ok
      let bind = (>>=)
    end
    
    include Monad.ToApplicative (ResultMonad)
  end
end
              
module StringParser = struct
  module PResult = Result.Make (String)

  module ParserMonad = struct
    type 'output t =
      string -> (('output * string), string) result
    let pure x = fun stream -> PResult.ok (x, stream)
    let bind prsr k = let open PResult in 
                      fun input ->
                      let* (result1, remainder1) = prsr input in
                      (k result1) remainder1
  end
  include Monad.ToApplicative (ParserMonad)

  let alternative prsr1 prsr2 input =
    match prsr1 input with
    | Error _ -> prsr2 input
    | _ -> prsr1 input
  let (<|>) = alternative
            
  module Combinators = struct

    let succeed input = PResult.ok input

    let fail _ = PResult.error "error: pfail"

    let choice prsrs = List.foldl (<|>) fail prsrs

    let optional prsr = prsr *> pure () <|> pure ()
                     
    let satisfy pred = let open String in function
      | "" -> PResult.error "end of file"
      | str ->
         let head = str.[0] in
         let tail = sub str 1 (length str - 1) in
         if pred head
         then PResult.ok (head, tail)
         else PResult.error "error: satisfy"

    let munch1 pred input =
      let open String in
      let rec span pred = function
        | "" -> ("", "")
        | str ->
           let head = sub str 0 1 in
           let recurse = sub str 1 (length str - 1) |> span pred in
           if pred str.[0]
           then head ^ fst recurse, snd recurse
           else "", str
      in
      match span pred input with
      | ("",_) -> PResult.error "error: span"
      | _ -> PResult.ok (span pred input)

    let eof = function
      | "" -> PResult.ok ((), "")
      | _ -> PResult.error "error: eof"
    
    let char c = satisfy (fun x -> x = c)

    let string str = 
      let concat_char strP chr =
        let+ str = strP
        and+ chr = char chr in
        str ^ String.make 1 chr
      in
      String.foldl concat_char (pure "") str
               
    let parse_string prsr str =
      match prsr str with
      | Ok (output, []) -> Ok output
      | Error _ as e -> e
      | _ -> Error "partial parse"
                
    let rec many prsr input =
      match prsr input with
      | Ok _ -> (pure cons <*> prsr <*> many prsr) input
      | Error _ -> (pure []) input

    let many1 prsr = pure cons <*> prsr <*> many prsr

    let sep_by1 prsr sepPrsr =
      let+ initial = many (prsr <* sepPrsr)
      and+ final = prsr
      in initial @ [final]

    let skip_spaces1 =
      let is_space chr =
        String.(mem chr whitespace)
      in
      pure () <* munch1 is_space

    let skip_spaces = skip_spaces1 <|> pure ()

    let rec sequence = function
      | [] -> pure []
      | x :: xs -> let+ p1 = x
                   and+ p2 = sequence xs
                   in cons p1 p2
    
    let chainl op p =
      let rec apply_all x = function
        | [] -> x
        | f :: fs -> apply_all (f x) fs
      in
      apply_all <$> p <*> many (flip <$> op <*> p)
   
    let pack l prsr r = string l *> prsr <* string r
    
    let op (func, str) = pure func <* string str

    let any_op input = (choice << List.map op) input
      
    let mk_expr atomic ranking left right =
      let rec expr input = (foldr chainl factor ranking) input
      and factor input = (atomic <|> pack left expr right) input
      in (expr, factor)      
  end
  include Combinators
end

module Constants = struct
  let use_output = {|(*
* provide 4.11's ocaml toplevel #use_output directive for pre-4.11 ocamls
* see: <https://dune.readthedocs.io/en/stable/toplevel-integration.html>
*)

#directory "+compiler-libs"

let try_finally ~always f =
  match f () with
  | x ->
    always ();
    x
  | exception e ->
    always ();
    raise e

let use_output command =
  let fn = Filename.temp_file "ocaml" "_toploop.ml" in
  try_finally
    ~always:(fun () -> try Sys.remove fn with Sys_error _ -> ())
    (fun () ->
      match
        Printf.ksprintf Sys.command "%s > %s" command (Filename.quote fn)
      with
      | 0 -> ignore (Toploop.use_file Format.std_formatter fn : bool)
      | n -> Format.printf "Command exited with code %d.@." n)

let () =
  let name = "use_output" in
  if not (Hashtbl.mem Toploop.directive_table name) then
    Hashtbl.add Toploop.directive_table name
      (Toploop.Directive_string use_output)

;;
#remove_directory "+compiler-libs" ;;
|}

  let ocamlinit = {|#use "./use-output.top" ;;
#use_output "opam exec -- dune top"  ;;
open Prelude ;;
#use "topfind";;
|}

  let dune_project project_name = sprintf {|(lang dune 2.7)
(name %s)
(generate_opam_files true)
|} project_name

  let lib =
    "let message = \"GOODBYE CRUEL WORLD (is underrated)\""

  let lib_dune = {|(library
 (name lib)
 (libraries prelude))
|}

  let exe =
    "let () = print_endline Lib.message"
    
  let exe_dune project_name = sprintf {|(executable
 (public_name %s)
 (name spinup %s)
 (libraries feather prelude lib))
|} project_name project_name
      
end

module Commands = struct
  open Feather
  open Infix
  open Constants
     
  let mk_project name =
    process "dune init project" [ name ]

  let cd name =
    process "cd" [ name ]

  let delete_bin =
    process "rm" [ "bin" ]

  let init_executable name =
    process "dune init exe" [ name ]

  let create_lib =
    echo lib > "lib/lib.ml"

  let create_lib_dune =
    echo lib_dune > "lib/dune"

  let create_exe name =
    echo exe > (name ^ ".ml")

  let create_exe_dune name =
    echo (exe_dune name) > "dune"

  let create_use_output =
    echo use_output > "use-output.top"

  let create_ocamlinit =
    echo ocamlinit > ".ocamlinit"

  let create_dune_project name =
    echo (dune_project name) > "dune-project"
end
