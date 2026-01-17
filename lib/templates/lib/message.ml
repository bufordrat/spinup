let response =
  let nil = Seq.empty |> List.of_seq in
  let sayings = "Your wish is granted.  Long live Jambi."
                :: "Why don't you take a picture?  It'll last longer."
                :: "Mecca lecca hi, mecca hiney ho."
                :: "You don't wanna get mixed up with a guy like \
                    me. I'm a loner, Dottie. A rebel."
                :: "I know you are, but what am I?"
                :: nil
  in
  let headers = "Status: 200"
                :: "Content-Type: text/plain"
                :: nil
  in
  let crlf = "\r\n" in
  let idx =
    let () = Random.self_init () in
    Random.full_int max_int mod List.length sayings
  in
  let body = List.nth sayings idx in
  String.concat "" @@ String.concat crlf headers
                      :: crlf
                      :: crlf
                      :: body
                      :: crlf
                      :: nil

(* Local Variables: *)
(* mode: tuareg *)
(* End: *)
