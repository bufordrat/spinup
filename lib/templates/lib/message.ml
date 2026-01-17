let response =
  let sayings = ##[left] "Your wish is granted.  Long live Jambi." ;
                  "Why don't you take a picture?  It'll last longer." ;
                  "Mecca lecca hi, mecca hiney ho." ;
                  "You don't wanna get mixed up with a guy like \
                   me. I'm a loner, Dottie. A rebel." ;
                  "I know you are, but what am I?" ; ##[right]
  in
  let headers = ##[left] "Status: 200" ;
                  "Content-Type: text/plain" ; ##[right]
  in
  let crlf = "\r\n" in
  let idx =
    let () = Random.self_init () in
    Random.full_int max_int mod List.length sayings
  in
  let body = List.nth sayings idx in
  String.concat "" ##[left] String.concat crlf headers ;
                     crlf ;
                     crlf ;
                     body ;
                     crlf ; ##[right]

(* Local Variables: *)
(* mode: tuareg *)
(* End: *)
