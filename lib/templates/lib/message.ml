let response = 
  let headers = [ "Status: 200" ;
                  "Content-Type: text/plain" ; ]
  in
  let crlf = "\r\n" in
  let body = "Your wish is granted.  Long live Jambi." in
  String.concat "" [ String.concat crlf headers ;
                     crlf ;
                     crlf ;
                     body ;
                     crlf ; ]

(* Local Variables: *)
(* mode: tuareg *)
(* End: *)
