let response =
  let nil = Seq.empty |> List.of_seq in
  let headers = "Status: 200" ::
                  "Content-Type: text/plain" ::
                    nil
  in
  let crlf = "\r\n" in
  let body = "Your wish is granted.  Long live Jambi." in
  String.concat "" @@ String.concat crlf headers
                      :: crlf
                      :: crlf
                      :: body
                      :: crlf
                      :: nil

(* Local Variables: *)
(* mode: tuareg *)
(* End: *)
