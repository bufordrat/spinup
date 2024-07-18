(lang dune 3.12)
(name spinup)
(generate_opam_files true)
 (package
  (name spinup)
  (description "Insert project description here.")
  (synopsis "Insert project synopsis, which is supposedly different, here")
  (maintainers "Your Name <youremail@gmail.com>")
  (authors "Your Name <youremail@gmail.com>")
  (homepage "https://your.website.here")
  (bug_reports "https://your.website.here")
  (depends
   (ocaml (>= 4.14.0))
    dune
    camlp-streams
    (prelude (= "~dev"))
    etude
    utop
    ocp-index
    crunch
    tint
    (alcotest :with-test)
    qcheck
    merlin)
  (source (github bufordrat/spinup)))

