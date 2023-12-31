Spinup should not let you create a project if the new directory name
would clash with a file in the current directory.
  $ mkdir unplowed_clinking_silliness_baffle
  $ dune clean
  $ dune exec spinup -- unplowed_clinking_silliness_baffle
  spinup: a directory called unplowed_clinking_silliness_baffle/ already exists.
  [1]
  $ rm -r unplowed_clinking_silliness_baffle
