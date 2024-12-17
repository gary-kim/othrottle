(lang dune 3.15)

(name othrottle)

(generate_opam_files true)

(source
 (sourcehut gary-kim/othrottle))

(authors "Gary Kim <gary@garykim.dev>")

(maintainers "Gary Kim <gary@garykim.dev>")

(license AGPL-3.0-or-later)

(documentation https://sr.ht/~gary-kim/othrottle)

(package
 (name othrottle)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
   (ocaml-base-compiler (>= 5.2.0))
   dune
   (base (>= 0.17.0))
   (core (>= 0.17.0))
   (core_unix (>= 0.17.0))
   (ppx_jane (>= 0.17.0))
   (async (>= 0.17.0))
   (async_unix (>= 0.17.0))
   (otoml (>= 1.0.5))
   (xdg (>= 3.16.0))
   (re2 (>= 0.17.0))
   ocamlformat
   dune-build-info)
 (tags
  ("unix")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
