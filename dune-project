(lang dune 3.0)
(name tiger)
(using menhir 2.0)

(generate_opam_files true)
(license BSD-3-clause)
(maintainers "Tim McGilchrist <timmcgil@gmail.com>")
(authors "Tim McGilchrist" )
(source (github tmcgilchrist/tiger))

(package
 (name tiger)
 (synopsis "Compiler for Tiger language from Modern Compiler Implementation in ML")
 (depends
   menhir
   smart-print
  (ounit :with-test)
  (ocaml (>= 5.1.0))))
