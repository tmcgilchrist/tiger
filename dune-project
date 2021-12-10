(lang dune 2.8)
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
  (core_kernel (>= v0.11.0))
   smart-print
  (ounit :with-test)
  (ocaml (>= 4.13.0))))