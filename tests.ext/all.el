(load "elunit")
(elunit-clear-suites)

(defsuite ext nil)

(mapcar
 #'load-file
 (directory-files "./" t "\\.test\\.el$"))

(elunit "ext")
