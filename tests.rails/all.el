(load "elunit")
(elunit-clear-suites)

(defsuite rails nil)

(mapcar
 #'load-file
 (directory-files "./" t "\\.test\\.el$"))

(elunit "rails")
