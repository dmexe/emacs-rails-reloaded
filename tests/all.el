(require 'elunit)

(defsuite ext nil)

(mapcar
 #'load-file
 (directory-files "./" t "\\.test\\.el$"))

(elunit "ext")
