(require 'rails-reloaded)

(mapcar
 #'byte-compile-file
 (directory-files "./" t "\\.el$"))