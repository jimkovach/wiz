(defparameter *file-path* "/users/jimkovach/program/lisp/LoLisp/wizards")
(load "/users/jimkovach/program/lisp/sdraw.lsp")
(load "/users/jimkovach/program/lisp/dtrace.lsp")

(defun ld-graph()
  (load "/users/jimkovach/program/lisp/LoLisp/wizards/graph.lsp"))

(defun cls()
  (screen:with-window (screen:clear-window screen:*window*)))

(defun ls()
  (run-shell-command "ls"))

(defun dr()
  (run-shell-command "ls -Aal"))

(defun ld-init()
  (load "/users/jimkovach/program/lisp/LoLisp/wizards/init.lsp"))

(defun wizards()
  (load "/users/jimkovach/program/lisp/LoLisp/wizards/wizards.lsp")
  (cls)
  (game-repl))