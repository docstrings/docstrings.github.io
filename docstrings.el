(require 'cl)

(put 'variable-documentation 'doc-name "Variable")

(defun insert-docstring (sym kind)
  (when (documentation-property sym kind)
    (insert (format "<h3>%s: %s</h3>\n"
		    (get kind 'doc-name) (symbol-name sym)))
    (insert (documentation-property sym 'variable-documentation))
    (insert "\n")))

(defun insert-all-docstrings (sym)
  (insert (format "<a name=\"%s\"></a>\n" (symbol-name sym)))
  (when (and (functionp sym) (documentation sym))
    (insert (format "<h3>Function: %s</h3>\n" (symbol-name sym)))
    (insert (documentation sym))
    (insert "\n"))
  (insert-docstring sym 'variable-documentation))

(defun fix-formatting ()
  (goto-char (point-min))
  (while (re-search-forward "\n\n" nil t)
    (replace-match "</p>\n<p>\n")))

(defun docstrings ()
  (with-temp-buffer
    (find-file "index.html")
    (erase-buffer)
    (insert "<!DOCTYPE html>\n")
    (insert "<html><head><title>Emacs docstrings</title></head>\n")
    (insert "<body>\n")
    (insert "<h1>Emacs docstrings</h1>\n")
    (mapatoms #'insert-all-docstrings)
    (fix-formatting)
    (goto-char (point-max))
    (insert "</body></html>\n")
    (save-buffer)))
