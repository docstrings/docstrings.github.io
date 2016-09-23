(require 'cl)

(put 'variable-documentation 'doc-name "Variable")

(defun insert-docstring (sym kind)
  (when (documentation-property sym kind)
    (insert (format "<h3>%s: %s</h3>\n"
		    (get kind 'doc-name) (symbol-name sym)))
    (insert "<p>\n")
    (insert (documentation-property sym 'variable-documentation))
    (insert "</p>\n")))

(defun insert-all-docstrings (sym)
  (insert (format "<a name=\"%s\"></a>\n" (symbol-name sym)))
  (when (and (functionp sym) (documentation sym))
    (insert (format "<h3>Function: %s</h3>\n" (symbol-name sym)))
    (insert "<p>\n")
    (insert (documentation sym))
    (insert "</p>\n"))
  (insert-docstring sym 'variable-documentation))

(defun insert-header ()
  (insert "<!DOCTYPE html>\n")
  (insert "<html><head><title>Emacs docstrings</title></head>\n")
  (insert "<body>\n")
  (insert "<h1>Emacs docstrings</h1>\n")
  (insert (format "<p>Generated from Emacs version %s.</p>\n"
		  emacs-version))
  (let ((url "http://github.com/docstrings/docstrings.github.io/"))
    (insert (format "<p>See <a href=\"%s\">%s</a>.</p>\n" url url))))

(defun fix-formatting ()
  (goto-char (point-min))
  (while (re-search-forward "\n\n" nil t)
    (replace-match "</p>\n<p>\n")))

(defun docstrings ()
  (with-temp-buffer
    (find-file "index.html")
    (erase-buffer)
    (insert-header)
    (mapatoms #'insert-all-docstrings)
    (fix-formatting)
    (goto-char (point-max))
    (insert "</body></html>\n")
    (save-buffer)))
