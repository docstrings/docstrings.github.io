(require 'cl)

(defvar *all-symbols* nil)

(defun fn-doc (sym)
  (and (functionp sym) (documentation sym)))

(defun face-doc (sym)
  (face-documentation sym))

(defun var-doc (sym)
  (documentation-property sym 'variable-documentation))

(defun group-doc (sym)
  (documentation-property sym 'group-documentation))

(defun insert-docstring (sym fn type)
  (let ((doc (funcall fn sym)))
    (when doc
      (insert (format "<h3>%s: %s</h3>\n" type (symbol-name sym)))
      (insert "<p>\n")
      (insert doc)
      (insert "</p>\n"))))

(defun escape-filename (string)
  (let ((n 0))
    (while (setq n (string-match "/" string n))
      (setq string (replace-match "-" t t string))
      (incf n)))
  string)

(defun insert-all-docstrings (sym)
  (insert (format "<a name=\"%s\"></a>\n" (symbol-name sym)))
  (let ((start (point)))
    (insert-docstring sym 'fn-doc "Function")
    (insert-docstring sym 'var-doc "Variable")
    (insert-docstring sym 'face-doc "Face")
    ;;(insert-docstring sym 'group-doc "Customization Group")
    (let ((copy (buffer-substring start (point))))
      (when (plusp (length copy))
	(with-temp-buffer
	  (insert copy)
	  (fix-formatting)
	  (write-region (point-min) (point-max)
			(format "sym/%s.html" (escape-filename (symbol-name sym))))
	  (push sym *all-symbols*)
	  (kill-buffer))))))

(defun insert-header (title more)
  (insert "<!DOCTYPE html>\n")
  (insert (format "<html><head><title>%s</title></head>\n" title))
  (insert "<body>\n")
  (insert (format "<h1>%s</h1>\n" title))
  (insert (format "<p>Generated from Emacs version %s.</p>\n"
		  emacs-version))
  (insert more))

(defun insert-index (sym)
  (insert (format "<a href=\"sym/%s.html\">%s</a><br>\n"
		  (escape-filename (symbol-name sym)) (symbol-name sym))))

(defun fix-formatting ()
  (goto-char (point-min))
  (while (re-search-forward "\n\n" nil t)
    (replace-match "</p>\n<p>\n")))

(defun write-index (syms)
  (with-temp-buffer
    (find-file "list.html")
    (erase-buffer)
    (insert-header "Alphabetical symbol list" "")
    (dolist (sym syms)
      (insert-index sym))
    (goto-char (point-max))
    (insert "</body></html>\n")
    (save-buffer)
    (kill-buffer)))

(defun docstrings ()
  (with-temp-buffer
    (find-file "index.html")
    (erase-buffer)
    (let ((url "http://github.com/docstrings/docstrings.github.io"))
      (insert-header "Emacs docstrings"
		     (format "<p>%s</p><p>Source: <a href=\"%s\">%s</a>.</p>\n"
			     "<a href=\"list.html\">Alphabetical list</a>"
			     url url)))
    (let ((*all-symbols* nil))
      (mapatoms #'insert-all-docstrings)
      (write-index (sort *all-symbols* #'string<)))
    (fix-formatting)
    (goto-char (point-max))
    (insert "</body></html>\n")
    (save-buffer)
    (kill-buffer)))
