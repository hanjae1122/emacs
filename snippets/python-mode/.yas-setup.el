;; Original written by: Egor Panfilov <egor.v.panfilov[at]gmail[dot]com>

(require 'yasnippet)

(defun python-split-args (arg-s)
  "Returns args split into list"
  (mapcar (lambda (s)
	    (split-string s "[[:blank:]]*=[[:blank:]]*" t))
	  (split-string arg-s "[[:blank:]]*,[[:blank:]]*" t)))

(defun python-args-to-docstring (yas-txt)
  "Returns docstring given python arguments in yas-text"
  (let* ((args (python-split-args yas-txt))
	 (format-arg (lambda (arg)
		     (concat (nth 0 arg)
			     " ("
			     (if (nth 1 arg) ", optional")
			     "): "
			     "\n")))
	 (doc-args (mapconcat format-arg args "")))
    (unless (string= doc-args "")
      (concat "\n" doc-args))))

(add-hook 'python-mode-hook
          '(lambda () (set (make-local-variable 'yas-indent-line) 'fixed)))
