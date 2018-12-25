;; Jae Han's init.el file
;; Emacs settings


;; Early configurations
;; when delete, move to trash
(setq delete-by-moving-to-trash t)
(setq trash-directory "~/.Trash")

(setq inhibit-startup-screen t)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
;; C-x <LEFT> undoes windows
(winner-mode t)


;; System/Files
;; reduce the frequency of garbage collection with threshold=50mb
(setq gc-cons-threshold 50000000)
;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)
;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
;;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)


;; Display
;; open saved windows; (Use M-x desktop-save for new setting)
(desktop-save-mode t)
;; disable autosaving desktop
(setq desktop-auto-save-timeout nil)
(setq desktop-save nil)
;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
;; use y/n answers
(fset 'yes-or-no-p 'y-or-n-p)
(setq dired-deletion-confirmer #'y-or-n-p)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Text editing
;; delete the selection with a keypress
(delete-selection-mode t)


;; Custom key bindings
(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)


;; Package install settings
;; ;; add additional trusted CAs to allow gnu, melpa
;; (require 'gnutls)
;; (add-to-list 'gnutls-trustfiles "/usr/local/etc/openssl/cert.pem")

;; Set up archives
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
			 ;; ("org" . "http://orgmode.org/elpa/")
			 ;; ("melpa-stable" . "https://stable.melpa.org/packages/")
			 ))
;; load path for packages download in raw form
(add-to-list 'load-path "~/.emacs.d/raw/")
;; install packages to .emacs.d/packages
(setq package-user-dir (expand-file-name "packages" user-emacs-directory))
; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))
(package-initialize)

;; Use-package
;; install and require use-package if not already
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
;; indicate package loading activity as a message
(setq use-package-verbose t)
;; always install package if not already
(setq use-package-always-ensure t)

;; Built-in modes
;; for useful autocompletion
;; move around with shift+arrow
(use-package windmove
  :config
  (windmove-default-keybindings))

;; hideshow (builtin)
(use-package hideshow
  :hook ((c-mode . hs-minor-mode)
	 (emacs-lisp-mode . hs-minor-mode)
	 (java-mode . hs-minor-mode)
	 (lisp-mode . hs-minor-mode)
	 (perl-mode . hs-minor-mode)
	 (sh-mode . hs-minor-mode)
	 (python-mode . hs-minor-mode))

  :config
  (defun toggle-selective-display (column)
      (interactive "P")
      (set-selective-display
       (or column
           (unless selective-display
             (1+ (current-column))))))

  (defun toggle-hiding (column)
    (interactive "P")
    (if hs-minor-mode
        (if (condition-case nil
                (hs-toggle-hiding)
              (error t))
            (hs-show-all))
      (toggle-selective-display column)))

  (global-set-key (kbd "C-+") 'toggle-hiding)
  (global-set-key (kbd "C-\\") 'toggle-selective-display)
  )

;; c++ custom style
(c-add-style "cpp-custom-style"
	     '("stroustrup"
	       (c-basic-offset . 4)))

(defun my/c++-mode-hook ()
  (interactive)
  (message "Running my/c++-mode-hook")
  (c-set-style "cpp-custom-style")
  (auto-fill-mode)
  (c-toggle-hungry-state t)
  ;; properly indent switch-statement
  (c-set-offset 'case-label '+))

(add-hook 'c++-mode-hook 'my/c++-mode-hook)

;; c custom style
(c-add-style "c-custom-style"
	     '("stroustrup"
	       (c-basic-offset . 4)))

(defun my/c-mode-hook ()
  (interactive)
  (message "Running my/c-mode-hook")
  (c-set-style "c-custom-style")
  (auto-fill-mode)
  (c-toggle-hungry-state t))

(add-hook 'c-mode-hook 'my/c-mode-hook)


;; Themes
(use-package dracula-theme
  :load-path "themes/dracula-theme"
  :config
  ;; (load-theme 'dracula t)
  )

(use-package spacemacs-dark-theme
  :load-path "themes/spacemacs-theme"
  )

;; disabled due to conflict with ox-jekyll
;; (use-package doom-themes
;;   )

;; ;; Package specifics
;; (use-package ace-window
;;   :bind ("C-x o" . 'ace-window)
;;   )


;; Packages
;; ;; get environment variables such as $PATH from the shell (only for unix systems)
;; (use-package exec-path-from-shell
;;   :if (memq window-system '(mac ns))
;;   :config
;;   (exec-path-from-shell-initialize))

(use-package org
  :hook (org-mode . visual-line-mode)
  :config
  ;; enable languages for org-
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (R . t)
     (haskell . t)))

  ;; initial settings
  ;; key bindings
  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cc" 'org-capture)
  (global-set-key "\C-cb" 'org-iswitchb)

  ;; sets a default target file for captures
  (setq org-default-notes-file
	(concat (file-name-as-directory org-directory) "tasks.org"))
  
  ;; create CLOSED timestamps when TODO -> DONE
  (setq org-log-done 'time)

  ;; disable using sub(_) and superscripts(^)
  (setq org-use-sub-superscripts nil)
  (setq org-export-with-sub-superscripts nil)

  ;; don't ask when evaluating code blocks
  (setq org-confirm-babel-evaluate nil)

  ;; use colored code blocks in org file
  (setq org-src-fontify-natively t)

  ;; ;; ;; export to pdf in color
  ;; (add-to-list 'org-latex-packages-alist '("" "color"))

  ;; ;; ;; settings for minted
  ;; (add-to-list 'org-latex-packages-alist '("" "minted"))
  ;; ;; (add-to-list 'org-latex-packages-alist '("" "minted" nil))
  ;; (setq org-latex-listings 'minted)
  ;; (setq org-latex-pdf-process
  ;; 	'("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
  ;; 	  "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
  ;; 	  "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  ;; ;; ;; minted conflicts with latex preview with dvipng so convert to imagemagick
  ;; (setq org-latex-create-formula-image-program 'imagemagick)
  ;; ;; (setq org-latex-create-formula-image-program 'dvipng)

  ;; src_block exporting
  ;; (setq org-src-preserve-indentation t)
  (setq org-src-preserve-indentation nil
	org-edit-src-content-indentation 0)
  ;; (setq indent-tabs-mode nil)

  ;; open folders in dired
  (add-to-list 'org-file-apps '(directory . emacs))
  
  ;; "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (defun org-summary-todo (n-done n-not-done)
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
  ;; open image files in same buffer
  (setq org-toggle-inline-images t)
  ;; use tab natively in source blocks
  (setq org-src-tab-acts-natively t)
  ;; place captions below
  (setq org-latex-caption-above nil)
  ;; export apostrophes correctly
  (setq org-export-with-smart-quotes t)

  ;; LATEX export: start each heading on new page
  ;; (defun org/get-headline-string-element  (headline backend info)
  ;;   (let ((prop-point (next-property-change 0 headline)))
  ;;     (if prop-point (plist-get (text-properties-at prop-point headline) :parent))))

  ;; (defun org/ensure-latex-clearpage (headline backend info)
  ;;   (when (org-export-derived-backend-p backend 'latex)
  ;;     (let ((elmnt (org/get-headline-string-element headline backend info)))
  ;; 	(when (member "newpage" (org-element-property :tags elmnt))
  ;; 	  (concat "\\clearpage\n" headline)))))

  ;; (eval-after-load 'ox
  ;; '(progn
  ;;    (add-to-list 'org-export-filter-headline-functions
  ;;                 'my-html-filter-headline-yesdot)))
  
  ;; set export backends (added beamer)
  (setq org-export-backends (quote (ascii beamer html icalendar latex odt)))
  )

;; package for exporting org to markdown
(use-package ox-gfm
  :after org)

;; Octopress with jekyll
(use-package org-octopress
  :after org
  :load-path "~/.emacs.d/raw/org-octopress/"
  :config
  (setq org-octopress-directory-top       "~/hanjae1122.github.io")
  (setq org-octopress-directory-posts     "~/hanjae1122.github.io/_posts")
  (setq org-octopress-directory-org-top   "~/hanjae1122.github.io/org")
  (setq org-octopress-directory-org-posts "~/hanjae1122.github.io/org/_posts")
  (setq org-octopress-setup-file          "~/.emacs.d/ox_jekyll_setupfile.org")

  ;; need to change org links to match website structure
  ;; we use permalink in _config.yml to fix path depth and use relative paths
  
  ;; another way is to add custom links but this disables ATTR_HTML, texted links etc
  ;; ;; checkout https://www.mfoot.com/blog/2015/11/17/using-org-mode-to-write-jekyll-blogs/
  ;; ;; and https://stackoverflow.com/questions/14684263/how-to-org-mode-image-absolute-path-of-export-html
  ;;  (defun org-custom-link-img-follow (path)
  ;;    (org-open-file-with-emacs
  ;;     (format "../files/image/%s" path)))

  ;;  (defun org-custom-link-img-export (path desc format)
  ;;    (cond
  ;;     ((eq format 'html)
  ;;      (format "<img src=\"/files/image/%s\" alt=\"%s\"/>" path desc))))

  ;;  (org-add-link-type "img" 'org-custom-link-img-follow 'org-custom-link-img-export)
  )

;; (use-package org-pdfview
;;   :after (:all org pdf-tools)
;;   :config
;;   (add-to-list 'org-file-apps 
;;                '("\\.pdf\\'" . (lambda (file link)
;; 				 (org-pdfview-open link)))))


;; ;; Programming
;; (use-package ess-site
;;   :load-path "~/.emacs.d/raw/ess/lisp/"
;;   :commands R
;;   )

;; (use-package flycheck
;;   :hook (python-mode . flycheck-mode)
;;   )

;; (use-package elpy
;;   :hook ((python-mode . elpy-mode)
;; 	 (python-mode . visual-line-mode))
;;   :config
;;   ;; (pyvenv-workon "py3-Egm4BeYe")
;;   ;; remove warning
;;   (setq python-shell-completion-native-enable nil)
;;   ;; use Ipython interpreter
;;   (setq python-shell-interpreter "ipython"
;;   	python-shell-interpreter-args "-i --simple-prompt")
;;   ;; (setq python-shell-interpreter "jupyter"
;;   ;; 	python-shell-interpreter-args "console --simple-prompt"
;;   ;; 	python-shell-prompt-detect-failure-warning nil)
;;   ;; (add-to-list 'python-shell-completion-native-disabled-interpreters
;;   ;; 	       "jupyter")
;;   ;; ;; disable flymake (use flycheck)
;;   ;; (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;   )

(use-package projectile
  :hook (prog-mode . projectile-mode)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  )

(use-package magit
  :bind (("C-x g" . 'magit-status)
	 ("C-x M-g" . 'magit-dispatch-popup))
  )

;; ;;; Install epdfinfo via 'brew install pdf-tools' and then install the
;; ;;; pdf-tools elisp via the use-package below. To upgrade the epdfinfo
;; ;;; server, just do 'brew upgrade pdf-tools' prior to upgrading to newest
;; ;;; pdf-tools package using Emacs package system. If things get messed
;; ;;; up, just do 'brew uninstall pdf-tools', wipe out the elpa
;; ;;; pdf-tools package and reinstall both as at the start.
;; (use-package pdf-tools
;;   :config
;;   ;; (custom-set-variables
;;   ;;   '(pdf-tools-handle-upgrades nil)) ; Use brew upgrade pdf-tools instead.
;;   (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")
;;   (pdf-tools-install)
;;   )

(use-package yasnippet
  :config
  (yas-global-mode t)
  )

(use-package eimp
  :hook (image-mode . eimp-mode)
  )

(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode))
  )

(use-package linum-relative
  :hook (prog-mode . linum-relative-mode)
  :config
  ;; Use `display-line-number-mode` as linum-mode's backend for smooth performance
  (setq linum-relative-backend 'display-line-numbers-mode))

(use-package undo-tree
  :config
  (global-undo-tree-mode))

(use-package dimmer
  :config
  (dimmer-mode))


;; bind right alt key to super
(setq ns-right-option-modifier 'super)
(use-package crux
  :config
  (global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
  (global-set-key (kbd "C-c o") #'crux-open-with)
  (global-set-key [(shift return)] #'crux-smart-open-line)
  (global-set-key (kbd "C-<backspace>") #'crux-kill-line-backwards)
  (global-set-key [remap kill-whole-line] #'crux-smart-kill-line)
  ;; (global-set-key [remap kill-whole-line] #'crux-kill-whole-line)
  (global-set-key (kbd "C-c n") #'crux-cleanup-buffer-or-region))


(use-package popup-kill-ring
  :config
  (global-set-key "\M-y" 'popup-kill-ring))


;; (use-package dashboard
;;   :config
;;   (dashboard-setup-startup-hook)
;;   (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
;;   (setq dashboard-items '((recents  . 5)
;;                         (bookmarks . 5)
;;                         (projects . 5)
;;                         (agenda . 5)
;;                         (registers . 5))))

(use-package company)

(use-package smartparens
  :config
  ;; Use the base configuration
  (require 'smartparens-config nil t)
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  ;; when you press RET, the curly braces automatically
  ;; add another newline
  (sp-with-modes '(c-mode c++-mode)
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET"))))
  ;; bug with '' in c-mode
  (setq sp-escape-quotes-after-insert nil))

;; (use-package beacon
;;   :config
;;   (beacon-mode 1))


;; enable move between sub-words
;; only for haskell for now
(use-package haskell-mode
  :hook (haskell-mode . subword-mode)
  )

(use-package ivy
  :config
  (ivy-mode t)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  ;; use ivy in magit
  (setq magit-completing-read-function 'ivy-completing-read)
  )

(use-package swiper
  :config
  (defun bind-swiper-key ()
    (local-set-key (kbd "C-s") 'swiper))
  (add-hook 'dired-mode-hook 'bind-swiper-key)
  (add-hook 'prog-mode-hook 'bind-swiper-key))

;; ====================================================================================
;; ====================================================================================
;; ====================================================================================
