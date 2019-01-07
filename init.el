;; Jae Han's init.el file
;; Emacs settings

;; Early configurations
(setq debug-on-error t)
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

;; ;; more useful frame title, that show either a file or a
;; ;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Text editing
;; delete the selection with a keypress
(delete-selection-mode t)


;; Custom key bindings
;; bind right alt key to super
(setq ns-right-alternate-modifier 'alt)

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
(package-initialize)
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
;; ; fetch the list of packages available
;; (unless package-archive-contents
;;   (package-refresh-contents))
;; (package-initialize)

(unless package-archive-contents    ;; Refresh the packages descriptions
  (package-refresh-contents))
(setq package-load-list '(all))     ;; List of packages to load
(package-initialize)                ;; Initialize & Install Package

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
(use-package spacemacs-dark-theme
  :load-path "themes/spacemacs-theme"
  )

;; Package specifics
(use-package ace-window
  :bind ("C-x o" . 'ace-window)
  )


;; Packages
;; get environment variables such as $PATH from the shell (only for unix systems)
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

(use-package org
  :hook (org-mode . visual-line-mode)
  :config
  ;; enable languages for org-
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (R . t)
     (haskell . t)
     (C . t)))

  (setq org-list-demote-modify-bullet
	'(("+" . "-") ("-" . "+") ("*" . "+")))
  
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

  ;; ;; export to pdf in color
  (add-to-list 'org-latex-packages-alist '("" "color"))

  ;; ;; settings for minted
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  ;; (add-to-list 'org-latex-packages-alist '("" "minted" nil))
  (setq org-latex-listings 'minted)
  (setq org-latex-pdf-process
  	'("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
  	  "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
  	  "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  ;; ;; minted conflicts with latex preview with dvipng so convert to imagemagick
  (setq org-latex-create-formula-image-program 'imagemagick)
  ;; (setq org-latex-create-formula-image-program 'dvipng)

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

  ;; set export backends (added beamer)
  (setq org-export-backends (quote (ascii beamer html icalendar latex odt)))
  )

;; package for exporting org to markdown
(use-package ox-gfm
  :after org)

;; Octopress with jekyll
(use-package org-octopress
  :ensure org
  :ensure orglue :ensure ctable
  :load-path "~/.emacs.d/raw/org-octopress/"
  :config
  (setq org-octopress-directory-top       "~/hanjae1122.github.io")
  (setq org-octopress-directory-posts     "~/hanjae1122.github.io/_posts")
  (setq org-octopress-directory-org-top   "~/hanjae1122.github.io/org")
  (setq org-octopress-directory-org-posts "~/hanjae1122.github.io/org/_posts")
  (setq org-octopress-setup-file          "~/.emacs.d/ox_jekyll_setupfile/page.org")

  ;; use provided css as stylesheet
  (setq org-html-htmlize-output-type 'css)  
  )

(use-package org-pdfview
  :after (:all org pdf-tools)
  :config
  (add-to-list 'org-file-apps 
               '("\\.pdf\\'" . (lambda (file link)
				 (org-pdfview-open link)))))


;; Programming
(use-package ess
  :commands R
  )

;; (use-package flycheck
;;   :hook (python-mode . flycheck-mode)
;;   )

(use-package elpy
  :hook ((python-mode . elpy-mode)
	 (python-mode . visual-line-mode))
  :config
  ;; remove warning
  (setq python-shell-completion-native-enable nil)
  )

(use-package projectile
  :hook (prog-mode . projectile-mode)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  )

(use-package magit
  :bind (("C-x g" . 'magit-status)
	 ("C-x M-g" . 'magit-dispatch-popup))
  )

;;; Install epdfinfo via 'brew install pdf-tools' and then install the
;;; pdf-tools elisp via the use-package below. To upgrade the epdfinfo
;;; server, just do 'brew upgrade pdf-tools' prior to upgrading to newest
;;; pdf-tools package using Emacs package system. If things get messed
;;; up, just do 'brew uninstall pdf-tools', wipe out the elpa
;;; pdf-tools package and reinstall both as at the start.
(use-package pdf-tools
  :config
  ;; (custom-set-variables
  ;;   '(pdf-tools-handle-upgrades nil)) ; Use brew upgrade pdf-tools instead.
  (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")
  (pdf-tools-install)
  )

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

(use-package crux
  :config
  (global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
  ;; (global-set-key (kbd "C-c o") #'crux-open-with)
  ;; (global-set-key [(shift return)] #'crux-smart-open-line)
  (global-set-key (kbd "C-<backspace>") #'crux-kill-line-backwards)
  ;; (global-set-key [remap kill-whole-line] #'crux-smart-kill-line)
  (global-set-key [remap kill-whole-line] #'crux-kill-whole-line)
  ;; (global-set-key (kbd "C-c n") #'crux-cleanup-buffer-or-region)
  )


(use-package popup-kill-ring
  :config
  (global-set-key "\M-y" 'popup-kill-ring))


(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents  . 5)
                        (projects . 5)
                        (agenda . 5)))
  (setq show-week-agenda-p t)
  )

(use-package company)

(use-package smartparens
  :config
  (smartparens-global-mode t)
  (setq show-paren-delay 0)
  (show-smartparens-global-mode t)
  
  ;; when you press RET, the curly braces automatically
  ;; add another newline
  (sp-with-modes '(c-mode c++-mode)
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET"))))
  ;; ;; bug with '' in c-mode
  ;; (setq sp-escape-quotes-after-insert nil)

  (bind-keys
   :map smartparens-mode-map
   ("C-M-a" . sp-beginning-of-sexp)
   ("C-M-e" . sp-end-of-sexp)

   ("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)

   ("C-M-n" . sp-next-sexp)
   ("C-M-p" . sp-previous-sexp)

   ("C-S-f" . sp-forward-symbol)
   ("C-S-b" . sp-backward-symbol)

   ("C-M-t" . sp-transpose-sexp)
   ("C-M-k" . sp-kill-sexp)
   ("C-M-w" . sp-copy-sexp)
   ;; ("C-M-d" . delete-sexp)

   ("M-[" . sp-backward-unwrap-sexp)
   ("M-]" . sp-unwrap-sexp))

   ;; C-<..> conflicts with emacs backward/fwd-sexp
   ;; use alt instead; bound to right alt key
   ("A-<right>" . sp-forward-slurp-sexp)
   ("A-<left>"  . sp-backward-slurp-sexp)
  )

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


(use-package command-log-mode)

(use-package fireplace)

(use-package yaml-mode)

(use-package easy-kill
  :config
  (global-set-key [remap kill-ring-save] #'easy-kill)
  (global-set-key [remap mark-sexp] #'easy-mark))

;; ====================================================================================
;; ====================================================================================
;; ====================================================================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("f8cf128fa0ef7e61b5546d12bb8ea1584c80ac313db38867b6e774d1d38c73db" "7e78a1030293619094ea6ae80a7579a562068087080e01c2b8b503b27900165c" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "1c082c9b84449e54af757bcae23617d11f563fc9f33a832a8a2813c4d7dfb652" "6b289bab28a7e511f9c54496be647dc60f5bd8f9917c9495978762b99d8c96a0" "93a0885d5f46d2aeac12bf6be1754faa7d5e28b27926b8aa812840fe7d0b7983" "75d3dde259ce79660bac8e9e237b55674b910b470f313cdf4b019230d01a982a" "ecba61c2239fbef776a72b65295b88e5534e458dfe3e6d7d9f9cb353448a569e" "d1b4990bd599f5e2186c3f75769a2c5334063e9e541e37514942c27975700370" "6d589ac0e52375d311afaa745205abb6ccb3b21f6ba037104d71111e7e76a3fc" "4697a2d4afca3f5ed4fdf5f715e36a6cac5c6154e105f3596b44a4874ae52c45" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "fe666e5ac37c2dfcf80074e88b9252c71a22b6f5d2f566df9a7aa4f9bea55ef8" "100e7c5956d7bb3fd0eebff57fde6de8f3b9fafa056a2519f169f85199cc1c96" "cd736a63aa586be066d5a1f0e51179239fe70e16a9f18991f6f5d99732cabb32" "b54826e5d9978d59f9e0a169bbd4739dd927eead3ef65f56786621b53c031a7c" default)))
 '(fci-rule-color "#383a42")
 '(jdee-db-active-breakpoint-face-colors (cons "#f0f0f0" "#4078f2"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#f0f0f0" "#50a14f"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#f0f0f0" "#9ca0a4"))
 '(nyan-mode t)
 '(org-agenda-files
   (quote
    ("~/hanjae1122.github.io/org/_posts/2018-12-26-c++-notes.org" "~/org/tasks.org")))
 '(package-selected-packages
   (quote
    (easy-kill yaml-mode fireplace command-log-mode ess smartparens-config org-pdfview swiper company-ghc counsel hindent beacon smartparens dashboard smex doom-themes popup-kill-ring browse-kill-ring crux dimmer undo-tree linum-relative rainbow-delimiters nyan-mode haskell-mode eimp pdf-tools magit projectile flycheck elpy exec-path-from-shell ace-window use-package)))
 '(projectile-mode t nil (projectile))
 '(vc-annotate-background "#fafafa")
 '(vc-annotate-color-map
   (list
    (cons 20 "#50a14f")
    (cons 40 "#688e35")
    (cons 60 "#807b1b")
    (cons 80 "#986801")
    (cons 100 "#ae7118")
    (cons 120 "#c37b30")
    (cons 140 "#da8548")
    (cons 160 "#c86566")
    (cons 180 "#b74585")
    (cons 200 "#a626a4")
    (cons 220 "#ba3685")
    (cons 240 "#cf4667")
    (cons 260 "#e45649")
    (cons 280 "#d2685f")
    (cons 300 "#c07b76")
    (cons 320 "#ae8d8d")
    (cons 340 "#383a42")
    (cons 360 "#383a42")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
