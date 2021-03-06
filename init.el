;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stop annoying custom-set-* showing up in this file

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; install packages locally if not installed

(require 'package)

;; package repositories
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

;; activate all packages
(package-initialize)

;; package list
(setq package-selected-packages
      '(zenburn-theme
        ess
        polymode
	poly-markdown
        fill-column-indicator
        sql-indent
        ejc-sql))

;; refresh & install if not
(defun install-packages ()
  "Install all required packages."
  (interactive)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package package-selected-packages)
    (unless (package-installed-p package)
      (package-install package))))

(install-packages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load & configure environment

;; load theme
(load-theme 'zenburn t)

;; interactively do things
;; (require 'ido)
;; (ido-mode t)

;; load ess
(require 'ess-site)

;; load org
(require 'org)
(load-library "find-lisp")
(setq org-agenda-files (find-lisp-find-files "~/org" "\.agenda\.org$"))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; active Babel languages for org
(org-babel-do-load-languages
 'org-babel-load-languages
 '((sql . t)))
;; add additional languages with '((language . t)))

;; faster than default scp
(setq tramp-default-method "ssh")

;; 80 column rule, dorks
(require 'fill-column-indicator)
(define-globalized-minor-mode
  global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode t)
(setq fci-rule-column 80)
;; (setq fci-rule-color "DarkSlateGray")
(setq fci-rule-use-dashes 1)

;; use sql-indent in sql-mode
(add-hook 'sql-mode-hook 'sqlind-minor-mode)

;; ;; if you're at work and this file exists, load it for Teradata connectivity
;; (when (file-exists-p "bby-teradata.el")
;;   (load-file "bby-teradata.el"))

;; remove trailing whitespace on save -- doesn't work for markdown mode!! :(
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set look & feel

(when (window-system)
;; fira code if available, just whatever if not
  (defun font-exists-p (font) "check if font exists"
         (if (null (x-list-fonts font)) nil t))
  (if (font-exists-p "Fira Code 12")
      (set-face-attribute 'default nil :font "Fira Code 12"))

  ;; ligatures for fira code -- this will be butt ugly until emacs actually
  ;; supports ligatures
  (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
                 (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
                 (36 . ".\\(?:>\\)")
                 (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
                 (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
                 (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
                 (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
                 (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
                 (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
                 (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
                 (48 . ".\\(?:x[a-zA-Z]\\)")
                 (58 . ".\\(?:::\\|[:=]\\)")
                 (59 . ".\\(?:;;\\|;\\)")
                 (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
                 (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
                 (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
                 (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
                 (91 . ".\\(?:]\\)")
                 (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
                 (94 . ".\\(?:=\\)")
                 (119 . ".\\(?:ww\\)")
                 (123 . ".\\(?:-\\)")
                 (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
                 (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
                 )
               ))
    (dolist (char-regexp alist)
      (set-char-table-range composition-function-table (car char-regexp)
                            `([,(cdr char-regexp) 0 font-shape-gstring]))))
  )

(setq inhibit-startup-message t)
;; (menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(show-paren-mode 1)

(setq linum-format "%d ")
(global-linum-mode 1)
(setq column-number-mode t)

(setq-default indent-tabs-mode nil)
(setq comment-style 'multi-line)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; major mode hooks

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; handle backups & temporary files

;; backup in one place. flat, no tree structure
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))

;; save all tempfiles in $TMPDIR/emacs$UID
(defconst emacs-tmp-dir
  (expand-file-name
   (format "emacs%d" (user-uid)) temporary-file-directory))
(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
      emacs-tmp-dir)
