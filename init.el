(setq custom-file "~/.emacs.d/emacs-custom.el")

(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ;; ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(setq package-enable-at-startup nil)
(package-initialize)

(prefer-coding-system 'utf-8)

(setenv "PATH" (concat (getenv "PATH") ":~/Apps/bin"))
(push "~/Apps/bin" exec-path)

(load-file "~/.emacs.d/emacs-custom.el")
(load-file "~/.emacs.d/emacs-func.el")


;;(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(blink-cursor-mode 0)
(global-linum-mode)
(global-undo-tree-mode)

(global-company-mode)
(projectile-global-mode 1)
(show-paren-mode 1)
;; (semantic-mode 1)
(electric-pair-mode 1)
(auto-image-file-mode)

(setq user-full-name "Pu Xingyu")
(setq user-mail-address "pu.stshine@email.com")

(setq column-number-mode t)
(setq visible-bell t)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq mouse-yank-at-point t)
(setq kill-whole-line t)
;; (setq backup-directory-alist

(setq kill-ring-max 500)
(setq default-major-mode 'text-mode)
(setq-default indent-tabs-mode nil) ;; don't use tabs to indent
(setq-default tab-width 4) ;; but maintain correct appearance
;; Newline at end of file
(setq require-final-newline t)
;; delete the selection with a keypress
(delete-selection-mode t)
(setq frame-title-format "%b - emacs")
;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
`((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
`((".*" ,temporary-file-directory t)))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

(setq display-time-format "%m-%e %T")
(setq display-time-interval 1)
(display-time)

(setq bookmark-save-flag 1)
(setq bookmark-default-file "~/.emacs.d/bookmarks")

(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.emacs.d/backup"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 3
   kept-old-versions 2
   version-control t)       ; use versioned backups

(fset 'yes-or-no-p 'y-or-n-p)


(setq company-idle-delay 0.5)
(setq company-tooltip-limit 10)
(setq company-minimum-prefix-length 2)
;; invert the navigation direction if the the completion popup-isearch-match
;; is displayed on top (happens near the bottom of windows)
(setq company-tooltip-flip-when-above t)
(setq company-dabbrev-downcase nil)
(setq company-dabbrev-ignore-case nil)
(global-company-mode)


(require 'cmuscheme)
(require 'geiser)
;;(setq geiser-active-implementations '(guile))

(require 'recentf)
(setq recentf-save-file "~/.emacs.d/.recentf"
      recentf-max-saved-items 500
      recentf-max-menu-items 15
      ;; disable recentf-cleanup on Emacs start, because it can cause
      ;; problems with remote files
      recentf-auto-cleanup 'never)
(recentf-mode 1)

(require 'undo-tree)
(require 'guide-key)

;;(require 'parenface)
;;(set-face-foreground 'paren-face "DimGray")

(require 'helm)
(require 'helm-config)
(require 'helm-eshell)
(require 'helm-files)
(require 'helm-grep)

(company-auctex-init)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

;; (define-key helm-grep-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
;; (define-key helm-grep-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
;; (define-key helm-grep-mode-map (kbd "p")  'helm-grep-mode-jump-other-window-backward)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(helm-mode 1)

;;; intergrate projectile with helm.
(require 'helm-projectile)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;;; semantic config
;; (semantic-add-system-include "/usr/include/" 'c-mode)

;;guide-key mode
(setq guide-key/guide-key-sequence '("C-c p" "C-c h"))
(setq guide-key/recursive-key-sequence-flag t)
(setq guide-key/popup-window-position 'bottom)
(guide-key-mode 1)

;;(require 'guide-key-tip)
;;(setq guide-key-tip/enabled t)

;;(require 'pager)

(setq org-agenda-files '("~/org/"))

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(global-set-key (kbd "M-/") 'hippie-expand)

(global-set-key (kbd "M-p") 'pager-row-up)
(global-set-key (kbd "M-n") 'pager-row-down)

(global-set-key (kbd "C-c j") 'ace-jump-mode)
(global-set-key (kbd "C-c J") 'ace-jump-mode-pop-mark)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

;; ------------------- language modes -------------------
(add-to-list 'auto-mode-alist '("\\.rkt$" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;;; ----------------------- Python ----------------------
(elpy-enable)
(setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
(add-hook 'elpy-mode-hook 'flycheck-mode)
(setq flycheck-flake8-maximum-line-length 120)
(elpy-use-ipython)
(setq python-shell-interpreter "ipython3")



;;; ------------------------Org Mode--------------------------
(define-key org-mode-map (kbd "C-c [") nil)
(define-key org-mode-map (kbd "C-c ]") nil)
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")))
(setq org-use-fast-todo-selection t)
(setq org-todo-state-tags-triggers
      (quote (
              ;;("CANCELLED" ("CANCELLED" . t))
              ;;("WAITING" ("WAITING" . t))
              ;;("HOLD" ("WAITING") ("HOLD" . t))
              ;;(done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))
(setq org-clock-in-switch-to-state 'bh/clock-in-to-next)

(add-hook 'org-mode-hook
          (lambda ()
            (guide-key/add-local-guide-key-sequence "C-c")
            (guide-key/add-local-guide-key-sequence "C-c C-x")
            (guide-key/add-local-highlight-command-regexp "org-")))


;; ------------------------ Elisp  --------------------------
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (paredit-mode )))

;; ------------------------ Scheme --------------------------
(setq scheme-program-name "guile")


(defvar *binding-constructs*
  '(let-values
    let*-values
    hash-for-each
    letv
    let\:
    lambda\:
    letv*
    match
    pmatch
    for
    for/list
    fun
    record))

(add-hook 'scheme-mode-hook
	  (lambda ()
	    (paredit-mode 1)
	    
	    ;; set proper indentation for non-standard binding constructs
	    (mapc (lambda (x) (put x 'scheme-indent-function 1)) *binding-constructs*)))

(setq default-frame-alist
      '((width . 100)
        (height . 41)
        (left . 200)
        (top . 0)))

(set-frame-font "Consolas-10")

(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
		    charset "方正黑体_GBK"))

(set-fontset-font (frame-parameter nil 'font)
		  'mathematical "STIX")

(set-fontset-font (frame-parameter nil 'font)
		  'symbol "STIX")

(require 'moe-theme)
(load-theme 'monokai)

