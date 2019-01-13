;;; init.el  -*- lexical-binding: t; -*-

(setq custom-file "~/.emacs.d/emacs-custom.el")
(setq gc-cons-threshold 100000000)

;; (setq url-proxy-services '(("http" . "127.0.0.1:8087")))

(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(setq package-enable-at-startup nil)
(package-initialize)
(require 'use-package)

(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-unix)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(setenv "PATH" (concat (getenv "PATH") ":~/Apps/bin:~/.local/bin:~/node_modules/.bin:~/.cargo/bin"))
(push "~/Apps/bin" exec-path)
(push "~/.local/bin" exec-path)
(push "~/node_modules/.bin" exec-path)
(push "~/.cargo/bin" exec-path)

(add-to-list 'load-path "~/.emacs.d/site-lisp/")
(load-file "~/.emacs.d/emacs-custom.el")
(load-file "~/.emacs.d/emacs-func.el")

;; (setq url-gateway-method 'socks)
;; (setq socks-server '("Default server" "127.0.0.1" 1080 5))

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
;; (display-battery-mode 1)
(blink-cursor-mode 0)
(setq-default display-line-numbers t)

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  :diminish undo-tree-mode)

(use-package projectile
  :config
  (progn
    (projectile-global-mode 1)
    (setq projectile-completion-system 'default)
    (setq-default projectile-mode-line
                  '(:eval (concat "[" (projectile-project-name) "]"))))
  )

;; (semantic-mode 1)
(electric-pair-mode -1)
(auto-image-file-mode)

(setq user-full-name "Pu Xingyu")
(setq user-mail-address "pu.stshine@gmail.com")

(setq column-number-mode t)
(setq visible-bell t)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq mouse-yank-at-point t)
(setq kill-whole-line t)
;; (setq backup-directory-alist

(setq kill-ring-max 500)
(setq-default major-mode 'text-mode)
(setq-default indent-tabs-mode nil) ;; don't use tabs to indent
(setq-default tab-width 4) ;; but maintain correct appearance
;; Newline at end of file
(setq save-interprogram-paste-before-kill t)
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
(use-package autorevert
  :config
  (global-auto-revert-mode 1)
  :diminish auto-revert-mode)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

(setq display-time-format "%T")
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

(use-package company
  :config
  (progn
    (setq company-idle-delay 0.2)
    (setq company-tooltip-limit 10)
    (setq company-minimum-prefix-length 2)
    ;; invert the navigation direction if the the completion popup-isearch-match
    ;; is displayed on top (happens near the bottom of windows)
    (setq company-tooltip-flip-when-above t)
    (setq company-dabbrev-downcase nil)
    (setq company-dabbrev-ignore-case nil)
    (global-company-mode)))

(require 'clojure-mode)
;; (--each sp-paredit-bindings
;;   (define-key lisp-mode-shared-map (read-kbd-macro (car it)) (cdr it))
;;   (define-key clojure-mode-map (read-kbd-macro (car it)) (cdr it)))
;; (dolist (sp--lisp-mode-hook
;;          (mapcar (lambda (x)
;;                    (intern (concat (symbol-name x) "-hook")))
;;                  sp--lisp-modes))
;;   (add-hook sp--lisp-mode-hook
;;             (lambda ()
;;               (smartparens-strict-mode 1)
;;               (paren-face-mode 1))))


;;(require 'geiser)
;;(setq geiser-active-implementations '(guile))


(use-package recentf
  :config
  (progn
    (setq recentf-save-file "~/.emacs.d/.recentf")
    (setq recentf-max-saved-items 1000)
    (setq recentf-max-menu-items 15)
    ;; disable recentf-cleanup on Emacs start, because it can cause
    ;; problems with remote files
    (setq recentf-auto-cleanup 300)
    (add-to-list 'recentf-exclude (expand-file-name package-user-dir))
    (recentf-mode 1)))


(require 'flycheck)

(if (equal window-system 'x)
    (pdf-tools-install))

(use-package paren
  :config
  (progn
    (setq show-paren-delay 0.0)
    (show-paren-mode 1)))

(use-package paren-face
  :config
  (progn
    (add-to-list 'paren-face-modes 'racket-mode)
    (set-face-foreground 'parenthesis "LightGrey")
    (global-paren-face-mode 1)))


;; (use-package smartparens
;;   :init
;;   (progn
;;     (use-package smartparens-config))
;;   :config
;;   (progn
;;     (smartparens-global-mode 1)
;;     (show-smartparens-global-mode)))


(use-package magit
  :config
  (progn
    (setq magit-repository-directories "~/Programs/")
    ;; (setq magit-completing-read-function 'helm-completing-read-with-cands-in-buffer)
    )
  :bind
  (("<f8>" . magit-dispatch-popup)
   ("C-<f8>" . magit-status)))


(company-auctex-init)

(ace-link-setup-default)

(require 'imenu-list)
(setq imenu-list-position 'left)

;; (popwin-mode t)

(use-package helm
  :defer t
  ;; :ensure helm
  ;; :commands (helm-find-files)
  :init
  (progn
    (use-package helm-config)
    (setq helm-prevent-escaping-from-minibuffer t
          helm-bookmark-show-location t
          helm-display-header-line nil
          helm-always-two-windows t
          helm-echo-input-in-header-line t
          helm-imenu-execute-action-at-once-if-one nil
          helm-ff-auto-update-initial-value t
          helm-display-header-line              nil
          helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
          helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
          helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
          helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
          helm-ff-file-name-history-use-recentf t)
    
    (when (executable-find "curl")
      (setq helm-google-suggest-use-curl-p t))
    
    ;; hide minibuffer in Helm session, since we use the header line already
    (defun helm-hide-minibuffer-maybe ()
      (when (with-helm-buffer helm-echo-input-in-header-line)
	(let ((ov (make-overlay (point-min) (point-max) nil nil t)))
	  (overlay-put ov 'window (selected-window))
	  (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
				  `(:background ,bg-color :foreground ,bg-color)))
	  (setq-local cursor-type nil))))
    (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)
    
    ;; fuzzy matching setting
    (setq helm-M-x-fuzzy-match t
	  helm-apropos-fuzzy-match t
	  helm-file-cache-fuzzy-match t
	  helm-imenu-fuzzy-match t
	  helm-lisp-fuzzy-completion t
	  helm-recentf-fuzzy-match t
	  helm-semantic-fuzzy-match t
	  helm-buffers-fuzzy-matching t))

  :config
  (progn
    (defvar helm-source-header-default-background (face-attribute 'helm-source-header :background))
    (defvar helm-source-header-default-foreground (face-attribute 'helm-source-header :foreground))
    (defvar helm-source-header-default-box (face-attribute 'helm-source-header :box))
    (defvar helm-source-header-default-height (face-attribute 'helm-source-header :height))

    (defun helm-toggle-header-line ()
      "Hide the `helm' header is there is only one source."
      (if (> (length helm-sources) 1)
	  (set-face-attribute 'helm-source-header
			      nil
			      :foreground helm-source-header-default-foreground
			      :background helm-source-header-default-background
			      :box helm-source-header-default-box
			      :height helm-source-header-default-height)
	(set-face-attribute 'helm-source-header
			    nil
			    :foreground (face-attribute 'helm-selection :background)
			    :background (face-attribute 'helm-selection :background)
			    :box nil
			    :height 0.1)))
    (add-hook 'helm-before-initialize-hook 'helm-toggle-header-line)
    (helm-mode 1)
    (helm-adaptive-mode 1)
    (helm-locate-set-command)
    (setq helm-locate-fuzzy-match (string-match "locate" helm-locate-command))

    (defun spacemacs//set-dotted-directory ()
      "Set the face of diretories for `.' and `..'"
      (set-face-attribute 'helm-ff-dotted-directory
			  nil
			  :foreground nil
			  :background nil
			  :inherit 'helm-ff-directory))
    (add-hook 'helm-find-files-before-init-hook 'spacemacs//set-dotted-directory)
    
    ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
    ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
    ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
    (global-set-key (kbd "C-c h") 'helm-command-prefix)
    (global-unset-key (kbd "C-x c"))

    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
    (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
    (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

    (define-key helm-command-map (kbd "o") 'helm-swoop)
    (define-key helm-command-map (kbd "g") 'helm-do-ag))
  :bind (("M-x" . 'helm-M-x)
         ("M-y" . 'helm-show-kill-ring)
         ("C-x b" . 'helm-mini)
         ("C-x C-b" . 'helm-buffer-list)
         ("C-x C-f" . 'helm-find-files))
  :diminish helm-mode)



;; (define-key helm-grep-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
;; (define-key helm-grep-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
;; (define-key helm-grep-mode-map (kbd "p")  'helm-grep-mode-jump-other-window-backward)


;; (set-face-attribute 'helm-source-header nil :height 0.1)

;;(setq helm-autoresize-max-height 50)
;;(helm-autoresize-mode 1)
;; (add-to-list 'helm-boring-file-regexp-list "\\.\\{1,2\\}\\'")
;; (setq helm-ff-skip-boring-files t)


;;; intergrate projectile with helm.
(use-package helm-projectile
  :commands (helm-projectile-switch-to-buffer
	     helm-projectile-find-dir
	     helm-projectile-dired-find-dir
	     helm-projectile-recentf
	     helm-projectile-find-file
	     helm-projectile-grep
	     helm-projectile
	     helm-projectile-switch-project)
  :init
  (progn
    (setq projectile-switch-project-action 'helm-projectile)))
;; (setq projectile-completion-system 'helm)
;; (helm-projectile-on)

;;; semantic config
;; (semantic-add-system-include "/usr/include/" 'c-mode)

(use-package which-key
  :config
  (progn
    (global-set-key (kbd "C-h C-k") 'which-key-show-top-level)
    (which-key-mode 1))
  :diminish which-key-mode)

;;(require 'pager)

(setq org-agenda-files '("~/org/"))

(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)
;; (global-set-key (kbd "M-/") 'hippie-expand)



(setq projectile-switch-project-action 'neotree-projectile-action)
(setq neo-theme 'arrow)
(global-set-key [f9] 'neotree-toggle)

;; (use-package youdao-dictionary
;;   :config
;;   (global-set-key (kbd "C-c d") 'youdao-dictionary-search-at-point+))

(global-set-key (kbd "M-p") 'pager-row-up)
(global-set-key (kbd "M-n") 'pager-row-down)


(global-set-key (kbd "C-z") 'avy-goto-char-timer)
(global-set-key (kbd "C-c z") 'avy-pop-mark)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

(global-set-key (kbd "C-=") 'er/expand-region)

;; ------------------- Mouse key bindings ---------------
(global-set-key (kbd "<mouse-3>") 'mouse-major-mode-menu)
(global-set-key (kbd "<S-down-mouse-1>") 'mouse-save-then-kill)
(global-set-key (kbd "<C-down-mouse-3>") 'mouse-appearance-menu)

(global-set-key (kbd "<C-tab>") 'ace-window)


;; ------------------- Language modes -------------------
(add-to-list 'auto-mode-alist '("\\.rkt$" . racket-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))


;;; --------------------- Mail Settings -------------------
(setq mu4e-maildir "~/Mail/"
      mu4e-sent-folder   "/Sent Messages"
      mu4e-drafts-folder "/Drafts"
      mu4e-trash-folder  "/Deleted Messages")

(setq mu4e-reply-to-address "hines@augustint.com"
      user-mail-address "hines@augustint.com"
      user-full-name  "Hines - August Intl")

(setq mu4e-get-mail-command "mbsync qqmail")
;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)
;; use 'fancy' non-ascii characters in various places in mu4e
(setq mu4e-use-fancy-chars t)
;; save attachment to my desktop (this can also be a function)
(setq mu4e-attachment-dir "~/Downloads/")
;; attempt to show images when viewing messages
(setq mu4e-view-show-images t)


;;; -------------------- Gnus Settings --------------------
;; (setq gnus-select-method '(nntp "news.gmane.org"))
(setq gnus-secondary-select-methods
      '(
        (nntp "gmane"
              (nntp-address "news.gmane.org"))
        (pop :server "mail.augustint.com"
             :user "hines" :password "aug608609"
             :leave t)
        ))

(setq gnus-asynchronous t)

(setq-default
  gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f  %B%s%)\n"
  gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
  gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
  gnus-sum-thread-tree-false-root ""
  gnus-sum-thread-tree-indent ""
  gnus-sum-thread-tree-leaf-with-other "-> "
  gnus-sum-thread-tree-root ""
  gnus-sum-thread-tree-single-leaf "|_ "
  gnus-sum-thread-tree-vertical "|")

(setq gnus-thread-sort-functions
      '(
        (not gnus-thread-sort-by-date)
        (not gnus-thread-sort-by-number)
        ))


;; sending mail

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-smtp-server "mail.augustint.com"      
      smtpmail-smtp-service 587)

;;; --------------------- EWW Browser ---------------------
(setq shr-use-fonts nil)


;;; ----------------------- Erc Mode ----------------------
(use-package erc
  :defer t
  :init
  ;; utf-8 always and forever
  (setq erc-server-coding-system '(utf-8 . utf-8))
  :config
  (progn
    ;; Interpret mIRC-style color commands in IRC chats
    (setq erc-interpret-mirc-color t)
    ;; The following are commented out by default, but users of other
    ;; non-Emacs IRC clients might find them useful.
    ;; Kill buffers for channels after /part
    (setq erc-kill-buffer-on-part t)
    ;; Kill buffers for private queries after quitting the server
    (setq erc-kill-queries-on-quit t)
    ;; Kill buffers for server messages after quitting the server
    (setq erc-kill-server-buffer-on-quit t)
    ;; open query buffers in the current window
    (setq erc-query-display 'buffer)
    ;; inhibit showup of join and quit messages
    (setq erc-hide-list '("JOIN" "QUIT" "PART"))
    ;; exclude boring stuff from tracking
    (erc-track-mode t)
    (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                    "324" "329" "332" "333" "353" "477"))
    (setq erc-save-buffer-on-part t)
    ;; FIXME - this advice is wrong and is causing problems on Emacs exit
    ;; (defadvice save-buffers-kill-emacs (before save-logs (arg) activate)
    ;; (save-some-buffers t (lambda () (when (eq major-mode 'erc-mode) t))))
    ;; truncate long irc buffers
    (erc-truncate-mode +1)
    ;; autoaway setup
    (use-package erc-autoaway
      :config
      (setq erc-auto-discard-away t)
      (setq erc-autoaway-idle-seconds 600)
      (setq erc-autoaway-use-emacs-idle t))))



;;; ----------------------- Eshell Mode ----------------------
(use-package eshell
  :defer t
  :init
  (progn
    (setq ;; auto truncate after 20k lines
          eshell-buffer-maximum-lines 20000
          ;; history size
          eshell-history-size 500
          ;; no duplicates in history
          eshell-hist-ignoredups t
          ;; buffer shorthand -> echo foo > #'buffer
          eshell-buffer-shorthand t
          ;; my prompt is easy enough to see
          eshell-highlight-prompt nil
          ;; treat 'echo' like shell echo
          eshell-plain-echo-behavior t))
  :config
  ;; use helm to list eshell history
  (add-hook 'eshell-mode-hook
            #'(lambda ()
                (substitute-key-definition 'eshell-list-history 'helm-eshell-history eshell-mode-map)
                (substitute-key-definition 'eshell-pcomplete 'helm-esh-pcomplete eshell-mode-map))))


;;; ----------------------- Web Mode ----------------------
(use-package web-mode
  :defer t
  :init
  (use-package company-web)
  ;; (push '(company-web-html company-css) company-backend)
  :config
  :mode
  (("\\.phtml\\'"      . web-mode)
   ("\\.html$"         . web-mode)
   ("\\.htm$"         . web-mode)
   ("\\.blade\\.php$"  . web-mode)))


;; JSX highlight
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))


(add-hook 'web-mode-hook
          (lambda ()
            (when (equal web-mode-content-type "jsx")
              ;; enable flycheck
              (flycheck-select-checker 'javascript-eslint))
            (emmet-mode 1)
            (flycheck-mode 1)))


;;; ----------------------- JavaScript ----------------------
(use-package js2-mode
  :config
  (setq js2-include-node-externs t)
  :hook
  (js2-mode . (lambda ()
                (js2-imenu-extras-mode 1)
                ;; (flycheck-mode 1)
                (tern-mode t)
                (add-to-list 'company-backends 'company-tern)))
  :mode
  (("\\.js\\'"         . js2-mode)
   ("\\.jsx\\'"        . js2-jsx-mode)))


;;; ----------------------- Python ----------------------
(elpy-enable)
(setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
(add-hook 'elpy-mode-hook 'flycheck-mode)
(setq flycheck-flake8-maximum-line-length 120)
(elpy-use-ipython)
(setq python-shell-interpreter "ipython3")


;;; ------------------------ Go --------------------------
(require 'go-mode)
(require 'company-go)
(add-hook 'go-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends) '(company-go))
            (go-eldoc-setup)
            (subword-mode t)
            (flycheck-mode 1)))


;;; ------------------------ Rust --------------------------
(setq racer-rust-src-path "/home/stshine/Programs/rust/src/")
(setq racer-cmd "/home/stshine/Programs/racer/target/release/racer")
;; (setq company-racer-executable "/home/stshine/Programs/racer/target/release/racer")
;; (unless (getenv "RUST_SRC_PATH")
  ;; (setenv "RUST_SRC_PATH" "/home/stshine/Programs/rust/src/"))
(add-hook 'rust-mode-hook
          (lambda ()
            (racer-mode 1)
            (eldoc-mode 1)))
           ;; (set (make-local-variable 'company-backends) '(company-racer))
            ;; (add-to-list 'company-backends 'company-racer)))
;; (add-to-list 'load-path "/home/stshine/Programs/racer/editors")



;;; ------------------------ PHP --------------------------
;; (require 'php-extras)
(setq php-mode-coding-style 'symfony2)
(add-hook 'php-mode-hook
          (lambda ()
            (flycheck-mode 1)
            (php-enable-symfony2-coding-style)
            (c-set-style "symfony2") ;; bug workaround
            ))


;;; ------------------------Org Mode--------------------------
(require 'org)
(define-key org-mode-map (kbd "C-c [") nil)
(define-key org-mode-map (kbd "C-c ]") nil)
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")))
(setq org-use-fast-todo-selection t)
(setq org-src-fontify-natively t)
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
            ()))


;; ------------------------ Elisp  --------------------------
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (paredit-mode 1)
            ))


;; ------------------------ Scheme --------------------------
;; (setq scheme-program-name "racket")


;; (defvar *binding-constructs*
;;   '(let-values
;;     let*-values
;;     hash-for-each
;;     letv
;;     let\:
;;     lambda\:
;;     letv*
;;     match
;;     pmatch
;;     for
;;     for/list
;;     fun
;;     record))

;; (add-hook 'scheme-mode-hook
;;           (lambda ()
;;             ;; (paredit-mode 1)
;;             ;; set proper indentation for non-standard binding constructs
;;             (mapc (lambda (x) (put x 'scheme-indent-function 1)) *binding-constructs*)))


;; ------------------------ Racket --------------------------
(add-hook 'racket-mode-hook
          (lambda ()
            ;; (paredit-mode 1)
            ))



;; ------------------------ Clojure --------------------------
(require 'cider)
(add-hook 'cider-mode-hook #'eldoc-mode)


;; ------------------------ Haskell --------------------------
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
;; (add-hook 'haskell-mode-hook 'stack-mode)
;; (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'haskell-doc-mode)

;; -------------------- Chinese Setup -------------------------------
(use-package pyim
  :init
  ;; (setq pyim-dicts
  ;;       '((:name "default-dict"
  ;;                :file "~/Documents/pyim-bigdict.pyim"
  ;;                :coding utf-8-unix)))
  :config
  (progn
    (use-package pyim-basedict)
    (pyim-basedict-enable)
    (setq default-input-method 'pyim))
  )


(setq default-frame-alist
      '((width . 103)
        (height . 39)
        (left . 200)
        (top . 0)))


(pcase system-type
  ('windows-nt (set-frame-font "Consolas-13"))
  ('gnu/linux (set-frame-font "-adobe-Source Code Pro-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1")))

;; (set-frame-font "-microsoft-Consolas-normal-normal-normal-*-17-*-*-*-m-0-iso10646-1")
;; (set-frame-font "Source Code Pro-12")

(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
                    charset "方正黑体_GBK"
                    ;; charset "Noto Sans CJK SC"
                    ))

(set-fontset-font (frame-parameter nil 'font)
		  'mathematical "STIX")

(set-fontset-font (frame-parameter nil 'font)
		  'symbol "STIX")

(require 'moe-theme)
(load-theme 'spacemacs-dark)

;; (sml/setup)
(require 'spaceline-config)
;; (setq powerline-default-separator 'wave)
;; (setq spaceline-battery-p t)
;; (setq spaceline-buffer-position-p nil)
(spaceline-spacemacs-theme)

;; (add-hook 'buffer-list-update-hook 'neotree-projectile-action)
