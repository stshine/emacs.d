;;; init.el  -*- lexical-binding: t; -*-

(setq custom-file "~/.emacs.d/emacs-custom.el")
(setq gc-cons-threshold (* 100 1024 1024))
(setq read-process-output-max (* 1 1024 1024))

;; (setq url-proxy-services '(("http" . "127.0.0.1:8087")))

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "http://melpa.org/packages/")))
(setq package-enable-at-startup nil)
(custom-set-variables '(package-gnupghome-dir "~/.emacs.d/elpa/gnupg"))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(custom-set-variables '(use-package-always-ensure t))

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
(setq user-full-name "Pu Xingyu")
(setq user-mail-address "pu.stshine@gmail.com")

(menu-bar-mode 1)
(when (display-graphic-p)
  (tool-bar-mode 0)
  (scroll-bar-mode 0))

;; (display-battery-mode 1)
(blink-cursor-mode 0)
(setq-default display-line-numbers t)

(auto-image-file-mode 1)

(setq column-number-mode t)
(setq visible-bell t)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq mouse-yank-at-point t)
(setq kill-whole-line t)
(setq kill-ring-max 500)
(setq-default major-mode 'text-mode)
(setq-default indent-tabs-mode nil) ;; don't use tabs to indent
(setq-default tab-width 4) ;; but maintain correct appearance
(setq save-interprogram-paste-before-kill t)
;; Newline at end of file
(setq require-final-newline t)
;; delete the selection with a keypress
(delete-selection-mode t)
(setq frame-title-format "%b - emacs")

;; revert buffers automatically when underlying files are changed externally
(use-package autorevert
  :config
  (global-auto-revert-mode 1)
  :delight auto-revert-mode)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

(setq display-time-format "%T")
(setq display-time-interval 1)
(display-time-mode 1)

(setq bookmark-save-flag 1)
(setq bookmark-default-file "~/.emacs.d/bookmarks")

(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
   '(("." . "~/.emacs.d/backup"))    ; don't litter my fs tree
   auto-save-file-name-transforms
   `((".*" ,temporary-file-directory t))
   delete-old-versions t
   kept-new-versions 3
   kept-old-versions 2
   version-control t)       ; use versioned backups

(setq cache-dir (expand-file-name ".cache" user-emacs-directory))
(fset 'yes-or-no-p 'y-or-n-p)


(use-package recentf
  :custom
  (recentf-save-file "~/.emacs.d/.cache/recentf")
  (recentf-max-saved-items 1500)
  (recentf-max-menu-items 15)
  (recentf-auto-cleanup 600)
  :config
  (add-to-list 'recentf-exclude (expand-file-name package-user-dir))
  (recentf-mode 1))


(let ((session-dir "~/.emacs.d/.session/"))
    `(progn
       (make-directory ,session-dir t)
       (defun emacs-session-filename (session-id)
         "Construct a filename to save the session in based on SESSION-ID.
This function overrides the one on `x-win' to use `no-littering'
directories."
         (expand-file-name session-id ,session-dir))))


;; saveplace remembers your location in a file when saving files
(use-package saveplace
  :config
  (setq save-place-file (expand-file-name "saveplace" cache-dir))
  ;; activate it for all buffers
  (save-place-mode 1))


(use-package savehist
  :custom
  ;; search entries
  (savehist-additional-variables '(search-ring regexp-search-ring))
  ;; save every minute
  (savehist-autosave-interval 60)
  ;; keep the home clean
  (savehist-file (expand-file-name "savehist" cache-dir))
  :config
  (savehist-mode 1))


(use-package helm
  ;; :commands (helm-find-files)
  :ensure helm-swoop
  :init
  :custom
  (helm-command-prefix-key         "C-c h")
  (helm-prevent-escaping-from-minibuffer t)
  (helm-split-window-default-side   'above)
  ;; open helm buffer inside current window, not occupy whole other window
  (helm-split-window-inside-p            t)
  (helm-always-two-windows               t)
  (helm-echo-input-in-header-line        t)
  (helm-autoresize-max-height           50)
  (helm-display-header-line            nil)
  (helm-bookmark-show-location           t)
  ;; (helm-imenu-execute-action-at-once-if-one nil)
  (helm-ff-auto-update-initial-value     t)
  ;; move to end or beginning of source when reaching top or bottom of source.
  (helm-move-to-line-cycle-in-source     t)
  ;; search for library in `require' and `declare-function' sexp.
  (helm-ff-search-library-in-sexp        t)
  ;; scroll 8 lines other window using M-<next>/M-<prior>
  (helm-scroll-amount                    8)
  (helm-ff-file-name-history-use-recentf t)
  :custom-face
  ;; Set the face of diretories for `.' and `..'
  (helm-ff-dotted-directory ((t (:foreground nil :background nil :inherit 'helm-ff-directory))))
  :config
  (helm-autoresize-mode t)
  (when (executable-find "curl")
    (setq helm-net-prefer-curl t))
  ;; hide minibuffer in Helm session, since we use the header line already
  (add-hook 'helm-minibuffer-set-up-hook #'helm-hide-minibuffer-maybe)

  ;; fuzzy matching setting
  (setq helm-M-x-fuzzy-match t
  ;;       helm-apropos-fuzzy-match t
  ;;       helm-file-cache-fuzzy-match t
  ;;       helm-imenu-fuzzy-match t
  ;;       helm-lisp-fuzzy-completion t
  ;;       helm-recentf-fuzzy-match t
  ;;       helm-semantic-fuzzy-match t
        helm-buffers-fuzzy-matching t)

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
  ;; (helm-locate-set-command)
  ;; (setq helm-locate-fuzzy-match (string-match "locate" helm-locate-command))


  ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
  ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
  ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
  :bind (("M-x"       . 'helm-M-x)
         ("M-y"       . 'helm-show-kill-ring)
         ("C-x b"     . 'helm-mini)
         ("C-x C-b"   . 'helm-buffer-list)
         ("C-x C-f"   . 'helm-find-files)
         :map helm-map
         ("<tab>"     . 'helm-execute-persistent-action)
         ("C-i"       . 'helm-execute-persistent-action)
         ("C-z"       . 'helm-select-action)
         ("C-c C-l"   . 'helm-minibuffer-history)
         :map helm-command-map
         ("o"         . 'helm-swoop)
         ("g"         . 'helm-do-ag))
  :delight helm-mode)

;; (define-key helm-grep-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
;; (define-key helm-grep-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
;; (define-key helm-grep-mode-map (kbd "p")  'helm-grep-mode-jump-other-window-backward)


;; (set-face-attribute 'helm-source-header nil :height 0.1)

;; (add-to-list 'helm-boring-file-regexp-list "\\.\\{1,2\\}\\'")
;; (setq helm-ff-skip-boring-files t)

;; (use-package counsel
;;   :config
;;   (setq ivy-use-virtual-buffers t)
;;   (setq enable-recursive-minibuffers t)
;;   (ivy-mode 1)
;;   :bind
;;   (("\C-s" . swiper)
;;    ("M-x" . counsel-M-x)
;;    ("C-x C-f" . counsel-find-file)
;;    ("C-x b" . ivy-switch-buffer)))


(use-package undo-tree
  :config
  (global-undo-tree-mode)
  :delight undo-tree-mode)


(use-package mwim
  :bind
  (("C-a" . mwim-beginning-of-code-or-line)
   ("C-e" . mwim-end-of-code-or-line)))


;;; intergrate projectile with helm.
(use-package helm-projectile
  :custom
  (projectile-cache-file "~/.emacs.d/.cache/projectile.cache")
  :config
  (setq projectile-mode-line-function
        (lambda ()
          (concat "[" (projectile-project-name) "]")))
  ;; (setq projectile-completion-system 'helm)
  (projectile-mode 1)
  (helm-projectile-on)
  :bind-keymap
  ("C-c p" . projectile-command-map))


(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query))


(use-package paren-face
  :config
  (add-to-list 'paren-face-modes 'racket-mode)
  (set-face-foreground 'parenthesis "DimGrey")
  (global-paren-face-mode 1))


(use-package smartparens-config
  :after paren-face
  :ensure smartparens
  :config
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1)
  :hook
  (smartparens-mode . (lambda ()
                        (when (member major-mode sp-lisp-modes)
                          (smartparens-strict-mode 1)
                          (sp-use-paredit-bindings))))
  :delight smartparens-mode)
  ;; (-each sp-lisp-modes (lambda (mode)
                         ;; (set-face-foreground 'sp-pair-overlay-face "DimGrey")))


(use-package magit
  :config
  (setq magit-repository-directories "~/Programs/")
  ;; (setq magit-completing-read-function 'helm-completing-read-with-cands-in-buffer)
  :bind
  (("<f8>" . magit-dispatch)
   ("C-<f8>" . magit-status)))

(use-package company-auctex
  :config
  (company-auctex-init))

(use-package ace-link
  :custom
  (avy-timeout-seconds 0.3)
  :config
  (ace-link-setup-default)
  :bind
  (("C-z" . avy-goto-char-timer)
   ("C-c z" . avy-pop-mark)))

(use-package imenu-list
  :config
  (setq imenu-list-position 'left))

;; (popwin-mode t)


(use-package which-key
  :config
  (which-key-mode 1)
  :bind
  (("C-h C-k" . 'which-key-show-top-level))
  :delight which-key-mode)

;;(require 'pager)

;; (global-set-key (kbd "M-/") 'hippie-expand)


;; (use-package youdao-dictionary
;;   :config
;;   (global-set-key (kbd "C-c d") 'youdao-dictionary-search-at-point+))

;; (global-set-key (kbd "<return>") 'newline-and-indent)

(global-set-key (kbd "M-p") 'pager-row-up)
(global-set-key (kbd "M-n") 'pager-row-down)

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

;; ------------------- Mouse key bindings ---------------
(global-set-key (kbd "<mouse-3>") 'mouse-major-mode-menu)
(global-set-key (kbd "<S-down-mouse-1>") 'mouse-save-then-kill)
(global-set-key (kbd "<C-down-mouse-3>") 'mouse-appearance-menu)

;; (global-set-key (kbd "<C-tab>") 'ace-window)


;; (evil-mode 1)

;;; --------------------- Mail Settings -------------------
(setq mu4e-maildir "~/Mail/"
      mu4e-sent-folder   "/Sent Messages"
      mu4e-drafts-folder "/Drafts"
      mu4e-trash-folder  "/Deleted Messages")

(setq mu4e-reply-to-address "hines@augustint.com")

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
    (setq erc-autoaway-use-emacs-idle t)))


;;; ----------------------- Eshell Mode ----------------------
;; Eshell has some special loading mechanisms, so we need to use
;; this special hooks to configure it.
(defun configure-eshell ()
  (setq
   ;; auto truncate after 20k lines
   eshell-buffer-maximum-lines 20000
   ;; history size
   eshell-history-size 5000
   ;; no duplicates in history
   eshell-hist-ignoredups t
   ;; my prompt is easy enough to see
   eshell-highlight-prompt nil
   ;; treat 'echo' like shell echo
   eshell-plain-echo-behavior t)
  ;; cannot simply done use use-package :bind.
  (bind-key [remap eshell-list-history] #'helm-eshell-history eshell-command-map)
  (bind-key [remap eshell-pcomplete] #'helm-esh-pcomplete eshell-command-map))

(use-package eshell
  :defer t
  :config
  (delq 'eshell-banner eshell-modules-list)
  :hook
  (eshell-first-time-mode . configure-eshell)
  :bind
  (("C-`" . #'eshell)))

;; (diminish 'eldoc-mode)


(use-package company
  :ensure company-box
  :custom
  (company-idle-delay 0.3)
  (company-tooltip-limit 10)
  (company-minimum-prefix-length 2)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (company-tooltip-flip-when-above t)
  (company-tooltip-align-annotations t)
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case nil)
  :hook
  (company-mode . company-box-mode)
  (emacs-lisp-mode . company-mode)
  :delight (company-mode) (company-box-mode))


(use-package flycheck
  :delight)


(use-package lsp-mode
  :requires (flycheck company)
  :commands lsp
  ;; :ensure lsp-ui
  :custom
  (lsp-session-file "~/.emacs.d/.session/.lsp-session-v1")
  :hook (lsp-mode . (lambda ()
                      (company-mode 1)
                      (flycheck-mode 1)
                      (lsp-enable-which-key-integration)))
  :bind
  (:map company-active-map
        ("<tab>" . company-complete-selection)
        :map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :delight lsp-mode)

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-position 'bottom)
  :bind
  (:map lsp-ui-mode-map
        ([remap xref-find-references] . #'lsp-ui-peek-find-references)))

(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; optionally if you want to use debugger
;; (use-package dap-mode)


(use-package lsp-treemacs
  :config
  (lsp-treemacs-sync-mode 1)
  :hook
  (treemacs-mode . (lambda ()
                     (display-line-numbers-mode 0)))
  :bind
  ([f12] . treemacs))



;;; ----------------------- Web Mode ----------------------
(use-package web-mode
  :defer t
  :ensure emmet-mode
  :init
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 4)
  (web-mode-enable-current-element-highlight t)
  :config
  (setq web-mode-engines-alist '(("jinja2" . "\\.tera$")))
  :mode
  (("\\.html?$"        . web-mode)
   ("\\.phtml\\'"      . web-mode)
   ("\\.blade\\.php$"  . web-mode)
   ("\\.tera$"         . web-mode))
  :hook
  (web-mode . (lambda ()
                (when (equal web-mode-content-type "jsx")
                  ;; enable flycheck
                  (flycheck-select-checker 'javascript-eslint))
                (emmet-mode 1)
                (lsp)))
  :delight emmet-mode)


;; JSX highlight
;; (defadvice web-mode-highlight-part (around tweak-jsx activate)
;;   (if (equal web-mode-content-type "jsx")
;;       (let ((web-mode-enable-part-face nil))
;;         ad-do-it)
;;     ad-do-it))


;;; ----------------------- JavaScript ----------------------
(use-package js2-mode
  :config
  (setq js2-include-node-externs t)
  :hook
  (js2-mode . (lambda ()
                (js2-imenu-extras-mode 1)
                ;; (flycheck-mode 1)
                ))
  :mode
  (("\\.js\\'"         . js2-mode)
   ("\\.jsx\\'"        . js2-jsx-mode)))


;;; ----------------------- Python ----------------------
;; (elpy-enable)
;; (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;; (add-hook 'elpy-mode-hook 'flycheck-mode)
;; (setq flycheck-flake8-maximum-line-length 120)
;; (setq elpy-rpc-python-command "python3")
;;(elpy-use-ipython)
;;(setq python-shell-interpreter "ipython3")
(use-package python
  :ensure lsp-pyright
  :custom
  (python-indent-guess-indent-offset-verbose nil)
  :hook
  (python-mode . (lambda ()
                   (require 'lsp-pyright)
                   (pyvenv-activate (lsp-pyright-locate-venv))
                   (lsp))))


;;; ------------------------ Go --------------------------
(use-package go-mode
  :hook
  (go-mode . (lambda ()
               (go-eldoc-setup)
               (subword-mode t)
               (flycheck-mode 1))))


;;; ------------------------ Rust --------------------------
(use-package rustic
  :hook
  (rust-mode . (lambda ()
                 ;; (setq rust-format-on-save t)
                 (subword-mode 1)
                 )))


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
(use-package org
  :ensure helm-org
  :custom
  (org-directory "~/OneDrive/org/")
  (org-agenda-files `(,org-directory))
  (org-default-notes-file (expand-file-name "tasks.org" org-directory))
  (org-agenda-diary-file (expand-file-name "Journal.org" org-directory))
  ;; Archive to a single file of datetree
  (org-archive-location "~/OneDrive/org/Archive.org::datetree/* %s")
  (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)" "CANCELLED(c@/!)")))
  (org-todo-state-tags-triggers
        '(("CANCELLED" ("CANCELLED" . t))
          ;;("WAITING" ("WAITING" . t))
          ;;("HOLD" ("WAITING") ("HOLD" . t))
          (done ("WAITING") ("HOLD"))
          ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
          ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
          ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))
  (org-use-fast-todo-selection t)
  (org-src-fontify-natively t)
  ;; Record closed timestamp when a task is DONE
  (org-log-done 'time)
  ;; Insert stage change notes into a drawer
  (org-log-into-drawer t)
  ;; No effect when `org-log-into-drawer' is non-nil.
  (org-log-state-notes-insert-after-drawers nil)
  ;; (org-todo-keyword-faces
  ;;  `(("TODO" :foreground "red" :weight bold)
  ;;   ("NEXT" :foreground "blue" :weight bold)
  ;;   ("DONE" :foreground "forest green" :weight bold)
  ;;   ("WAITING" :foreground "orange" :weight bold)
  ;;   ("HOLD" :foreground "magenta" :weight bold)
  ;;   ("CANCELLED" :foreground "forest green" :weight bold)
  ;;   ("MEETING" :foreground "forest green" :weight bold)
  ;;   ("PHONE" :foreground "forest green" :weight bold)))
  (org-capture-templates
   `(("t" "Task" entry (file+headline "" "Tasks")
      "* TODO %?\n %U\n %a\n %i" :empty-lines-before 1)
     ("j" "Journal" entry (file+olp+datetree "Journal.org")
      "* %U - %^{heading} %^g\n %?\n %i\n")
     ("n" "Note" entry (file "notes.org")
      "* %^{heading} \n %?\n %i\n%a" :empty-lines 1)
     ("m" "Meeting" entry (file "")
      "* DONE Meeting with %?\n %U\n" :clock-in t :clock-resume t)))
  (org-agenda-custom-commands
   `(("n" "Agenda and all TODOs"
     ((agenda #1="")
      (alltodo #1#)))
     ("y" "List TODO entries sort by time" todo "TODO"
         ((org-agenda-sorting-strategy '(priority-down time-up))))
     (" " "GTD Agenda"
      ((agenda ""
               ((org-agenda-span 'day)
                (org-deadline-warning-days 365)
                ;; Display agenda with log
                (org-agenda-start-with-log-mode t)))
       (todo "NEXT"
             ((org-agenda-overriding-header "In Progress")))
       (alltodo ""
             ((org-agenda-overriding-header "Inbox")
              (org-agenda-files '(,(expand-file-name "tasks.org" org-directory)))))
       (tags-todo "work"
                  ((org-agenda-overriding-header "Works")))
       (tags "PRIORITY=\"A\""
             ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
              (org-agenda-overriding-header "A-priority")))
       nil))))
  ;; Format for org agenda column view
  (org-columns-default-format "%40ITEM(Task) %Effort(EE){:} %CLOCKSUM(Time Spent) %SCHEDULED(Scheduled) %DEADLINE(Deadline)")
  ;; Make agenda more compact
  (setq org-agenda-block-separator nil)
  ;; Display agenda with log
  (org-agenda-start-with-log-mode t)
  ;; Refile to any of agenda files with leve up to 1.
  (org-refile-targets '((org-agenda-files :maxlevel . 1)))
  ;; Include file name in the refile path
  (org-refile-use-outline-path 'file)
  ;; Show the refile paths in one step
  (org-outline-path-complete-in-steps nil)
  ;; Allow to create new targets when refiling
  (org-refile-allow-creating-parent-nodes 'confirm)
  ;; Resume clocking task on clock-in if the clock is open
  (org-clock-in-resume t)
  ;; Removes clocked tasks with duration less than one minute
  (org-clock-out-remove-zero-time-clocks t)
  ;; Clock out when moving task to a done state
  (org-clock-out-when-done t)
  ;; Save the running clock and all clock history when exiting Emacs, load it on startup
  (org-clock-persist t)
  ;; Do not prompt to resume an active clock
  (org-clock-persist-query-resume nil)
  ;; Change tasks to NEXT when clocking in
  (org-clock-in-switch-to-state 'bh/clock-in-to-next)
  ;; Enable auto clock resolution for finding open clocks
  (org-clock-auto-clock-resolution 'when-no-clock-is-running)
  ;; Org export settings
  (org-latex-compiler "xelatex")
  :custom-face
  (org-block ((t (:foreground nil :inherit 'fixed-pitch))))
  (org-table ((t (:inherit (shadow fixed-pitch)))))
  (org-verbatim ((t (:inherit (shadow fixed-pitch)))))
  (org-code ((t (:inherit (shadow fixed-pitch)))))
  (org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  (org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  :config
  (setq bh/keep-clock-running nil)
  (require 'helm-org)
  (add-to-list 'helm-completing-read-handlers-alist '(org-capture . helm-org-completing-read-tags))
  (add-to-list 'helm-completing-read-handlers-alist '(org-set-tags . helm-org-completing-read-tags))
  ;; Resume clocking task when emacs is restarted
  (org-clock-persistence-insinuate)
  :hook
  ;;   (org-clock-out . bh/clock-out-maybe)
  (org-clock-out . org-clock-out-resume-interrupted)
  (org-mode . (lambda ()
                ;; (org-indent-mode 1)
                (variable-pitch-mode 1)))
  :bind (([f7] . 'org-dispatch)
         :map org-mode-map
         ("C-c [" . nil)
         ("C-c ]" . nil))
  :delight org-indent-mode)


(use-package org-roam
  :custom
  (org-roam-directory "~/Documents/org-roam/")
  (org-roam-complete-everywhere t)
  :config
  (org-roam-setup))

(use-package org-roam-ui
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t)
  :delight org-roam-ui-mode)

;; ------------------------ Elisp  --------------------------
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            ;; (paredit-mode 1)
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
(use-package racket-mode
  :mode "\\.rkt\\'"
  :hook ((racket-mode . paredit-mode))
  )


;; ------------------------ Clojure --------------------------
(require 'cider)
(require 'clojure-mode)
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
  (use-package pyim-basedict)
  (pyim-basedict-enable)
  (setq default-input-method 'pyim))


(setq default-frame-alist
      '((width . 108)
        (height . 44)
        (left . 580)
        (top . 10)))


(pcase system-type
  ('windows-nt
   (progn
     (set-frame-font "Consolas-14")
     (setq tramp-default-method "plink")
     ))
  ('gnu/linux
   (set-frame-font "Fira Mono-18")))

;; (set-frame-font "-microsoft-Consolas-normal-normal-normal-*-17-*-*-*-m-0-iso10646-1")
;; (set-frame-font "Source Code Pro-12")

(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
                    ;; charset "方正黑体_GBK"
                    charset "Source Han Sans CN"
                    ))

(set-fontset-font (frame-parameter nil 'font)
		  'mathematical "STIX")

(set-fontset-font (frame-parameter nil 'font)
		  'symbol "STIX")

(setq-default frame-background-mode 'dark)

(require 'moe-theme)
(load-theme 'spacemacs-dark)

;; (sml/setup)
(require 'spaceline-config)
;; (setq powerline-default-separator 'wave)
;; (setq spaceline-battery-p t)
;; (setq spaceline-buffer-position-p nil)
(spaceline-emacs-theme)

;; (add-hook 'buffer-list-update-hook 'neotree-projectile-action)


;; Start server.
(require 'server)
(unless (server-running-p)
  (server-start))
