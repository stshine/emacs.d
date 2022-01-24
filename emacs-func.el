;;; emacs-func.el --- Some useful functions and commands -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contain functions that both I wrote and collected from the internet.
;; In use for configuration.

;;; Code:
(defun range (start end step)
  (cond
   ((> start end) '())
   (t (cons start (range (+ step start) end step)))))

;; useful comment commands
(defun comment-defun ()
  (interactive)
  (mark-defun)
  (comment-region (region-beginning)
		  (region-end)))


(defun first-avail-font (&rest fonts)
  "Accept multiple font family names and return the first available one."
  (let ((system-font-list (font-family-list)))
    (--first (member it system-font-list) fonts)))


(defvar pager-temporary-goal-column 0
  "Similat to temporary-goal-column byt used by the pager.el functions")
;(make-variable-buffer-local 'pager-temporary-goal-column)

(defconst pager-keep-column-commands
  '(pager-row-down pager-row-up
		   pager-page-down pager-page-up)
"Commands which when called without any other intervening command should keep the `pager-temporary-goal-column'.")

(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;; ----------------------------------------------------------------------
(defun pager-row-up ()
  "Move point to previous line while scrolling screen down one line.
The effect is that the cursor stays in the same position on the screen."
  (interactive)
  (if (not (memq last-command pager-keep-column-commands))
      (setq pager-temporary-goal-column (current-column)))
  (if (not (pos-visible-in-window-p (point-min)))
      (scroll-down 1))
  (forward-line -1)
  (move-to-column pager-temporary-goal-column))

(defun pager-row-down ()
  "Move point to next line while scrolling screen up one line.
The effect is that the cursor stays in the same position on the screen."
  (interactive)
  (if (not (memq last-command pager-keep-column-commands))
      (setq pager-temporary-goal-column (current-column)))
  (if (not (pos-visible-in-window-p (point-max)))
      (scroll-up 1))
  (if (<= (point) (point-max))
      (forward-line 1))
  (move-to-column pager-temporary-goal-column))

;; --------------------------- Org mode functions -----------------------
(defun org-buffer-p (&optional buffer)
  "Return t if BUFFER is an org buffer.
If BUFFER is not specified, use the current buffer."
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (derived-mode-p 'org-mode))))

(defvar org-clocking-stack '())

(defun org-clock-out-resume-interrupted ()
  "If there is interrupted clocks when we are clocking out, the clocking into it."
  ;; `org-clock-clocking-in' indicates that we are doing a clocking in
  ;; while running this clock out hook.
  (if org-clock-clocking-in
      (push (copy-marker org-clock-interrupted-task) org-clocking-stack)
    (let ((previous-clock (pop org-clocking-stack)))
      (if previous-clock
          (org-with-point-at previous-clock
            (org-clock-in))))))

(defvar bh/keep-clock-running nil)

(defun bh/is-task-p ()
  "Any task with a todo keyword and no subtask."
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun bh/is-project-p ()
  "Any task with a todo keyword subtask."
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

;; (defun bh/is-subproject-p ()
;;   "Any task which is a subtask of another project"
;;   (let ((is-subproject)
;;         (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
;;     (save-excursion
;;       (while (and (not is-subproject) (org-up-heading-safe))
;;         (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
;;           (setq is-subproject t))))
;;     (and is-a-task is-subproject)))


(defun bh/clock-in-to-next (kw)
  "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO"))
           (bh/is-task-p))
      "NEXT")
     ((and (member (org-get-todo-state) (list "NEXT"))
           (bh/is-project-p))
      "TODO"))))

(defun bh/punch-in (arg)
  "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
  (interactive "p")
  (setq bh/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (bh/clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
                                        ; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
          (org-clock-in '(16))
        (bh/clock-in-organization-task-as-default)))))

(defun bh/punch-out ()
  (interactive)
  (setq bh/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun bh/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun bh/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
              (org-clock-in))
          (when bh/keep-clock-running
            (bh/clock-in-default-task)))))))

(defvar bh/organization-task-id  "0ddb65b0-519f-4b46-9907-44ea96251737")

(defun bh/clock-in-organization-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find bh/organization-task-id 'marker)
    (org-clock-in '(16))))

(defun bh/clock-out-maybe ()
  (when (and bh/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (bh/clock-in-parent-task)))


(defun org-roam-create-note-from-headline ()
  "Create an Org-roam note from the current headline and jump to it.

Normally, insert the headline’s title using the ’#title:’ file-level property
and delete the Org-mode headline. However, if the current headline has a
Org-mode properties drawer already, keep the headline and don’t insert
‘#+title:'. Org-roam can extract the title from both kinds of notes, but using
‘#+title:’ is a bit cleaner for a short note, which Org-roam encourages."
  (interactive)
  (let ((title (nth 4 (org-heading-components)))
        (has-properties (org-get-property-block)))
    (org-cut-subtree)
    (org-roam-node-find title nil nil 'no-confirm)
    (org-paste-subtree)
    (unless has-properties
      (kill-line)
      (while (outline-next-heading)
        (org-promote)))
    (goto-char (point-min))
    (when has-properties
      (kill-line)
      (kill-line))))

;;; -----------------------------------------------
(defun prelude-copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))


;; Defining a function like this makes it possible to type 'clear' in eshell and have it work
(defun eshell/clear ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (eshell-send-input))

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
        (filename (buffer-file-name))
        (basename (file-name-nondirectory filename)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " (file-name-directory filename) basename nil basename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun capitalize-underline ()
  "Capitailize an underline word."
  (interactive)
  (if (eq (char-after (point)) ?_)
      (delete-char 1))
  (capitalize-word 1))

(defun underline-capitalize ()
  "Turn a capitailized word to an underline one."
  (interactive)
  (let ((c (char-after (point))))
    (if (= c (upcase c))
        (insert ?_)))
  (downcase-word 1))

(define-globalized-minor-mode
  smartparens-paredit-global-mode
  smartparens-strict-mode
  (lambda ()
    (when (member major-mode sp-lisp-modes)
      (smartparens-strict-mode 1)
      (sp-use-paredit-bindings))))


(use-package transient
  :ensure t)

(transient-define-prefix org-times ()
  "Invoke Org clock commands"
  [["Clock commands"
    ("s" "Insert scheduled" org-schedule)
    ("d" "Insert deadline" org-deadline)
    ("i" "Clock in" org-clock-in :if-not org-clock-is-active)
    ("o" "Clock out" org-clock-out :if org-clock-is-active)
    ("I" "Clock last" org-clock-in-last)
    ("j" "Clock jump" org-clock-goto :if org-clock-is-active)
    ("q" "Cancel clock" org-clock-cancel)
    ("D" "Display clock" org-clock-display)
    ("r" "Clock report" org-clock-report)
    ("z" "Resolve clocks" org-resolve-clocks)]
   ["Timer commands"
    ("." "Insert timestamp" org-time-stamp)
    ("t" "Start timer" org-timer-start)
    ("p" "Stop timer" org-timer-stop)
    ("t" "Insert timer" org-timer)]])

(transient-define-prefix org-toggles ()
  "common toggle commands"
  [("c" "Toggle checkbox" org-toggle-checkbox :transient t)
   ("e" "Toggle pretties" org-toggle-pretty-entities)
   ("i" "Toggle inline" org-toggle-inline-images)
   ("l" "Toggle link" org-toggle-link-display)
   ("T" "Toggle todo tree" org-show-todo-tree)
   ("t" "Toggle todo" org-todo)
   ("m" "Toggle math view" org-toggle-latex-fragment)])

(transient-define-prefix org-subtrees ()
  "Common tree editing commands"
  [("a" "Toggle archive tag" org-toggle-archive-tag)
   ("A" "Archive subtree" org-archive-subtree)
   ("b" "Indirect subtree" org-tree-to-indirect-buffer)
   ("k" "Kill subtree" org-cut-subtree)
   ("h" "Promote subtree" org-promote-subtree)
   ("j" "Move subtree down" org-move-subtree-down)
   ("k" "Move subtree up" org-move-subtree-up)
   ("l" "Demote subtree" org-demote-subtree)
   ("n" "Narrow to subtree" org-narrow-to-subtree)
   ("N" "Widen subtree" widen)
   ("r" "Refile" org-refile)
   ("s" "Create sparse tree" org-sparse-tree)
   ("S" "Sort" org-sort)])

(transient-define-prefix org-edits ()
  "Editing"
  [["Move heading"
    ("<RET>" "Confirm" transient-quit-all)
    ("N" "Move down" org-move-item-down)
    ("P" "Move up" org-move-item-up)
    ("," "Promote" org-metaleft)
    ("." "Demote" org-metaright)
    ("<" "Promote subtree" org-shiftmetaleft)
    (">" "Demote subtree" org-shiftmetaright)]
   ["Edit heading"
    ("t" "Set todo" org-todo)
    ("g" "Set tags" org-set-tags-command)
    ("p" "Set property" org-set-property)
    ("," "Set priority" org-priority)]])

(transient-define-prefix org-tables ()
  "Common table commands"
  [("a" "Align table commands" org-table-align)
   ("b" "Blank field" org-table-blank-field)
   ("d" "Delete row" org-table-kill-row)
   ("k" "Delete column" org-table-delete-column)
   ("e" "Eval formula" org-table-eval-formula)
   ("E" "Export table" org-table-export)
   ("n" "Next row" next-line :transient t)
   ("N" "Move row down" org-table-move-row-down)
   ("p" "Previous row" previous-line :transient t)
   ("P" "Move row up" org-table-move-row-up)
   ("h" "Previous field" org-table-previous-field)
   ("l" "Next field" org-table-next-field)
   ("<" "Move column left" org-table-move-column-left)
   (">" "Move column right" org-table-move-column-right)
   ("n" "Create table" org-table-create)
   ("s" "Sort table" org-table-sort-lines)])

(transient-define-prefix org-babels ()
  "Invoke org bable commands"
  ["Bable commands"
   [("n" "Next block" org-babel-next-src-block)
    ("p" "Previous block" org-babel-previous-src-block)]])

(transient-define-prefix org-inserts ()
  "Common insert commands"
  [("d" "Insert drawer" org-insert-drawer)
   ("e" "Set effort" org-set-effort)
   ("f" "Insert footnote" org-footnote-new)
   ("h" "Insert heading" org-insert-heading)
   ("i" "Insert item" org-insert-item)
   ("l" "Insert link" org-insert-link)
   ("n" "Insert note" org-add-note)
   ("p" "Set property" org-set-property)
   ("s" "Insert subheading" org-insert-subheading)
   ("t" "Insert structured template" org-insert-structure-template)
   ("g" "Insert tags" org-set-tags-command)])

(transient-define-prefix org-roams ()
  "Org Roam Commands"
  [("i" "Insert roam node" org-roam-node-insert)
   ("I" "Create Node on heading" org-id-get-create)
   ("l" "Roam buffer" org-roam-buffer-toggle)
   ("/" "Find roam node" org-roam-node-find)
   ("c" "Capture roam node" org-roam-capture)
   ("r" "Roam refile" org-roam-refile)])


(transient-define-prefix org-dispatch ()
  "Invoke common Org mode commands"
  [["Org mode entries"
    ("a" "Agenda" org-agenda)
    ("c" "Capture" org-capture)
    ("l" "Store link" org-store-link)
    ("b" "Switch buffer" org-switchb)]
   ["Commands" :if org-buffer-p
    ("w" "Refile" org-refile)
    ("v" "Archive" org-archive-subtree-default-with-confirmation)
    ("e" "Editing" org-edits)]
   ["Dispatchs"
    ("." "Time and clock" org-times)
    ("t" "Table commands" org-tables)
    ("i" "Insert element" org-inserts)
    ("x" "Toggle element" org-toggles)
    ("s" "Editing subtrees" org-subtrees)
    ("r" "Org Roam" org-roams)
    ("E" "Org export" org-export-dispatch)]
   ["Clocking"
    ("g" "Goto Clocking" org-clock-goto)
    ("p" "Punch in" bh/punch-in)
    ("o" "Punch out" bh/punch-out)]])

;;; emacs-func.el ends here
