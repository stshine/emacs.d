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
  "Toggle org elements"
  ["common toggle commands"
   [("c" "Toggle checkbox" org-toggle-checkbox)
    ("e" "Toggle pretties" org-toggle-pretty-entities)
    ("i" "Toggle inline" org-toggle-inline-images)
    ("l" "Toggle link" org-toggle-link-display)
    ("T" "Toggle todo tree" org-show-todo-tree)
    ("t" "Toggle todo" org-todo)
    ("m" "Toggle math view" org-toggle-latex-fragment)]])

(transient-define-prefix org-subtrees ()
  "Edit org subtrees"
  ["Common tree editing commands"
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
    ("S" "Sort" org-sort)]])

(transient-define-prefix org-tables ()
  "Invoke org table comands"
  ["Common table commands"
   [("a" "Align table commands" org-table-align)
    ("b" "Blank field" org-table-blank-field)
    ("d" "Delete row" org-table-kill-row)
    ("k" "Delete column" org-table-delete-column)
    ("e" "Eval formula" org-table-eval-formula)
    ("E" "Export table" org-table-export)
    ("h" "Previous field" org-table-previous-field)
    ("H" "Move column left" org-table-move-column-left)
    ("j" "Next row" org-table-next-row)
    ("J" "Move row down" org-table-move-row-down)
    ("K" "Move row up" org-table-move-row-up)
    ("l" "Next field" org-table-next-field)
    ("L" "Move column right" org-table-move-column-right)
    ("n" "Create table" org-table-create)
    ("s" "Sort table" org-table-sort-lines)]])

(transient-define-prefix org-babels ()
  "Invoke org bable commands"
  ["Bable commands"
   [("n" "Next block" org-babel-next-src-block)
    ("p" "Previous block" org-babel-previous-src-block)]])

(transient-define-prefix org-inserts ()
  "Insert org elements"
  ["Common insert commands"
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
    ("g" "Insert tags" org-set-tags-command)]])

(transient-define-prefix org-roams ()
  "Org Roam Commands"
  ["Org Roam Commands"
   [("i" "Insert roam node" org-roam-node-insert)
    ("I" "Create Node on heading" org-id-get-create)
    ("l" "Roam buffer" org-roam-buffer-toggle)
    ("/" "Find roam node" org-roam-node-find)
    ("c" "Capture roam node" org-roam-capture)
    ("r" "Roam refile" org-roam-refile)]])

(transient-define-prefix org-dispatch ()
  "Invoke common Org mode commands"
  ["Org mode commands"
   [("l" "Store link" org-store-link)
    ("C" "Capture" org-capture)
    ("c" "Time" org-times)
    ("a" "Agenda" org-agenda)
    ("b" "Switch buffer" org-switchb)
    ("t" "Table commands" org-tables)
    ("i" "Insert element" org-inserts)
    ("x" "Toggle element" org-toggles)
    ("s" "Editing subtrees" org-subtrees)
    ("r" "Org Roam" org-roams)
    ("e" "Org export" org-export-dispatch)
    ("p" "Punch in" bh/punch-in)
    ("o" "Punch out" bh/punch-out)]])

;;; emacs-func.el ends here
