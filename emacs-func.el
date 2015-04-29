;;; emacs-func.el --- Some useful functions and commands

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


;;; emacs-func.el ends here
