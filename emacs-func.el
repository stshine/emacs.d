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
"Commands which when called without any other intervening command should keep the `pager-temporary-goal-column'")

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
  (move-to-column pager-temporary-goal-column)
  )

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
  (move-to-column pager-temporary-goal-column)
  )

(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
