;;; org-dispatch.el --- Org mode command dispatcher using transient -*- lexical-binding: t -*-

;; Author: Pu Xingyu <pu.stshine@gmail.com>

;;; Code:

(require 'org)
(require 'transient)


(defun org-buffer-p (&optional buffer)
  "Return t if BUFFER is an org buffer.
If BUFFER is not specified, use the current buffer."
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (derived-mode-p 'org-mode))))


;; (transient-define-prefix org-export-dispatcher ()
;;   "Org Export Dispatcher"
;;   [("L" "As Latex buffer" org-latex-export-as-latex)
;;    ("l" "As Latex file" org-latex-export-to-latex)
;;    ("p" "As PDF file" org-latex-export-to-pdf)
;;    ("o" "As PDF file and open" (lambda ()
;;                                  (interactive)
;;                                  (org-open-file (org-latex-export-to-pdf))))])

(transient-define-prefix org-time-dispatch ()
  "Invoke Org clock commands"
  [["Clock commands"
    ("i" "Clock in" org-clock-in :if-not org-clock-is-active)
    ("o" "Clock out" org-clock-out :if org-clock-is-active)
    ("I" "Clock last" org-clock-in-last)
    ("j" "Clock jump" org-clock-goto :if org-clock-is-active)
    ("q" "Cancel clock" org-clock-cancel)
    ("D" "Display clock" org-clock-display)
    ("r" "Clock report" org-clock-report)
    ("z" "Resolve clocks" org-resolve-clocks)]
   ["Timer commands"
    ("s" "Insert scheduled" org-schedule)
    ("d" "Insert deadline" org-deadline)
    ("." "Insert timestamp" org-time-stamp)
    ("t" "Start timer" org-timer-start)
    ("p" "Stop timer" org-timer-stop)
    ("t" "Insert timer" org-timer)]])

;; (transient-define-prefix org-toggles ()
;;   "common toggle commands"
;;   [("c" "Toggle checkbox" org-toggle-checkbox :transient t)
;;    ("e" "Toggle pretties" org-toggle-pretty-entities)
;;    ("i" "Toggle inline" org-toggle-inline-images)
;;    ("l" "Toggle link" org-toggle-link-display)
;;    ("T" "Toggle todo tree" org-show-todo-tree)
;;    ("t" "Toggle todo" org-todo)
;;    ("m" "Toggle math view" org-toggle-latex-fragment)])

;; (transient-define-prefix org-subtrees ()
;;   "Common tree editing commands"
;;   [("a" "Toggle archive tag" org-toggle-archive-tag)
;;    ("A" "Archive subtree" org-archive-subtree)
;;    ("b" "Indirect subtree" org-tree-to-indirect-buffer)
;;    ("k" "Kill subtree" org-cut-subtree)
;;    ("h" "Promote subtree" org-promote-subtree)
;;    ("j" "Move subtree down" org-move-subtree-down)
;;    ("k" "Move subtree up" org-move-subtree-up)
;;    ("l" "Demote subtree" org-demote-subtree)
;;    ("n" "Narrow to subtree" org-narrow-to-subtree)
;;    ("N" "Widen subtree" widen)
;;    ("r" "Refile" org-refile)
;;    ("s" "Create sparse tree" org-sparse-tree)
;;    ("S" "Sort" org-sort)])

(transient-define-prefix org-edit-heading ()
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
    ("," "Set priority" org-priority)
    ("e" "Set effort" org-set-effort)
    ("n" "Add notes" org-add-note)]])

(transient-define-prefix org-table-dispatch ()
  "Table commands"
  [("a" "Align table" org-table-align)
   ("b" "Blank field" org-table-blank-field)
   ("k" "Delete row" org-table-kill-row)
   ("d" "Delete column" org-table-delete-column)
   ;;    ("e" "Eval formula" org-table-eval-formula)
   ;;    ("E" "Export table" org-table-export)
   ("n" "Next row" org-table-next-row :transient t)
   ("N" "Move row down" org-table-move-row-down :transient t)
   ("p" "Previous row" previous-line :transient t)
   ("P" "Move row up" org-table-move-row-up :transient t)
   ;;    ("h" "Previous field" org-table-previous-field)
   ;;    ("l" "Next field" org-table-next-field)
   ;;    ("<" "Move column left" org-table-move-column-left)
   ;;    (">" "Move column right" org-table-move-column-right)
   ;;    ("n" "Create table" org-table-create)
   ("s" "Sort table" org-table-sort-lines)])

;; (transient-define-prefix org-babels ()
;;   "Invoke org bable commands"
;;   ["Bable commands"
;;    [("n" "Next block" org-babel-next-src-block)
;;     ("p" "Previous block" org-babel-previous-src-block)]])

(transient-define-prefix org-insert-dispatch ()
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
   ("t" "Insert structured template" org-insert-structure-template)])

(transient-define-prefix org-roam-dispatch ()
  "Org Roam Commands"
  [:class transient-row
          ("i" "Insert roam node" org-roam-node-insert)
          ("I" "Create Node on heading" org-id-get-create)
          ("l" "Roam buffer" org-roam-buffer-toggle)
          ("/" "Find roam node" org-roam-node-find)
          ("c" "Capture roam node" org-roam-capture)
          ("r" "Roam refile" org-roam-refile)
          ("w" "Extract" org-roam-extract-subtree)])

(transient-define-prefix org-dispatch ()
  "Invoke common Org mode commands"
  :transient-non-suffix 'transient--do-stay
  [["Org mode entries"
    ("a" "Agenda" org-agenda)
    ("c" "Capture" org-capture)
    ("l" "Store link" org-store-link)
    ("b" "Switch buffer" org-switchb)
    ("<f12>" "GTD agenda" (lambda () (interactive) (org-agenda nil " ")))]
   ["Move heading" :if org-at-heading-p
    ("n" "next" org-next-visible-heading :transient t)
    ("p" "prev" org-previous-visible-heading :transient t)
    ("N" "down" org-move-subtree-down :transient t)
    ("P" "up" org-move-subtree-up :transient t)
    ("e" "Edit" org-edit-heading)]
   ["Move table" :if org-at-table-p
    ("n" "next" org-table-next-row :transient t)
    ("p" "prev" previous-line :transient t)
    ("N" "down" org-table-move-row-down :transient t)
    ("P" "up" org-table-move-row-up :transient t)]
   ["Move item" :if org-in-item-p
    ("n" "next" org-next-item :transient t)
    ("p" "prev" org-previous-item :transient t)
    ("N" "down" org-move-item-down :transient t)
    ("P" "up" org-move-item-up :transient t)]
   ["Commands" :if org-buffer-p
    ("w" "Refile" org-refile)
    ("v" "Archive" org-archive-subtree-default-with-confirmation)
    ("e" "Editing" org-edit-heading)]
   ["Dispatchs"
    ("t" "Time and clock" org-time-dispatch)
    ("b" "Table commands" org-table-dispatch)
    ("i" "Insert element" org-insert-dispatch)
    ;;     ("x" "Toggle element" org-toggles)
    ;;     ("s" "Editing subtrees" org-subtrees)
    ("r" "Org Roam" org-roam-dispatch)
    ("E" "Org export" org-export-dispatch)]
   ;; ["Clocking"
   ;; ("g" "Goto Clocking" org-clock-goto)
   ;; ("p" "Punch in" bh/punch-in)
   ;; ("o" "Punch out" bh/punch-out)]
   ])

(provide 'org-dispatch)
;;; org-dispatch.el ends here
