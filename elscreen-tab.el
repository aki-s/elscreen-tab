;;; elscreen-tab.el --- minor mode to display tabs of elscreen in a dedicated buffer -*- lexical-binding: t; -*-

;; Copyright (C) 2017 - 2021 Syunsuke Aki

;; Author: Aki Syunsuke <sunny.day.dev@gmail.com>
;; URL: https://github.com/aki-s/elscreen-tab
;; Package-Version: 1.0.0
;; Package-Requires: ((emacs "26") (elscreen "20180321") (dash "2.14.1"))
;; Keywords: tools, extensions
;; Created: 2017-02-26
;; Updated: 2020-11-21T14:16:49Z;

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This minor is for users who;
;; - dislike setting `elscreen-display-tab' to `t', because which highjacks `header-line-format'
;;   which you reserved for the other purpose such as `which-func-mode' or alike.
;; - dislike the tab menu is displayed at the top.

;; [Usage]
;; (require 'elscreen)
;; (elscreen-start)
;; (require 'elscreen-tab)
;; (elscreen-tab-mode)  ; Enable `elscreen-tab'.
;;
;; (elscreen-tab-set-position 'right) ; Show on the right side.
;; (elscreen-tab-set-position 'top) ; Show at the top.
;; (elscreen-tab-set-position 'left) ; Show on the left side.
;; (elscreen-tab-set-position 'bottom) ; Show at the bottom.
;; (elscreen-tab-mode -1)  ; Disable `elscreen-tab'.

;;; TODO:
;; Improve usability
;; + Naturally support using multiple frames by creating a indigenous buffer of elscreen-tab for each frame.
;; + Update name of current elscreen-tab soon after selected buffer is changed.
;; + Create interactive function to reset screen-id provided by elscreen.
;;
;;; Known issues:
;; + elscreen-tab becomes non-dedicated when ECB is activated.

;;; Code:
(require 'cl-lib)
(require 'dash)
(require 'seq)
(require 'elscreen)


(defgroup elscreen-tab nil
  "Show tabs of elscreen in dedicated buffer.
Alternative to `elscreen-display-tab'."
  :tag "elscreen-tab-style"
  :group 'elscreen
  :package-version '("elscreen-tab" "1.0.1"))


(defconst elscreen-tab--tab-window-parameters
  '(window-parameters .
                      ((no-other-window . t) (no-delete-other-windows . t) (delete-window . ignore))))
(defconst elscreen-tab--dedicated-tab-buffer-name " *elscreen-tab*")
(defconst elscreen-tab--unmet-condition 'unmet-condition
  "Throw this value if some condition is not met.")
(defconst elscreen-tab--tab-unit-separator
  #s(hash-table size 4
                test eq
                data (right "\n" top "|" left "\n" bottom "|"))
  "Separator between tab-units")

(defcustom elscreen-tab-debug-flag nil
  "Non-nil means showing message about what happened, for debug purpose."
  :type 'boolean
  :group 'elscreen-tab)

(defcustom elscreen-tab-position 'bottom
  "Specify where to place the window of elscreen-tab."
  :type '(radio (const top) (const :value bottom) (const left) (const right))
  :initialize 'custom-initialize-default
  :set 'elscreen-tab--set-position
  :group 'elscreen-tab)

(defcustom elscreen-tab-undesirable-name-regexes
  `("\\*Help\\*" " \\*.*")
  "Try to avoid using these names for elscreen-tab."
  :type '(list string)
  :group 'elscreen-tab)

(defcustom elscreen-tab-unload-hooks nil
  "Call `run-hooks' for this value when `elscreen-tab' is unloaded."
  :type '(list function)
  :group 'elscreen-tab)

(defcustom elscreen-tab-delay-of-updating-display .5
  "Second of delay from the last update request before starting update of display."
  :type 'number
  :group 'elscreen-tab)


(defvar elscreen-tab--mode-line-format nil "Remove mode-line for elscreen-tab if nil.")
(defvar elscreen-tab-hooks '(elscreen-create-hook elscreen-goto-hook elscreen-kill-hook)
  "A group of hooks to update elscreen-tab.")
(defvar elscreen-tab--display-idle-timer nil "Idle timer object to update display.")
(defvar elscreen-tab--last-screen-id 0)


(defface elscreen-tab-current-screen-face
  '((default :inherit header-line-highlight)
     (((class color))
       (:background "yellow" :foreground "red" :box t))
     (t (:underline t)))
  "Face for current screen tab."
  :group 'elscreen-tab)

(defface elscreen-tab-other-screen-face
  '((default :inherit default)
     (((type x w32 mac ns) (class color))
       :background "Gray85" :foreground "Gray50" :box t)
     (((class color))
       (:background "blue" :foreground "black" :underline t)))
  "Face for tabs other than current screen one."
  :group 'elscreen-tab)

(defface elscreen-tab-mouse-face
  `((default :inherit default)
     (t
       :inherit link
       :background ,(face-attribute 'elscreen-tab-current-screen-face :foreground)
       :foreground ,(face-attribute 'elscreen-tab-current-screen-face :background)
       ))
  "Face for when mouse cursor is over each tab of elscreen.")


(defun elscreen-tab--debug-log (form &rest args)
  "Logging function of the same format with (message FORM ARGS)."
  (when elscreen-tab-debug-flag (apply #'message (concat "[ELSCREEN-TAB]" form) args)))

(defun elscreen-tab--display-buffer-alist ()
  "Return alist for `elscreen-tab' with the same format of `display-buffer-alist'."
  `((side . ,elscreen-tab-position) (slot . 0) (window-height . 1) (preserve-size . (nil . t))
    ,elscreen-tab--tab-window-parameters))

(defun elscreen-tab-toggle-debug-flag ()
  "Toggle `elscreen-tab-debug-flag'."
  (interactive)
  (setq elscreen-tab-debug-flag (not elscreen-tab-debug-flag)))

(defun elscreen-tab--dedicated-tab-buffer-name ()
  "Get or create singleon buffer."
  (get-buffer-create elscreen-tab--dedicated-tab-buffer-name))

(defun elscreen-tab--update-buffer ()
  "Update tab buffer if it has changed."
  (elscreen-tab--debug-log "[%s>%s]called" this-command "elscreen-tab--update-buffer")
  (setq elscreen-tab--display-idle-timer nil)
  (with-current-buffer (elscreen-tab--dedicated-tab-buffer-name)
    (setq buffer-read-only nil
      mode-line-format elscreen-tab--mode-line-format
      show-trailing-whitespace nil
      )
    (cursor-intangible-mode 1)
    (erase-buffer)
    (insert
      (let ((screen-ids (sort (elscreen-get-screen-list) '<))
             (sep (gethash elscreen-tab-position elscreen-tab--tab-unit-separator "|")))
        (mapconcat #'elscreen-tab--create-tab-unit screen-ids sep)))
    ;; Finish
    (setq buffer-read-only t))
  (setq elscreen-tab--last-screen-id (elscreen-get-current-screen)))

(defun elscreen-tab--set-idle-timer-for-updating-display ()
  (elscreen-tab--debug-log "[%s>%s]called" this-command "elscreen-tab--set-idle-timer-for-updating-display")
  (unless (and elscreen-tab--display-idle-timer (= elscreen-tab--last-screen-id (elscreen-get-current-screen)))
    (elscreen-tab--debug-log "elscreen-tab--display-idle-timer : %s" elscreen-tab--display-idle-timer)
    (setq elscreen-tab--display-idle-timer
      (run-with-idle-timer elscreen-tab-delay-of-updating-display nil 'elscreen-tab--update-buffer))))

(defun elscreen-tab--create-tab-unit (screen-id)
  "Return text of a tab unit which is added properties for SCREEN-ID."
  (let* ((nickname-or-buf-names (assoc-default screen-id (elscreen-get-screen-to-name-alist)))
         (nickname-or-1st-buffer
          (elscreen-tab--avoid-undesirable-name (split-string nickname-or-buf-names ":")))
         (tab-name
          (elscreen-truncate-screen-name nickname-or-1st-buffer (elscreen-tab-width) t))
         (tab-status (elscreen-status-label screen-id " "))
         (tab-id (concat "[" (number-to-string screen-id) "]"))
         tab-title
         tab-unit)
    ;; Colorize tab-id.
    (if (eq (elscreen-get-current-screen) screen-id)
        ;; Add face only to currently selected tab.
        (put-text-property 0 3 'face 'elscreen-tab-current-screen-face tab-id)
      (put-text-property 0 3 'face 'elscreen-tab-other-screen-face tab-id))
    (setq tab-title (format "%s%s%s" tab-status tab-id tab-name))
    (setq tab-unit (elscreen-tab--propertize-click-to-jump tab-title screen-id))
    tab-unit))

(defun elscreen-tab--avoid-undesirable-name (name-list)
  "Length of NAME-LIST must be more than 0."
  (cl-loop for name = (pop name-list)
           when (or
                 (cl-notany (lambda (e) (string-match e name))
                            elscreen-tab-undesirable-name-regexes)
                 (not name-list))
           return (or name "?")
           ))

(defun elscreen-tab--propertize-click-to-jump (text screen-id)
  "Return a copy of TEXT having feature to switch to screen of SCREEN-ID by mouse-click."
  (propertize text
              'cursor-intangible t
              'mouse-face 'elscreen-tab-mouse-face
              'help-echo (assoc-default screen-id (elscreen-get-screen-to-name-alist))
              'local-map (elscreen-tab--create-keymap screen-id)))

(defun elscreen-tab--create-keymap (screen-id)
  "Return keymap to select `SCREEN-ID' with click."
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "<mouse-1>")
      (lambda (_)
        (interactive "e")
        (elscreen-goto screen-id)))
    keymap))

(defun elscreen-tab--elscreen-tab-name-p (buffer)
  "Return t if BUFFER is named `elscreen-tab--dedicated-tab-buffer-name'."
  (equal (buffer-name buffer) elscreen-tab--dedicated-tab-buffer-name))

(defun elscreen-tab--window-count ()
  "Window number (s) of currently displayed `elscreen-tab--dedicated-tab-buffer-name'."
  (-count #'elscreen-tab--elscreen-tab-name-p
          (mapcar #'window-buffer (window-list))))

(cl-defun elscreen-tab--ensure-one-window ()
  "Delete elscreen-tab if it is not side window.
This case can happen if `desktop-read' is called."
  (elscreen-tab--debug-log "[%s>%s]called" this-command "elscreen-tab--ensure-one-window")
  (when (= (elscreen-tab--window-count) 1)
    (cl-return-from elscreen-tab--ensure-one-window))
  (let* ((ignore-window-parameters t)
         (win-list (window-list)))
    (cl-loop for win in win-list do
             (when (and (elscreen-tab--elscreen-tab-name-p (window-buffer win))
                        (not (window-at-side-p win)))
               (delete-window win)
               ))))

(defun elscreen-tab--set-position (symbol-custom symbol-pos)
  (set symbol-custom symbol-pos)
  (elscreen-tab--delete-all-winows)
  (elscreen-tab--get-window)
  )

(defun elscreen-tab-set-position (symbol-pos)
  "Set position of elscreen-tab to SYMBOL-POS by updating `elscreen-tab-position'."
  (save-excursion
    (elscreen-tab--set-position 'elscreen-tab-position symbol-pos))
  )

(defun elscreen-tab--get-window ()
  "Create or get `elscreen-tab--dedicated-tab-buffer-name' in\
current visible display."
  (elscreen-tab--debug-log "[%s>%s]called" this-command "elscreen-tab--get-window")
  (let* ((buf (elscreen-tab--dedicated-tab-buffer-name))
         (win (get-buffer-window buf)))
    (unless win
      (with-current-buffer buf
        ;; Hide header-line.
        (setq header-line-format nil)
        (setq buffer-read-only t)
        ))
    (setq win (display-buffer-in-side-window buf (elscreen-tab--display-buffer-alist)))
    (elscreen-tab--stingy-height win) ; It seems `display-buffer-in-side-window didn't make window less than window-min-height.
    (set-window-dedicated-p win t) ; Because newly created window is not dedicated.
    (elscreen-tab--ensure-one-window)
    win))

(defun elscreen-tab--update-and-display ()
  "Show window of elscreen-tab, then refresh the buffer."
  (elscreen-tab--get-window)
  (elscreen-tab--set-idle-timer-for-updating-display)
  )

(cl-defun elscreen-tab--stingy-height (window)
  "Set WINDOW height as small as possible."
  (unless window (cl-return-from elscreen-tab--stingy-height
                   "Invalid argument: window must not be nil"))
  (with-selected-window window
    (let* ((expected-height 1)
           (delta (- expected-height (window-body-height)))
           (delta-allowed (window-resizable window delta nil window)))
      (with-demoted-errors "Unable to minimize %s"
        (window-resize window delta-allowed nil t)
        (window-preserve-size window nil t)
        (setq window-size-fixed 'height)
        )))
  )

(defun elscreen-tab--delete-window-if-exists ()
  "Delete window of `elscreen-tab' of current screen, if it exists."
  (let ((window (get-buffer-window elscreen-tab--dedicated-tab-buffer-name)))
    (when window
      (progn
        (elscreen-tab--debug-log
          "[%s>%s]called_for_screen[%d]"
          this-command "elscreen-tab--delete-window-if-exists" (elscreen-get-current-screen))
        (setf (window-parameter window 'delete-window) nil)
        (delete-window window)))))

(defun elscreen-tab--delete-window (&optional screen-id)
  "Delete `elscree-tab''s window of SCREEN-ID's  if it is specified,\
else delete current window of SCREEN-ID."
  (let (org-screen-id)
    (if screen-id
        (progn
          (setq org-screen-id (elscreen-get-current-screen))
          (elscreen-goto-internal screen-id)
          (elscreen-tab--delete-window-if-exists)
          (elscreen-set-window-configuration screen-id (elscreen-current-window-configuration))
          (elscreen-goto-internal org-screen-id)
          )
      (elscreen-tab--delete-window-if-exists))
    ))

(defun elscreen-tab--delete-all-winows ()
  "Delete all windows of `elscreen-tab'.
Call this function to disable this mode."
  (save-excursion
    (mapc #'elscreen-tab--delete-window (elscreen-get-screen-list)))
  )

(defun elscreen-tab--manage-hook (choice func hooks)
  "CHOICE (add/remove) FUNC to/from HOOKS.
All argument must be given as symbol.
CHOICE is either 'add or 'rm.
HOOKS is such as '(hook1 hook2) or 'hook3."
  (catch elscreen-tab--unmet-condition
    (unless (functionp func) (throw elscreen-tab--unmet-condition "2nd arg must be function."))
    (let ((add-or-rm (cl-case choice
                       (add 'add-hook)
                       (rm 'remove-hook)
                       (t (throw elscreen-tab--unmet-condition "Specify 'add or 'rm")))))
      (mapc
       (lambda (hook)
         (let ((preposition (cl-case choice ('add "to") ('rm "from"))))
           (elscreen-tab--debug-log "%s `%s' %s `%s'" choice func preposition hook))
         (funcall add-or-rm hook func)
         )
       (if (atom hooks) (list hooks) hooks))))
  )

(defun elscreen-tab--remove-all-hooks ()
  "Remove hooks enabled by `elscreen-tab-mode'."
  (elscreen-tab--manage-hook 'rm 'elscreen-tab--update-and-display elscreen-tab-hooks)
  (remove-hook 'elscreen-screen-update-hook 'elscreen-tab--set-idle-timer-for-updating-display)
  )

(defun elscreen-tab--add-all-hooks ()
  "Add hooks for `elscreen-tab-mode'."
  (elscreen-tab--manage-hook 'add 'elscreen-tab--update-and-display elscreen-tab-hooks)
  (add-hook 'elscreen-screen-update-hook 'elscreen-tab--set-idle-timer-for-updating-display)
  )

(defun elscreen-tab--clear-objects ()
  "Delete all GUI objects related to elscreen-tab."
  ;; Delete windows which displayed elscreen-tab.
  (elscreen-tab--delete-all-winows)
  ;; Kill the buffer of elscreen-tab.
  (let* ((buf (get-buffer elscreen-tab--dedicated-tab-buffer-name)))
    (when buf
      (set-window-dedicated-p (get-buffer-window buf) nil)
      (kill-buffer buf)))
  )

(defun elscreen-tab--check-prerequisite ()
  "Throw `elscreen-tab--unmet-condition' if prerequisite to start ‘elscreen-tab-mode’ is not met."
  (cond
   ((null (featurep 'elscreen))
    (let ((msg "Please load `elscreen' before hand."))
      (lwarn 'elscreen-tab :error msg)
      (throw elscreen-tab--unmet-condition msg)))
   ;; Judge by variable whether elscreen is loaded, because elscreen does not provide mode.
   ((null elscreen-frame-confs)
    (let ((msg "Please start `elscreen' with `elscreen-start'."))
      (lwarn 'elscreen-tab :error msg)
      (throw elscreen-tab--unmet-condition msg))
    )
   ))


;;;; Mode
;;;###autoload
(define-minor-mode elscreen-tab-mode
  "Show tab window of elscreen at `elscreen-tab-position' instead of 'header-line.
Because header line is precious and tab is only displayed in
`frame-first-window' in elscreen-mode.
"
  :group 'elscreen-tab
  :global t
  :require 'elscreen ; This line doesn't work?

  (catch elscreen-tab--unmet-condition
    (elscreen-tab--debug-log "elscreen-tab-mode is called when its value is `%s'" elscreen-tab-mode)
    (cond (elscreen-tab-mode
           (elscreen-tab--check-prerequisite)
           (setq elscreen-display-tab nil) ; Disable `tab' using header-line.
           (elscreen-tab--add-all-hooks)
           (elscreen-tab--update-and-display)
           )
          (t
           (elscreen-tab--remove-all-hooks) ; Delete side-effects.
           (elscreen-tab--clear-objects)
           )))
  )


;; Utility for debug:
(defun elscreen-tab--pp-buffer-and-window ()
  "Show each buffer name and its connected window.
Buffers having connected window are displayed first."
  (interactive)
  (cl-labels (
              (tuplize (buf) (cons (buffer-name buf) (get-buffer-window buf)))
              (window-bound-p (e1 e2)
                              (pcase `(,e1 ,e2)
                                (`((,e11 . ,e12) (,e21 . ,e22))
                                 (cond
                                  ((or (and e12 e22) (and (null e12) (null e22))) (string< e11 e21))
                                  ((null e12) nil)
                                  ((null e22) t)
                                  ))))
              )
    (let* ((buffer-window (cl-map 'list #'tuplize (buffer-list)))
           (res (seq-sort #'window-bound-p buffer-window)))
      (pp res (get-buffer-create "*elscreen-tab-debug*"))
      (switch-to-buffer-other-window "*elscreen-tab-debug*")
      )))

;; Unload function:
(defun elscreen-tab-unload-function ()
  "Unload function to ensure normal behavior when feature 'elscreen-tab is unloaded."
  (interactive)
  (elscreen-tab--debug-log "[%S>%s]called" this-command "elscreen-tab-unload-function")
  (with-demoted-errors "%S"
    (run-hooks elscreen-tab-unload-hooks)
    ))

(provide 'elscreen-tab)
;;; elscreen-tab.el ends here

;; Local variables:
;; eval: (add-hook 'write-file-functions 'time-stamp)
;; time-stamp-start: ";; Updated:"
;; time-stamp-format: " %:y-%02m-%02dT%02H:%02M:%02SZ"
;; time-stamp-line-limit: 13
;; time-stamp-time-zone: "UTC"
;; time-stamp-end: ";"
;; End:
