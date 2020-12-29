;;; elscreen-tab--emacs27.el --- helper function defined since Emacs27  -*- lexical-binding: t; -*-

;; Copyright (C) 2020

;; Author: Aki Syunsuke <sunny.day.dev@gmail.com>
;; URL: https://github.com/aki-s/elscreen-tab
;; Package-Version: 0.0.0
;; Package-Requires: ((emacs "26"))
;; Keywords: lisp
;; Created: 2020-12-29
;; Updated: 2020-12-29T13:10:07Z; # UTC

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

;; Functions for backword compatibility of Emacs.

;;; Code:


(cond
  ((string-lessp emacs-version "27.1")
    (defun elscreen-tab--emacs27-window-state-buffers (state)
      "Return all buffers saved to the given window state STATE."
      ;; Accustom to the behavior of `window-state-get' of
      ;; GNU Emacs 26.3 on Darwin.
      ;; `window-state-get' returns buffer object on Emacs 27 instead.
      (let ((buffer (cadr (assq 'buffer state)))
             (buffers (mapcan (lambda (item)
                                (when (memq (car item) '(leaf vc hc))
                                  (elscreen-tab--emacs27-window-state-buffers item)))
                        (if (consp (car state)) (list (cdr state)) (cdr state)))))
        (if buffer (cons (get-buffer buffer) buffers) buffers))))
  (t
    (defalias #'elscreen-tab--emacs27-window-state-buffers #'window-state-buffers)))

;;------------------------------------------------
;; Unload function:

(defun elscreen-tab-emacs27-unload-function ()
   "Unload function to ensure normal behavior when feature 'elscreen-tab--emacs27 is unloaded."
   (interactive))

(provide 'elscreen-tab--emacs27)
;;; elscreen-tab--emacs27.el ends here

;; Local variables:
;; eval: (add-hook 'write-file-functions 'time-stamp)
;; time-stamp-start: ";; Updated:"
;; time-stamp-format: " %:y-%02m-%02dT%02H:%02M:%02SZ"
;; time-stamp-line-limit: 13
;; time-stamp-time-zone: "UTC"
;; time-stamp-end: "; # UTC"
;; End:
