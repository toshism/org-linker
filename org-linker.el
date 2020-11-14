;;; org-linker.el --- Link things in orgmode          -*- lexical-binding: t; -*-

;; Copyright (C) 2020  tosh

;; Author: tosh <tosh.lyons@gmail.com>
;; Version: 0.1
;; Package-Requires: (org org-ql helm)
;; URL: https://github.com/toshism/org-linker
;; Keywords: convenience, hypermedia

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Link things in org
;;
;; I should describe how it actually works here.

;;; Code:

(require 'helm-org-ql)

(defgroup org-linker nil
  "FIXME: Link things in orgmode")

;; Silence byte compiler
(declare-function helm "ext:helm")
(declare-function helm-org-ql-source "ext:helm-org-ql")
(defvar helm-input-idle-delay)
(defvar helm-org-ql-input-idle-delay)

(defvar org-linker-to-heading nil)

(defcustom org-linker-buffer-function #'org-agenda-files
  "Function used to get a list of Org files or buffers.

Each item can be either a file name or a buffer, since `org-ql'
accepts any of both."
  :type 'function)

(defun org-linker-buffer-mode (&optional buffer-or-name)
  "Return the major mode associated with a buffer.
If BUFFER-OR-NAME is nil return current buffer's mode."
  (buffer-local-value 'major-mode
		      (if buffer-or-name (get-buffer buffer-or-name) (current-buffer))))


(defun org-linker-get-search-buffers ()
  "Return a list of buffers to provide to `helm-org-ql`.

This is defined by `org-linker-buffer-function', and it defaults
to `org-agenda-files'.

If the current buffer is in `org-mode', it is also prepended to
the list."
  ;; this needs to add the buffer you were on before opening a capture
  ;; template too (if it's an org mode file)
  (append (when (and (derived-mode-p 'org-mode)
                     (buffer-file-name))
            (list (current-buffer)))
          (funcall org-linker-buffer-function)))

(defun org-linker-search-interface (callback)
  "Setup the helm-org-ql search interface.
Call CALLBACK with a marker to target heading."
  (let* ((boolean 'and)
         (helm-input-idle-delay helm-org-ql-input-idle-delay)
         (source (helm-org-ql-source (org-linker-get-search-buffers)
                                     :name "org-linker target")))
    ;; You'll need a better action name here
    (setcdr (assoc 'action source) callback)
    (helm :prompt (format "Query (boolean %s): " (upcase (symbol-name boolean)))
          :sources source)))

(defun org-linker-callback-wrapper (callback m)
  "Call user provided CALLBACK with source marker and target marker M.
Recieve the user provided callback and the target marker (M).
Call the user callback with target marker (M) and source marker."
  (save-excursion
    (when org-linker-to-heading
      (org-back-to-heading))
    (let ((source (point-marker))
	  (target m))
      (funcall callback source target))))


(defun org-linker (callback)
  "Call CALLBACK function with two markers.
CALLBACK should be a function that accepts two arguments SOURCE and
TARGET markers.

SOURCE: 'marker-position' is in the heading where org-linker was
called.  If 'org-linker-to-heading' is non-nil, source
'marker-position' will be the first char in the heading.  Otherwise it
will be the point were it was called.

TARGET: 'marker-postion' will be the first char in the target
heading."
  (org-linker-search-interface (apply-partially 'org-linker-callback-wrapper callback)))


(provide 'org-linker)

;;; org-linker.el ends here
