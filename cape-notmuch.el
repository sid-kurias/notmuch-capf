;;; cape-notmuch.el --- arx cape notmuch config -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Trey Peacock
;;
;; Author: Trey Peacock <http://github/tpeacock19>
;; Maintainer: Trey Peacock <git@treypeacock.com>
;; Created: December 22, 2021
;; Modified: December 22, 2021
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/tpeacock19/cape-notmuch
;; Package-Requires: ((emacs 29.0.50) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any
;; later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;;  arx cape notmuch config
;;
;;; Code:

(require 'notmuch-address)

(defvar cape--notmuch-properties
  (list :annotation-function (lambda (_) " Notmuch")
        :company-kind (lambda (_) 'text))
  "Completion extra properties for `cape-notmuch'.")

(declare-function notmuch-lookup-words "notmuch")

(defun prefix-grab (regexp &optional expression limit)
  (when (looking-back regexp limit)
    (or (match-string-no-properties (or expression 0)) "")))

(defun cape-notmuch-lookup-emails (&optional str)
  (when (and (or (derived-mode-p 'message-mode)
                 (derived-mode-p 'org-msg-edit-mode))
             (looking-back
              (concat notmuch-address-completion-headers-regexp ".*")
              (line-beginning-position)))
    (let* ((prefix (prefix-grab "[:,][ \t]*\\(.*\\)" 1 (point-at-bol)))
           (str (if (> (length prefix) 0)
                    prefix
                  (or str "a"))))
      (if (notmuch-address--harvest-ready)
          (progn
            (notmuch-address-harvest-trigger)
            (notmuch-address-matching str))
        (notmuch-address-harvest-trigger)
        (split-string
         (shell-command-to-string
          (string-join `("notmuch" "address" "--format=text" ,str) " "))
         "\n" t)))))

(defun cape-notmuch--emails (str)
  "Return all words from Notmuch matching STR."
  (with-demoted-errors "Notmuch Error: %S"
    (require 'notmuch)
    (cape--silent (cape-notmuch-lookup-emails str))))

;;;###autoload
(defun cape-notmuch (&optional interactive)
  "Complete with Notmuch at point.
If INTERACTIVE is nil the function acts like a capf."
  (interactive (list t))
  (if interactive
      (cape--interactive #'cape-notmuch)
    (let ((bounds (cape--bounds 'word)))
      `(,(car bounds) ,(cdr bounds)
        ,(cape--table-with-properties
          (cape--cached-table (car bounds) (cdr bounds) #'cape-notmuch--emails 'substring)
          :category 'cape-notmuch)
        :exclusive no
        :exit-function (lambda (x _status)
                         (run-hook-with-args 'notmuch-address-post-completion-functions x))
        ,@cape--notmuch-properties))))



(provide 'cape-notmuch)
;;; cape-notmuch.el ends here
