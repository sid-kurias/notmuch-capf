;;; notmuch-capf.el --- arx cape notmuch config -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Trey Peacock
;;
;; Author: Trey Peacock <http://github/tpeacock19>
;; Maintainer: Trey Peacock <git@treypeacock.com>
;; Created: December 22, 2021
;; Modified: December 22, 2021
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/tpeacock19/notmuch-capf
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

(defvar notmuch-capf--properties
  (list :annotation-function (lambda (_) " Notmuch")
        :company-kind (lambda (_) 'text))
  "Completion extra properties for `notmuch-capf'.")

(declare-function notmuch-lookup-words "notmuch")

(defmacro notmuch-capf--silent (&rest body)
  "Silence BODY."
  (declare (indent 0))
  `(cl-letf ((inhibit-message t)
             (message-log-max nil)
             ((symbol-function #'minibuffer-message) #'ignore))
     (ignore-errors ,@body)))

(defun notmuch-capf--bounds (thing)
  "Return bounds of THING."
  (or (bounds-of-thing-at-point thing) (cons (point) (point))))

(defun notmuch-capf--interactive (capf)
  "Complete with CAPF."
  (let ((completion-at-point-functions (list capf)))
    (or (completion-at-point) (user-error "%s: No completions" capf))))

(cl-defun notmuch-capf--table-with-properties (table &key category (sort t) &allow-other-keys)
  "Create completion TABLE with properties.
CATEGORY is the optional completion category.
SORT should be nil to disable sorting."
  (if (or (not table) (and (not category) sort))
      table
    (let ((metadata `(metadata
                      ,@(and category `((category . ,category)))
                      ,@(and (not sort) '((display-sort-function . identity)
                                          (cycle-sort-function . identity))))))
      (lambda (str pred action)
        (if (eq action 'metadata)
            metadata
          (complete-with-action action table str pred))))))

(defun notmuch-capf--input-valid-p (old-input new-input cmp)
  "Return non-nil if the NEW-INPUT is valid in comparison to OLD-INPUT.
The CMP argument determines how the new input is compared to the old input.
- never: Never treat the input as valid.
- prefix/nil: The old input is a prefix of the new input.
- equal: The old input is equal to the new input.
- substring: The old input is a substring of the new input."
  ;; Treat input as not changed if it contains space to allow
  ;; Orderless completion style filtering.
  (or (string-match-p "\\s-" new-input)
      (pcase-exhaustive cmp
        ('never nil)
        ((or 'prefix 'nil) (string-prefix-p old-input new-input))
        ('equal (equal old-input new-input))
        ('substring (string-match-p (regexp-quote old-input) new-input)))))

(defun notmuch-capf--cached-table (beg end fun valid)
  "Create caching completion table.
BEG and END are the input bounds.
FUN is the function which computes the candidates.
VALID is the input comparator, see `notmuch-capf--input-valid-p'."
  (let ((input 'init)
        (beg (copy-marker beg))
        (end (copy-marker end t))
        (table nil))
    (lambda (str pred action)
      (let ((new-input (buffer-substring-no-properties beg end)))
        (when (or (eq input 'init) (not (notmuch-capf--input-valid-p input new-input valid)))
          ;; NOTE: We have to make sure that the completion table is interruptible.
          ;; An interruption should not happen between the setqs.
          (setq table (funcall fun new-input)
                input new-input)))
      (complete-with-action action table str pred))))

(defun prefix-grab (regexp &optional expression limit)
  (when (looking-back regexp limit)
    (or (match-string-no-properties (or expression 0)) "")))

(defun notmuch-capf--lookup-emails (&optional str)
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

(defun notmuch-capf--emails (str)
  "Return all words from Notmuch matching STR."
  (with-demoted-errors "Notmuch Error: %S"
    (require 'notmuch)
    (notmuch-capf--silent (notmuch-capf--lookup-emails str))))

;;;###autoload
(defun notmuch-capf (&optional interactive)
  "Complete with Notmuch at point.
If INTERACTIVE is nil the function acts like a capf."
  (interactive (list t))
  (if interactive
      (notmuch-capf--interactive #'notmuch-capf)
    (let ((bounds (notmuch-capf--bounds 'word)))
      `(,(car bounds) ,(cdr bounds)
        ,(notmuch-capf--table-with-properties
          (notmuch-capf--cached-table (car bounds) (cdr bounds) #'notmuch-capf--emails 'substring)
          :category 'notmuch-capf)
        :company-kind (lambda (_) 'email)
        :exclusive no
        :exit-function (lambda (x _status)
                         (run-hook-with-args 'notmuch-address-post-completion-functions x))
        ,@notmuch-capf--properties))))



(provide 'notmuch-capf)
;;; notmuch-capf.el ends here
