;;; autoformat-message.el --- Automatically add format specifiers to message calls -*- lexical-binding: t -*-

;; Copyright © 2020-2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/autoformat-message
;; Version: 0.1.0
;; Keywords: tools
;; Package-Requires: ((emacs "27.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides functionality to automatically format `message' calls
;; in Emacs Lisp code, ensuring that format strings with `%s' escapes are added
;; where necessary.

;;; Code:

(require 'subr-x)

(defcustom autoformat-message-top-level-symbols
  '(defun defmacro defsubst
     define-inline define-advice defadvice
     define-skeleton
     define-compilation-mode
     define-minor-mode define-global-minor-mode
     define-globalized-minor-mode
     define-derived-mode
     define-generic-mode
     ert-deftest cl-defun cl-defsubst cl-defmacro
     cl-define-compiler-macro
     cl-defgeneric
     cl-defmethod define-compiler-macro
     define-modify-macro
     defsetf
     define-setf-expander
     define-method-combination
     defalias cl-flet
     defun-ivy-read
     pretty-hydra-define defhydra defgeneric
     defmethod defun-ivy+
     defgroup deftheme
     define-widget
     transient-define-suffix
     transient-define-argument
     transient-define-prefix
     transient-define-infix
     define-error defface
     cl-deftype cl-defstruct
     deftype defstruct
     defpackage defclass use-package use-package!)
  "Allowed functions to get title."
  :type '(repeat symbol)
  :group 'autoformat-message)

(defcustom autoformat-message-types '(message)
  "Allowed functions calls to format."
  :type '(repeat symbol)
  :group 'autoformat-message)

(defcustom autoformat-message-complete-fn 'autoformat-message-read-elisp-items
  "Function that return symbol name to log.
Used in `autoformat-message-insert-message-or-complete'."
  :type 'function
  :group 'autoformat-message)

(defun autoformat-message-move-with (fn &optional n)
  "Move by calling FN N times.
Return new position if changed, nil otherwise."
  (unless n (setq n 1))
  (when-let* ((str-start (nth 8 (syntax-ppss (point)))))
    (goto-char str-start))
  (let ((init-pos (point))
        (pos)
        (count n))
    (while (and (not (= count 0))
                (when-let* ((end (ignore-errors
                                  (funcall fn)
                                  (point))))
                  (unless (= end (or pos init-pos))
                    (setq pos end))))
      (setq count (1- count)))
    (if (= count 0)
        pos
      (goto-char init-pos)
      nil)))

(defun autoformat-message-backward-up-list (&optional arg)
  "Move backward up across ARG balanced group of parentheses.
Return new position if changed, nil otherwise."
  (autoformat-message-move-with 'backward-up-list arg))

(defmacro autoformat-message-up-list-until-nil (&rest body)
  "Move backward up across and execute BODY until it's return value is nil."
  `(save-excursion
     (let ((result))
       (while (and (null result)
                   (autoformat-message-backward-up-list))
         (setq result (progn ,@body)))
       result)))

(defun autoformat-message-current-function-name ()
  "Return name of the current function as string."
  (autoformat-message-up-list-until-nil
   (when-let* ((sexp (sexp-at-point)))
     (when (listp sexp)
       (let ((type (car sexp))
             (name (nth 1 sexp)))
         (when (memq type  autoformat-message-top-level-symbols)
           (format "%s" name)))))))

(defun autoformat-message--count-specifier-matches (str)
  "Return the count of specifier matches in the string STR.

Argument STR is the string to be searched for specifier matches."
  (when (string-match-p "\\(?:%[SXc-gosx]\\)" str)
    (let ((count 0))
      (with-temp-buffer (insert str)
                        (while
                            (re-search-backward "\\(?:%[SXc-gosx]\\)" nil t 1)
                          (setq count (1+ count))))
      count)))

(defun autoformat-message--fix-message-at-point ()
  "Fix the format string and arguments of a message call at point."
  (let* ((sexp (sexp-at-point))
         (rep
          (pcase sexp
            (`(,(and
                 (pred (symbolp))
                 sym
                 (guard (memq sym autoformat-message-types))
                 sym)
               .
               ,args)
             (pcase args
               (`(,(and
                    (pred (stringp))
                    format-str)
                  .
                  ,format-args)
                (let ((specifiers-count (or (autoformat-message--count-specifier-matches
                                             format-str)
                                            0))
                      (required-count (length format-args)))
                  (cond ((> specifiers-count required-count)
                         (let* ((remove-count
                                 (- specifiers-count required-count))
                                (description (with-temp-buffer
                                               (insert format-str)
                                               (while (> remove-count 0)
                                                 (when (re-search-backward
                                                        "\\(?:%[SXc-gosx]\\)"
                                                        nil t 1)
                                                   (replace-match ""))
                                                 (setq remove-count (1- remove-count)))
                                               (buffer-string))))
                           `(,sym ,description ,@format-args)))
                        ((< specifiers-count required-count)
                         (let ((description (autoformat-message--format-args
                                             (seq-drop format-args
                                                       specifiers-count))))
                           `(,sym ,(concat format-str " " description)
                             ,@format-args))))))
               (_
                (let ((description (autoformat-message--format-args args)))
                  `(,sym ,description ,@args))))))))
    (when rep
      (pcase-let ((`(,beg . ,end)
                   (bounds-of-thing-at-point 'sexp)))
        (replace-region-contents beg end (lambda ()
                                           (prin1-to-string rep)))))))


(defun autoformat-message--format-args (args)
  "Format a list of ARGS into a descriptive string with indexed labels.

Argument ARGS is a list of items to be formatted and concatenated."
  (let ((result)
        (count))
    (while args
      (setq count (1+ (or count 0)))
      (let ((item (car args)))
        (setq args (cdr args))
        (let* ((label
                (pcase item
                  (`(funcall
                     (,(or 'quote 'function)
                      ,(and (pred (symbolp)) sym))
                     .
                     ,_)
                   (format "funcall `%s'" (symbol-name sym)))
                  (`(,(and (pred (symbolp)) sym)
                     .
                     ,_)
                   (format "funcall `%s'" (symbol-name sym)))
                  ((pred vectorp))
                  ((pred listp))
                  ((pred numberp)
                   (format "%s" item))
                  ((pred symbolp)
                   (symbol-name item))
                  ((pred stringp) item)
                  (_ (format "%s" item)))))
          (setq label (concat (unless (and (= count 1)
                                           (not args))
                                (concat (format "%d" count)
                                        (if label ". " "")))
                              label (if label "=") "`%S'"))
          (setq result (push label result)))))
    (string-join (reverse result) ", ")))

;;;###autoload
(defun autoformat-message-cond-clauses ()
  "Format `cond' clauses by inserting or updating `message' statements."
  (interactive)
  (let ((name (autoformat-message-current-function-name)))
    (save-excursion
      (when-let* ((start (autoformat-message-up-list-until-nil
                         (pcase (sexp-at-point)
                           (`(cond ,(pred listp) . ,_rest)
                            (point))))))
        (goto-char start)
        (down-list)
        (forward-sexp 2)
        (forward-sexp -1)
        (let ((count 0))
          (while (ignore-errors
                   (when (listp (sexp-at-point))
                     (save-excursion
                       (down-list 1)
                       (forward-sexp 1)
                       (skip-chars-forward "\s\t\n\r\f")
                       (pcase (sexp-at-point)
                         (`(message
                            ,(and (pred stringp)
                                  (pred (string-match-p (concat
                                                         "^"
                                                         (if name
                                                             (regexp-quote name)
                                                           "")
                                                         " "
                                                         "clause [0-9]+$")))))
                          (pcase-let
                              ((`(,beg . ,end)
                                (bounds-of-thing-at-point 'sexp)))
                            (delete-region beg end))))
                       (insert (if name (format "(message \"%s clause %s\")\n"
                                                name
                                                count)
                                 (format
                                  "(message \"clause %s\")\n" count))))
                     (forward-sexp 2)
                     (forward-sexp -1)
                     (setq count (1+ count))))))
        (when (memq (car-safe (sexp-at-point)) autoformat-message-types)
          (pcase-let ((`(,beg . ,end)
                       (bounds-of-thing-at-point 'sexp)))
            (delete-region beg end)))
        (insert)))))

;;;###autoload
(defun autoformat-message ()
  "Format the message call at the current point if it matches specified types."
  (interactive)
  (save-excursion
    (autoformat-message-up-list-until-nil
     (when-let* ((sexp (sexp-at-point)))
       (when (listp sexp)
         (let ((type (car sexp)))
           (when (memq type autoformat-message-types)
             (autoformat-message--fix-message-at-point))))))))

(defun autoformat-message-insert (item)
  "Insert ITEM, adjusting for the current word at point if it matches a prefix.

Argument ITEM is the string to be inserted into the buffer."
  (apply #'insert
         (if-let* ((current-word
                   (symbol-at-point)))
             (progn
               (if
                   (string-prefix-p
                    (symbol-name
                     current-word)
                    item)
                   (list
                    (substring-no-properties
                     item
                     (length
                      (symbol-name
                       current-word))))
                 (list "\s" item)))
           (list item))))
(defun autoformat-message-read-elisp-items ()
  "Read a symbol from the minibuffer, excluding keywords, with completion."
  (completing-read "Log: "(append
                           (seq-filter (lambda (it)
                                         (and (symbolp it)
                                              (not (keywordp it))
                                              (boundp it)))
                                       obarray)
                           (elisp--local-variables))))

;;;###autoload
(defun autoformat-message-insert-message-or-complete ()
  "Insert a formatted message or complete an item based on context."
  (interactive)
  (let ((item (or (if (functionp autoformat-message-complete-fn)
                      (funcall autoformat-message-complete-fn)
                    (autoformat-message-read-elisp-items)))))
    (if (catch 'inside-message
          (dolist (it (nth 9 (syntax-ppss (point))))
            (save-excursion
              (goto-char it)
              (when (eq (car-safe (sexp-at-point)) 'message)
                (throw 'inside-message t)))))
        (progn
          (autoformat-message-insert item)
          (autoformat-message))
      (insert (concat "(message " item
                      ")"))
      (forward-char -1)
      (autoformat-message))))

;;;###autoload
(define-minor-mode autoformat-message-mode
  "Inside message automatically add format string with %s escapes before save."
  :lighter " autoformat-message"
  :global nil
  (if autoformat-message-mode
      (add-hook 'before-save-hook #'autoformat-message nil 'local)
    (remove-hook 'before-save-hook #'autoformat-message 'local)))

(provide 'autoformat-message)
;;; autoformat-message.el ends here