;;; autoformat-message.el --- Configure message -*- lexical-binding: t -*-

;; Copyright © 2020-2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/autoformat-message
;; Version: 0.1.0
;; Keywords: tools
;; Package-Requires: ((emacs "27.1"))

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

;; This file configures operations with message

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

(defun autoformat-message-move-with (fn &optional n)
  "Move by calling FN N times.
Return new position if changed, nil otherwise."
  (unless n (setq n 1))
  (when-let ((str-start (nth 8 (syntax-ppss (point)))))
    (goto-char str-start))
  (let ((init-pos (point))
        (pos)
        (count n))
    (while (and (not (= count 0))
                (when-let ((end (ignore-errors
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
   (when-let ((sexp (sexp-at-point)))
     (when (listp sexp)
       (let ((type (car sexp))
             (name (nth 1 sexp)))
         (when (memq type  autoformat-message-top-level-symbols)
           (format "%s" name)))))))

(defun autoformat-message-make-description (args)
  "Message make description.
ARGS is ."
  (setq args (mapcar (lambda (it)
                       (capitalize
                        (replace-regexp-in-string
                         "[^a-zZ-A0-9]"
                         "\s"
                         (format "%s" it))))
                     args))
  (let ((result-list)
        (max (length (car (seq-sort-by #'length '> args))))
        (align-indent 1))
    (dotimes (i (length args))
      (let* ((it (string-trim (nth i args)))
             (indent (+ (- max (length it))
                        align-indent))
             (parts (list
                     "|\s"
                     it
                     (make-string indent ?\ )
                     "\s|\s"
                     "%s"))
             (res (apply #'concat parts)))
        (push res result-list)))
    (string-join (reverse result-list) "\n")))

(defun autoformat-message-join-vars (sexps &optional col)
  "Format SEXPS with indent COL."
  (unless col (setq col 1))
  (with-temp-buffer
    (erase-buffer)
    (goto-char (point-min))
    (let ((strs (mapcar #'prin1-to-string sexps))
          (row)
          (acc-length 0))
      (while (setq row (pop strs))
        (let* ((l (length row))
               (sum (+ acc-length l)))
          (if (>= sum (- fill-column (or col 0)))
              (progn
                (insert "\n" (make-string col ?\ ) row)
                (setq acc-length (length row)))
            (insert "\s" row)
            (setq acc-length (+ 2 sum)))))
      (string-trim
       (buffer-substring-no-properties
        (point-min)
        (point-max))))))

(defun autoformat-message-make-message-str (sexp &optional col title)
  "Make message from SEXP with TITLE.
COL is column number for format string."
  (let* ((current-description
          (when (stringp (nth 1 sexp))
            (nth 1 sexp)))
         (parts (seq-drop sexp (if current-description 2 1)))
         (func (prin1-to-string (car sexp)))
         (rows (split-string
                (autoformat-message-make-description (mapcar
                                                      #'prin1-to-string
                                                      parts))
                "\n"))
         (description (string-join
                       (seq-map-indexed (lambda (it idx)
                                          (if (= idx 0)
                                              it
                                            (concat
                                             (make-string
                                              (+ 2 col) ?\ )
                                             it)))
                                        rows)
                       "\n"))
         (vars (autoformat-message-join-vars parts
                                             (+ 1 (or col 2)))))
    (if (> (length parts) 0)
        (let ((indent (make-string
                       (+ 1 (or col
                                2))
                       ?\ )))
          (concat "(" func "\s" "\""
                  (if title
                      (concat (format "%s" title)
                              "\n"
                              indent "\s")
                    "")
                  description "\"\n" indent
                  vars ")"))
      (when (and current-description
                 (string-match-p "%s"
                                 current-description))
        (string-join (split-string current-description "%s" t) "\s")))))

(defun autoformat-message ()
  "Inside message automatically add format string with %s escapes."
  (interactive)
  (save-excursion
    (autoformat-message-up-list-until-nil
     (when-let ((sexp (sexp-at-point)))
       (when (listp sexp)
         (let ((type (car sexp)))
           (when (memq type autoformat-message-types)
             (when-let* ((bounds (bounds-of-thing-at-point 'sexp))
                         (column
                          (+ (current-column)
                             (1+
                              (length
                               (prin1-to-string
                                (car
                                 (memq type
                                       autoformat-message-types)))))))
                         (start (car bounds))
                         (end (cdr bounds)))
               (when-let ((rep
                           (save-match-data
                             (autoformat-message-make-message-str
                              sexp column
                              (autoformat-message-current-function-name)))))
                 (replace-region-contents start end (lambda ()
                                                      rep)))))))))))

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