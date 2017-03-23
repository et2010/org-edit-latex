;;; org-lfe.el --- Org LaTeX Fragment Editor

;; Copyright (C) 2017-2018 James Wong

;; Author:  James Wong <jianwang.academic@gmail.com>
;; Keywords: convenience
;; Version: 0.3.0
;; Package-Requires: ((emacs "24.4"))

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

;; This package will let you edit a latex fragment like editing a src code
;; block. It's very easy to use. Just move the cursor to the fragment you want
;; to change and use `org-edit-special' to edit the fragment in a dedicated latex
;; buffer with all the fancy features provided by AucTeX (presuming you already
;; have it), like completion, highlighting and auto-indent. When you are done
;; editing, just exit the buffer with `org-edit-src-exit'.

;; Note that currently only latex environment or display math, i.e. latex
;; fragments wrapped by $$ (double dollar), \[\] and \begin{} ... \end{} are
;; supported. Since I don't think it's a good idea to use complicated inline
;; equations and I want to keep this package simple. If you think different,
;; please contact me.

;;; Code:

(require 'org)
(require 'org-element)

(setq org-src-preserve-indentation t)

(defun org-lfe-wrap-latex-fragment ()
  "Wrap latex fragment in a latex src block."
  (interactive)
  (let* ((ele (org-element-context))
         (beg (org-element-property :begin ele))
         (end (org-element-property :end ele))
         (nb (org-element-property :post-blank ele))
         (env-p (save-excursion
                  (goto-char beg)
                  (looking-at-p "^[ \t]*\\\\begin"))))
    (when (memq (org-element-type ele)
                '(latex-fragment latex-environment))
      (save-excursion
        (cond
         (env-p
          (goto-char end)
          (when (not (and (eobp)
                          (equal 0 nb)
                          (save-excursion
                            (beginning-of-line)
                            (looking-at-p "[ \t]*\\\\end{"))))
            (forward-line (- (1+ nb)))
            (end-of-line))
          (insert "\n#+END_SRC")
          (goto-char beg)
          (insert "#+BEGIN_SRC latex\n"))
         (t
          (goto-char end)
          (insert "\n#+END_SRC")
          (goto-char beg)
          (beginning-of-line)
          (insert "#+BEGIN_SRC latex\n")))))))

(defun org-lfe-unwrap-latex-fragment (&rest args)
  "Unwrap latex fragment."
  (interactive)
  (let* ((ele (org-element-context))
         (lang (org-element-property :language ele))
         (beg (org-element-property :begin ele))
         (end (org-element-property :end ele))
         (nb (org-element-property :post-blank ele)))
    (when (and (eq 'src-block
                   (org-element-type ele))
               (string= "latex" lang))
      (save-excursion
        (goto-char end)
        (if (and (eobp)
                 (equal 0 nb)
                 (save-excursion
                   (beginning-of-line)
                   (looking-at-p "#\\+end_src")))
            (delete-region (point-at-bol) (point-at-eol))
          (forward-line (- (1+ nb)))
          (delete-region (point-at-bol) (1+ (point-at-eol))))
        (goto-char beg)
        (delete-region (point-at-bol) (1+ (point-at-eol)))))))

(defun org-lfe-wrap-latex-fragment-maybe (&rest args)
  "Wrap a latex fragment with \"begin_src latex\" and \"end_src\".
This only works on display math."
  (when (save-excursion
          (goto-char (org-element-property :begin (org-element-context)))
          ;; display math :
          (looking-at-p "[ \t]*\\$\\$\\|[ \t]*\\\\\\[\\|[ \t]*\\\\begin"))
    (org-lfe-wrap-latex-fragment)))

(advice-add #'org-edit-special :before #'org-lfe-wrap-latex-fragment-maybe)
(advice-add #'org-edit-src-exit :after #'org-lfe-unwrap-latex-fragment '((depth . 100)))


(provide 'org-lfe)
;;; org-lfe.el ends here
