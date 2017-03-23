;;; org-edit-latex.el --- Edit LaTeX at New Buffer

;; Copyright (C) 2017-2018 James Wong

;; Author:  James Wong <jianwang.academic@gmail.com>
;; Keywords: convenience
;; Version: 0.5.0
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
;; block.

;; It's very easy to use. Just toggle this feature on with
;; `org-edit-latex-toggle'. Then you can move the cursor to the fragment you
;; want  to change and use `org-edit-special' to edit the fragment in a
;; dedicated latex buffer. When you are done editing, just exit the buffer with
;; `org-edit-src-exit'.

;; Note that currently only latex environment or display math, i.e. latex
;; fragments wrapped by $$ ... $$ (double dollar), \[ ... \] and \begin{} ...
;; \end{} are supported. Since I don't think it's a good idea to use complicated
;; inline equations and I want to keep this package simple. If you think
;; different, please contact me.

;;; Code:

(require 'org)
(require 'org-element)

(defvar org-edit-latex-enable nil
  "Indicating whether LaTeX fragment editor is enabled.")

(defun org-edit-latex--wrap-latex ()
  "Wrap latex fragment in a latex src block."
  (let* ((ele (org-element-context))
         (beg (org-element-property :begin ele))
         (end (org-element-property :end ele))
         (nb (org-element-property :post-blank ele))
         (type (save-excursion
                 (goto-char beg)
                 (cond
                  ((looking-at-p "^[ \t]*\\\\begin") 'environment)
                  ((looking-at-p "\\\\(\\|\\$[^$]") 'inline)
                  (t nil)))))
    (when (memq (org-element-type ele)
                '(latex-fragment latex-environment))
      (save-excursion
        (cond
         ((eq type 'environment)
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
         ((eq type 'inline)
          (goto-char (- end nb))
          (insert "}")
          (goto-char beg)
          (insert "src_latex{"))
         (t
          (goto-char end)
          (insert "\n#+END_SRC")
          (goto-char beg)
          (beginning-of-line)
          (insert "#+BEGIN_SRC latex\n")))))))

(defun org-edit-latex--unwrap-latex (&rest args)
  "Unwrap latex fragment."
  (let* ((ele (org-element-context))
         (lang (org-element-property :language ele))
         (beg (org-element-property :begin ele))
         (end (org-element-property :end ele))
         (nb (org-element-property :post-blank ele))
         (type (org-element-type ele)))
    (when (string= "latex" lang)
      (if (eq 'src-block type)
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
            (delete-region (point-at-bol) (1+ (point-at-eol))))
        ;; inline src block
        (delete-region (- end nb 1) (- end nb))
        (delete-region beg (+ beg 10))))))

(defun org-edit-latex--wrap-maybe (oldfun &rest args)
  "Wrap element at point if its type is latex-fragment or
latex-environment."
  (if (memq (org-element-type (org-element-context))
            '(latex-fragment latex-environment))
      (progn
        (org-edit-latex--wrap-latex)
        (let ((org-src-preserve-indentation t))
          (apply oldfun args)))
    (apply oldfun args)))

;;;###autoload
(defun org-edit-latex-toggle (&optional force-enable)
  "Toggle LaTeX editing."
  (interactive)
  (setq org-edit-latex-enable
        (or force-enable (not org-edit-latex-enable)))
  (if org-edit-latex-enable
      (progn
        (message "LaTeX editing is enabled.")
        (advice-add #'org-edit-special :around #'org-edit-latex--wrap-maybe)
        (advice-add #'org-edit-src-exit :after #'org-edit-latex--unwrap-latex '((depth . 100))))
    (message "LaTeX editing is disabled.")
    (advice-remove #'org-edit-special #'org-edit-latex--wrap-maybe)
    (advice-remove #'org-edit-src-exit #'org-edit-latex--unwrap-latex)))


(provide 'org-edit-latex)
;;; org-edit-latex.el ends here
