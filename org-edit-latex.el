;;; org-edit-latex.el --- Edit embedded LaTeX in a dedicated buffer

;; Copyright (C) 2017-2018 James Wong

;; Author: James Wong <jianwang.academic@gmail.com>
;; URL: https://github.com/et2010/org-edit-latex
;; Keywords: org, LaTeX
;; Version: 0.7.0
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

;; Usage
;; =====

;; First, turn on `org-edit-latex-mode'. Then you can edit a LaTeX fragment just
;; as what you'll do to edit a src block.

;; Use `org-edit-special' to enter a dedicated LaTeX buffer.
;; Use `org-edit-src-exit' to exit LaTeX buffer when you finished editing.
;; Use `org-edit-src-abort' to quit editing without saving changes.

;; Note that all above commands are built-in Org commands, so your current
;; keybindings to them will do the job.

;;; Code:

(require 'org)
(require 'org-element)

(defvar-local org-edit-latex--before-type nil
  "Element type before wrapping.")

(defconst org-edit-latex-inline-beg-regexp
  "\\\\(\\|\\$[^$]\\|\\\\\\sw"
  "Regexp to match beginning of inline LaTeX")

;;;###autoload
(define-minor-mode org-edit-latex-mode
  "LaTeX editing in org mode."
  :lighter " Edit-LaTeX"
  (if org-edit-latex-mode
      (progn
        (advice-add #'org-edit-special :around #'org-edit-latex--wrap-maybe)
        (advice-add #'org-edit-src-exit :around #'org-edit-latex--unwrap-maybe))
    (advice-remove #'org-edit-special #'org-edit-latex--wrap-maybe)
    (advice-remove #'org-edit-src-exit #'org-edit-latex--unwrap-maybe)))


(defun org-edit-latex--wrap-latex (ele)
  "Wrap latex fragment in a latex src block."
  (let* ((beg (org-element-property :begin ele))
         (end (org-element-property :end ele))
         (pb (org-element-property :post-blank ele))
         (pa (org-element-property :post-affiliated ele))
         (type (cond
                ((eq (car ele) 'latex-environment) 'environment)
                ((save-excursion
                   (goto-char beg)
                   (looking-at-p org-edit-latex-inline-beg-regexp)) 'inline)
                (t nil)))
         (pt (point)))
    (save-excursion
      (cond
       ;; latex environment
       ((eq type 'environment)
        (goto-char end)
        (when (not (and (eobp)
                        (equal 0 pb)
                        (save-excursion
                          (beginning-of-line)
                          (looking-at-p "[ \t]*\\\\end{"))))
          (forward-line (- (1+ pb)))
          (end-of-line))
        (insert "\n#+END_SRC")
        (goto-char (setq beg pa))
        (insert "#+BEGIN_SRC latex\n"))
       ;; inline latex fragment
       ((eq type 'inline)
        (goto-char (- end pb))
        (insert "}")
        (goto-char beg)
        (insert " src_latex{"))
       ;; display latex fragment
       (t
        (goto-char end)
        (insert "\n#+END_SRC")
        (goto-char beg)
        (beginning-of-line)
        (insert "#+BEGIN_SRC latex\n"))))
    (when (= pt beg) (goto-char (1+ pt)))))

(defun org-edit-latex--unwrap-latex (ele)
  "Unwrap latex fragment."
  (let* ((lang (org-element-property :language ele))
         (beg (org-element-property :begin ele))
         (end (org-element-property :end ele))
         (pa (org-element-property :post-affiliated ele))
         (pb (org-element-property :post-blank ele))
         (type (car ele)))
    (cond ((eq 'src-block type)
           (save-excursion
             (goto-char end)
             (if (and (eobp)
                      (equal 0 pb)
                      (save-excursion
                        (beginning-of-line)
                        (looking-at-p "#\\+end_src")))
                 (delete-region (point-at-bol) (point-at-eol))
               (forward-line (- (1+ pb)))
               (delete-region (point-at-bol) (1+ (point-at-eol))))
             (goto-char pa)
             (delete-region pa (1+ (point-at-eol)))))
          ;; inline src block
          ((eq 'inline-src-block type)
           (save-excursion
             ;; delete trailing "}"
             (goto-char (- end pb 1))
             (delete-char 1)
             ;; delete " src_block{", note there is a space before "src_block{"
             (goto-char (1- beg))
             (delete-char 11)))
          (t nil))))

(defun org-edit-latex--unwrap-maybe (oldfun &rest args)
  "Unwrap latex fragment only if it meets certain predicates."
  (if (and (boundp 'org-src--beg-marker)
           (let ((beg org-src--beg-marker))
             (save-excursion
               (set-buffer (marker-buffer beg))
               (goto-char beg)
               (eq 'inline-src-block (car (org-element-context))))))
      (let ((org-src--remote t))
        (funcall oldfun))
    (funcall oldfun))
  (when (and org-edit-latex-mode
             (memq org-edit-latex--before-type
                   '(latex-fragment latex-environment)))
    (org-edit-latex--unwrap-latex (org-element-context))
    (setq org-edit-latex--before-type nil)))

(defun org-edit-latex--wrap-maybe (oldfun &rest args)
  "Wrap element at point if its type is latex-fragment or
latex-environment."
  (if org-edit-latex-mode
      (let* ((ele (org-element-context))
             (type (car ele)))
        (setq org-edit-latex--before-type type)
        (if (or (eq type 'latex-fragment)
                (and (eq type 'latex-environment)
                     (save-excursion
                       (beginning-of-line)
                       (not (looking-at-p "^#\\+")))))
            (progn
              (org-edit-latex--wrap-latex ele)
              (let ((org-src-preserve-indentation t))
                (apply oldfun args)))
          (apply oldfun args)))
    (apply oldfun args)))


(provide 'org-edit-latex)
;;; org-edit-latex.el ends here
