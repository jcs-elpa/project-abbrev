;;; customize-shortcut.el --- Customize your own complete shortcut in the project.                     -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Shen, Jen-Chieh
;; Created date 2018-06-02 10:15:37

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Remind current line status by current buffer.
;; Keyword: auto-complete customizable shortcut swap
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4") (cl-lib "0.6"))
;; URL: https://github.com/jcs090218/customize-shortcut

;; This file is NOT part of GNU Emacs.

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
;;
;; Customize your own complete shortcut in the project.
;;

;;; Code:

(defgroup customize-shortcut nil
  "Reminder what is the status of each line for current buffer/file."
  :prefix "customize-shortcut-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs090218/customize-shortcut.git"))


(defcustom customize-shortcut-config-file "customize-shortcut.config"
  "File for your own customizable complete file exists in the relative \
root directory."
  :group 'customize-shortcut
  :type 'string)


(defun customize-shortcut-config-file-get-string-from-file (file-path)
  "Return file-path's file content.
FILE-PATH : file path."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun customize-shortcut-parse-ini (file-path)
  "Parse a .ini file.
FILE-PATH : .ini file to parse."
  (let ((tmp-ini (customize-shortcut-config-file-get-string-from-file file-path))
        (tmp-ini-list '())
        (tmp-pair-list nil)
        (tmp-keyword "")
        (tmp-value "")
        (count 0))
    (setq tmp-ini (split-string tmp-ini "\n"))

    (dolist (tmp-line tmp-ini)
      ;; check not comment.
      (when (not (string-match-p "#" tmp-line))
        ;; Split it.
        (setq tmp-pair-list (split-string tmp-line "="))

        ;; Assign to temporary variables.
        (setq tmp-keyword (nth 0 tmp-pair-list))
        (setq tmp-value (nth 1 tmp-pair-list))

        ;; Check empty value.
        (when (and (not (string= tmp-keyword ""))
                   (not (equal tmp-value nil)))
          (let ((tmp-list '()))
            (push tmp-keyword tmp-list)
            (setq tmp-ini-list (append tmp-ini-list tmp-list)))
          (let ((tmp-list '()))
            (push tmp-value tmp-list)
            (setq tmp-ini-list (append tmp-ini-list tmp-list)))))
      (setq count (1+ count)))

    ;; return list.
    tmp-ini-list))

(defun customize-shortcut-get-properties (ini-list in-key)
  "Get properties data.  Search by key and return value.
INI-LIST : ini list.  Please use this with/after using
`customize-shortcut-parse-ini' function.
IN-KEY : key to search for value."
  (let ((tmp-index 0)
        (tmp-key "")
        (tmp-value "")
        (returns-value ""))

    (while (< tmp-index (length ini-list))
      ;; Get the key and data value.
      (setq tmp-key (nth tmp-index ini-list))
      (setq tmp-value (nth (1+ tmp-index) ini-list))

      ;; Find the match.
      (when (string= tmp-key in-key)
        ;; return data value.
        (setq returns-value tmp-value))

      ;; Search for next key word.
      (setq tmp-index (+ tmp-index 2)))

    ;; Found nothing, return empty string.
    returns-value))


;;;###autoload
(defun customize-shortcut-complete-word ()
  "Complete the current word that point currently on."
  (interactive)

  (let ((config-filepath (concat (cdr (project-current))
                                 customize-shortcut-config-file))
        (current-word (thing-at-point 'word))
        (swap-word ""))
    (save-excursion
      ;; Get the corresponding target complete word.
      (setq swap-word (customize-shortcut-get-properties config-filepath current-word))

      ;; Swap word cannot be empty string.
      (unless (string= "" swap-word)
        (backward-kill-word 1)
        (insert )
        ))))


(provide 'customize-shortcut)
;;; customize-shortcut.el ends here
