;;; lux-mode.el --- Major mode for editing lux files

;; Copyright 2012-2022 Tail-f Systems AB
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;

;; Author: Håkan Mattsson
;; Version: 1.0
;; Homepage: https://github.com/hawk/lux
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:
;; Major mode for editing lux files.
;;
;; Lux (LUcid eXpect scripting) is a test automation framework with
;; Expect style execution of commands.
;;
;; Make sure lux-mode.el is found in your elisp path, and
;; add to your .emacs:
;;  (require 'lux-mode)


;;; Code:

(require 'cl-lib)
(require 'ring)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation

(defvar lux-keywords
  '(
    "progress" "doc" "enddoc" "doc0" "doc1" "doc2" "doc3" "doc4" "doc5"
    "shell" "newshell" "endshell" "cleanup"
    "macro" "endmacro" "invoke"
    "loop" "endloop"
    "timeout" "sleep"
    ))

(defvar lux-meta-commands
  '("my" "local" "global" "config" "include"))

(defvar lux-events
  '("set-line-terminator" "reset-line-terminator" "set-timestamp"))

(defvar lux-config-params
  '(
    "case_prefix"
    "case_timeout"
    "cleanup_timeout"
    "config_dir"
    "config_name"
    "debug"
    "debug_file"
    "extend_run"
    "fail_when_warning"
    "file_pattern"
    "filter_trace"
    "flush_timeout"
    "hostname"
    "html"
    "junit"
    "log_dir"
    "mode"
    "multiplier"
    "newshell"
    "poll_timeout"
    "post_case"
    "progress"
    "require"
    "rerun"
    "revision"
    "risky_threshold"
    "root_dir"
    "run"
    "shell_args"
    "shell_cmd"
    "shell_prompt_cmd"
    "shell_prompt_regexp"
    "shell_wrapper"
    "shell_wrapper_mode"
    "skip"
    "skip_skip"
    "skip_unless"
    "skip_unstable"
    "sloppy_threshold"
    "suite"
    "suite_timeout"
    "tap"
    "timeout"
    "var"
    ))

(defvar lux-indent 4)
(defvar lux-keywords-regexp (regexp-opt lux-keywords 'words))
(defvar lux-events-regexp (regexp-opt lux-events 'words))
(defvar lux-meta-commands-regexp (regexp-opt lux-meta-commands 'words))
(defvar lux-config-params-regexp (regexp-opt lux-config-params 'words))

(defvar lux-font-lock-keywords
  `(
    (,lux-keywords-regexp . font-lock-keyword-face)
    (,lux-events-regexp . font-lock-builtin-face)
    (,lux-meta-commands-regexp . font-lock-preprocessor-face)
    (,lux-config-params-regexp . font-lock-function-name-face)
    ("^\s*@.*$" . font-lock-warning-face)     ;; match a break regexp
    ("^\s*-.*$" . font-lock-warning-face)     ;; match a failure regexp
    ("^\s*\\+.*$" . font-lock-type-face)      ;; match a success regexp
    ("\\$\\([[:alnum:]_-]+\\)" 1 font-lock-variable-name-face)
    ("\\${\\([[:alnum:]_-]+\\)}" 1 font-lock-variable-name-face)))

(defconst lux-regexp-special-characters "[][^{}()$+*.]")

(defun lux-quote-region (start end)
  "Quote special characters by insert a preceding '\' character in region.
Region is defined by START and END."
  (interactive "r")
  (save-excursion
    (save-match-data
      (goto-char start)
      (let ((end-mark (make-marker))) ; Markers grow as we insert.
        (set-marker end-mark end)
        (while (re-search-forward lux-regexp-special-characters end-mark t)
          (replace-match "\\\\\\&" t )
          ())))))

(defun lux-indent-line ()
  "Indent current line in a lux."
  (interactive)
  ;; Set the point to beginning of line.
  (beginning-of-line)

  ;; check if this is the first line in the buffer
  (if (bobp)
      (indent-line-to 0)

    (let ((done nil)
          (cur-indent nil)
          (comment "^[ \t]*#")
          (major-comment "^[ \t]*##")
          (major-expr "^[ \t]*\\[\\(shell\\|newshell\\|cleanup\\|macro\\)")
          (block-expr "^[ \t]*\\[\\(loop\\)")
          (end-expr "^[ \t]*\\[\\(endloop\\|endmacro\\)"))

      (if (or (looking-at major-expr)
              (looking-at major-comment))
          (setq cur-indent 0)
        (cond
         ((looking-at end-expr)
          (save-excursion
            (forward-line -1)
            (setq cur-indent (max 0 (- (current-indentation) lux-indent)))
            (setq done t)))
         ((looking-at comment)
          ;; check if next non-comment line is a major expression; if so
          ;; assume this comment belongs to the major expression line
          (save-excursion
            (while (and (not (eolp)) (not done))
              (forward-line 1)
              (cond ((looking-at major-expr)
                     (setq cur-indent 0)
                     (setq done t))
                    ((not (looking-at comment))
                     (setq done t))))
            (if (not cur-indent)
                (setq done nil)))))
        (save-excursion
          (while (not done)
            (forward-line -1)
            (cond ((looking-at major-expr)
                   (setq cur-indent lux-indent)
                   (setq done t))
                  ((looking-at block-expr)
                   (setq cur-indent ( + (current-indentation) lux-indent))
                   (setq done t))
                  ((not (looking-at "^[ \t]*$"))
                   (setq cur-indent (current-indentation))
                   (setq done t))
                  ((bobp)
                   (setq done t))))))

      (if cur-indent
          (indent-line-to cur-indent)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find include file or macro def

(defun lux-find-source ()
  "Open an include file or source code for macro."
  (interactive)
  (let* ((keyword-param1 (lux-which-cmd))
         (keyword        (car keyword-param1))
         (param1         (cdr keyword-param1)))
    (cond
     ((equal keyword "include") (lux-find-file param1))
     ((equal keyword "invoke")  (lux-find-macro param1))
     (t (error "Not a lux statement")))))

(defun lux-which-cmd ()
  "Return nil or command name and first parameter."
  (save-excursion
    (let* ((regexp  (lux-build-meta-regexp)))
      (beginning-of-line)
      (when (looking-at regexp)
        (cons (match-string-no-properties 1)
              (match-string-no-properties 2))))))

(cl-defun lux-build-meta-regexp (&optional (keyword "[^ \t]+")
                                           (param1 "[^] \t]+"))
  "Build lux meta statement regexp with two subexpressions:
1 - KEYWORD
2 - PARAM1"
  (concat "[ \t]*\\[\\(" keyword "\\)[ \t]+\\(" param1 "\\)[] \t]"))

(defun lux-find-file (file)
  "Wrapper for `find-file-existing' FILE."
  (let* ((mark (copy-marker (point-marker)))
         (new-file (lux-expand-file file)))
    (find-file-existing new-file)
    (ring-insert-at-beginning (lux-window-history-ring) mark)))

(defun lux-window-history-ring ()
  "Get or create window history ring for selected window."
  (let ((window (selected-window)))
    (or (window-parameter window 'lux-find-history-ring)
        (set-window-parameter window 'lux-find-history-ring (make-ring 20)))))

(defun lux-find-source-unwind ()
  "Unwind back from use of `lux-find-source'."
  (interactive)
  (let ((ring (lux-window-history-ring)))
    (unless (ring-empty-p ring)
      (let* ((marker (ring-remove ring))
             (buffer (marker-buffer marker)))
        (if (buffer-live-p buffer)
            (progn (switch-to-buffer buffer)
                   (goto-char (marker-position marker)))
          ;; If this buffer was deleted, recurse to try the next one
          (lux-find-source-unwind))))))

(defun lux-find-macro (macro)
  "Find MACRO definition."
  (let* ((macro-regexp (lux-build-meta-regexp "macro" macro))
         (case-fold-search nil))
    (or (lux-search-current-buffer macro-regexp)
           (lux-find-external-macro macro-regexp)
           (error (concat "Cannot find the lux macro " macro)))))

(defun lux-search-current-buffer (regexp)
  "Find the first match for REGEXP in the current buffer.
Move point there and make an entry in `lux-window-history-ring'."
  (let ((mark  (copy-marker (point-marker))))
    (goto-char (point-min))
    (if (re-search-forward regexp nil t)
        (progn
          (beginning-of-line)
          (ring-insert-at-beginning (lux-window-history-ring) mark))
        (null (goto-char mark)))))

(defun lux-find-external-macro (macro-regexp)
  "Find external macro matching MACRO-REGEXP."
  (let ((includes (lux-list-includes))
        (found    nil))
    (with-temp-buffer
      (save-excursion
        (while (and includes (not found))
          (let ((cur-file (lux-expand-file (car includes))))
            (insert-file-contents cur-file nil nil nil t)
            (goto-char (point-min))
            (when (re-search-forward macro-regexp nil t)
              (setq found cur-file))
            (pop includes)))))
    (when found
      (lux-find-file found)
      (goto-char (point-min))
      (re-search-forward macro-regexp nil t)
      (beginning-of-line))
    found))

(defun lux-list-includes ()
  "Return a list of lux include files."
  (let ((files nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (lux-build-meta-regexp "include") nil t)
        (setq files (append files (cons (match-string-no-properties 2) nil))))
      files)))

(defun lux-expand-file (orig-file)
  "Expand environment variables in ORIG-FILE and ask user if non-existent."
  (let*
      ((file (expand-file-name (substitute-in-file-name orig-file)))
       (dir (file-name-directory file)))
    (if (not (file-exists-p orig-file))
        (read-file-name
         "OK? "
         "/" ;; dir
         file
        'confirm-after-completion
         file)
      orig-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autoload

(dolist (a '(("\\.lux$"    . lux-mode)
             ("\\.luxcfg$" . lux-mode)
             ("\\.luxinc$" . lux-mode)))
  (add-to-list 'auto-mode-alist a))

(define-derived-mode lux-mode fundamental-mode
  (set (make-local-variable 'mode-name) "lux")
  (set (make-local-variable 'font-lock-defaults) '(lux-font-lock-keywords))

  ;; perl style comment: "# ..."
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-start-skip) "#+\\s-*")
  (modify-syntax-entry ?# "< b" lux-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" lux-mode-syntax-table)
  (modify-syntax-entry ?= "." lux-mode-syntax-table)

  (local-set-key "\M-." 'lux-find-source)
  (local-set-key "\M-," 'lux-find-source-unwind)

  (set (make-local-variable 'indent-line-function) 'lux-indent-line))

(provide 'lux-mode)

;;; lux-mode.el ends here
