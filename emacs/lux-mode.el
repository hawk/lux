;; Copyright 2012-2014 Tail-f Systems AB
;;
;; See the file "LICENSE" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major mode for editing lux files
;; Make sure lux-mode.el is found in your elisp path, and
;; add to your .emacs:
;;  (require 'lux-mode)

(defvar lux-keywords
  '("cleanup" "shell" "timeout" "sleep" "say"
    "doc" "doc0" "doc1" "doc2" "doc3" "doc4" "doc5"
    "macro" "endmacro" "invoke" "endshell"))

(defvar lux-meta-commands
  '("my" "local" "global" "config" "include" "macro" "endshell"))

(defvar lux-events
  '("set-line-terminator" "reset-line-terminator" "set-timestamp"))

(defvar lux-config-params
  '("progress" "debug" "log_dir" "arch" "config_dir" "skip" "skip_unless"
    "timeout" "multiplier" "suite_timeout" "case_timeout" "flush_timeout"
    "poll_timeout" "require" "shell_wrapper" "shell_cmd" "shell_arg"
    "line_term" "file_pattern" "var"))

(defvar lux-indent 4)

(defvar lux-keywords-regexp (regexp-opt lux-keywords 'words))
(defvar lux-events-regexp (regexp-opt lux-events 'words))
(defvar lux-meta-commands-regexp (regexp-opt lux-meta-commands 'words))
(defvar lux-config-params-regexp (regexp-opt lux-config-params 'words))

(setq lux-font-lock-keywords
  `(
    (,lux-keywords-regexp . font-lock-keyword-face)
    (,lux-events-regexp . font-lock-builtin-face)
    (,lux-meta-commands-regexp . font-lock-preprocessor-face)
    (,lux-config-params-regexp . font-lock-function-name-face)
    ;; match a failure regexp
    ("^\s*-.*$" . font-lock-warning-face)
    ;; match a success regexp
    ("^\s*\\+.*$" . font-lock-type-face)
    ("\\$\\([[:alnum:]_-]+\\)" 1 font-lock-variable-name-face)
    ("\\${\\([[:alnum:]_-]+\\)}" 1 font-lock-variable-name-face)))



(defconst lux-regexp-special-characters "[][^{}()$+*.]")

(defun lux-quote-region (start end)
  "Qoute special characters by insert a preceding '\' character."
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
  "Indent current line in a lux"
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
          (major-expr "^[ \t]*\\[\\(shell\\|cleanup\\|macro\\|endmacro\\)"))

      (if (or (looking-at major-expr)
              (looking-at major-comment))
          (setq cur-indent 0)
        (if (looking-at comment)
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
                  (setq done nil))))
        (save-excursion
          (while (not done)
            (forward-line -1)
            (cond ((looking-at major-expr)
                   (setq cur-indent lux-indent)
                   (setq done t))
                  ((not (looking-at "^[ \t]*$"))
                   (setq cur-indent (current-indentation))
                   (setq done t))
                  ((bobp)
                   (setq done t))))))

      (if cur-indent
          (indent-line-to cur-indent)))))

;;;###autoload
(dolist (a '(("\\.lux\\'" . lux-mode)
             ("\\.luxinc\\'" . lux-mode)))
  (add-to-list 'auto-mode-alist a))

 ;;;###autoload
(define-derived-mode lux-mode fundamental-mode
  (set (make-local-variable 'mode-name) "lux")

  (set (make-local-variable 'font-lock-defaults) '(lux-font-lock-keywords))

  ;; perl style comment: "# ..."
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-start-skip) "#+\\s-*")
  (modify-syntax-entry ?# "< b" lux-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" lux-mode-syntax-table)
  (modify-syntax-entry ?= "." lux-mode-syntax-table)

  (set (make-local-variable 'indent-line-function) 'lux-indent-line))

(provide 'lux-mode)
