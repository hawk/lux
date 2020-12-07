;; Copyright 2012-2020 Tail-f Systems AB
;;
;; See the file "LICENSE" for information on usage and redistribution
;; of this file, and for a DISCLAIMER OF ALL WARRANTIES.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major mode for editing lux files
;; Make sure lux-mode.el is found in your elisp path, and
;; add to your .emacs:
;;  (require 'lux-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation

(defvar lux-keywords
  '("shell" "endshell" "cleanup"
    "timeout" "sleep"
    "doc" "doc0" "doc1" "doc2" "doc3" "doc4" "doc5"
    "loop" "endloop"
    "macro" "endmacro" "invoke"))

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
          (major-expr "^[ \t]*\\[\\(shell\\|cleanup\\|macro\\)")
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
  "Open an include file or source code for macro"
  (interactive)
  (let* ((keyword-param1 (lux-which-cmd))
         (keyword        (car keyword-param1))
         (param1         (cdr keyword-param1)))
    (cond
     ((equal keyword "include") (lux-find-file param1))
     ((equal keyword "invoke")  (lux-find-macro param1))
     (t (error "Not a lux statement")))))

(defun lux-which-cmd ()
  "Return nil or command name and first parameter"
  (save-excursion
    (let* ((regexp  (lux-build-meta-regexp)))
      (beginning-of-line)
      (when (looking-at regexp)
        (cons (match-string-no-properties 1)
              (match-string-no-properties 2))))))

(cl-defun lux-build-meta-regexp (&optional (keyword "[^ \t]+")
                                           (param1 "[^] \t]+"))
  "Build lux meta statement regexp with two subexpressions:
   1 - keyword
   2 - first parameter"
  (concat "[ \t]*\\[\\(" keyword "\\)[ \t]+\\(" param1 "\\)[] \t]"))

(defun lux-find-file (file)
  "Wrapper for find-file-existing."
  (let ((new-file (lux-check-include file))
        (mark (copy-marker (point-marker))))
    (find-file-existing new-file)
    (ring-insert-at-beginning (lux-window-history-ring) mark)))

(defun lux-window-history-ring ()
  (let ((window (selected-window)))
    (or (window-parameter window 'lux-find-history-ring)
        (set-window-parameter window 'lux-find-history-ring (make-ring 20)))))

(defun lux-find-source-unwind ()
  "Unwind back from uses of lux-find-source."
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
  "Find macro definition"
  (let* ((macro-regexp (lux-build-meta-regexp "macro" macro))
         (case-fold-search nil))
    (or (lux-search-current-buffer macro-regexp)
           (lux-find-external-macro macro-regexp)
           (error (concat "Cannot find the lux macro " macro)))))

(defun lux-search-current-buffer (regexp)
  "Find the first match for RE in the current buffer. Move point there
and make an entry in lux-window-history-ring."
  (let ((mark  (copy-marker (point-marker))))
    (goto-char (point-min))
    (if (re-search-forward regexp nil t)
        (progn
          (beginning-of-line)
          (ring-insert-at-beginning (lux-window-history-ring) mark))
        (null (goto-char mark)))))

(defun lux-find-external-macro (macro-regexp)
  ""
  (let* ((mark         (copy-marker (point-marker)))
         (includes     (lux-list-includes))
         (locals       (directory-files "./" 'full "luxinc"))
         (search-files (append includes locals))
         (found        nil))
    (with-temp-buffer
      (save-excursion
        (while (and search-files (not found))
          (insert-file-contents (car search-files) nil nil nil t)
          (goto-char (point-min))
          (when (re-search-forward macro-regexp nil t)
            (setq found (car search-files)))
          (pop search-files))))
    (when found
      (lux-find-file found)
      (goto-char (point-min))
      (re-search-forward macro-regexp nil t)
      (beginning-of-line))
    found))

(defun lux-list-includes ()
  "Return a list of lux include files"
  (save-excursion
    (let ((include-files nil)
          (child-include-files nil))
      (goto-char (point-min))
      (while (re-search-forward (lux-build-meta-regexp "include") nil t)
        (let* ((path (match-string-no-properties 2))
               (new-path (lux-check-include path))
               (commented (lux-check-commented))
               (test (and (not (equal new-path nil))
                          (equal commented nil))))
          (when test
            (with-current-buffer
                (find-file-noselect new-path)
              (setq child-include-files (lux-list-includes))
              (when child-include-files
                (setq include-files
                      (append include-files child-include-files))))
            (setq include-files (append include-files (cons new-path nil))))))
      include-files)))

(defun lux-check-include (path)
  "Give a chance to lookup enviroment variable.
If the path including enviroment variable, try to lookup the value.
If not possible to lookup the value, return nil."
  (if (string-match "\\$" path)
      (let ((new-path (substitute-in-file-name path)))
        (if (equal new-path path) nil new-path))
    (expand-file-name path)))

(defun lux-check-commented ()
  "Check whether the include file is commented or not.
If the include file is commented, return nil."
  (save-excursion
    (let ((stop (point))
          (start (progn (re-search-backward "^\s*#*\s*\\[")
                        (point))))
      (narrow-to-region start stop)
      (setq commented (condition-case nil
                          (re-search-forward "^\s*#+\s*\\[")
                        (error nil)))
      (widen)
      commented)))

(defun lux-check-include (path)
  "Give a chance to lookup enviroment variable.
If the path including enviroment variable, try to lookup the value.
If not possible to lookup the value, return nil."
  (if (string-match "\\$" path)
      (let ((new-path (substitute-in-file-name path)))
        (if (equal new-path path) nil new-path))
    path))

(defun lux-check-commented ()
  "Check whether the include file is commented or not.
If the include file is commented, return nil."
  (save-excursion
    (let ((stop (point))
          (start (progn (re-search-backward "^\s*#*\s*\\[")
                        (point))))
      (narrow-to-region start stop)
      (setq commented (condition-case nil
                          (re-search-forward "^\s*#+\s*\\[")
                        (error nil)))
      (widen)
      commented)))

(defun lux-check-include (path)
  "Give a chance to lookup enviroment variable.
If the path including enviroment variable, try to lookup the value.
If not possible to lookup the value, return nil."
  (if (string-match "\\$" path)
      (let ((new-path (substitute-in-file-name path)))
        (if (equal new-path path) nil new-path))
    path))

(defun lux-check-commented ()
  "Check whether the include file is commented or not.
If the include file is commented, return nil."
  (save-excursion
    (let ((stop (point))
          (start (progn (re-search-backward "^\s*#*\s*\\[")
                        (point))))
      (narrow-to-region start stop)
      (setq commented (condition-case nil
                          (re-search-forward "^\s*#+\s*\\[")
                        (error nil)))
      (widen)
      commented)))

(defun lux-expand-file (orig-file)
  "Expand environment variables and ask user if non-existent"
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

(dolist (a '(("\\.lux$" . lux-mode)
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
