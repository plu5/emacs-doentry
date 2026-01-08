;;; doentry-mode.el -- A major mode to edit doentry files
;; created 2025-12-01 08:02:48

(defvar doentry-mode-font-lock-keywords
  '(("<.*>" . font-lock-function-name-face)
    ;; [nn:nn] timestamps
    ("\\[[0-9]\\{2\\}:[0-9]\\{2\\}\\]" . font-lock-keyword-face)
    ;; headlines
    ("\\(#+ .*$\\)" . font-lock-type-face)
    ;; quotes
    ("^&gt;[^\n]*\\(?:\n[^\n]+\\)*" . font-lock-preprocessor-face)
    ;; % responses
    ("^% [^\n]*\\(?:\n[^\n]+\\)*" . font-lock-doc-markup-face)
    ;; code blocks
    ("^[ ]\\{4,\\}.+$" . font-lock-string-face)))

(defvar doentry-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; On dit que " n’est plus un délimiteur de chaîne
    (modify-syntax-entry ?\" "." st)
    st))

(defvar doentry-mode-end-string-tag
  "</string>")

(defvar doentry-mode-entry-key
  "Entry Text")

(defvar doentry-mode-separator
  "-----")

(defvar doentry-mode-subheading
  "## ")

;; & doit être en premier parce que sinon &lt; serait transformé en &amp;lt;
(defvar doentry-mode-escapements
  '(("&" . "&amp;")
    ("<" . "&lt;")
    (">" . "&gt;")))

(defun doentry-mode-escape-characters-in-text (text)
  (let ((res text))
    (dolist (item doentry-mode-escapements)
      (setq res (replace-regexp-in-string (car item) (cdr item) res)))
    res))

(defun doentry-mode-unescape-characters-in-text (text)
  ;; même chose mais car et cdr inversés
  (let ((res text))
    (dolist (item doentry-mode-escapements)
      (setq res (replace-regexp-in-string (cdr item) (car item) res)))
    res))

;; de pm.el, magnars. modifié to unescape
(defun doentry-mode-copy-to-end-of-line ()
  (interactive)
  (let* ((beg (point))
         (end (line-end-position))
         (text (buffer-substring-no-properties beg end)))
    (setq text (doentry-mode-unescape-characters-in-text text))
    (kill-new text))
  (message "Copied to end of line"))
(defun doentry-mode-copy-whole-lines (arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (let* ((beg (line-beginning-position))
         (end (line-beginning-position (+ 1 arg)))
         (text (buffer-substring-no-properties beg end)))
    (setq text (doentry-mode-unescape-characters-in-text text))
    (kill-new text)
    (message "%d line%s copied" arg (if (= 1 arg) "" "s"))))
(defun doentry-mode-copy-line (arg)
  "Copy to end of line, or as many lines as prefix argument"
  (interactive "P")
  (if (null arg)
      (doentry-mode-copy-to-end-of-line)
    (doentry-mode-copy-whole-lines (prefix-numeric-value arg))))

(defun doentry-mode-copy (arg)
  (interactive "P")
  (if (region-active-p)
      (let* ((beg (region-beginning))
             (end (region-end))
             (text (buffer-substring-no-properties beg end)))
        (setq text (doentry-mode-unescape-characters-in-text text))
        (kill-new text)
        (deactivate-mark))
    (doentry-mode-copy-line arg)))

(defun doentry-mode-kill-to-end-of-line ()
  (interactive)
  (let* ((beg (point))
         (end (line-end-position))
         (text (delete-and-extract-region beg end)))
    (setq text (doentry-mode-unescape-characters-in-text text))
    (kill-new text))
  (message "Killed to end of line"))

(defun doentry-mode-kill-region ()
  (interactive)
  (if (region-active-p)
      (let* ((beg (region-beginning))
             (end (region-end))
             (text (delete-and-extract-region beg end)))
        (setq text (doentry-mode-unescape-characters-in-text text))
        (kill-new text))))

(defun doentry-mode-yank (arg)
  "Yank with special characters escape <>&
with prefix argument other than 16, also indents lines by
that number of spaces.
With prefix argument 16 (C-u C-u) removes empty lines."
  (interactive "P")
  ;; récupère le dernier élément du kill-ring
  (let* ((text (current-kill 0 t)))
    (setq text (doentry-mode-escape-characters-in-text text))
    (when (not (null arg))
      (if (/= 16 (prefix-numeric-value arg))
          (setq text (replace-regexp-in-string
                      "^" (make-string (prefix-numeric-value arg) ?\s) text))
        (setq text (replace-regexp-in-string "^[[:space:]]*\n" "" text))))
    (insert text)))

(defun doentry-key-before-point ()
  (interactive)
  (when (not (bobp))
    (let ((keybeg "<key>")
          (keyend "</key>"))
      (save-excursion
        (setq end
              (when (re-search-backward keyend nil t)
                (point)))
        (setq beg
              (when (re-search-backward keybeg nil t)
                (+ (point) (length keybeg))))
        (when (and beg end)
          (buffer-substring-no-properties beg end))))))

(defun doentry-mode-end-of-buffer ()
  (interactive)
  (when (not (eobp))
    (let ((reg doentry-mode-end-string-tag)
          (entrykey doentry-mode-entry-key))
      (if (save-excursion
            (when (re-search-forward reg (line-end-position) t)
              (string= (doentry-key-before-point) entrykey)))
          (end-of-buffer)
        (re-search-forward reg nil "a")
        (while (and (not (eobp))
                    (not (string= (doentry-key-before-point) entrykey)))
          (re-search-forward reg nil "a"))
        (when (not (eobp)) (beginning-of-line))))))

(defun doentry-mode-end-of-entry-string ()
  "returns t on success nil not found"
  (interactive)
  (let ((reg doentry-mode-end-string-tag)
        (entrykey doentry-mode-entry-key))
    (beginning-of-buffer)
    (re-search-forward reg nil "a")
    (while (and (not (eobp))
                (not (string= (doentry-key-before-point) entrykey)))
      (re-search-forward reg nil "a"))
    (string= (doentry-key-before-point) entrykey)))

(defun doentry-mode-beginning-of-buffer ()
  (interactive)
  (when (not (bobp))
    (let ((reg "<key>Entry Text</key>[\n\t ]*<string>")
          (point1 (point)))
      (if (re-search-backward reg nil t)
          ;; but the following line should always pass so don't know if i need the when or what to do if not
          (when (re-search-forward reg nil t)
            (when (= point1 (point))
              (beginning-of-buffer)))
        (beginning-of-buffer)))))

(defun doentry-mode-add-to-log (arg)
  "Add a new timestamped entry to the log.
with C-u also inserts a `doentry-mode-subheading' before the entry and
positions the cursor after it.
with C-u C-u does not insert timestamp.
with C-u C-u C-u also inserts a `doentry-mode-separator'.
Doesn't insert anything if entry xml tag not found."
  (interactive "P")
  (when (doentry-mode-end-of-entry-string)
    (beginning-of-line)
    (open-line 2)
    (when (= 64 (prefix-numeric-value arg))
      (insert doentry-mode-separator)
      (newline 2))
    (when (/= 16 (prefix-numeric-value arg))
      (insert (format-time-string "[%H:%M] ")))
    (when (= 4 (prefix-numeric-value arg))
      (beginning-of-line)
      (open-line 1)
      (insert doentry-mode-subheading))))

(defun doentry-mode-meta-return ()
  (interactive)
  (let ((warning-suppress-types '((org-element))))
    (org-meta-return)))

(defvar doentry-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-w") #'doentry-mode-copy)
    (define-key map (kbd "C-k") #'doentry-mode-kill-to-end-of-line)
    (define-key map (kbd "C-w") #'doentry-mode-kill-region)
    (define-key map (kbd "C-y") #'doentry-mode-yank)
    (define-key map (kbd "M->") #'doentry-mode-end-of-buffer)
    (define-key map (kbd "M-<") #'doentry-mode-beginning-of-buffer)
    (define-key map (kbd "C-c C-c") #'doentry-mode-add-to-log)
    (define-key map (kbd "M-<return>") #'doentry-mode-meta-return)
    map))

(define-derived-mode doentry-mode fundamental-mode "doentry"
  "A major mode to edit doentry files."
  (font-lock-add-keywords nil doentry-mode-font-lock-keywords)
  :syntax-table doentry-mode-syntax-table)

(add-to-list 'auto-mode-alist '("\\.doentry\\'" . doentry-mode))

(provide 'doentry-mode)

;;; doentry-mode.el ends here
