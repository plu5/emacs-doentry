;;; doentry-mode.el -- doentry major mode  -*- lexical-binding: t; -*-

;; 2025-12-01 08:02:48
;; Author: plu5
;; Keywords: languages
;; URL: https://github.com/plu5/emacs-doentry

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Major mode for editing doentry files.

;;; Code:

(defgroup doentry-mode nil
  "doentry major mode."
  :group 'languages)

(defcustom doentry-mode-beg-entry-regexp
  "<key>\\(Entry Text\\)</key>[\n\t ]*<string>"
  "Regexp to find the beginning of the Entry Text element
(element that contains the markdown contents of the entry).
Used for navigation and imenu."
  :type 'regexp
  :group 'doentry-mode)

(defcustom doentry-mode-entry-key
  "Entry Text"
  "Entry Text XML key name.
(element that contains the markdown contents of the entry)."
  :type 'string
  :group 'doentry-mode)

(defcustom doentry-mode-end-string-tag
  "</string>"
  "XML end string tag.
Used to find end of Entry Text string."
  :type 'string
  :group 'doentry-mode)

(defcustom doentry-mode-entry-time-string
  "[%H:%M] "
  "Format of timestamp inserted on `doentry-mode-add-to-entry'.
Set this to an empty string to disable inserting timestamps."
  :type 'string
  :group 'doentry-mode)

(defcustom doentry-mode-add16
  "## "
  "Inserted before the update on C-u C-u `doentry-mode-add-to-entry'."
  :type 'string
  :group 'doentry-mode)

(defcustom doentry-mode-add64
  "-----

## "
  "Inserted before the update on C-u C-u C-u `doentry-mode-add-to-entry'."
  :type 'string
  :group 'doentry-mode)

(defcustom doentry-mode-escapements
  '(("&" . "&amp;")
    ("<" . "&lt;")
    (">" . "&gt;"))
  "Alist of substitutions for illegal characters in doentry files.
The substitutions are applied in order. Thus for example & to &amp;
should be before < to &lt; to avoid &lt; being transformed to
&amp;lt;."
  :type '(repeat (cons string string))
  :group 'doentry-mode)

;; taken from markdown-mode `markdown-regex-header'
(defconst doentry-mode-header-regexp
  "^\\(?:\\(?1:[^\r\n\t -].*\\)\n\\(?:\\(?2:=+\\)\\|\\(?3:-+\\)\\)\\|\\(?4:#+[ \t]+\\)\\(?5:.*?\\)\\(?6:[ \t]+#+\\)?\\)$"
  "Regexp identifying Markdown headings.
Group 1 matches the text of a setext heading.
Group 2 matches the underline of a level-1 setext heading.
Group 3 matches the underline of a level-2 setext heading.
Group 4 matches the opening hash marks of an atx heading and whitespace.
Group 5 matches the text, without surrounding whitespace, of an atx heading.
Group 6 matches the closing whitespace and hash marks of an atx heading.")

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
    ;; stop "" being fontified as string
    (modify-syntax-entry ?\" "." st)
    st))

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
    (let ((reg doentry-mode-beg-entry-regexp)
          (point1 (point)))
      (if (re-search-backward reg nil t)
          ;; but the following line should always pass so don't know if i need the when or what to do if not
          (when (re-search-forward reg nil t)
            (when (= point1 (point))
              (beginning-of-buffer)))
        (beginning-of-buffer)))))

(defun doentry-mode-add-to-entry (arg)
  "Add a new timestamped update to the entry.
with C-u does not insert timestamp.
with C-u C-u also inserts a `doentry-mode-add16' before the entry.
with C-u C-u C-u also inserts a `doentry-mode-add64' before the entry.
Doesn't insert anything if entry xml tag not found."
  (interactive "P")
  (when (doentry-mode-end-of-entry-string)
    (beginning-of-line)
    (open-line 2)
    (when (= 16 (prefix-numeric-value arg))
      (insert doentry-mode-add16)
      (newline))
    (when (= 64 (prefix-numeric-value arg))
      (insert doentry-mode-add64)
      (newline))
    (unless (= 4 (prefix-numeric-value arg))
      (insert (format-time-string doentry-mode-entry-time-string)))))

(defun doentry-mode-list-item-prefix-or-nil ()
  "Return prefix of list item or heading at point.
Return nil if not on a list item or heading."
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^[\t ]*[-\\+\\*] \\|^[#]+ \\|^[\t ]*[0-9]+\\. ")
      (match-string-no-properties 0))))

(defun doentry-mode-renumber-list-after-point (offset)
  "Add OFFSET to each list item number after point."
  (save-excursion
    (forward-line 1)
    (while (looking-at "^[\t ]*\\([0-9]+\\)\\. ")
      (let* ((n (string-to-number (match-string 1)))
             (replacement (number-to-string (+ n offset))))
        (replace-match replacement t t nil 1))
      (forward-line 1))))

(defun doentry-mode-meta-return ()
  "Contextual insert new list item or heading.
Intended to behave like `org-meta-return'.
When point is on a list item or heading, create a a new list item or
heading of the same type and indentation. With numbered lists, support
is limited; if an item is inserted in the middle, only list items that
are 1-line long with no empty lines in between will be renumbered."
  (interactive)
  (let ((item-prefix (doentry-mode-list-item-prefix-or-nil)))
    (when item-prefix
      (let ((split-prefix (split-string item-prefix "\\.")))
        ;; numbered list
        (when (string-match-p "^[0-9]+$" (car split-prefix))
          (setf (car split-prefix)
                (number-to-string (1+ (string-to-number (car split-prefix)))))
          (setq item-prefix (mapconcat 'identity split-prefix "."))
          (doentry-mode-renumber-list-after-point 1)))
      (while (eq (char-before) ?\s)
        (delete-char -1))
      (newline)
      (insert item-prefix))))

(defun doentry-mode-before-first-heading-p ()
  (not
   (save-excursion
     (beginning-of-line)
     (re-search-backward doentry-mode-header-regexp nil t))))

(defun doentry-mode-after-last-heading-p ()
  (not
   (save-excursion
     (end-of-line)
     (re-search-forward doentry-mode-header-regexp nil t))))

(defun doentry-mode-next-heading (arg)
  (interactive "p")
  (if (doentry-mode-after-last-heading-p)
      (doentry-mode-end-of-buffer)
    (outline-next-visible-heading arg)))

(defun doentry-mode-previous-heading (arg)
  (interactive "p")
  (if (doentry-mode-before-first-heading-p)
      (doentry-mode-beginning-of-buffer)
    (outline-previous-visible-heading arg)))

(defvar doentry-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-w") #'doentry-mode-copy)
    (define-key map (kbd "C-k") #'doentry-mode-kill-to-end-of-line)
    (define-key map (kbd "C-w") #'doentry-mode-kill-region)
    (define-key map (kbd "C-y") #'doentry-mode-yank)
    (define-key map (kbd "M->") #'doentry-mode-end-of-buffer)
    (define-key map (kbd "M-<") #'doentry-mode-beginning-of-buffer)
    (define-key map (kbd "C-c C-c") #'doentry-mode-add-to-entry)
    (define-key map (kbd "M-<return>") #'doentry-mode-meta-return)
    (define-key map (kbd "C-c C-n") #'doentry-mode-next-heading)
    (define-key map (kbd "C-c C-p") #'doentry-mode-previous-heading)
    map))

;;;###autoload
(define-derived-mode doentry-mode text-mode "doentry"
  "A major mode to edit doentry files."
  (font-lock-add-keywords nil doentry-mode-font-lock-keywords)
  (setq-local outline-regexp doentry-mode-header-regexp)
  (setq-local imenu-generic-expression
              `((nil ,doentry-mode-beg-entry-regexp 1)
                (nil ,doentry-mode-header-regexp 0)))
  (put 'doentry-mode-yank 'delete-selection t)
  :syntax-table doentry-mode-syntax-table)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.doentry\\'" . doentry-mode))

(provide 'doentry-mode)

;;; doentry-mode.el ends here
