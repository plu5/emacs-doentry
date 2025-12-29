;;; doentry-gen.el -- create-doentry
;; created 2025-01-05 09:59:34

(setq doentry-template "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">
<plist version=\"1.0\">
<dict>
	<key>Activity</key>
	<string>Stationary</string>
	<key>Creation Date</key>
	<date>{date}</date>
	<key>Entry Text</key>
	<string># {heading}

■</string>
	<key>Starred</key>
	<false />
	<key>UUID</key>
	<string>{uuid}</string>
	<key>Creator</key>
	<dict>
		<key>Device Agent</key>
		<string>PC</string>
		<key>Generation Date</key>
		<date>{date}</date>
		<key>Host Name</key>
		<string>{host}</string>
		<key>OS Agent</key>
		<string>{os}</string>
		<key>Software Agent</key>
		<string>Emacs</string>
	</dict>
</dict>
</plist>")

(defun doentry-uuid ()
  (upcase (string-replace "-" "" (org-id-uuid))))

(defun doentry-date ()                  ; UTC
  (format-time-string "%FT%TZ" nil t))

(defun doentry-host ()
  (system-name))

(defun doentry-os ()
  ;; system-type and window-system are symbols so have to be converted
  ;; to string before concat
  (concat (symbol-name system-type) " (" (symbol-name window-system) ") "
          operating-system-release))

(defun latest-file (path &optional match)
  (car (sort (directory-files path 'full match t) #'file-newer-than-file-p)))

(defun doentry-latest-log-contents ()
  ;; requires p-logs-dir to be defined
  (let ((latest-log-path (latest-file p-logs-dir ".*\\.doentry$")))
    (with-temp-buffer
      (insert-file-contents latest-log-path)
      (buffer-string))))

(defun doentry-n ()
  "Return next log number"
  ;; requires p-logs-dir to be defined
  (let ((latest-log-contents (doentry-latest-log-contents)))
    (if (string-match "# \\([0-9]+\\) |" latest-log-contents)
        (+ 1 (string-to-number (match-string 1 latest-log-contents)))
      1)))

(defun doentry-heading ()
  (concat (format "%05d" (doentry-n)) (format-time-string " | %F %a")))

(defun doentry-populated-template (uuid)
  (string-replace
   "{heading}" (doentry-heading)
   (string-replace
    "{os}" (doentry-os)
    (string-replace
     "{host}" (doentry-host)
     (string-replace
      "{date}" (doentry-date)
      (string-replace "{uuid}" uuid doentry-template))))))

(defun create-doentry ()
  ;; requires p-logs-dir to be defined
  (interactive)
  (let ((uuid (doentry-uuid)))
    (find-file (expand-file-name (concat uuid ".doentry") p-logs-dir))
    (insert (doentry-populated-template uuid))
    (search-backward "■")
    (previous-line)
  ))

(provide 'doentry-gen)

;;; doentry-gen ends here
