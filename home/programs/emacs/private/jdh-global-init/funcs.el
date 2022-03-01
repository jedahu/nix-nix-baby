(defun jdh/save-all ()
  (interactive)
  (save-some-buffers t))

(defun jdh/make-executable-when-hashbang ()
  (and (save-excursion
         (save-restriction
           (widen)
           (goto-char (point-min))
           (save-match-data
             (looking-at "^#!"))))
       (not (file-executable-p buffer-file-name))
       (set-file-modes buffer-file-name
                       (logior (file-modes buffer-file-name) #o111))
       (message "made executable %s" buffer-file-name)
       (normal-mode 1)))

(defun jdh/sentinel-bury-proc-buffer-after-success (process signal)
  (let ((pbuf (process-buffer process)))
    (when (and pbuf
               (memq (process-status process) '(exit signal))
               (eq 0 (process-exit-status process)))
      (delete-window (get-buffer-window pbuf 'visible))
      (bury-buffer pbuf))))

(defun jdh/sentinel-goto-inbox-after-exit (process signal)
  (jdh/sentinel-bury-proc-buffer-after-success process signal)
  (spacemacs/notmuch-inbox))

(defun jdh/fetch-mail ()
  (interactive)
  (let* ((default-directory "~/Maildir/me")
         (output (get-buffer-create "*fetch-mail-me*"))
         (proc (start-process "fetch-mail-me" output (expand-file-name "~/bin/fetch-mail-me"))))
    (with-current-buffer output
      (read-only-mode -1)
      (erase-buffer)
      (setq buffer-undo-list nil)
      (compilation-mode "Lieer+Notmuch"))
    (set-process-sentinel proc 'jdh/sentinel-goto-inbox-after-exit)
    (popwin:popup-buffer output :noselect t)))

(defun jdh/notmuch-search-tag-trash ()
  (interactive)
  (notmuch-search-tag-all notmuch-message-deleted-tags))

(defun jdh/notmuch-tag-all-jump (reverse)
  (interactive "P")
  (let (action-map)
    (pcase-dolist (`(,key ,tag ,name) notmuch-tagging-keys)
      (let* ((tag-function (cl-case major-mode
                             (notmuch-search-mode #'notmuch-search-tag)
                             (notmuch-show-mode #'notmuch-show-tag)
                             (notmuch-tree-mode #'notmuch-tree-tag)))
             (tag (if (symbolp tag)
                      (symbol-value tag)
                    tag))
             (tag-change (if reverse
                             (notmuch-tag-change-list tag 't)
                           tag))
             (name (or (and (not (string= name ""))
                            name)
                       (and (symbolp name)
                            (symbol-name name))))
             (name-string (if name
                              (if reverse (concat "Reverse " name)
                                name)
                            (mapconcat #'identity tag-change " "))))
        (push (list key name-string
                    `(lambda () (,tag-function ',tag-change)))
              action-map)))
    (push (list notmuch-tag-jump-reverse-key
                (if reverse
                    "Forward tag changes "
                  "Reverse tag changes")
                (apply-partially 'notmuch-tag-jump (not reverse)))
          action-map)
    (setq action-map (nreverse action-map))
    (notmuch-jump action-map "Tag: ")))

(defun jdh/nixos-options-generate ()
  (interactive)
  (setq nixos-options-json-file
        (progn (copy-file "/ssh:jdh@marmot:/var/run/current-system/sw/share/doc/nixos/options.json"
                          "/tmp/marmot-nixos-options.json")
               "/tmp/marmot-nixos-options.json"))
  (setq nixos-options
        (if (file-exists-p nixos-options-json-file)
            (let* ((json-key-type 'string)
                   (raw-options (json-read-file nixos-options-json-file)))
              (message "Generating nixos-options alist. This will take some time.")
              (mapcar 'nixos-options--make-alist raw-options))
          (message "Warning: Cannot find nixos option file."))))

(defun jdh/helm-nixos-options ()
  (interactive)
  (unless (file-exists-p nixos-options-json-file)
    (jdh/nixos-options-generate))
  (helm-nixos-options))

(defun jdh/notmuch-helm-go-action (candidate)
  (notmuch-search (notmuch-saved-search-get candidate :query)))

(defun jdh/notmuch-helm-saved-searches ()
  (interactive)
  (helm :sources (helm-build-sync-source "notmuch"
                   :candidates (mapcar (lambda (s) (cons (plist-get s :name) s)) notmuch-saved-searches)
                   :action (helm-make-actions
                            "Go" 'jdh/notmuch-helm-go-action)
                   )
        :buffer "*helm notmuch searches*"))

(defun jdh/notmuch-write-saved-searches-to-config ()
  (interactive)
  (dolist (s notmuch-saved-searches)
    (let ((name (replace-regexp-in-string " " "-" (downcase (plist-get s :name))))
          (query (plist-get s :query)))
      (notmuch-command-to-string "config" "set" (format "query.%s" name) query))))

(defun jdh/setup-notmuch-mua-send ()
  (save-excursion
    (let* ((from (save-restriction
                   (message-narrow-to-headers)
                   (message-fetch-field "from")))
           (dir (cdr (find-if (lambda (x) (string-match (car x) from)) jdh/notmuch-dirs))))
      (if (null dir)
          (error "Unable to derive maildir from From header")
        (setq message-sendmail-extra-arguments (list "--account" (file-name-nondirectory dir)))))))

(defun jdh/setup-notmuch-mode ()
  (setq notmuch-fcc-dirs
        (mapcar (lambda (x)
                  (let ((p (cdr x)))
                    (cons (plist-get p :address) (plist-get p :fcc))))
                jdh/mail-identities)))

(defun jdh/setup-message-mode ()
  (setq jdh/notmuch-dirs
        (mapcar (lambda (x)
                  (let ((p (cdr x)))
                    (cons (plist-get p :address) (plist-get p :maildir))))
                jdh/mail-identities))
  (setq gnus-alias-identity-alist
        (mapcar (lambda (x)
                  (let ((id (car x))
                        (p (cdr x)))
                    (list
                     id
                     nil
                     (format "%s <%s>" (plist-get p :name) (plist-get p :address))
                     nil
                     (list (cons "Fcc" (plist-get p :fcc)))
                     nil
                     nil)))
                jdh/mail-identities)))

(defun jdh/toggle-compilation-use-warn ()
  (interactive)
  (setq jdh/compilation-defns-use-warn (not jdh/compilation-defns-use-warn)))

(defun jdh/compilation-next-error-found (from-buffer to-buffer)
  (save-selected-window
    (switch-to-buffer-other-window from-buffer t)
    (with-current-buffer from-buffer
      (recenter 0))))

(defun jdh/setup-compilation-mode ()
  (setq next-error-found-function #'jdh/compilation-next-error-found)
  (let ((defns (if jdh/compilation-defns-use-warn jdh/compilation-defns-with-warn jdh/compilation-defns)))
    (setq compilation-error-regexp-alist-alist defns)
    (setq compilation-error-regexp-alist
          (mapcar (lambda (def) (car def)) defns))))

(defun jdh/ansi-colorify ()
  (interactive)
  (if (use-region-p)
      (ansi-color-apply-on-region (region-beginning) (region-end))
    (ansi-color-apply-on-region (point-min) (point-max))))

(defun jdh/get-prettier-print-width ()
  (ignore-errors
      (let* ((current-path (or (buffer-file-name) (pwd)))
             (config-path
              (with-temp-buffer
                (call-process "prettier" nil t nil "--find-config-path" current-path)
                (s-trim-right (thing-at-point 'line t))))
             (config (json-read-file config-path)))
        (alist-get 'printWidth config nil))))

(defun jdh/set-fill-column-from-prettier ()
  (setq-local fill-column (or (jdh/get-prettier-print-width) fill-column)))

(defun jdh/set-tab-width-8 ()
  (setq-local tab-width 8))

(defun jdh/popup-sbt ()
  (interactive)
  (popwin:popup-buffer-tail (sbt:buffer-name) :stick t))

(defun auto-dark-emacs/is-dark-mode-builtin ()
  "Invoke applescript using Emacs built-in AppleScript support to see if dark mode is enabled. Returns true if it is."

  (string-equal "true" (do-applescript "tell application \"System Events\"
	tell appearance preferences
		if (dark mode) then
			return true
		else
			return false
		end if
	end tell
end tell")))

(defun jdh/git-commit-setup ()
  (let ((b (magit-get-current-branch))
        (p "jeremy/gc-"))
    (when (s-prefix-p p b)
      (let ((n (string-to-number (s-chop-prefix p b) 10)))
        (when n
          (insert "GC-" (number-to-string n) ": "))))))
