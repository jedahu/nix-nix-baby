(when (symbolp 'focus-out-hook)
  (add-hook 'focus-out-hook 'jdh/save-all))

(add-hook 'after-save-hook 'jdh/make-executable-when-hashbang)

(setq auth-sources '(password-store))

;; (with-eval-after-load 'slack
;;   (slack-register-team
;;    :name "typesafe"
;;    :default t
;;    :token (auth-source-pick-first-password
;;            :host "typesafe.slack.com"
;;            :user "jeremy.hughes@lightbend.com")
;;    :subscribed-channels '(cloudstate
;;                           cloudstate-notifications
;;                           external-cloudstate
;;                           applied-cloudstate
;;                           general)))

(setq slack-buffer-create-on-notify nil)

(setq epa-pinentry-mode 'loopback)

(setq jdh/compilation-defn-good-fences
      '(good-fences "^Good-fences violation in \\(.*?\\):$" 1))

(setq jdh/compilation-defn-js-runtime
      '(js-runtime "\\(?:^[\t ]*at \\|(\\)\\(\\(?:[A-Za-z]:\\)?[^:()\n]+\\):\\([0-9]+\\):\\([0-9]+\\)\\(?:)\\|$\\)" 1 2 3))

(setq jdh/compilation-defn-tsc
      '(tsc "^\\([^:\n\r\t]+\\):\\([0-9]+\\):\\([0-9]+\\) -" 1 2 3))
      ;; '(tsc " *\\([^:\n\r\t ]+\\)(\\([0-9]+\\),\\([0-9]+\\)):" 1 2 3))

(setq jdh/compilation-defn-tsc-build
      '(tsc-build "^ *\\([^:\n\r\t ]+\\)(\\([0-9]+\\),\\([0-9]+\\)):" 1 2 3))

(setq jdh/compilation-defn-pnpm-tsc
      '(pnpm-tsc "│ *\\([^:\n\r\t ]+\\)(\\([0-9]+\\),\\([0-9]+\\)):" 1 2 3))

(setq jdh/compilation-defn-eslint
      '(eslint "\\(\\([0-9]+\\):\\([0-9]+\\)\\) +\\(error\\|warning\\)" jdh-compile-eslint-find-filename 2 3 2 1))

(setq jdh/compilation-defn-webpack-tsc
      '(webpack-tsc ".*?ERROR in \\([^(]+\\)(\\([0-9]+\\),\\([0-9]+\\))" 1 2 3))

(setq jdh/compilation-defn-psc
      '(psc "^ *at \\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3))

(setq jdh/compilation-defn-bloop
      '(bloop "^\\[E\\] \\[E[0-9]+\\] \\([^:\n\r\t]+\\):\\([0-9]+\\):\\([0-9]+\\)$" 1 2 3))

(setq jdh/compilation-defn-bloop-warn
      '(bloop-warn "^\\[W\\] +\\[E[0-9]+\\] \\([^:\n\r\t]+\\):\\([0-9]+\\):\\([0-9]+\\)$" 1 2 3))

(setq jdh/compilation-defn-psa
      '(psa "\\[[^]]+\\] \\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3))

    ;; (setq compilation-error-regexp-alist '(psa psc js-runtime tsc eslint webpack-tsc bloop))
    ;; ;; (setq compilation-error-regexp-alist
    ;; ;;       (append '(psa psc js-runtime tsc eslint webpack-tsc bloop) compilation-error-regexp-alist))

    ;; )

(setq jdh/compilation-defns
      (list jdh/compilation-defn-js-runtime
            jdh/compilation-defn-tsc
            jdh/compilation-defn-tsc-build
            jdh/compilation-defn-eslint
            jdh/compilation-defn-webpack-tsc
            jdh/compilation-defn-bloop
            jdh/compilation-defn-good-fences
            jdh/compilation-defn-pnpm-tsc))

(setq jdh/compilation-defns-with-warn
      (list jdh/compilation-defn-js-runtime
            jdh/compilation-defn-tsc
            jdh/compilation-defn-tsc-build
            jdh/compilation-defn-eslint
            jdh/compilation-defn-webpack-tsc
            jdh/compilation-defn-bloop
            jdh/compilation-defn-bloop-warn
            jdh/compilation-defn-good-fences
            jdh/compilation-defn-pnpm-tsc))

(setq jdh/compilation-defns-use-warn nil)

(add-hook 'compilation-shell-minor-mode-hook 'jdh/setup-compilation-mode)
(add-hook 'compilation-minor-mode-hook 'jdh/setup-compilation-mode)
(add-hook 'compilation-mode-hook 'jdh/setup-compilation-mode)

(setq compilation-ask-about-save nil)

(setq notmuch-search-result-format
      '(("date" . "%12s ")
        ("count" . "%-7s ")
        ("authors" . "%-20s ")
        ("subject" . "%s ")
        ("tags" . "(%s)")))

(setq notmuch-message-deleted-tags '("+trash" "-new" "-inbox" "-unread"))

(setq notmuch-tagging-keys
  `((,(kbd "a") notmuch-archive-tags "Archive")
    (,(kbd "u") notmuch-show-mark-read-tags "Mark read")
    (,(kbd "f") ("+flagged") "Flag")
    (,(kbd "s") ("+spam" "-inbox") "Mark as spam")
    (,(kbd "d") notmuch-message-deleted-tags "Delete")))

(setq notmuch-saved-searches
      '(
        (:key "i" :name "Me in" :query "tag:me and tag:inbox")
        (:key "I" :name "LB in" :query "tag:lb and tag:inbox and (tag:cloudstate tag:cloudstate-aas)")
        (:key "M" :name "LB misc" :query "tag:lb and tag:inbox and tag:misc")
        (:key "S" :name "LB sent" :query "tag:lb and tag:sent")
        (:key "D" :name "LB drafts" :query "tag:lb and tag:draft")
        (:key "a" :name "Me all" :query "tag:me")
        (:key "A" :name "LB all" :query "tag:lb")
        (:name "LB fastlane" :query "tag:lb and tag:inbox and tag:fastlane")
        ))

(setq notmuch-search-oldest-first nil)

(setq notmuch-show-empty-saved-searches t)

(setq notmuch-maildir-use-notmuch-insert t)

(setq jdh/mail-identities
      `(("me"
         :name "Jeremy Hughes"
         :address "jedahu@gmail.com"
         :fcc "me +sent"
         :maildir ,(expand-file-name "~/Maildir/me"))
        ("lb"
         :name "Jeremy Hughes"
         :address "jeremy.hughes@lightbend.com"
         :fcc "lb +sent"
         :maildir ,(expand-file-name "~/Maildir/lb"))))

(setq sendmail-program "msmtp")

(setq send-mail-function 'sendmail-send-it)
(setq message-send-mail-function 'message-send-mail-with-sendmail)

(add-hook 'notmuch-mua-send-hook 'jdh/setup-notmuch-mua-send)
(add-hook 'message-mode-hook 'jdh/setup-message-mode)
(add-hook 'message-setup-hook 'gnus-alias-select-identity)

(setq evil-want-minibuffer t)

(setq ranger-cleanup-on-disable t)

(add-hook 'typescript-mode-hook 'jdh/set-fill-column-from-prettier)
(add-hook 'markdown-mode-hook 'jdh/set-fill-column-from-prettier)

(with-eval-after-load 'treemacs
  (remove-hook 'dired-mode-hook 'treemacs-icons-dired-mode)
  (treemacs-icons-dired-mode -1))

(setq transient-default-level 7)

(with-eval-after-load 'flycheck
  (setq flycheck-display-errors-function #'flycheck-display-error-messages))

(setq auto-dark-emacs/dark-theme 'material)
(setq auto-dark-emacs/light-theme 'material-light)
(setq auto-dark-emacs/polling-interval-seconds 5)
(setq auto-dark-emacs/allow-osascript t)

(setq lsp-metals-show-inferred-type nil)
(setq lsp-metals-show-implicit-arguments nil)
(setq lsp-metals-show-implicit-conversions-and-classes nil)

(add-hook 'emacs-lisp-mode-hook 'jdh/set-tab-width-8)

(setq create-lockfiles nil)

(with-eval-after-load 'magit
  (defun jdh/magit-catch-up-to-upstream ()
    (interactive)
    (magit-merge "@{upstream}" '("--ff" "--ff-only")))

  (magit-define-popup-action 'magit-merge-popup ?u
    "Catch up to upstream"
    #'jdh/magit-catch-up-to-upstream))

(add-hook 'git-commit-setup-hook 'jdh/git-commit-setup)

(setq ivy-initial-inputs-alist
      '((counsel-minor . "^+")
        (counsel-package . "^+")
        (counsel-org-capture . "^")
        (counsel-M-x . "")
        (counsel-describe-symbol . "")
        (org-refile . "^")
        (org-agenda-refile . "^")
        (org-capture-refile . "^")
        (Man-completion-table . "^")
        (woman . "^")))

(global-hl-line-mode -1)

(with-eval-after-load 'lsp
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection 'lsp-metals--server-command)
                    :major-modes '(scala-mode)
                    :priority -1
                    :initialization-options '((decorationProvider . t)
                                              (inlineDecorationProvider . t)
                                              (didFocusProvider . t)
                                              (executeClientCommandProvider . t)
                                              (doctorProvider . "html")
                                              (statusBarProvider . "on")
                                              (debuggingProvider . t)
                                              (treeViewProvider . t))
                    :notification-handlers (ht ("metals/executeClientCommand" #'lsp-metals--execute-client-command)
                                               ("metals/publishDecorations" #'lsp-metals--publish-decorations)
                                               ("metals/treeViewDidChange" #'lsp-metals-treeview--did-change)
                                               ("metals-model-refresh" #'lsp-metals--model-refresh)
                                               ("metals/status" #'lsp-metals--status-string))
                    :action-handlers (ht ("metals-debug-session-start" (-partial #'lsp-metals--debug-start :json-false))
                                         ("metals-run-session-start" (-partial #'lsp-metals--debug-start t)))
                    :server-id 'metals
                    :initialized-fn (lambda (workspace)
                                      (lsp-metals--add-focus-hooks)
                                      (with-lsp-workspace workspace
                                        (lsp--set-configuration
                                         (lsp-configuration-section "metals"))))
                    :after-open-fn (lambda ()
                                     (add-hook 'lsp-on-idle-hook #'lsp-metals--did-focus nil t))
                    :completion-in-comments? t
                    :download-server-fn #'lsp-metals--download-server)))
