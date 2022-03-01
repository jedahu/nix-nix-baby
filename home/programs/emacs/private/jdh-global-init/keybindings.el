(spacemacs/set-leader-keys
  "En" 'next-error
  "Ep" 'previous-error
  "EN" 'previous-error
  "bU" 'bury-buffer
  "aNm" 'jdh/notmuch-helm-saved-searches
  "h>" 'jdh/helm-nixos-options
  "wps" 'jdh/popup-sbt)

(evil-define-key 'insert vterm-mode-map
  (kbd "C-r") 'vterm-send-C-r)

(spacemacs/set-leader-keys-for-major-mode 'notmuch-search-mode
  "*d" 'jdh/notmuch-search-tag-trash)

(define-key evil-insert-state-map (kbd "C-k") nil)
