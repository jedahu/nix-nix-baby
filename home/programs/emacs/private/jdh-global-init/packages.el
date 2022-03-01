;;; packages.el --- jdh-global-init layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: System administrator <root@sanity>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `jdh-global-init-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `jdh-global-init/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `jdh-global-init/pre-init-PACKAGE' and/or
;;   `jdh-global-init/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst jdh-global-init-packages
  '(direnv
    evil-smartparens
    pinentry
    polymode
    terminal-focus-reporting
    (auto-dark-emacs :location local))
  "The list of Lisp packages required by the jdh-global-init layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun jdh-global-init/init-direnv ()
  (use-package direnv
    :init (direnv-mode 1)))

(defun jdh-global-init/init-evil-smartparens ()
  (use-package evil-smartparens
    :defer t
    :init (with-eval-after-load 'smartparens
            (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))))

(defun jdh-global-init/init-pinentry ()
  (use-package pinentry
    :init (pinentry-start)))

(defun jdh-global-init/init-polymode ()
  (use-package polymode
    :ensure t
    :init
    (progn
      (define-hostmode poly-scala-hostmode
        :mode 'scala-mode)
      (define-innermode poly-scala-css-interpolator-innermode
        :mode 'less-css-mode
        :head-matcher "\\bcss\"\"\"\n"
        :tail-matcher "\n\s*\"\"\""
        :body-indent-offset 'scala-indent:step
        :head-mode 'host
        :tail-mode 'host)
      (define-polymode poly-scala-mode
        :hostmode 'poly-scala-hostmode
        :innermodes '(poly-scala-css-interpolator-innermode))
    )))

(defun jdh-global-init/init-terminal-focus-reporting ()
  (use-package terminal-focus-reporting
    :init
    (unless (display-graphic-p)
      (terminal-focus-reporting-mode 1))))

(defun jdh-global-init/init-auto-dark-emacs ()
  (use-package auto-dark-emacs
    :demand t))

;;; packages.el ends here
