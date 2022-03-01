(defvar *rebuild* nil)
(defvar *build-first* nil)
(defvar *rerun* nil)

(with-eval-after-load 'projectile
  (setq projectile-project-root-files '(".projectile"))

  (setq projectile-project-root-files-functions
        '(projectile-root-local projectile-root-top-down))

  (projectile-register-project-type
   'rush '("rush.json")
   :compile 'jdh-rush-project-compile
   :test 'jdh-rush-project-test
   :related-files-fn 'jdh-rush-get-related-files
   )

  (advice-add 'projectile-create-test-file-for :around #'jdh-projectile-create-test-file-for)
  (advice-add 'projectile-compilation-command :before #'jdh-advice-projectile-compilation-command)
  (advice-add 'projectile-test-command :before #'jdh-advice-projectile-test-command)
  (advice-add 'projectile-compile-project :around #'jdh-advice-projectile-compile-project)
  (advice-add 'projectile-test-project :around #'jdh-advice-projectile-test-project))
