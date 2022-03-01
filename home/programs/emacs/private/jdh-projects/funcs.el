(defun jdh-projectile-create-test-file-for (func impl-file-path)
  (if (eq 'rush (projectile-project-type))
      (plist-get (jdh-rush-get-related-files impl-file-path) :test)
    (funcall func impl-file-path)))

(defun jdh-rush-get-related-files (file)
  (let* ((dir (file-name-as-directory (f-dirname file)))
         (kind (cond
                ((s-ends-with? ".test.ts" file) :test)
                ((s-ends-with? ".arb.ts" file) :arb)
                (t :impl)))
         (name (case kind
                 (:impl (f-base file))
                 (:test (f-base (f-base file)))
                 (:arb (f-base (f-base file))))))
    (message "dir: %s, name: %s, kind: %s" dir name kind)
    (append
     (when (not (eq kind :impl)) (list :impl (concat dir name ".ts")))
     (when (not (eq kind :test)) (list :test (concat dir name ".test.ts")))
     (when (not (eq kind :arb)) (list :arb (concat dir name ".arb.ts"))))))

(defun jdh-rush-get-project-names (rush-json-path)
  (with-temp-buffer
    (insert-file-contents-literally rush-json-path)
    (json-mode)
    (goto-char (point-min))
    (comment-kill (count-lines (point-min) (point-max)))
    (goto-char (point-min))
    (let* ((json-object-type 'hash-table)
           (json-array-type 'list)
           (json-key-type 'string)
           (rush (json-read)))
      (mapcar #'(lambda (pkg) (gethash "packageName" pkg)) (gethash "projects" rush)))))

(defun jdh-rush-completing-read-project-name (prompt)
  (completing-read
   prompt
   (cons "*"
         (jdh-rush-get-project-names
          (concat (file-name-as-directory (projectile-project-root))
                  "rush.json")))))

(defun jdh-rush-project-format-cmd (cmd project-name)
  (if (string-equal project-name "*")
      (format "rush %s -v" cmd)
    (format "rush %s -vt %s" cmd project-name)))

(defun jdh-rush-project-do (&rest cmds)
  (let ((project-name
         (jdh-rush-completing-read-project-name
          (format "rush %s -vt " cmds))))
    (mapconcat
     #'(lambda (cmd) (jdh-rush-project-format-cmd cmd project-name))
     cmds
     " && ")))

(defun jdh-rush-project-compile ()
  (apply #'jdh-rush-project-do (if *rebuild* '("clean" "rebuild") '("build"))))

(defun jdh-rush-project-test ()
  (apply #'jdh-rush-project-do
         (if *build-first* '("build" "test") '("test"))))

(defun jdh-advice-projectile-compile-project (func arg &rest args)
  (let ((*rebuild* arg))
    (apply func arg args)))

(defun jdh-advice-projectile-test-project (func arg &rest args)
  (let ((*build-first* arg))
    (apply func arg args)))

(defun jdh-advice-projectile-compilation-command (compile-dir)
  (when (and (not *rerun* ) (eq (projectile-project-type) 'rush))
    (remhash compile-dir projectile-compilation-cmd-map)))

(defun jdh-advice-projectile-test-command (compile-dir)
  (when (and (not *rerun*) (eq (projectile-project-type) 'rush))
    (remhash compile-dir projectile-test-cmd-map)))

(defun jdh-projectile-rerun-last-compile ()
  (interactive)
  (let ((*rerun* t)
        (compilation-read-command nil))
    (projectile-compile-project nil)))

(defun jdh-projectile-rerun-last-test ()
  (interactive)
  (let ((*rerun* t)
        (compilation-read-command nil))
    (projectile-test-project nil)))

(defun jdh-projectile-run-command-with-compile-mode ()
  (interactive)
  (projectile-with-default-dir (projectile-ensure-project (projectile-project-root))
    (let ((cmd (read-shell-command "$ ")))
      (compile ;; (maybe-with-nix-shell cmd)
       cmd
       ))))
