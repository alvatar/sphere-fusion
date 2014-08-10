(lambda (target-info)
  (let ((project-path (cadr (assq project-path: target-info)))
        (target (cadr (assq target: target-info)))
        (target-path (cadr (assq target-path: target-info))))
    ;; Instantiate the target just copying the files
    (copy-directory target-path project-path merge: #t)))

