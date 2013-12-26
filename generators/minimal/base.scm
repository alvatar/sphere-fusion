(lambda (target-info)
  (let ((project-path (cadr (assq project-path: target-info)))
        (target (cadr (assq target: target-info)))
        (target-path (cadr (assq target-path: target-info))))
    ;; Instantiate the target copying the files and processing the templates
    (task:instantiate-target project-path target-path)))

