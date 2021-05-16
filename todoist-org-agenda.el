(defun mlemosf/todoist/get-url-json-plist (url headers)
  "Get URL with HEADERS and return JSON converted to plist"
  (interactive)
  (with-temp-buffer
    (let ((url-request-extra-headers headers))
      (url-insert-file-contents url))
    (let* (
           (resp (buffer-string))
           (resp-json-plist (json-parse-string resp :object-type 'plist)))
      resp-json-plist)))

(defun mlemosf/todoist/get-bearer-token (host)
  "Get Bearer token from authinfo file for host HOST"
  (interactive)
  (let (
        (auth (nth 0 (auth-source-search :host host
                                         :requires '(user secret))))
        )
    (concat "Bearer " (funcall (plist-get auth :secret)))))

(defun mlemosf/todoist/get-todoist-tasks-by-project (project-id buffer)
  "Get tasks from project given in PROJECT-ID and write them to buffer."
  (interactive)
  (let (
      (project-tasks-url (format "https://api.todoist.com/rest/v1/tasks?project_id=%s" project-id))
      (headers `(("Authorization" . ,(mlemosf/get-bearer-token "api.todoist.com")))))
    (let (
        (ids (seq-map (lambda (json-plist)
                        (plist-get json-plist :id))
                      (mlemosf/get-url-json-plist project-tasks-url headers)))
        (task-url (format "https://api.todoist.com/rest/v1/tasks"))
        )
      (seq-doseq (task-id ids)
        (let ((task (mlemosf/get-url-json-plist (format "%s/%s" task-url task-id) headers)))
          (let (
                (id (plist-get task :id))
                (origin "todoist")
                (content (plist-get task :content))
                (due-date
                 (format-time-string "<%Y-%m-%d %a>" (parse-iso8601-time-string
                                                    (plist-get (plist-get task :due) :date))))
                (created
                 (format-time-string "[%Y-%m-%d %a %H:%M]" (parse-iso8601-time-string
                                                            (plist-get (plist-get task :due) :date))))
                )
            (princ (format "** TODO %s \n   SCHEDULED: %s\n   :PROPERTIES:\n   :id: %s\n   :origin: %s\n   :END:\n   :CREATED: %s\n" content due-date id origin created) buffer))
          )))))

(defun mlemosf/todoist/get-todoist-tasks ()
  "Get tasks from Todoist and store them in org buffer"
  (interactive)
  (setq agenda-file "~/.config/org/todoist.org")
  (let (
        (orgbuf (generate-new-buffer "*org-todoist-output"))
        (project-url "https://api.todoist.com/rest/v1/projects")
        (headers `(("Authorization" . ,(mlemosf/get-bearer-token "api.todoist.com")))))
    (seq-map (lambda (project-plist)
               (let (
                     (id (plist-get project-plist :id))
                     (name (plist-get project-plist :name)))
               (princ (format "* %s\n   :PROPERTIES:\n   :CATEGORY: %s\n   :END:\n\n" name name) orgbuf)
               (mlemosf/get-todoist-tasks-by-project id orgbuf))
             )
             (mlemosf/get-url-json-plist project-url headers))
    (set-buffer orgbuf)
    (write-file agenda-file)))
