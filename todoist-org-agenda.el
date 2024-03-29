(load-file "$HOME/.emacs.d/etc/todoist-org-agenda/http.el")

(defun mlemosf/todoist/get-todoist-tasks-by-project (project-id buffer)
  "Get tasks from project given in PROJECT-ID and write them to buffer."
  (let (
        (project-tasks-url (format "https://api.todoist.com/rest/v1/tasks?project_id=%s" project-id))
        (headers `(("Authorization" . ,(mlemosf/http/get-bearer-token "api.todoist.com")))))
    (let (
          (ids (seq-map (lambda (json-plist)
                          (plist-get json-plist :id))
                        (mlemosf/http/get-url-json-plist project-tasks-url headers)))
          (task-url (format "https://api.todoist.com/rest/v1/tasks")))
      (seq-doseq (task-id ids)
        (let* (
               (task (mlemosf/http/get-url-json-plist (format "%s/%s" task-url task-id) headers))
               (due-date (plist-get (plist-get task :due) :date))
			   (pa)
               (id (plist-get task :id))
               (origin "todoist")
               (content (plist-get task :content))
               (due-date-formatted (format-time-string "<%Y-%m-%d %a>" (parse-iso8601-time-string (concat due-date "T00:00:00"))))
               (created (format-time-string "[%Y-%m-%d %a %H:%M]" (parse-iso8601-time-string (concat due-date "T00:00:00")))))
            (princ (format "** TODO %s\nSCHEDULED: %s\n:PROPERTIES:\n:id:%s\n:origin:%s\n:END:\n" content due-date-formatted id origin created) buffer))))))

(defun mlemosf/todoist/get-todoist-tasks ()
  "Get tasks from Todoist and store them in org buffer"
  (interactive)
  (setq agenda-file "~/.config/org/todoist.org")
  (let (
        (orgbuf (generate-new-buffer "*org-todoist-output*"))
        (project-url "https://api.todoist.com/rest/v1/projects")
        (headers `(("Authorization" . ,(mlemosf/http/get-bearer-token "api.todoist.com")))))
      (seq-map (lambda (project-plist)
                 (let (
                       (id (plist-get project-plist :id))
                       (name (plist-get project-plist :name)))
                   (princ (format "* %s\n   :PROPERTIES:\n   :CATEGORY: %s\n   :END:\n\n" name name) orgbuf)
                   (mlemosf/todoist/get-todoist-tasks-by-project id orgbuf)))
               (mlemosf/http/get-url-json-plist project-url headers))
      (set-buffer orgbuf)
      (write-file agenda-file)
      (message "Todoist tasks downloaded successfully")
      (kill-buffer orgbuf)))
(mlemosf/todoist/get-todoist-tasks)

(defun mlemosf/todoist/close-task (id)
  "Mark task with id ID as closed on Todoist API"
  (let (
        (url (format "https://api.todoist.com/rest/v1/tasks/%s/close" id))
        (headers
         `(("Authorization" . ,(mlemosf/http/get-bearer-token "api.todoist.com")))))
    (mlemosf/http/post-url url headers)))

(defun mlemosf/todoist/get-todoist-done-ids ()
  "Get list of all DONE tasks on org buffer"
  (org-element-map (org-element-parse-buffer) 'headline
    (lambda (x)
      (let (
            (todo-state (org-element-property :todo-type x))
            (origin (org-element-property :ORIGIN x))
            (id (org-element-property :ID x)))
        (if (and
             (eq todo-state 'done))
            id)))))

(defun mlemosf/todoist/close-done-tasks ()
  (interactive)
  (catch 'buffer-not-exist
    (if (eq (get-buffer "todoist.org") nil)
        (throw 'buffer-not-exist "todoist.org buffer is not open")
      (with-current-buffer "todoist.org"
        (let (
              (tasks (mlemosf/todoist/get-todoist-done-ids)))
          (progn
            (seq-doseq (task tasks)
              (mlemosf/todoist/close-task task))
            (set-buffer-modified-p -1)
            (message "Tasks closed successfully")))))))
