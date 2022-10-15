(defun mlemosf/http/get-url-json-plist (url headers)
  "Get URL with HEADERS and return JSON converted to plist"
  (with-temp-buffer
    (let ((url-request-extra-headers headers))
      (url-insert-file-contents url))
    (let* (
           (resp (buffer-string))
           (resp-json-plist (json-parse-string resp :object-type 'plist)))
      resp-json-plist)))

(defun mlemosf/http/get-bearer-token (host)
  "Get Bearer token from authinfo file for host HOST"
  (let (
        (auth (nth 0 (auth-source-search :host host
                                         :requires '(user secret)))))
    (concat "Bearer " (funcall (plist-get auth :secret)))))

(defun my-kill-buffer (status)
  (kill-buffer (current-buffer)))

(defun mlemosf/http/post-url (url headers)
  "Send HTTP POST request to 'url' with headers 'headers' "
        (let (
              (url-request-method "POST")
              (url-request-extra-headers headers)
              (url-request-data nil)
              )
          (url-retrieve url 'my-kill-buffer)))
