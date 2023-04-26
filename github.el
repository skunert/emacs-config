;;; github.el -*- lexical-binding: t; -*-


;; This shit powered by chatgopt
(require 'request)
(require 'json)

(defun fetch-github-issue-or-pr-title-and-insert (url)
  "Fetch the title of a GitHub issue or pull request from its URL and insert it in Org format."
  (interactive "sEnter GitHub issue or pull request URL: ")
  (let* ((api-url (concat "https://api.github.com/repos/"
                          (replace-regexp-in-string
                           "https?://github\\.com/\\(.*\\)/\\(issues\\|pull\\)/\\([0-9]+\\)" "\\1" url)
                          "/issues/"
                          (replace-regexp-in-string
                           "https?://github\\.com/.*/\\(issues\\|pull\\)/\\([0-9]+\\)" "\\2" url)))
         (headers '(("Accept" . "application/vnd.github+json")))
         (response (request api-url
                            :type "GET"
                            :headers headers
                            :parser 'json-read
                            :sync t
                            :complete (cl-function
                                       (lambda (&key response &allow-other-keys)
                                         (message "Fetched issue or pull request data."))))))
    (if (request-response-error-thrown response)
        (error "Error fetching issue or pull request data: %S" (request-response-error-thrown response))
      (let ((title (cdr (assoc 'title (request-response-data response)))))
        (insert (format "[[%s][%s]]" url title))
        (message "Inserted issue or pull request title and URL in Org format.")))))
