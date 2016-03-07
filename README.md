# Hyper Drakma

## WIP WIP WIP WIP WIP

```lisp
(let* ((response (get "https://api.github.com/"))
       (first-page (follow response "user_repositories"
                                    '(("user" . "deadtrickster")
                                      ("type" . "public"))))
       (first-page-repos (response-body first-page))
       (repo (elt first-page-repos 13))
       (repo-branches (response-body (follow repo "branches")))
       (last-page (follow first-page "last")))

  (format t "First page count: ~a~%" (length first-page-repos))
  (format t "Last page count: ~a~%" (length (response-body last-page)))

  (format t "Repo name: ~a~%" (gethash "full_name" repo))
  (format t "Repo description: ~a~%" (gethash "description" repo))
  (format t "Branches count: ~a~%" (length repo-branches)))


First page count: 30
Last page count: 12
Repo name: deadtrickster/cl-events
Repo description: Events (Hooks) for Common Lisp
Branches count: 1

```

## License
MIT
