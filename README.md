# Hyper Drakma

## WIP WIP WIP WIP WIP

```lisp
(let* ((response (get "https://api.github.com/"))
       (first-page (response.follow response "user_repositories"
                                             '(("user" . "deadtrickster")
                                               ("type" . "public"))))
       (last-page (response.follow first-page "last")))

  (format t "First page count: ~a~%" (length (response-body first-page)))
  (format t "Last page count: ~a~%" (length (response-body last-page))))


First page count: 30
Last page count: 11

```

## License
MIT
