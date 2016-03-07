(in-package #:cl-user)

(defpackage #:hyper-drakma
  (:use :cl :alexandria)
  (:shadow :get)
  (:nicknames :hdrakma)
  (:export #:get
           #:follow
           #:response-body
           #:response-headers))

(in-package #:hyper-drakma)

(defclass response ()
  ((headers :initarg :headers :reader response-headers)
   (code :initarg :code :reader response-code)
   (body :initarg :body :reader response-body)
   (links :initarg :links :reader response-links)))

(defun get-links-from-body (body links)
  (when (hash-table-p body)
    (with-hash-table-iterator (next body)
      (loop
        (multiple-value-bind (more? key value) (next)
          (unless more?
            (return))
          (when (ends-with-subseq "_url" key)
            (setf (gethash (subseq key 0 (- (length key) 4)) links) value)
            (remhash key body)))))))

(defun get-links-from-headers (headers links)
  (loop for link in (link-field:parse (gethash "Link" headers)) do
        (setf (gethash (gethash "rel" link) links) (gethash "href" link))))

(defmethod initialize-instance :after ((instance response) &rest args)
  (let ((links (ia-hash-table:make-ia-hash-table)))
    (get-links-from-body (response-body instance) links)
    (get-links-from-headers (response-headers instance) links)
    (setf (slot-value instance 'links) links)))

(defmethod follow ((response response) link &optional args)
  (if-let ((href (gethash link (response-links response))))
    (get (uri-template:expand href args))
    (error "Unknown link ~a" link)))

(defmethod follow ((item hash-table) link &optional args)
  (if-let ((href (gethash (concatenate 'string link "_url") item)))
    (get (uri-template:expand href args))
    (error "Unknown link ~a" link)))

(defun parsed-body-to-ht (body)
  (if (typep body 'vector)
      (map (type-of body) (lambda (item)
                            (parsed-body-to-ht item))
           body)
      (ia-hash-table:alist-ia-hash-table body)))

(defun get (url &rest drakma-args)
  (let ((yason:*parse-object-as* :alist)
        (yason:*parse-json-arrays-as-vectors* t)
        (yason:*parse-json-null-as-keyword* t))
    (multiple-value-bind (body code headers)
        (apply #'drakma:http-request url :method :get
                                         :content-type "application/json"
                                         :accept "application/json"
                                         drakma-args)
      (case code
        (200
         (let ((parsed-body (yason:parse (babel:octets-to-string body))))
           (make-instance 'response :code code
                                    :headers (ia-hash-table:alist-ia-hash-table headers)
                                    :body (parsed-body-to-ht parsed-body))))
        (t (error "Unexpected response"))))))
