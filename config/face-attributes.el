(defun dump-face-attributes (face &rest props)
  (cl-loop for prop in (or props
               (sort (mapcar #'car face-attribute-name-alist)
                 (lambda (s1 s2)
                   (string< (symbol-name s1) (symbol-name s2)))))
       for val = (face-attribute face prop nil t)
       unless (eq val 'unspecified)
       append (list prop val)))

(defun get-face-attribute (face prop)
  (plist-get (dump-face-attributes face prop) prop))
