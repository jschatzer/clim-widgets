(in-package :clim-widgets)

;;; An application for viewing file directories. 

(defun directory-p (pathname) (not (pathname-name pathname)))
(defun file-name (pathname) (file-namestring pathname))
(defun make-directory-pathname (directory) (fad:pathname-directory-pathname directory))
(defun read-directory (pathname) (cl-fad:list-directory pathname))

(defun directory-name (pathname)
  (let ((list (pathname-directory pathname)))
    (when (consp list)
      (string (car (last list))))))

(defclass directory-display (essential-group)
  ((pathname :initarg :pathname :accessor encapsulated-pathname)
   (contents :accessor group-contents)))

;not needed
(defmethod print-object ((self directory-display) stream)
  (format stream "#<~A>" (group-name self)))

(defmethod group-name ((self directory-display))
  (directory-name (encapsulated-pathname self)))

(defmethod group-contents :around ((self directory-display))
  (if (not (slot-boundp self 'contents))
    (let ((stuff (read-directory (encapsulated-pathname self))))
      (setf (group-contents self)
            (append (mapcar
                      (lambda (p)
                        (make-instance 'directory-display :pathname (make-directory-pathname p)))
                      (sort (remove-if-not #'directory-p stuff) #'string-lessp :key #'directory-name))
                    (sort (mapcar #'file-name (remove-if #'directory-p stuff)) #'string-lessp))))
    (call-next-method self)))

(defun view-directory (directory)
  (view-group (make-instance 'directory-display :pathname (make-directory-pathname directory) :display-contents t)
              'string))

