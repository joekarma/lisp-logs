(in-package #:lisp-logs)

(cl-interpol:enable-interpol-syntax)

(defparameter *cached-logs* ())

(defmacro protected-eval (&body forms)
  "Returns nil on error instead of bailing to the debugger."
  `(handler-case
       (progn ,@forms)
     (condition nil nil)))

(defun get-cached-log (date)
  (let ((result (assoc date *cached-logs* :test #'string=)))
    (if result
      (cdr result)
      nil)))

(defun add-log-to-cache-and-return (date)
  (let ((log-result (filter-lines
		     (http-request (irc-log-url-from-date date)))))
    (push (cons date log-result) *cached-logs*)
    log-result))

(defun get-log (year month day)
  (let* ((date #?"$(year)-$(month)-$(day)")
	 (cached-log (get-cached-log date)))
    (if cached-log
	cached-log
	(add-log-to-cache-and-return date))))

(defun filter-lines (lisp-log-lines)
  (with-input-from-string (log lisp-log-lines)
    (loop for line = (read-line log nil)
       while line
       if (protected-eval (eq (char line 9) #\<)) collect line)))

(defun date-path (date)
  (destructuring-bind (year month day)
      (cl-ppcre:split "[^\\d]" date)
    #?"${year}-${month}/lisp-${year}.${month}.${day}.txt"))

(defun irc-log-url-from-date (date)
  (let ((date-path (date-path date)))
    #?"http://ccl.clozure.com/irc-logs/lisp/${date-path}"))
