(in-package #:lisp-logs)

(cl-interpol:enable-interpol-syntax)

(defclass acceptor (hunchentoot:acceptor) ())

(defvar *acceptor* (make-instance 'acceptor
				  :port 5477))

(defparameter *routes* ())

(defun start ()
  (hunchentoot:start *acceptor*))

(defun stop ()
  (hunchentoot:stop *acceptor*))

(defmacro pick-route ((request-object) &body routes-form)
  (alexandria:with-gensyms (uri routes route match match-groups)
    `(let ((,uri (hunchentoot:request-uri ,request-object))
	   (,routes (append ',routes-form *routes*)))
       (loop for ,route in ,routes
	     do
	    (multiple-value-bind (,match ,match-groups)
		(cl-ppcre:scan-to-strings (car ,route) ,uri)
      	      (declare (ignorable ,match-groups))
	      (if ,match
		  (return (funcall (second ,route) ,match-groups))))))))

(defmacro defpage (page-function-name (route-regex &optional route-binding-pattern)
		   &body body)
  (alexandria:with-gensyms (matches-from-url)
    `(progn
       (defun ,page-function-name (,matches-from-url)
	 (,@(typecase route-binding-pattern
		(null '(progn))
		(list `(destructuring-bind ,route-binding-pattern (coerce ,matches-from-url 'list)))
		(atom `(let ((,route-binding-pattern (coerce ,matches-from-url 'list))))))
	    ,@body))
       (add-route ,route-regex ',page-function-name))))

(defun add-route (route function)
  (push (list route function)  *routes*))

(defmethod hunchentoot:handle-request ((acceptor-object acceptor) request-object)
  (or
   (pick-route (request-object)
     ("^/css\\.css$" 'css))
   "404!"))

(defun chat-log-parts (log-entry)
  (map 'list
       #'hunchentoot:escape-for-html
       (second
	(multiple-value-list
	 (ppcre:scan-to-strings "([^ ]+) (<[^>]+>) (.*)" log-entry)))))

(defpage index ("^/(\\d+)-(\\d+)-(\\d+)$"
		(year month day))
  (with-html-output-to-string (s nil :prologue t)
    (:html
     (:head
      (:title "Lisp Logs")
      (:style :type "text/css"
	      (htm (princ (css) s))))
     (:body
      (:div :id "header"
	    (:canvas :id "header-canvas"))
      (:table 
       (let ((zebra nil)
	     (last-speaker ""))
	 (labels ((last-speaker-same-p (speaker)
		    (string= speaker last-speaker))
		  (zebra (user)
		    (when (not (last-speaker-same-p user)) (setf zebra (not zebra)))
		    (if zebra
			"zebra"
			"")))
	   (loop for line in (get-log year month day)
	      do (destructuring-bind (timestamp user message)
		     (chat-log-parts line)
		   (htm
		    (:tr
		     :class (zebra user)
		     (htm
		      (:td (princ timestamp s))
		      (:td :class "username" (unless (last-speaker-same-p user) (princ user s)))
		      (:td (:div :class "message-div" (princ message s))))
		     (setf last-speaker user))))))))))))

(defun css (&rest ignore)
  (ultralight-css:css
   '(("html, body"      :border "0" :padding "0" :margin "0")
     ("body"            :background-color "#fafafa"
                        :color "black"
                        :font-family "monaco"
                        :font-size "12px")
     ("table, td, tr"   :border "0" :padding "0" :border-collapse "collapse" #|"separate"|#)
     ("table"           :width "600px"
                        :background-color "white"
                        :border-left "solid #eaeaea 20px"
                        :border-right "solid #eaeaea 20px"
                        :margin-left "auto"
                        :margin-right "auto"
                       #| :border-spacing "10px"|#)
     ("#header"         :width "640px" :height "200px"
                        :margin-left "auto" :margin-right "auto")
     ("#header #header-canvas"
                        :width "640px" :height "200px")
     ("tr.zebra td"     :background-color "#f0f0f0")
     ("td"              #|:border-radius "10px"|#
                        :padding "10px"
                        :vertical-align "top"
                        :background-color "#fefefe")
     ("td.username"     :text-align "center")
     ("div.message-div" :width "400px"
                        :word-wrap "break-word"))))


(defun replace-links (string)
  (multiple-value-bind (matched match-data)
      (ppcre:all-matches-as-strings "^http://[^\s]+" string)))
