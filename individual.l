(defconstant *pc-m* 50)

;; TASK 6

; An individual
(defclass individual ()
  (
    (rbg-string :accessor individual-rbg-string :initarg :rbg-string)
    (fitness :accessor individual-fitness :initarg :fitness)
    (number :accessor individual-number :initarg :number)
  )
)

; Generates a random individual with number 0
(defmethod random-individual (&aux rbg)
  (setf rbg-str (rbg-string))
  (make-instance 'individual
    :rbg-string rbg-str
    :fitness (funcall *fitness* rbg-str)
    :number 0
  )
)

; Generates a new individual with number 'nr and rbg-string 'rbg-str
(defmethod new-individual ((nr number) (rbg-str list))
  (make-instance 'individual
    :rbg-string rbg-str
    :fitness (funcall *fitness* rbg-str)
    :number nr
  )
)

; Displays individual 'i
(defmethod display ((i individual))
  (display-nnl i)
  (terpri)
)

; Displays individual 'i with proper spacing
(defmethod display-nnl ((i individual))
  (prin1 (individual-number i))
  (princ " ")
  (princ (filler (individual-number i)))
  (prin1 (individual-rbg-string i))
  (princ " ")
  (princ (filler (individual-fitness i)))
  (prin1 (individual-fitness i))
)

; Fills whitespace for 6 characters depending on the number 'n
(defmethod filler ((n number))
  (cond
    ((< n 10) "     ")
    ((< n 100) "    ")
    ((< n 1000) "   ")
    ((< n 10000) "  ")
    ((< n 100000) " ")
  )
)

; Determines the r-fitness of individual 'i
(defmethod fitness-r ((i individual))
  (fitness-r (individual-rbg-string i))
)

; Determines the b-fitness of individual 'i
(defmethod fitness-b ((i individual))
  (fitness-b (individual-rbg-string i))
)

; Determines the g-fitness of individual 'i
(defmethod fitness-g ((i individual))
  (fitness-g (individual-rbg-string i))
)

; Demos methods relation to the individual class
(defmethod individual-demo (&aux i0 i1 i2 i3 one two three)
  (setf *fitness* #'fitness-r)
  (setf i0 (random-individual))
  (display i0)
  (setf one (rbg-string))
  (setf i1 (new-individual 1 one))
  (display i1)
  (setf two (rbg-string))
  (setf i2 (new-individual 2 two))
  (display i2)
  (setf three (rbg-string))
  (setf i3 (new-individual 3 three))
  (display i3)
  (format t "Fitness of i0 = ~A~%" (funcall *fitness* i0))
  (format t "Fitness of i1 = ~A~%" (funcall *fitness* i1))
  (format t "Fitness of i2 = ~A~%" (funcall *fitness* i2))
  (format t "Fitness of i3 = ~A~%" (funcall *fitness* i3))
)

;; TASK 8

; Mutates an individual 'i
(defmethod mutate ((i individual) &aux mutation)
  (setf mutation (mutation (individual-rbg-string i)))
  (make-instance 'individual
    :number (individual-number i)
    :rbg-string mutation
    :fitness (funcall *fitness* mutation)
  )
)

; Possibly mutates an individual 'i
(defmethod maybe-mutate ((i individual))
  (if (<= (+ 1 (random 100)) *pc-m*)
    (mutate i)
    i
  )
)

; Demoes the mutate method
(defmethod mutate-demo ()
  (setf i (random-individual))
  (display i)
  (dotimes (x 20)
    (setf i (mutate i))
    (display i)
  )
)

; Demoes the maybe-mutate method
(defmethod maybe-mutate-demo ()
  (setf i (random-individual))
  (display i)
  (dotimes (x 20)
    (setf n (maybe-mutate i))
    (display-nnl n)
    (if (not (equal n i)) (princ " *"))
    (terpri)
    (setf i n)
  )
)
