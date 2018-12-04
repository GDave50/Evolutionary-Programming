(setf *limit* 20)

;; TASK 1

; Randomly generates an r, b or g
(defmethod rbg ()
  (nth (random 3) '(r b g))
)

; Constructs a list of length 'n randomly made of r, b and g
(defmethod rbg-list ((n number))
  (if (<= n 1)
    (list (rbg))
    (cons (rbg) (rbg-list (- n 1)))
  )
)

; Constructs a list of length *limit* randomly made of r, b and g
(defmethod rbg-string ()
  (rbg-list *limit*)
)


;; TASK 2

; Mutates 'rbg-str, changing one element randomly to r, b or g
(defmethod mutation ((rbg-str list) &aux index other-letters)
  (setf index (random (length rbg-str)))
  (setf other-letters (others '(r b g) (nth index rbg-str)))
  (change rbg-str (pick other-letters) index)
)

; Removes all occurances of 'ele from 'seq
(defmethod others ((seq list) (ele symbol))
  (remove ele seq)
)

; Replaces the element in 'seq at 'index with 'ele
(defmethod change ((seq list) (ele symbol) (index integer))
  (if (= index 0)
    (cons ele (cdr seq))
    (cons (car seq) (change (cdr seq) ele (- index 1)))
  )
)

; Selects a random element from 'seq
(defmethod pick ((seq list))
  (nth (random (length seq)) seq)
)

;; TASK 3

; Selects a random point and takes elements before that
; point from father and the rest from mother
(defmethod crossover ((mother list) (father list))
  (setf pos (+ 1 (random (length mother))))
  (append (first-n mother pos) (rest-n father pos))
)

; Removes all elements at and past index 'pos in 'seq
(defmethod first-n ((seq list) (pos integer))
  (if (= pos 0)
    nil
    (cons (car seq) (first-n (cdr seq) (- pos 1)))
  )
)

; Removes all elements before 'pos in 'seq
(defmethod rest-n ((seq list) (pos integer))
  (if (= pos 0)
    seq
    (rest-n (cdr seq) (- pos 1))
  )
)

;; TASK 4

; Runs a demo featuring the mutation method
(defmethod mutation-demo (&aux str mut)
  (setf str (rbg-string))
  (dotimes (i 10)
    (format t "str = ~A~%" str)
    (setf mut (mutation str))
    (format t "mut = ~A~%~%" mut)
  )
)

; Runs a demo featuring the crossover method
(defmethod crossover-demo (&aux mother father cross)
  (setf mother (rbg-string))
  (setf father (rbg-string))
  (dotimes (i 10)
    (format t "mother = ~A~%" mother)
    (setf cross (crossover mother father))
    (format t "cross  = ~A~%" cross)
    (format t "father = ~A~%~%" father)
  )
)

;; TASK 5

; Determines the number of r's in 'rbg-str
(defmethod fitness-r ((rbg-str list))
  (count 'r rbg-str)
)

; Determines the number of b's in 'rbg-str
(defmethod fitness-b ((rbg-str list))
  (count 'b rbg-str)
)

; Determines the number of g's in 'rbg-str
(defmethod fitness-g ((rbg-str list))
  (count 'g rbg-str)
)

; Runs a demo featuring the fitness methods
(defmethod fitness-demo (&aux rbg-str fitness)
  (setf rbg-str (rbg-string))
  (format t "rbg-str = ~A~%" rbg-str)
  (format t "Directly applying the fitness metrics ...~%")
  (format t "fitness-r = ~A~%" (fitness-r rbg-str))
  (format t "fitness-b = ~A~%" (fitness-b rbg-str))
  (format t "fitness-g = ~A~%" (fitness-g rbg-str))
  (format t "Indirectly applying the fitness metrics ...~%")
  (setf fitness #'fitness-r)
  (format t "fitness-r = ~A~%" (funcall fitness rbg-str))
  (setf fitness #'fitness-b)
  (format t "fitness-b = ~A~%" (funcall fitness rbg-str))
  (setf fitness #'fitness-g)
  (format t "fitness-g = ~A~%" (funcall fitness rbg-str))
)
