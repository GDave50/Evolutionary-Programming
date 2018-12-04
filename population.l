;; TASK 7

(defconstant *population-size* 50)
(defconstant *selection-size* 10)
(defconstant *pc-x* 60)
(defconstant *pc-c* 40)
(setf *fitness* #'fitness-b)
(setf *select-demo* nil)
(setf *copy-demo* nil)
(setf *crossover-demo* nil)

; A population
(defclass population ()
  (
    (individuals :accessor population-individuals :initarg :individuals)
    (generation :accessor population-generation :initform 0)
  )
)

; Determines the size of population 'p
(defmethod size ((p population))
  (length (population-individuals p))
)

; Displays population 'p
(defmethod display ((p population))
  (format t "~%~%Generation ~A population ...~%~%" (population-generation p))
  (dolist (indiv (population-individuals p))
    (display indiv)
  )
  (terpri)
)

; Creates a population of size *population-size*
(defmethod initial-population (&aux individuals)
  (setf individuals '())
  (dotimes (i *population-size*)
    (push (new-individual (+ i 1) (rbg-string)) individuals)
  )
  (make-instance 'population :individuals (reverse individuals))
)

; Determines the average fitness of population 'p
(defmethod average ((p population) &aux (sum 0))
  (dotimes (i (size p))
    (setf sum (+ sum (individual-fitness (nth i (population-individuals p)))))
  )
  (float (/ sum (size p)))
)

; Selects the most fit individual from a selection of population 'p
(defmethod select-individual ((p population) &aux candidates)
  (setf candidates (select-individuals p))
  (setf mfi (most-fit-individual candidates))
  (if *select-demo* (select-demo-helper candidates mfi))
  mfi
)

; Generates a list of individuals of size *selection-size*
; from population 'p
(defmethod select-individuals ((p population) &aux individuals candidates rn)
  (setf individuals (population-individuals p))
  (setf candidates '())
  (dotimes (i *selection-size*)
    (setf rn (random (size p)))
    (push (nth rn individuals) candidates)
  )
  candidates
)

; Determines the most fit individual in list 'l of individuals
(defmethod most-fit-individual ((l list) &aux max-value max-individual)
  (setf max-individual (car l))
  (setf max-value (individual-fitness max-individual))
  (dotimes (i (- (length l) 1))
    (if (> (individual-fitness (nth (+ i 1) l)) max-value)
      (let ()
        (setf max-individual (nth (+ i 1) l))
        (setf max-value (individual-fitness max-individual))
      )
    )
  )
  max-individual
)

; Adds extra output if *select-demo* is true
(defmethod select-demo-helper ((l list) (i individual))
  (princ "the sample of individuals ...")
  (terpri)
  (mapcar #'display l)
  (terpri)
  (princ "the most fit of the sample ...")
  (terpri)
  (display i)
  (terpri)
)

; Demos methods relation to the population class
(defmethod population-demo (&aux p)
  (setf p (initial-population))
  (display p)
  (format t "Average fitness = ~A~%~%" (average p))
  (setf *select-demo* t)
  (format t "Sampling ...~%~%")
  (select-individual p)
  (terpri)
  (format t "Sampling ...~%~%")
  (select-individual p)
  (terpri)
  (format t "Sampling ...~%~%")
  (select-individual p)
  (terpri)
)

;; TASK 9

; Puts copies of the current population into the new population
(defmethod perform-copies ((cp population) (np population))
  (dotimes (i (nr-copies))
    (perform-one-copy cp np)
  )
)

; Determines the number of individuals that will be
; copied for evolution
(defmethod nr-copies ()
  (* (/ *pc-c* 100) *population-size*)
)

; Moves one individual to the new population, possibly mutating
(defmethod perform-one-copy ((cp population) (np population) &aux x m mm new-i)
  (setf m (select-individual cp))
  (if *copy-demo* (format t "Selected individual = ~%"))
  (if *copy-demo* (display m))
  (setf mm (maybe-mutate m))
  (if *copy-demo* (format t "Possibly mutated individual = ~&"))
  (if *copy-demo* (display mm))
  (setf (individual-number mm) (+ 1 (size np)))
  (if *copy-demo* (format t "Renumbered individual = ~&"))
  (if *copy-demo* (display mm))
  (setf new-i (new-individual (+ 1 (size np)) (individual-rbg-string mm)))
  (setf
    (population-individuals np)
    (append (population-individuals np) (list new-i))
  )
)

; Creates an empty population of the next generation
(defmethod empty-population ((cp population) &aux np)
  (setf np (make-instance 'population))
  (setf (population-individuals np) '())
  (setf (population-generation np) (+ 1 (population-generation cp)))
  np
)

; Demoes the copies-evolution methods
(defmethod perform-copies-demo (&aux cp np)
  (setf cp (initial-population))
  (setf np (empty-population cp))
  (terpri)
  (display np)
  (terpri)
  (terpri)
  (setf *select-demo* t)
  (setf *copy-demo* t)
  (dotimes (i 10)
    (perform-one-copy cp np)
    (terpri)
    (display np)
    (terpri)
    (terpri)
  )
  (setf *select-demo* nil)
  (setf *copy-demo* nil)
)

;; TASK 10

; Puts crossovers of the current population into the new population
(defmethod perform-crossovers ((cp population) (np population))
  (dotimes (i (nr-crossovers))
    (perform-one-crossover cp np)
  )
)

; Determines the number of crossover individuals that will
; be added for evolution
(defmethod nr-crossovers ()
  (* (/ *pc-x* 100) *population-size*)
)

; Moves one crossover individual to the new population, possibly mutating
(defmethod perform-one-crossover ((cp population) (np population))
  (let (x m mm mother father new-i)
    (setf mother (select-individual cp))
    (setf father (select-individual cp))
    (if *crossover-demo* (format t "Selected mother = ~%"))
    (if *crossover-demo* (display mother))
    (if *crossover-demo* (format t "Selected father = ~%"))
    (if *crossover-demo* (display father))
    (setf m (crossover mother father))
    (if *crossover-demo* (format t "the crossover = ~&"))
    (if *crossover-demo* (display m))
    (setf mm (maybe-mutate m))
    (if *crossover-demo* (format t "the possibly-mutated individual = ~&"))
    (if *crossover-demo* (display mm))
    (setf (individual-number mm) (+ 1 (size np)))
    (if *crossover-demo* (format t "the renumbered individual = ~&"))
    (if *crossover-demo* (display mm))
    (setf new-i (new-individual (+ 1 (size np)) (individual-rbg-string mm)))
    (setf
      (population-individuals np)
      (append (population-individuals np) (list new-i))
    )
  )
)

; Performs a crossover on two individuals
(defmethod crossover ((mother individual) (father individual) &aux mi fi cross i)
  (setf mi (individual-rbg-string mother))
  (setf fi (individual-rbg-string father))
  (setf cross (crossover mi fi))
  (setf i (new-individual 0 cross))
  i
)

; Demoes the crossover-evolution methods
(defmethod perform-crossovers-demo (&aux cp np)
  (setf cp (initial-population))
  (setf np (empty-population cp))
  (terpri)
  (display np)
  (terpri)
  (terpri)
  (setf *select-demo* t)
  (setf *crossover-demo* t)
  (dotimes (i 10)
    (perform-one-crossover cp np)
    (terpri)
    (display np)
    (terpri)
    (terpri)
  )
  (setf *select-demo* nil)
  (setf *copy-demo* nil)
)

;; TASK 11 (partial)

; Summarizes a population 'p
(defmethod summarize ((p population))
  (display p)
  (check-average p)
  (terpri)
)

; Displays the average fitness of a population
(defmethod check-average ((p population))
  (format t "average fitness of population ~A = ~A~%"
    (population-generation p)
    (average p)
  )
)
