;; Unused for history -------------------------------------------------------------------------
(defmacro <*>!% (lis-fn)
  "The applicative for lists"
  (let ((x (gensym)))
    `(mapcar (lambda (,x) (curry ,@x)) ,lis-fn)))

(defmacro <*>!%% (lis-fn lis)
  "The applicative for lists"
  `(join (mapcar ,(lambda (fn) (eval `(mapcar (curry ,@fn) ,lis))) ,lis-fn)))

;; Was slower than the macro version due to how function calls can be reduced... more direct in the class graph 
(defun <>% (&rest str)
  (apply #'concatenate (cons 'string str)))

(defmacro <>!% (&body lis)
  `(append ,@lis))

(declaim (ftype (function (fixnum function &rest t) function) curryf-num%))
(defun curryf-num% (num fn &optional &rest args)
  (let ((left num))
    (labels ((tco (fn &rest args2)
               (if (= left num)
                   (progn
                     (setf left (- left (length args2)))
                     (setf args2 (append args args2)))
                   (setf left (- left (length args2))))
               (if (> left 0)
                   (curryf #'tco (apply (curryf #'curryf fn) args2))
                   (apply fn args2))))
      (curryf #'tco fn))))
;; Eval is evil for macros!!!!!!!!!!!!!!-------------------------------------------------------

(defmacro flip% (fn x y . rest)
  "Flips the first two arguments that is applied to a function"
  `(if (listp ',fn)
       (eval (append ',fn '(,y) '(,x) ,rest))
       (,fn ,y ,x . ,rest)))

;; The eval on the macro makes the processor  :: 4,652 processor cycles -->   1,576,876 processor cycles
(defmacro currys%% (num fn . args)
  "Creates a partially applied function that takes 1 argument"
  (let ((fun (gensym))
        (curr (gensym)))
    `(reduce (lambda (,fun ,curr) (cons ,curr ,fun))
             (cons '(curry ,fn ,@args) (gensymbol-list (- ,num 1) 'curry)))))

(defmacro currys% (num fn . args)
  "Creates a partially applied function that takes 1 argument"
  (let ((clos (gensym))
        (iter (gensym)))
    `(progn
       (let ((,clos nil))
         (setf ,clos '(curry ,fn ,@args))
         (dolist (,iter (=<< (range) (- ,num) 1) (eval ,clos))
           (declare (ignore ,iter))
           (setf ,clos (cons 'curry ,clos)))))))


;; Can't just (apply #'curry) sadly because macros aren't first class citizens
;; Rewrite without eval in it
(defun <*>!%%% (lis-fn lis)
  "The applicative for lists"
  (>>=! lis-fn
        (lambda (fn) (mapcar (lambda (x) (eval (append fn (list x))))
                        lis))))

;; ODD BROKEN BEHAVIOR--------------------------------------------------------------
;; (defmacro curryf% (fn . args)
;;   "Creates a partially applied function that takes 1 argument"
;;   (lambda (&rest args2) (apply fn (append args args2))))
;; (defmacro curry% (fn . args)
;;   "Creates a partially applied function that takes 1 argument"
;;   `(if (functionp (macro-function ',fn))
;;        (currym% ,fn ,@args)
;;        (curryf% ,fn ,@args)))
;; (defmacro curryf (fn . args)
;;   "Creates a partially applied function that takes 1 argument"
;;   (let ((args2 (gensym)))
;;     `(lambda (&rest ,args2) (apply ,fn (append ,args ,args2)))))
