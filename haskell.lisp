;; This would be a lot more elegant if Common Lisp was a Lisp-1!!!

(ql:quickload '(:fare-quasiquote-readtable
                :trivia
                :swank
                :let-over-lambda))

(defpackage #:haskell-lisp
  (:nicknames hl)
  (:documentation "Abstractions Inspired from Haskell")
  (:use #:let-over-lambda)
  (:shadowing-import-from #:let-over-lambda #:when-match #:if-match)
  (:use #:swank-backend
        #:common-lisp
        #:trivia)
  (:export :join :flip
           :curry :currys
           :curryl :compose
           :<*> :<*>!
           :<>  :<>!  :<>!!
           :>>= :>>=! :=<< :=<<!))

(in-package :haskell-lisp)
;;; Miscellaneous Haskell Commands-------------------------------------------------------------
(defun join (lis)
  "((1) (2)) --> (1 2) removes 1 layer of a list"
  (apply #'append lis))

(defmacro flip (fn x y . rest)
  "Flips the first two arguments that is applied to a function"
  `(if (listp ',fn)                             ; check if the fn has arguments already applied to it (>>= (+))
       (macrolet ((flip-fn-list (fn x y . rest) ; need to create a local macro because we can't splice a non-list
                    `(if (null ',@rest)
                         (,@fn ,y ,x)
                         (,@fn ,y ,x ,rest))))
         (flip-fn-list ,fn ,x ,y ,rest))
       (,fn ,y ,x . ,rest)))

(defmacro curry (fn . args)
  "Creates a partially applied function that takes 1 argument if it is a macro
   (a limitation of &rest closures in CL) and multiple if it is a function"
  (if (functionp (macro-function fn))
      `(currym ,fn ,@args)
      `(curryf #',fn ,@args)))

;; Maybe use macrolet to create our lexical closure or at least get the list so we can take multiple arguments
(defmacro currym (fn . args)
  "Creates a partially applied function that takes 1 argument"
  (let ((arg (gensym)))
    `(lambda (,arg) (,fn ,@args ,arg))))

(declaim (ftype (function (function &rest t) function) curryf)
         (inline curryf))
(defun curryf (fn &rest args)
  "Creates a partially applied function that takes many argument"
  (lambda (&rest args2) (apply fn (append args args2))))

;; can now take variables as input!! (let ((y 2)) (currys 2 + 1 2 3))
(defmacro currys (num fn . args)
  "Creates a partially applied function that takes 1 argument"
  (if (integerp num)                    ; can't expand the environment optimally if a number isn't directly passed
      (if (functionp (macro-function fn))
          `(currym ,@(gensymbol-list (1- num) 'currym) ,fn ,@args)
          `(curryf ,@(gensymbol-list (1- num) #'curryf) #',fn ,@args))
      `(apply #'curryf (nconc (gensymbol-list (1- ,num) #'curryf)
                              (list (curry ,fn ,@args))))))

(declaim (ftype (function ((integer 0) function &rest t) function) curryf-num))
(defun curryf-num (num fn &rest args)
  "contentiously curries a function until the num has been surpassed"
  (lambda (&rest args2)
    (let ((left (- num (length args2)))
          (args-comb (append args args2)))
      (declare (type integer left))
      (if (> left 0)
          (curryf-num left
                      (apply (curryf #'curryf fn) args-comb))
          (apply fn args-comb)))))

;;  Will correctly display the right amount for &rest but not for &optional and &keyword yet
;; arglist is also very slow (8k processor cycles!!!) so make sure to optimize this away by having it only expand in a macro!!
(defun num-args (fn)
  "Gets the number of args in a function"
  (let* ((args (arglist fn))
         (len (length args)))
    (if (and (< 2 len) (member '&rest args))
        (- len 2)
        len)))

(defun auto-curryf (fn &rest args)
  (apply #'curryf-num (list* (num-args fn) fn args)))

;; Can't take (let ((y +)) (auto-curry y)) but can do (auto-curry +) with 10k less processor cycles than auto-curryf
(defmacro auto-curry (fn &rest args)
  (if (null args)
      `(curryf-num ,(num-args fn) #',fn)
      `(curryf-num ,(num-args fn) #',fn ,@args)))

(defmacro curryl (&rest fn-list)
  "curries a list by default 1... if you supply a number as the
     first argument it will curry the entire list by that amount"
  (flet ((struct (lambda list)
           `(list ,@(mapcar lambda list))))
    (let ((1st (car fn-list)))
      (if (numberp 1st)
          (struct (lambda (x) `(currys ,1st ,@x)) (cdr fn-list))
          (struct (lambda (x) `(curry ,@x)) fn-list)))))

;; From Practical Common Lisp
(defun compose (&rest fns)
  "Returns a function like F(G(x)).... the functions,
   when applied happen in reverse order of how they were inputted"
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
        (lambda (&rest args)
          (reduce #'funcall fns
                  :from-end t
                  :initial-value (apply fn1 args))))
      #'identity))

;; Read/Partially Applied Functions------------------------------------------------------------
(defmacro <*> (fn-1 fn-2 arg)
  "The applicative for partially applied functions... (f x (g x))"
  (let ((x (gensym)))
    `(let ((,x ,arg))
       (,@fn-1 ,x (,@fn-2 ,x)))))

(defmacro =<< (fn-1 fn-2 arg &optional &rest extra)
  "the reverse bind for partially applied functions (f (g x) x) || (f . g) x x"
  (let ((x (gensym)))
    `(let ((,x ,arg))
       (,@fn-1 (,@fn-2 ,x) ,x ,@extra))))


(defmacro >>= (fn-1 fn-2 arg)
  "the bind for partially applied functions (f (g x) x) || (f . g) x x"
  `(=<< ,fn-2 ,fn-1 ,arg))

;; For Lists-----------------------------------------------------------------------------------

;; call wtih (<*>! (list (curry + 2) (curry - 3)) '(1 2 3 ))
(defun <*>! (lis-fn lis)
  "The applicative for lists "
  (>>=! lis-fn
        (lambda (fn) (mapcar (lambda (x) (funcall fn x))
                        lis))))

(defmacro fun-append (fn num)
  `(progn (princ ,fn) (,@fn ,num)))


(defun =<<! (fn lis)
  "The Reverse Bind Monad for Lists "
  (mapcan fn lis))

(defun >>=! (lis fn)
  "The Bind Monad for lists "
  (mapcan fn lis))

;; Rerwrite with fast-apply later


;; The Monoid ---------------------------------------------------------------------------------

(defmacro <> (&body str)
  "The monoid for Strings "
  `(concatenate 'string ,@str))

(defun <>! (&rest lis)
  "The monoid for lists"
  (apply #'append lis))

(defmacro <>!! (&body vec)
  "The monoid for Vectors "
  `(concatenate 'vector ,@vec))

;; Helper functions----------------------------------------------------------------------------

(defun gensymbol-list (num word)
  (loop for i from 1 to num
     collect word))

(defun range (max &optional (min 0) (step 1))
  (loop for x from min to max by step
     collect x))

(defmacro fn-print (fn)
  `(progn (print ',fn)
          (print ,fn)
          (print "#######")
          ,fn))

(defmacro map-fn-print (&rest fns)
  `(list ,@(mapcar (lambda (x) `(fn-print ,x)) fns)))


;;; Fun Testing--------------------------------------------------------------------------------
;;; General functions--------------------------------------------
;; FLIP IS BROKEN IN RACKET!!!
;; (flip (<*> (* 2)) 3 (-))
;; (flip + 2 3)

(defun test-general ()
  (map-fn-print
   ;; (flip (<*> (* 2)) 3 (-))
   (funcall (curry + 1 2 3) 3)
   (mapcar (curry expt 2) '(1 2 3 4))
   (funcall (compose #'list #'apply) #'+ '(1 2 3 4))
   (funcall (funcall (apply #'compose (curryl (curry + 1 2 3) (- 2 3))) 3) 2)
   ;; (curryl 2 (+ 1 2 3) (- 2 3 4))
   ))

;;; Lists--------------------------------------------------------
(defun test-list ()
  (map-fn-print
   (<*>! (list (curry + 2) (curry - 3)) '(1 2 3))
   (>>=! '((1 2 3) (2 3 4)) (curry <*>! (list (curry + 2) (curry + 3))))))

(defun test-reader ()
  (map-fn-print 
   (<*> (+ 3) (/ 2) 3)
   (mapcar (curry <*> (+ 3) (/ 2)) '(2 3 4))
   (=<< (+ 3) (/ 2) 3)
   (>>= (+ 3) (/ 2) 3)))
;; Read/Partially Applied Functions------------------------------


;;; Planned features---------------------------------------------------------------------------

;;  We will eventually want a macro that will put funcalls as
;; long as we have arguments or until the closure is finished

;; Macro that adds implicit currying... then add it to the various definitions we have here

;; Add syntax for auto currying based on a default

;; ([+ 1 2 3 :apply 2] 2)
;; ([+ 1 2 3 :a 2] 2)
;; ([+ 1 2 3 :flip-apply 2] 2)
;; ([+ 1 2 3 :b 2] 2)
