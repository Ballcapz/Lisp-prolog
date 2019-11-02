(defun large_atom (L)
    (labels ((atomize (L A)
        (cond ((null L) A)
            ((consp (car L))
                (atomize (cdr L) (atomize (car L) A)))
            (t (atomize (cdr L) (cons (car L) A))))))
    (apply #'max (atomize L nil))))


(defun biggerthan (num L)
    (cond 
        ((null L) 
            nil)
        ((> (car L) num)
            (cons (car L) (biggerthan num (cdr L))))
        (t 
            (biggerthan num (cdr L)))))

(defun doubleodd (L)
    (cond
        ((null L)
            nil)
        ((oddp (car L))
            (cons (* 2 (car L)) (doubleodd (cdr L))))
        (t
            (cons (* 1 (car L)) (doubleodd (cdr L))))))

(defun evenprod (L)
    (cond
        ((null L)
            1)
        ((evenp (car L))
            (* (car L) (evenprod (cdr L))))
        (t (evenprod (cdr L)))    
))