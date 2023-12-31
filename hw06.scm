(define (no-repeats lst)
  (if 
    (null? lst) '()
    (cons (car lst)
    (no-repeats (filter (lambda (x) (not (= (car lst) x))) (cdr lst))))
    )
)

(define (student-attend-class student class)
  (let  ((student-name (student-get-name student))
        (student-class(student-get-classes student)))
        (student-create student-name (cons  class student-class)))
  )


(define (teacher-hold-class teacher)
    (let ((teacher-name (teacher-get-name teacher))
          (teacher-class (teacher-get-class teacher))
          (teacher-students (teacher-get-students teacher)))
        (let ((updated-students 
               (map (lambda (student) (student-attend-class student teacher-class)) teacher-students)))
            (teacher-create teacher-name teacher-class updated-students))))


(define (add-leaf t x)
    (if (is-leaf t)
        t
        (let ((tree-label (label t))
              (tree-branches (branches t)))
            (let ((mapped-branches (map (lambda (branch) (add-leaf branch x)) tree-branches)))
                (tree tree-label (append mapped-branches (list (tree x '()))))))))




