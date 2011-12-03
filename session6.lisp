(defpackage :session-6
    (:use :cl :unit-test :alexandria))

(in-package :session-6)

(defun new-generation (&rest coords)
  (let ((generation (make-hash-table :test 'equal)))
    (loop for (x y) on coords by #'cddr
          do (setf (gethash (cons x y) generation) t))
    generation))

(defun alive-p (generation x y)
  (gethash (cons x y) generation))

(defun (setf alive-p) (new-value generation x y)
  (when new-value
    (setf (gethash (cons x y) generation) t)))

(defmacro do-neighbors ((x y of-x of-y &key and-myself-p) &body body)
  `(loop for ,x from (- ,of-x 1) upto (+ ,of-x 1)
         do (loop for ,y from (- ,of-y 1) upto (+ ,of-y 1)
                  ,@(if and-myself-p
                        `(do (progn ,@body))
                        `(when (not (and (= ,of-x ,x)
                                         (= ,of-y ,y)))
                           do (progn ,@body))))))

(defmacro do-alive-cells ((x y generation) &body body)
  (with-gensyms (key value)
    `(maphash (lambda (,key ,value)
                (declare (ignore ,value))
                (destructuring-bind (,x . ,y) ,key
                  ,@body))
              ,generation)))

(defun count-neighbors (generation x y)
  (let ((count 0))
    (do-neighbors (neighbor-x neighbor-y x y)
      (when (alive-p generation neighbor-x neighbor-y)
        (incf count)))
    count))

(defun next-generation (old-generation)
  (let ((next-generation (new-generation))
        (processed-cells (make-hash-table :test #'equal)))
    (do-alive-cells (old-x old-y old-generation)
      (do-neighbors (x y old-x old-y :and-myself-p t)
        (unless (gethash (cons x y) processed-cells)
          (setf (gethash (cons x y) processed-cells) t)
          (let ((neighbors-of-this-cell (count-neighbors old-generation x y)))
            (setf (alive-p next-generation x y)
                  (if (alive-p old-generation x y)
                      (<= 2 neighbors-of-this-cell 3)
                      (= 3 neighbors-of-this-cell)))))))
    next-generation))

(deftest 'session-6 "")
