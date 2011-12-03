(defpackage :life
    (:use :cl :unit-test :alexandria))

(in-package :life)

(defun new-generation (&optional coords)
  (let ((generation (make-hash-table :test 'equal)))
    (loop for (x y) on coords by #'cddr
          do (setf (gethash (cons x y) generation) t))
    generation))

(defun alive-p (generation coords)
  (gethash coords generation))

(defun (setf alive-p) (new-value generation coords)
  (when new-value
    (setf (gethash coords generation) t)))

(defmacro do-neighbors ((coords of-coords &key and-myself-p) &body body)
  (with-gensyms (x y)
    `(loop for ,x from (- (car ,of-coords) 1) upto (+ (car ,of-coords) 1)
           do (loop for ,y from (- (cdr ,of-coords) 1) upto (+ (cdr ,of-coords) 1)
                    for ,coords = (cons ,x ,y)
                    ,@(if and-myself-p
                          `(do (progn ,@body))
                          `(unless (equal ,coords ,of-coords)
                             do (progn ,@body)))))))

(defmacro do-alive-cells ((coords generation) &body body)
  (with-gensyms (value)
    `(maphash (lambda (,coords ,value)
                (declare (ignore ,value))
                ,@body)
              ,generation)))

(defun count-neighbors (generation coords)
  (let ((count 0))
    (do-neighbors (neighbor-coords coords)
      (when (alive-p generation neighbor-coords)
        (incf count)))
    count))

(defun next-generation (old-generation)
  (let ((next-generation (make-hash-table :test 'equal :size (hash-table-size old-generation)))
        (processed-cells (make-hash-table :test 'equal)))
    (do-alive-cells (old-coords old-generation)
      (do-neighbors (coords old-coords :and-myself-p t)
        (unless (gethash coords processed-cells)
          (setf (gethash coords processed-cells) t)
          (let ((number-of-neighbors-of-this-cell (count-neighbors old-generation coords)))
            (setf (alive-p next-generation coords)
                  (if (alive-p old-generation coords)
                      (<= 2 number-of-neighbors-of-this-cell 3)
                      (= 3 number-of-neighbors-of-this-cell)))))))
    next-generation))

(deftest 'life "empty generation has no cells"
  (let ((generation (new-generation)))
    (test-equal (count-neighbors generation (cons 0 0)) 0)
    (let ((counted-by-loop 0))
      (do-alive-cells (coords generation)
        (declare (ignore coords))
        (incf counted-by-loop))
      (test-equal counted-by-loop 0))))

(deftest 'life "basic neighboring test with one cell"
  (let ((generation (new-generation '(0 0))))
    (test-equal (count-neighbors generation (cons 0 0)) 0)
    (let ((counted-by-loop 0))
      (do-alive-cells (coords generation)
        (declare (ignore coords))
        (incf counted-by-loop))
      (test-equal counted-by-loop 1))
    (test-equal 1 (count-neighbors generation (cons -1 -1)))
    (test-equal 1 (count-neighbors generation (cons 0 -1)))
    (test-equal 1 (count-neighbors generation (cons 1 -1)))
    (test-equal 1 (count-neighbors generation (cons -1 0)))
    (test-equal 0 (count-neighbors generation (cons 0 0)))
    (test-equal 1 (count-neighbors generation (cons 1 0)))
    (test-equal 1 (count-neighbors generation (cons -1 1)))
    (test-equal 1 (count-neighbors generation (cons 0 1)))
    (test-equal 1 (count-neighbors generation (cons 1 1)))))

(deftest 'life "iterator count matches hash-table count"
  (let ((generation (new-generation '(0 0 0 1 0 2))))
    (test-equal 3 (hash-table-count generation))
    (test-equal (hash-table-count generation)
                (let ((count 0))
                  (do-alive-cells (coords generation)
                    (declare (ignore coords))
                    (incf count))
                  count))))

(deftest 'life "do-neighbors test"
  (test-equal 8 (let ((count 0))
                  (do-neighbors (coords (cons 0 0))
                    (incf count))
                  count))
  (test-equal 9 (let ((count 0))
                  (do-neighbors (coords (cons 0 0) :and-myself-p t)
                    (identity coords)   ; silence warning
                    (incf count))
                  count)))

(deftest 'life "count neighbors"
  (let ((generation (new-generation '(0 0 0 1 0 2))))
    (macrolet ((expect-neighbor-count (count x y)
                 `(test-equal ,count (count-neighbors generation (cons ,x ,y)))))
      ;;
      (expect-neighbor-count 0 -1 -2)
      (expect-neighbor-count 1 -1 -1)
      (expect-neighbor-count 2 -1 0)
      (expect-neighbor-count 3 -1 1)
      (expect-neighbor-count 2 -1 2)
      (expect-neighbor-count 1 -1 3)
      (expect-neighbor-count 0 -1 4)
      ;;
      (expect-neighbor-count 0 1 -2)
      (expect-neighbor-count 1 1 -1)
      (expect-neighbor-count 2 1 0)
      (expect-neighbor-count 3 1 1)
      (expect-neighbor-count 2 1 2)
      (expect-neighbor-count 1 1 3)
      (expect-neighbor-count 0 1 4)
      ;;
      (expect-neighbor-count 2 0 1))))

(deftest 'life "blinker"
  (let ((generation (new-generation '(0 0 0 1 0 2))))
    (macrolet ((expect-alive (x y)
                 `(test-assert (alive-p generation (cons ,x ,y)))))
      (setf generation (next-generation generation))
      (expect-alive 1 1)
      (expect-alive 0 1)
      (expect-alive -1 1)
      (test-equal 3 (hash-table-count generation))
      (setf generation (next-generation generation))
      (expect-alive 0 0)
      (expect-alive 0 1)
      (expect-alive 0 2)
      (test-equal 3 (hash-table-count generation)))))