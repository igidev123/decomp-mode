(defun neg-vec-get (vector index)
  "Get the element from a vector at INDEX, allowing negative indexing."
  (if (< index 0)
      (aref vector (+ (length vector) index))
    (aref vector index)))
(defun neg-vec-set (vector index value)
  "Set the element in a vector at INDEX to VALUE, allowing negative indexing."
  (if (< index 0)
      (setf (aref vector (+ (length vector) index)) value)
    (setf (aref vector index) value)))


(defun shortest-edit (a n b m &optional test)
  "Finds the differences between two sequences A of length N and B of length M,
   with elements comparable under the function TEST. Returns a table suitable
   for the BACKTRACK function."
  (unless test (setq test #'equal))
  (let* ((max (+ n m))
         (v (make-vector (+ max max 3) nil))
         (trace nil)
         (result '()))

    ;; Compute the diff using Myers' algorithm
    (neg-vec-set v 1 0)
    (catch 'done
      (dotimes (d (1+ max))
        (dotimes (k* (1+ d))
          (let* ((k (- (+ k* k*) d))
                 (x (if (or (eq k (- d))
                            (and (not (eq k d))
                                 (< (neg-vec-get v (1- k))
                                    (neg-vec-get v (1+ k)))))
                        (neg-vec-get v (1+ k))
                        (1+ (neg-vec-get v (1- k)))))
                 (y (- x k)))
            (while (and (< x n) (< y m)
                        (funcall test (nth x a) (nth y b)))
              (setq x (1+ x))
              (setq y (1+ y)))
            (neg-vec-set v k x)
            (when (and (>= x n) (>= y m))
              (push v trace)
              (throw 'done d))))
        (message "%s" v)
        (push (vconcat v) trace)))
    (message "%s" v)
    trace))

(defun backtrack (trace n m)
  "Backtracks a TRACE as returned by the SHORTEST-EDIT function to determine
   the shortest set of edit required. Returns a list containing the indexes
   of unchanged lines."
  (let ((x n)
        (y m)
        (d (length trace))
        (result '()))
    (dolist (v trace)
      (setq d (1- d))
      (message "%s %d (%d,%d)" v d x y)
      (let* ((k (- x y))
             (prev-k (progn (message "%d" k)
                            (if (or (eq k (- d))
                                    (and (not (eq k d))
                                  (< (neg-vec-get v (1- k))
                                     (neg-vec-get v (1+ k)))))
                                (1+ k)
                              (1- k))))
             (prev-x (neg-vec-get v prev-k))
             (prev-y (- prev-x prev-k)))
        (while (and (< prev-x x) (< prev-y y))
          (push (list x y) result)
          (setq x (1- x))
          (setq y (1- y)))
        (message "%s %d k=%d prev-k=%d (%d,%d) ← (%d,%d)" result d k prev-k x y prev-x prev-y)
        (setq x prev-x)
        (setq y prev-y)))
    result))

(defun diff-output (a b matches)
  "Takes two lists A and B as well as a list of MATCHES as returned by BACKTRACK
   and returns a list containing an aligned diff of A and B."
  (let ((x 1)
        (y 1)
        (result '()))
    (dolist (match matches)
      (message "match: %s (%d,%d)" match x y)
      (let ((u (car  match))
            (v (cadr match)))
        (message "match: %d %d" u v)
        ;; Insert non-matching lines
        (while (and a b (< x u) (< y v))
          (message "a+b: %s (%d,%d)" match x y)
          (push (list :! (car a) (car b)) result)
          (setq x (1+ x))
          (setq y (1+ y))
          (setq a (cdr a))
          (setq b (cdr b)))
        ;; Insert widowed lines from A
        (while (and a (< x u))
          (message "a  : %s (%d,%d)" match x y)
          (push (list :A (car a) nil) result)
          (setq x (1+ x))
          (setq a (cdr a)))
        ;; And from B
        (while (and b (< y v))
          (message "  b: %s (%d,%d)" match x y)
          (push (list :B nil (car b)) result)
          (setq y (1+ y))
          (setq b (cdr b))))
      ;; Finally, insert the matching line
      (push (list := (car a) (car b)) result)
      (setq x (1+ x))
      (setq y (1+ y))
      (setq a (cdr a))
      (setq b (cdr b)))

    ;; Insert remaining non-matching lines
    (while (and a b)
      (push (list :! (car a) (car b)) result)
      (setq x (1+ x))
      (setq y (1+ y))
      (setq a (cdr a))
      (setq b (cdr b)))
    ;; Insert widowed lines from A
    (while a
      (push (list :A (car a) nil) result)
      (setq x (1+ x))
      (setq a (cdr a)))
    ;; And from B
    (while b
      (push (list :B nil (car b)) result)
      (setq y (1+ y))
      (setq b (cdr b)))
    result))

(defun diff (a b &optional test)
  "Computes the difference between lists A and B containing elements comparable
   under TEST. The output contains elements of the form (OP LINE-A LINE-B),
   where OP can be := if the lines are matching, :! if the lines are
   non-matching, :A if there is no LINE-B, or :B if there is no LINE-A."
  (unless test (setq test #'equal))
  (let ((n (length a))
        (m (length b)))
    (let ((matches (backtrack
                    (shortest-edit a n b m test)
                    n m)))
      (message "matches : %s" matches)
      (diff-output a b matches))))
