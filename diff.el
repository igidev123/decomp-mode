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
  (let* ((dlla (list-to-dll a)) ; use doubly-linked lists for faster lookup
         (curx 0)
         (dllb (list-to-dll b))
         (cury 0)
         (max (+ n m))
         (v (make-vector (+ max max 3) nil))
         (trace nil)
         (result '()))

    ;; Start with a virtual move from (0 -1) to (0 0)
    (debug-message "%s" dlla)
    (debug-message "%s" dllb)
    (neg-vec-set v 1 0)
    (catch 'done
      (dotimes (d (1+ max))
        (debug-message "%s" v)
        (dotimes (k* (1+ d))
          (let* ((k (- (+ k* k*) d))
                 (x (if (or (eq k (- d))
                            (and (not (eq k d))
                                 (< (neg-vec-get v (1- k))
                                    (neg-vec-get v (1+ k)))))
                        (neg-vec-get v (1+ k))
                        (1+ (neg-vec-get v (1- k)))))
                 (y (- x k)))
            (when (and (< x n) (< y m))
              (setq dlla (dll-advance dlla (- x curx)))
              (setq curx x)
              (setq dllb (dll-advance dllb (- y cury)))
              (setq cury y)
              (while (and (< curx n) (< cury m)
                          (funcall test (dll-get-value dlla) (dll-get-value dllb)))
                (setq dlla (dll-next dlla))
                (setq curx (1+ curx))
                (setq dllb (dll-next dllb))
                (setq cury (1+ cury)))
              (neg-vec-set v k curx))
            (when (or (>= x n) (>= y m))
              (neg-vec-set v k x))
            (when (and (>= curx n) (>= cury m))
              (push v trace)
              (throw 'done d))))
        (push (vconcat v) trace)))
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
      (let* ((k (- x y))
             (prev-k (if (or (eq k (- d))
                             (and (not (eq k d))
                                  (< (neg-vec-get v (1- k))
                                     (neg-vec-get v (1+ k)))))
                         (1+ k)
                       (1- k)))
             (prev-x (neg-vec-get v prev-k))
             (prev-y (- prev-x prev-k)))
        (while (and (< prev-x x) (< prev-y y))
          (push (list x y) result)
          (setq x (1- x))
          (setq y (1- y)))
        (debug-message "%s %d k=%d prev-k=%d (%d,%d) ← (%d,%d)" v d k prev-k x y prev-x prev-y)
        (setq x prev-x)
        (setq y prev-y)))
    result))

(defun diff-output (a b matches)
  "Takes two lists A and B as well as a list of MATCHES as returned by BACKTRACK
   and returns a list containing an aligned diff of A and B."
  (debug-message "%s" matches)
  (let ((x 1)
        (y 1)
        (result '()))
    (dolist (match matches)
      (let ((u (car  match))
            (v (cadr match)))
        ;; Insert non-matching lines
        (while (and a b (< x u) (< y v))
          (debug-message "a+b: %s (%d,%d)" match x y)
          (setq result (nconc result (list (list :! (car a) (car b)))))
          (setq x (1+ x))
          (setq y (1+ y))
          (setq a (cdr a))
          (setq b (cdr b)))
        ;; Insert widowed lines from A
        (while (and a (< x u))
          (debug-message "a  : %s (%d,%d)" match x y)
          (setq result (nconc result (list (list :A (car a) nil))))
          (setq x (1+ x))
          (setq a (cdr a)))
        ;; And from B
        (while (and b (< y v))
          (debug-message "  b: %s (%d,%d)" match x y)
          (setq result (nconc result (list (list :B nil (car b)))))
          (setq y (1+ y))
          (setq b (cdr b))))
      ;; Finally, insert the matching line
      (setq result (nconc result (list (list := (car a) (car b)))))
      (setq x (1+ x))
      (setq y (1+ y))
      (setq a (cdr a))
      (setq b (cdr b)))

    ;; Insert remaining non-matching lines
    (while (and a b)
      (setq result (nconc result (list (list :! (car a) (car b)))))
      (setq x (1+ x))
      (setq y (1+ y))
      (setq a (cdr a))
      (setq b (cdr b)))
    ;; Insert widowed lines from A
    (while a
      (setq result (nconc result (list (list :A (car a) nil))))
      (setq x (1+ x))
      (setq a (cdr a)))
    ;; And from B
    (while b
      (setq result (nconc result (list (list :B nil (car b)))))
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
    (diff-output
     a b
     (backtrack
      (shortest-edit a n b m test)
      n m))))

;; (diff (list "a" "b" "c" "a" "b" "b" "a") (list "c" "b" "a" "b" "a" "c"))
