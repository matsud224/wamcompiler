;;(defun main ()
;;  (format t "hello,world~%"))
;;
;;(sb-ext:save-lisp-and-die "wamcompiler"
;;			  :toplevel #'main
;;			  :executable t)

(defvar *unbound-variable* (gensym))

(defvar *operator-list* nil)
(defvar *clause-code-table* (make-hash-table :test #'equal)) ;; key (functor . arity). 
(defvar *dispatching-code-table* (make-hash-table :test #'equal))


(define-condition prolog-query-failed (simple-condition) ())

(define-condition prolog-found-solution (simple-condition)
  ((vars :initarg :vars
	 :accessor vars)
   (can-backtrack? :initarg :can-backtrack?
		   :accessor can-backtrack?)))

(define-condition prolog-syntax-error (simple-error)
  ((cause :initarg :cause
	  :initform nil
	  :accessor cause))
  (:report (lambda (condition stream)
	     (format stream "Syntax error!")
	     (when (cause condition)
	       (format stream " (~A)" (cause condition))))))

(define-condition prolog-bad-instruction-error (simple-error)
  ((inst :initarg :inst
	 :initform nil
	 :accessor bad-inst))
  (:report (lambda (condition stream)
	     (format stream "Bad instruction: ~A" (bad-inst condition)))))


(defmacro op-atom (x) `(car ,x))
(defmacro op-prec (x) `(cadr ,x))
(defmacro op-assoc (x) `(caddr ,x))

(defmacro dostream (bind &body body)
  `(loop (let ((,(car bind) (read-char ,(cadr bind))))
	   (if (null ,(car bind)) (return nil)) ,@body)))


(defvar *non-alphanum-chars*
  '(#\# #\$ #\& #\* #\+ #\- #\. #\/ #\: #\< #\= #\> #\? #\@ #\^ #\~ #\\))

(defun get-token (s)
  (skip-whitespace s)
  (let ((c (read-char s)))
    (cond ((null c) nil)
	  ((eq c #\%) (skip-comment s) (get-token s))
	  ((eq c #\)) '(rparen))
	  ((eq c #\() '(lparen))
	  ((eq c #\]) '(rbracket))
	  ((eq c #\[) '(lbracket))
	  ((eq c #\|) '(vertical-bar))
	  ((eq c #\,) (cons 'atom '|,|))
	  ((eq c #\;) (cons 'atom '|;|))
	  ((eq c #\!) (cons 'atom '|!|))
	  ((digit-char-p c) (unread-char c s) (cons 'int (read-int s)))
	  ((member c *non-alphanum-chars*)
	   (unread-char c s) (read-non-alphanum-atom s))
	  (t (unread-char c s) (read-alphanum-atom s)))))

(defun skip-whitespace (s)
  (dostream (c s)
    (unless (or (not (graphic-char-p c)) (eq c #\Space))
      (unread-char c s) (return nil))))

(defun skip-comment (s)
  (dostream (c s)
    (if (eq c #\Newline) (return nil))))

(defun read-int (s)
  (let ((acc))
    (dostream (c s)
      (unless (digit-char-p c)
	(unread-char c s) (return nil))
      (setq acc (cons c acc)))
    (parse-integer (concatenate 'string (reverse acc)))))

(defun read-alphanum-atom (s)
  (let ((acc))
    (dostream (c s)
      (unless (or (alphanumericp c) (eq #\_ c))
	(unread-char c s) (return nil))
      (setq acc (cons c acc)))
    (let ((str (concatenate 'string (reverse acc))))
      (cond ((or (upper-case-p (char str 0)) (equal str "_"))
	     (cons 'variable (intern str)))
	    (t (cons 'atom (intern str)))))))

(defun read-non-alphanum-atom (s)
  (let ((acc))
    (dostream (c s)
      (unless (member c *non-alphanum-chars*)
	(unread-char c s) (return nil))
      (setq acc (cons c acc)))
    (let ((str (concatenate 'string (reverse acc))))
      (cond ((equal str ".") '(dot))
	    (t (cons 'atom (intern str)))))))

(defun register-operator (op)
  (labels ((op< (x y)
	     (if (eq (op-prec x) (op-prec y))
		 (string< (symbol-name (op-assoc x)) (symbol-name (op-assoc y)))
		 (> (op-prec x) (op-prec y))))
	   (insert-op-sorted (head x)
	     (cond ((null head) (list x))
		   ((op< (car head) x)
		    (cons (car head) (insert-op-sorted (cdr head) x)))
		   (t (cons x head)))))
    (setf (get (op-atom op) 'op) t)
    (setq *operator-list* (insert-op-sorted *operator-list* op))))

(defmacro init-operators (oplist)
  (setq *operator-list* nil)
  (dolist (op oplist)
    (register-operator op)))

(init-operators
 ((|?-| 1200 fx)
  (|:-| 1200 fx)
  (|:-| 1200 xfx)
  (|-->| 1200 xfx)
  (|public| 1150 fx)
  (|mode| 1150 fx)
  (|index| 1150 fx)
  (|extern| 1150 fx)
  (|dynamic| 1150 fx)
  (|bltin| 1150 fx)
  (|###| 1150 fx)
  (|module| 1150 fy)
  (|help| 1150 fy)
  (|;| 1100 xfy)
  (-> 1050 xfy)
  (|,| 1000 xfy)
  (|spy| 900 fy)
  (|nospy| 900 fy)
  (|¥+| 900 fy)
  (|is| 700 xfx)
  (|¥==| 700 xfx)
  (|¥=| 700 xfx)
  (|@>=| 700 xfx)
  (|@>| 700 xfx)
  (|@=<| 700 xfx)
  (|@<| 700 xfx)
  (|>=| 700 xfx)
  (|>| 700 xfx)
  (|=¥=| 700 xfx)
  (|==| 700 xfx)
  (|=<| 700 xfx)
  (|=:=| 700 xfx)
  (|=/=| 700 xfx)
  (|=..| 700 xfx)
  (|=| 700 xfx)
  (< 700 xfx)
  (|:=| 700 xfx)
  (|/==| 700 xfx)
  (|#¥=| 700 xfx)
  (|#>=| 700 xfx)
  (|#>| 700 xfx)
  (|#=<| 700 xfx)
  (|#=| 700 xfx)
  (|#<| 700 xfx)
  (|notin| 580 xfx)
  (|in| 580 xfx)
  (|::| 580 xfy)
  (|..| 560 yfx)
  (|:| 550 xfy)
  (|or| 500 yfx)
  (|and| 500 yfx)
  (|¥/| 500 yfx)
  (|/¥| 500 yfx)
  (- 500 yfx)
  (+ 500 yfx)
  (|rem| 400 yfx)
  (|mod| 400 yfx)
  (>> 400 yfx)
  (<< 400 yfx)
  (// 400 yfx)
  (/ 400 yfx)
  (* 400 yfx)
  (|¥| 200 fy)
  (- 200 fy)
  (+ 200 fy)
  (|**| 200 xfx)
  (^ 200 xfy)))


(defun commap (x)
  (and (eq (car x) 'atom) (eq (cdr x) '|,|)))



(defun parse (stream)
  (let ((tokenstack)) ;;used by unget-tokens
    (labels
	((next-token ()
	   (if (null tokenstack) (get-token stream) (pop tokenstack)))
	 (unget-token (tok)
	   (push tok tokenstack))
	 (next-prec-head (head)
	   (let ((current-prec (op-prec (car head))))
	     (labels ((scan (h)
			(if (and (not (null h)) (eq (op-prec (car h)) current-prec))
			    (scan (cdr h)) h)))
	       (scan head))))
	 (arrange (tree)
	   (case (car tree)
	     (struct (cons (cadr tree) (mapcar #'arrange (cddr tree))))
	     (list (make-listterm (mapcar #'arrange (cdr tree))))
	     (int (cdr tree))
	     (variable (cdr tree))
	     (otherwise	(error (make-condition 'prolog-syntax-error)))))
	 (make-listterm (elements)
	   (if (= (length elements) 2)
	       `(|.| ,@elements)
	       `(|.| ,(car elements) ,(make-listterm (cdr elements)))))
	 (parse-arguments ()
	   (let ((next (next-token)))
	     (if (eq (car next) 'lparen)
		 (labels ((get-args (acc)
			    (let ((arg (parse-sub *operator-list* t))
				  (n (next-token)))
			      (cond ((eq (car n) 'rparen) (cons arg acc))
				    ((commap n)
				     (get-args (cons arg acc)))
				    (t
				     (error (make-condition 'prolog-syntax-error
							    :cause "invalid argument list")))))))
		   (reverse (get-args nil)))
		 (progn (unget-token next) nil))))
	 (parse-list ()
	   (let ((next (next-token)))
	     (if (eq (car next) 'rbracket)
		 (list 'struct '|[]|)
		 (labels
		     ((get-args (acc)
			(let ((arg (parse-sub *operator-list* t))
			      (n (next-token)))
			  (cond ((eq (car n) 'rbracket)
				 (cons (list 'struct '|[]|) (cons arg acc)))
				((eq (car n) 'vertical-bar)
				 (cons
				  (prog1
				      (parse-sub *operator-list* t)
				    (unless (eq (car (next-token)) 'rbracket)
				      (error (make-condition 'prolog-syntax-error
							     :cause "mismatched brackets"))))
				  (cons arg acc)))
				((commap n)
				 (get-args (cons arg acc)))
				(t (error (make-condition 'prolog-syntax-error
							  :cause "near argument list")))))))
		   (unget-token next)
		   `(list ,@(reverse (get-args nil)))))))
	 (parse-prim (ignore-comma)
	   (let ((next (next-token)))
	     (cond ((eq (car next) 'lparen)
		    (let ((inside (parse-sub *operator-list* ignore-comma)))
		      (if (eq (car (next-token)) 'rparen)
			  inside
			  (error (make-condition 'prolog-syntax-error
						 :cause "mismatched parentheses")))))
		   ((eq (car next) 'atom)
		    `(struct ,(cdr next) ,@(parse-arguments)))
		   ((eq (car next) 'lbracket)
		    (parse-list))
		   (t  next))))
	 (separatorp (tok ignore-comma)
	   (or (and ignore-comma (commap tok))
	       (member (car tok) '(rparen dot rbracket vertical-bar))))
	 (operatorp (tok)
	   (and (eq (car tok) 'atom) (get (cdr tok) 'op)))
	 (non-operandp (tok ignore-comma)
	   (or (separatorp tok ignore-comma) (operatorp tok)))
	 (parse-sub (head &optional (ignore-comma nil))
	   (block exit
	     (let ((current-prec (op-prec (car head)))
		   (current-head head)
		   (next-head (next-prec-head head)))
	       (let ((gottok (next-token)))
		 (cond
		   ((separatorp gottok ignore-comma)
		    (error (make-condition 'prolog-syntax-error
					   :cause "unexpected end of clause")))
		   ((null head)
		    (unget-token gottok)
		    (return-from exit (parse-prim ignore-comma)))
		   ((operatorp gottok)
		    (let* ((gottok-2 (let ((n (next-token))) (unget-token n) n))
			   (have-operand (not (non-operandp gottok-2 ignore-comma))))
		      (do ((op (car head) (progn (setq head (cdr head)) (car head))))
			  ((or (null op) (not (eq current-prec (op-prec op)))
			       (not (member (op-assoc op) '(fx fy))))
			   (unget-token gottok))
			(if (and (eq (car gottok) 'atom) (eq (cdr gottok) (op-atom op)))
			    (if have-operand
				(case (op-assoc op)
				  (fx
				   (return-from exit
				     `(struct ,(op-atom op)
					      ,(parse-sub next-head ignore-comma))))
				  (fy
				   (return-from exit
				     `(struct ,(op-atom op)
					      ,(parse-sub current-head ignore-comma))))))))))
		   (t (unget-token gottok))))
	       (let ((operand-1 (parse-sub next-head ignore-comma))
		     (gottok (next-token)))
		 (cond
		   ((separatorp gottok ignore-comma)
		    (unget-token gottok) (return-from exit operand-1))
		   ((operatorp gottok)
		    (let* ((gottok-2 (let ((n (next-token))) (unget-token n) n))
			   (have-operand (not (non-operandp gottok-2 ignore-comma))))
		      (do ((op (car head) (progn (setq head (cdr head)) (car head))))
			  ((or (null op) (not (eq current-prec (op-prec op))))
			   (progn (unget-token gottok) (return-from exit operand-1)))
			(if (and (eq (car gottok) 'atom) (eq (cdr gottok) (op-atom op)))
			    (if have-operand
				(case (op-assoc op)
				  (xfy
				   (return-from exit
				     `(struct ,(op-atom op)
					      ,operand-1
					      ,(parse-sub current-head ignore-comma))))
				  (xfx
				   (return-from exit
				     `(struct ,(op-atom op)
					      ,operand-1
					      ,(parse-sub next-head ignore-comma))))
				  (yfx
				   (unget-token
				    `(struct
				      ,(op-atom op)
				      ,operand-1
				      ,(parse-sub next-head ignore-comma)))
				   (return-from exit
				     (parse-sub current-head ignore-comma))))
				(case (op-assoc op)
				  (xf
				   (return-from exit
				     `(struct ,(op-atom op) ,operand-1)))
				  (yf
				   (unget-token `(struct ,(op-atom op) ,operand-1))
				   (return-from exit
				     (parse-sub current-head ignore-comma)))))))))))))))
      (let ((result (arrange (parse-sub *operator-list*))))
	(if (not (eq (car (next-token)) 'dot))
	    (error (make-condition 'prolog-syntax-error
				   :cause "operator arity error")) result)))))


(defun arity (term) (length (cdr term)))

(defmacro variablep (x) `(atom ,x))
(defmacro anonymousvar-p (x) `(eq ,x '|_|))
(defmacro termp (x) `(listp ,x))
(defmacro listterm-p (x) `(and (listp ,x) (eq (car ,x) '|.|)))
(defmacro cut-operator-p (x) `(and (listp ,x) (eq (car ,x) '|!|) (= (arity ,x) 0)))


(defun flatten-comma (tree)
  (cond ((and (consp tree) (eq '|,| (car tree)))
	 (append (flatten-comma (cadr tree)) (flatten-comma (caddr tree))))
	(t (list tree))))

(defun collect-vars (goal)
  (cond ((anonymousvar-p goal) nil)
	((variablep goal) (list goal))
	((cut-operator-p goal) (list '|!|))
	((termp goal) (mapcan #'collect-vars (cdr goal)))
	(t nil)))

;;In variable assignment, head term is treated as the part of first body term.
;;(VAR . (FIRST . LAST))
(defun make-varposition-table (goals)
  (let ((tbl nil) (cnt 0))
    (dolist (g goals)
      (dolist (v (collect-vars g))
	(if (assoc v tbl)
	    (rplacd (assoc v tbl) (cons (cadr (assoc v tbl)) cnt))
	    (if (eq v '|!|)
		(setq tbl (cons (cons v (cons 0 cnt)) tbl))
		(setq tbl (cons (cons v (cons cnt cnt)) tbl)))))
      (incf cnt))
    tbl))

(defun make-arity-list (head body)
  (let ((head-arity (arity head))
	(body1-arity (arity (car body))))
    (cons (max head-arity body1-arity)
	  (mapcar (lambda (term) (arity term)) (cdr body)))))		  

(defun head-firstbody-conj (head body)
  (cond ((null head) (car body))
	(t (append head (cdar body)))))

(defun assign-variables (head body)
  (let* ((Y-counter 0)
	 (arity-list (make-arity-list head body))
	 (A-counter (apply #'max arity-list))
	 (postbl
	  (remove-if
	   (lambda (vardata)
	     (and (eq (car vardata) '|!|) (= (cddr vardata) 0)))
	   (make-varposition-table (cons (head-firstbody-conj head body) (cdr body)))))
	 (sorted-tbl (sort postbl (lambda (x y) (> (cddr x) (cddr y)))))
	 (assigned-tbl
	  (mapcar
	   (lambda (v)
	     (let ((var (car v))
		   (first-appear (cadr v)) (last-appear (cddr v)))
	       (if (= first-appear last-appear)
		   (cons var (cons 'temporary (incf A-counter)))
		   (cons var (cons 'permanent (incf Y-counter)))))) sorted-tbl))
	 (remain-list (make-list (length body) :initial-element 0)))
    ;;make remain-list
    (dolist (v sorted-tbl)
      (loop for i from 0 to (1- (cddr v)) do
	   (incf (nth i remain-list))))
    (when (consp remain-list)
      (setf (car (last remain-list)) -1))
    (values assigned-tbl A-counter remain-list)))

(defun assign-variables-query (head body)
  (let* ((Y-counter 0)
	 (arity-list (make-arity-list head body))
	 (A-counter (apply #'max arity-list))
	 (postbl
	  (remove-if
	   (lambda (vardata)
	     (and (eq (car vardata) '|!|) (= (cddr vardata) 0)))
	   (make-varposition-table (cons (head-firstbody-conj head body) (cdr body)))))
	 (sorted-tbl (sort postbl (lambda (x y) (> (cddr x) (cddr y)))))
	 (assigned-tbl
	  (mapcar
	   (lambda (v)
	     (let ((var (car v)))
	       (cons var (cons 'permanent (incf Y-counter))))) sorted-tbl))
	 (remain-list (make-list (length body) :initial-element (length assigned-tbl))))
    (values assigned-tbl A-counter remain-list)))

(defun assign-test ()
  (let ((expr (parse *standard-input*)))
    (cond ((and (eq (car expr) '|:-|) (= (arity expr) 2))
	   (assign-variables (cadr expr) (flatten-comma (caddr expr))))
	  ((and (eq (car expr) '|:-|) (= (arity expr) 1))
	   (assign-variables nil (flatten-comma (cadr expr))))
	  (t
	   (assign-variables expr nil)))))

(defun compile-test (&optional (query-mode nil))
  (let ((expr (parse *standard-input*)))
    (print-wamcode (compile-clause expr query-mode))))

(defun print-hashtable (ht)
  (princ "{")
  (let ((counter 0) (len (hash-table-count ht)))
    (maphash (lambda (k v)
	       (incf counter)
	       (if (consp k)
		   (format t "~A/~A => ~A" (car k) (cdr k) v)
		   (format t "~A => ~A" k v))
	       (when (/= counter len)
		 (princ ", "))) ht)
    (princ "}")))

(defun print-wamcode (code)
  (dolist (inst code)
    (case (car inst)
      (put-variable-temporary
       (format t "~10Tput-variable X~A,A~A~%" (cadr inst) (caddr inst)))
      (put-variable-permanent
       (format t "~10Tput-variable Y~A,A~A~%" (cadr inst) (caddr inst)))
      (put-value-temporary
       (format t "~10Tput-value X~A,A~A~%" (cadr inst) (caddr inst)))
      (put-value-permanent
       (format t "~10Tput-value Y~A,A~A~%" (cadr inst) (caddr inst)))
      (put-unsafe-value
       (format t "~10Tput-unsafe-value Y~A,A~A~%" (cadr inst) (caddr inst)))
      (put-structure
       (format t "~10Tput-structure ~A/~A,A~A~%" (caadr inst) (cdadr inst) (caddr inst)))
      (put-list
       (format t "~10Tput-list A~A~%" (cadr inst)))
      (put-constant
       (format t "~10Tput-constant ~A,A~A~%" (cadr inst) (caddr inst)))
      (set-variable-temporary
       (format t "~10Tset-variable X~A~%" (cadr inst)))
      (set-variable-permanent
       (format t "~10Tset-variable Y~A~%" (cadr inst)))
      (set-value-temporary
       (format t "~10Tset-value X~A~%" (cadr inst)))
      (set-value-permanent
       (format t "~10Tset-value Y~A~%" (cadr inst)))
      (set-local-value-temporary
       (format t "~10Tset-local-value X~A~%" (cadr inst)))
      (set-local-value-permanent
       (format t "~10Tset-local-value Y~A~%" (cadr inst)))
      (set-constant
       (format t "~10Tset-constant ~A~%" (cadr inst)))
      (set-void
       (format t "~10Tset-void ~A~%" (cadr inst)))
      (get-variable-temporary
       (format t "~10Tget-variable X~A,A~A~%" (cadr inst) (caddr inst)))
      (get-variable-permanent
       (format t "~10Tget-variable Y~A,A~A~%" (cadr inst) (caddr inst)))
      (get-value-temporary
       (format t "~10Tget-value X~A,A~A~%" (cadr inst) (caddr inst)))
      (get-value-permanent
       (format t "~10Tget-value Y~A,A~A~%" (cadr inst) (caddr inst)))
      (get-structure
       (format t "~10Tget-structure ~A/~A,A~A~%" (caadr inst) (cdadr inst) (caddr inst)))
      (get-list
       (format t "~10Tget-list A~A~%" (cadr inst)))
      (get-constant
       (format t "~10Tget-constant ~A,A~A~%" (cadr inst) (caddr inst)))
      (unify-variable-temporary
       (format t "~10Tunify-variable X~A~%" (cadr inst)))
      (unify-variable-permanent
       (format t "~10Tunify-variable Y~A~%" (cadr inst)))
      (unify-value-temporary
       (format t "~10Tunify-value X~A~%" (cadr inst)))
      (unify-value-permanent
       (format t "~10Tunify-value Y~A~%" (cadr inst)))
      (unify-local-value-temporary
       (format t "~10Tunify-local-value X~A~%" (cadr inst)))
      (unify-local-value-permanent
       (format t "~10Tunify-local-value Y~A~%" (cadr inst)))
      (unify-constant
       (format t "~10Tunify-constant ~A~%" (cadr inst)))
      (unify-void
       (format t "~10Tunify-void ~A~%" (cadr inst)))
      (allocate
       (format t "~10Tallocate~%"))
      (deallocate
       (format t "~10Tdeallocate~%"))
      (allocate-for-query
       (format t "~10Tallocate-for-query ~A~%" (cadr inst)))
      (call
       (format t "~10Tcall ~A/~A,~A~%" (caadr inst) (cdadr inst) (caddr inst)))
      (execute
       (format t "~10Texecute ~A/~A~%" (caadr inst) (cdadr inst)))
      (proceed
       (format t "~10Tproceed~%"))
      (try-me-else
       (format t "~10Ttry-me-else ~A~%" (cadr inst)))
      (retry-me-else
       (format t "~10Tretry-me-else ~A~%" (cadr inst)))
      (trust-me
       (format t "~10Ttrust-me~%"))
      (try
       (format t "~10Ttry ~A~%" (cadr inst)))
      (retry
       (format t "~10Tretry ~A~%" (cadr inst)))
      (trust
       (format t "~10Ttrust ~A~%" (cadr inst)))
      (switch-on-term
       (format t "~10Tswitch-on-term ~A,~A,~A,~A~%"
	       (cadr inst) (caddr inst) (cadddr inst) (car (cddddr inst))))
      (switch-on-constant
       (format t "~10Tswitch-on-constant ") (print-hashtable (cadr inst)) (princ #\Newline))
      (switch-on-structure
       (format t "~10Tswitch-on-structure ") (print-hashtable (cadr inst)) (princ #\Newline))
      (neck-cut
       (format t "~10Tneck-cut~%"))
      (get-level
       (format t "~10Tget-level Y~A~%" (cadr inst)))
      (cut
       (format t "~10Tcut Y~A~%" (cadr inst)))
      (label
       (format t "~A:" (cadr inst)))
      (notify-solution
       (format t "~10Tnotify-solution~%"))
      (t (error (make-condition 'prolog-bad-instruction-error :inst (car inst)))))))


(defun have-key? (key ht)
  (multiple-value-bind (val exist) (gethash key ht)
    (declare (ignore val))
    exist))

(defmacro awhen (condition &body body)
  `(let ((it ,condition))
     (when it
       ,@body)))

(defmacro awhile (condition &body body)
  `(loop
      (let ((it ,condition))
	(if it
	    (progn ,@body)
	    (return)))))

(defmacro aif (condition true-part &optional false-part)
  `(let ((it ,condition))
     (if it ,true-part ,false-part)))


(defun sublis-temporary! (code replace-alist)
  (dolist (inst code)
    (case (car inst)
      ((put-variable-temporary
	put-value-temporary
	put-list
	set-variable-temporary
	set-value-temporary
	get-variable-temporary
	get-value-temporary
	get-list
	unify-variable-temporary
	unify-value-temporary)
       (awhen (assoc (cadr inst) replace-alist)
	 (setf (cadr inst) (cdr it))))
      ((put-structure
	put-constant
	get-structure
	get-constant)
       (awhen (assoc (caddr inst) replace-alist)
	 (setf (caddr inst) (cdr it))))))
  code)


(defun make-replace-alist (partial-code A-start)
  (let (modified-temporary-list modified-register-list replace-alist)
    (labels ((x? (n)
	       (>= n A-start))
	     (can-replace (tempvar register)
	       (unless (assoc tempvar replace-alist)
		 (push (cons tempvar register) replace-alist)))
	     (modify-register (r)
	       (mapc (lambda (p)
		       (when (eq (cdr p) r)
			 (pushnew (car p) modified-temporary-list))) replace-alist))
	     (modify-temporary (temp)
	       (awhen (assoc temp replace-alist)
		 (pushnew (cdr it) modified-register-list)))
	     (use-temporary (temp-var)
	       (when (member temp-var modified-temporary-list)
		 (setq replace-alist (remove-if (lambda (p)
						  (eq (car p) temp-var))
						replace-alist))))
	     (use-register (reg)
	       (when (member reg modified-register-list)
		 (setq replace-alist (remove-if (lambda (p)
						  (eq (cdr p) reg))
						replace-alist)))))
      (dolist (inst partial-code)
	(case (car inst)
	  (put-variable-temporary
	   (can-replace (second inst) (third inst)))
	  (put-value-temporary
	   (can-replace (second inst) (third inst)))
	  (get-variable-temporary
	   (can-replace (second inst) (third inst)))
	  (get-value-temporary
	   (can-replace (second inst) (third inst)))))
      (dolist (inst partial-code)
	(case (car inst)
	  (put-variable-temporary
	   (modify-temporary (second inst))
	   (modify-register (third inst)))
	  (put-variable-permanent
	   (modify-register (third inst)))
	  (put-value-temporary
	   (use-temporary (second inst))
	   (modify-register (third inst)))
	  (put-value-permanent
	   (modify-register (third inst)))
	  (put-structure
	   (if (x? (third inst))
	       (modify-temporary (third inst))
	       (modify-register (third inst))))
	  (put-list
	   (if (x? (second inst))
	       (modify-temporary (second inst))
	       (modify-register (second inst))))
	  (put-constant
	   (if (x? (third inst))
	       (modify-temporary (third inst))
	       (modify-register (third inst))))
	  (get-variable-temporary
	   (use-register (third inst))
	   (modify-temporary (second inst)))
	  (get-variable-permanent
	   (use-register (third inst)))
	  (get-value-temporary
	   (use-register (third inst))
	   (use-temporary (second inst)))
	  (get-value-permanent
	   (use-register (third inst)))
	  (get-structure
	   (if (x? (third inst))
	       (use-temporary (third inst))
	       (use-register (third inst))))
	  (get-list
	   (if (x? (second inst))
	       (use-temporary (second inst))
	       (use-register (second inst))))
	  (get-constant
	   (if (x? (third inst))
	       (use-temporary (third inst))
	       (use-register (third inst))))
	  (set-variable-temporary
	   (if (x? (second inst))
	       (modify-temporary (second inst))
	       (modify-register (second inst))))
	  (set-value-temporary
	   (if (x? (second inst))
	       (use-temporary (second inst))
	       (use-register (second inst))))
	  (unify-variable-temporary
	   (if (x? (second inst))
	       (modify-temporary (second inst))
	       (modify-register (second inst))))
	  (unify-value-temporary
	   (if (x? (second inst))
	       (use-temporary (second inst))
	       (use-register (second inst))))))
      replace-alist)))

(defun make-unnecessary-temporary-list (partial-code A-start)
  (let (temporary-occur-alist)
    (labels ((x? (n)
	       (>= n A-start))
	     (occur-temporary (temp-var)
	       (aif (assoc temp-var temporary-occur-alist)
		    (incf (cdr it))
		    (push (cons temp-var 1) temporary-occur-alist))))
      (dolist (inst partial-code)
	(case (car inst)
	  ((put-variable-temporary
	    put-value-temporary)
	   (occur-temporary (second inst)))
	  (put-structure
	   (when (x? (third inst))
	     (occur-temporary (third inst))))
	  (put-list
	   (when (x? (second inst))
	     (occur-temporary (second inst))))
	  (put-constant
	   (when (x? (third inst))
	     (occur-temporary (third inst))))
	  ((get-variable-temporary
	    get-value-temporary)
	   (occur-temporary (second inst)))
	  (get-structure
	   (when (x? (third inst))
	     (occur-temporary (third inst))))
	  (get-list
	   (when (x? (second inst))
	     (occur-temporary (second inst))))
	  (get-constant
	   (when (x? (third inst))
	     (occur-temporary (third inst))))
	  (set-variable-temporary
	   (when (x? (second inst))
	     (occur-temporary (second inst))))
	  (set-value-temporary
	   (when (x? (second inst))
	     (occur-temporary (second inst))))
	  (unify-variable-temporary
	   (when (x? (second inst))
	     (occur-temporary (second inst))))
	  (unify-value-temporary
	   (when (x? (second inst))
	     (occur-temporary (second inst))))))
      (mapcar #'car (remove-if (lambda (pair) (> (cdr pair) 1)) temporary-occur-alist)))))


(defun optimize-head (partial-code A-start)
  (sublis-temporary! partial-code (make-replace-alist partial-code A-start)))

(defun analyze-body (partial-code)
  (let ((replace-alist nil))
    (dolist (inst partial-code)
      (case (car inst)
	((put-variable-temporary
	  put-value-temporary)
	 (if (assoc (cadr inst) replace-alist)
	     (rplacd (assoc (cadr inst) replace-alist) (caddr inst))
	     (push (cons (cadr inst) (caddr inst)) replace-alist)))))
    replace-alist))

(defun optimize-body (partial-code A-start)
  (sublis-temporary! partial-code (make-replace-alist partial-code A-start)))


(defun remove-unnecessary-code (code)
  (remove nil
	  (maplist (lambda (current-code)
		     (let ((inst (car current-code)))
		       (case (car inst)
			 ((put-value-temporary
			   get-variable-temporary get-value-temporary)
			  (unless (= (cadr inst) (caddr inst))
			    inst))
			 (t inst))))
		   code)))


;;must be called before calling reallocate-registers!
(defun set-unsafe-and-local! (code)
  (let ((temporary-state-table (make-hash-table :test #'eq))
	(permanent-state-table (make-hash-table :test #'eq))
	(permanent-lastgoal-table (make-hash-table :test #'eq)))
    (let ((body-num 0))
      (dolist (inst code)
	(case (car inst)
	  ((put-variable-permanent
	    put-value-permanent
	    set-variable-permanent
	    set-value-permanent
	    get-variable-permanent
	    get-value-permanent
	    unify-variable-permanent
	    unify-value-permanent)
	   (setf (gethash (cadr inst) permanent-lastgoal-table) body-num))
	  ((call execute proceed)
	   (incf body-num)))))
    (let ((body-num 0))
      (dolist (inst code)
	(case (car inst)
	  (put-variable-temporary
	   (unless (have-key? (cadr inst) temporary-state-table)
	     (setf (gethash (cadr inst) temporary-state-table) 'on-heap)))
	  (put-variable-permanent
	   (unless (have-key? (cadr inst) permanent-state-table)
	     (setf (gethash (cadr inst) permanent-state-table) 'initialized)))
	  (put-value-temporary
	   (unless (have-key? (cadr inst) temporary-state-table)
	     (setf (gethash (cadr inst) temporary-state-table) 'initialized)))
	  (put-value-permanent
	   (when (and
		  (have-key? (cadr inst) permanent-state-table)
		  (eq 'initialized (gethash (cadr inst) permanent-state-table))
		  (eq body-num (gethash (cadr inst) permanent-lastgoal-table)))
	     (setf (car inst) 'put-unsafe-value)
	     (setf (gethash (cadr inst) permanent-state-table) 'on-heap))    
	   (unless (have-key? (cadr inst) permanent-state-table)
	     (setf (gethash (cadr inst) permanent-state-table) 'initialized)))
	  (set-variable-temporary
	   (unless (have-key? (cadr inst) temporary-state-table)
	     (setf (gethash (cadr inst) temporary-state-table) 'on-heap)))
	  (set-variable-permanent
	   (unless (have-key? (cadr inst) permanent-state-table)
	     (setf (gethash (cadr inst) permanent-state-table) 'on-heap)))
	  (set-value-temporary
	   (when (and
		  (have-key? (cadr inst) temporary-state-table)
		  (eq 'initialized (gethash (cadr inst) temporary-state-table)))
	     (setf (car inst) 'set-local-value-temporary)
	     (setf (gethash (cadr inst) temporary-state-table) 'on-heap)))
	  (set-value-permanent
	   (when (and
		  (have-key? (cadr inst) permanent-state-table)
		  (eq 'initialized (gethash (cadr inst) permanent-state-table)))
	     (setf (car inst) 'set-local-value-permanent)
	     (setf (gethash (cadr inst) permanent-state-table) 'on-heap)))
	  (get-variable-temporary
	   (unless (have-key? (cadr inst) temporary-state-table)
	     (setf (gethash (cadr inst) temporary-state-table) 'initialized)))
	  (get-variable-permanent
	   (unless (have-key? (cadr inst) permanent-state-table)
	     (setf (gethash (cadr inst) permanent-state-table) 'initialized)))
	  (get-value-temporary
	   (unless (have-key? (cadr inst) temporary-state-table)
	     (setf (gethash (cadr inst) temporary-state-table) 'initialized)))
	  (get-value-permanent
	   (unless (have-key? (cadr inst) permanent-state-table)
	     (setf (gethash (cadr inst) permanent-state-table) 'initialized)))
	  (unify-variable-temporary
	   (unless (have-key? (cadr inst) temporary-state-table)
	     (setf (gethash (cadr inst) temporary-state-table) 'on-heap)))
	  (unify-variable-permanent
	   (unless (have-key? (cadr inst) permanent-state-table)
	     (setf (gethash (cadr inst) permanent-state-table) 'on-heap)))
	  (unify-value-temporary
	   (when (and
		  (have-key? (cadr inst) temporary-state-table)
		  (eq 'initialized (gethash (cadr inst) temporary-state-table)))
	     (setf (car inst) 'unify-local-value-temporary)
	     (setf (gethash (cadr inst) temporary-state-table) 'on-heap))
	   (unless (have-key? (cadr inst) temporary-state-table)
	     (setf (gethash (cadr inst) temporary-state-table) 'on-heap)))
	  (unify-value-permanent
	   (when (and
		  (have-key? (cadr inst) permanent-state-table)
		  (eq 'initialized (gethash (cadr inst) permanent-state-table)))
	     (setf (car inst) 'unify-local-value-permanent)
	     (setf (gethash (cadr inst) permanent-state-table) 'on-heap))
	   (unless (have-key? (cadr inst) permanent-state-table)
	     (setf (gethash (cadr inst) permanent-state-table) 'on-heap)))
	  ((call execute proceed)
	   (incf body-num)))))
    code))



(defun reallocate-registers! (code A-start arity-list)
  (let ((using-register-list (list nil)))
    ;;collect temporary variables...
    (dolist (inst code)
      (case (car inst)
	((put-variable-temporary
	  put-value-temporary
	  put-list
	  get-variable-temporary
	  get-value-temporary
	  get-list
	  set-variable-temporary
	  set-value-temporary
	  set-local-value-temporary
	  unify-variable-temporary
	  unify-value-temporary
	  unify-local-value-temporary)
	 (when (>= (cadr inst) A-start)
	   (pushnew (cadr inst) (car using-register-list))))
	((put-structure
	  put-constant
	  get-structure
	  get-constant)
	 (when (>= (caddr inst) A-start)
	   (pushnew (caddr inst) (car using-register-list))))
	((call execute proceed)
	 (push nil using-register-list))))
    (let ((reallocate-list
	   (mapcar (lambda (clause-regs clause-arity)
		     (mapcar (lambda (r)
			       (cons r (incf clause-arity)))
			     clause-regs))
		   (reverse using-register-list) arity-list)))
      (dolist (inst code)
	(case (car inst)
	  ((put-variable-temporary
	    put-value-temporary
	    put-list
	    get-variable-temporary
	    get-value-temporary
	    get-list
	    set-variable-temporary
	    set-value-temporary
	    set-local-value-temporary
	    unify-variable-temporary
	    unify-value-temporary
	    unify-local-value-temporary)
	   (awhen (assoc (cadr inst) (car reallocate-list))
	     (setf (cadr inst) (cdr it))))
	  ((put-structure
	    put-constant
	    get-structure
	    get-constant)
	   (awhen (assoc (caddr inst) (car reallocate-list))
	     (setf (caddr inst) (cdr it))))
	  ((call execute proceed)
	   (setq reallocate-list (cdr reallocate-list)))))
      code)))


(defun remove-unnecessary-pair (code)
  (let (perm-pair-list)
    (remove nil
	    (mapcar (lambda (inst)
		      (case (car inst)
			((get-variable-permanent
			  get-value-permanent)
			 (pushnew (cons (second inst) (third inst)) perm-pair-list)
			 inst)
			((put-variable-permanent
			  put-value-permanent)
			 (if (assoc (second inst) perm-pair-list)
			     (if (eq
				  (cdr (assoc (second inst) perm-pair-list))
				  (third inst))
				 nil
				 inst)
			     inst))
			((call execute proceed)
			 (setq perm-pair-list nil)
			 inst)
			(t inst)))
		    code))))


(defun optimize-test (&optional (query-mode nil))
  (let ((clause (parse *standard-input*)))
    (multiple-value-bind (head body) (devide-head-body clause)
      (let* ((arity-list (make-arity-list head body))
	     (A-start (1+ (apply #'max arity-list)))
	     (compiled (compile-clause clause query-mode)))
	(print-wamcode compiled)
	(format t "<--optimize-->~%")
	(let ((newcode (optimize-wamcode compiled A-start)))
	  (print-wamcode newcode)
	  (format t "<--remove-unnecessary-code-->~%")
	  (let ((newcode (remove-unnecessary-code newcode)))
	    (print-wamcode newcode)
	    (format t "<--set-unsafe-and-local-->~%")
	    (let ((newcode (set-unsafe-and-local! newcode)))
	      (print-wamcode newcode)
	      (format t "<--reallocate-registers-->~%")
	      (let ((newcode (reallocate-registers! newcode A-start arity-list)))
		(print-wamcode newcode)
		(format t "<--remove-unnecessary-pair-->~%")
		(let ((newcode (remove-unnecessary-pair newcode)))
		  (print-wamcode newcode)
		  newcode)))))))))


(defmacro cons-when (condition element list)
  `(if ,condition
       (cons ,element ,list)
       ,list))

(defun devide-head-body (clause)
  (destructuring-bind
	(head body clause-type) (cond
				  ((and (eq (car clause) '|:-|) (= (arity clause) 2))
				   (list (cadr clause) (flatten-comma (caddr clause)) 'rule))
				  ((and (eq (car clause) '|:-|) (= (arity clause) 1))
				   (list nil (flatten-comma (cadr clause)) 'call))
				  ((and (eq (car clause) '|?-|) (= (arity clause) 1))
				   (list nil (flatten-comma (cadr clause)) 'query))
				  (t
				   (list clause nil 'fact)))
    (values head body clause-type)))


(defun optimize-wamcode (code A-start)
  (let (optimized-code (first-block? t) current-block
		       void-type (void-counter 0)
		       (unnecessary-temp-list (make-unnecessary-temporary-list code A-start)))
    (labels ((push-void-if-possible ()
	       (unless (zerop void-counter)
		 (push (list void-type void-counter) current-block)
		 (setq void-counter 0))))
      (dolist (inst code)
	(case (car inst)
	  (set-variable-temporary
	   (if (member (second inst) unnecessary-temp-list)
	       (progn
		 (setq void-type 'set-void)
		 (incf void-counter))
	       (progn
		 (push-void-if-possible)
		 (push inst current-block))))
	  (unify-variable-temporary
	   (if (member (second inst) unnecessary-temp-list)
	       (progn
		 (setq void-type 'unify-void)
		 (incf void-counter))
	       (progn
		 (push-void-if-possible)
		 (push inst current-block))))
	  ((set-void unify-void)
	   (setq void-type (car inst))
	   (incf void-counter))
	  ((call execute proceed)
	   (push-void-if-possible)
	   (setq optimized-code
		 (append optimized-code
			 (if first-block?
			     (progn
			       (setq first-block? nil)
			       (optimize-head (reverse current-block) A-start))
			     (optimize-body (reverse current-block) A-start))
			 (list inst)))
	   (setq current-block nil))
	  (notify-solution
	   (setq optimized-code (append optimized-code (list inst))))
	  (t
	   (push-void-if-possible)
	   (push inst current-block))))
      optimized-code)))


(defun compile-clause (clause &optional (query-mode nil))
  (multiple-value-bind (head body) (devide-head-body clause)
    (multiple-value-bind
	  (assign-table register-next remain-list) (if query-mode
						       (assign-variables-query head body)
						       (assign-variables head body))
      (let ((initialized-vars nil))
	(labels
	    ((initialized-p (var)
	       (member var initialized-vars))
	     (compile-head-term (term)
	       (let* ((A 0) (remaining-code nil)
		      (main-code
		       (mapcan
			(lambda (arg)
			  (incf A)
			  (cond ((anonymousvar-p arg) nil)
				((variablep arg)
				 (let ((vardata (cdr (assoc arg assign-table))))
				   (list
				    (case (car vardata)
				      (temporary
				       (if (initialized-p arg)
					   `(get-value-temporary ,(cdr vardata) ,A)
					   (prog1 `(get-variable-temporary ,(cdr vardata) ,A)
					     (pushnew arg initialized-vars))))
				      (permanent
				       (if (initialized-p arg)
					   `(get-value-permanent ,(cdr vardata) ,A)
					   (prog1 `(get-variable-permanent ,(cdr vardata) ,A)
					     (pushnew arg initialized-vars))))))))
				((listterm-p arg)
				 `((get-list ,A)
				   ,@(multiple-value-bind
				      (m r) (compile-head-struct-args (cdr arg))
				      (setq remaining-code (append remaining-code r))
				      m)))
				((termp arg)
				 (if (= (arity arg) 0)
				     `((get-constant ,(car arg) ,A))
				     `((get-structure ,(cons (car arg) (arity arg)) ,A)
				       ,@(multiple-value-bind
					  (m r) (compile-head-struct-args (cdr arg))
					  (setq remaining-code (append remaining-code r))
					  m))))))
			(cdr term))))
		 (nconc main-code remaining-code)))
	     (compile-head-struct-args (struct-args)
	       (let* ((remaining-code nil)
		      ;;code-list for nested structure
		      ;; which must be put after unification of parent structure
		      (main-code
		       (mapcan
			(lambda (arg)
			  (cond
			    ((anonymousvar-p arg) (list (list 'unify-void 1)))
			    ((variablep arg)
			     (let ((vardata (cdr (assoc arg assign-table))))
			       (list
				(case (car vardata)
				  (temporary
				   (if (initialized-p arg)
				       `(unify-value-temporary ,(cdr vardata))
				       (prog1 `(unify-variable-temporary ,(cdr vardata))
					 (pushnew arg initialized-vars))))
				  (permanent
				   (if (initialized-p arg)
				       `(unify-value-permanent ,(cdr vardata))
				       (prog1 `(unify-variable-permanent ,(cdr vardata))
					 (pushnew arg initialized-vars))))))))
			    ((listterm-p arg)
			     (let ((tempvar (incf register-next)))
			       (prog1 `((unify-variable-temporary ,tempvar))
				 (setq remaining-code
				       (append remaining-code
					       `((get-list ,tempvar)
						 ,@(multiple-value-bind
						    (m r) (compile-head-struct-args (cdr arg))
						    (append m r))))))))
			    ((termp arg)
			     (let ((tempvar (incf register-next)))
			       (if (= (arity arg) 0)
				   `((unify-constant ,(car arg)))
				   (prog1 `((unify-variable-temporary ,tempvar))
				     (setq
				      remaining-code
				      (append
				       remaining-code
				       `((get-structure
					  ,(cons (car arg) (arity arg)) ,tempvar)
					 ,@(multiple-value-bind
					    (m r) (compile-head-struct-args (cdr arg))
					    (append m r)))))))))))
			struct-args)))
		 (values main-code remaining-code)))
	     (compile-body-term (term)
	       (let* ((A 0) (prep-code nil)
		      (main-code
		       (mapcan
			(lambda (arg)
			  (incf A)
			  (cond ((anonymousvar-p arg)
				 (let ((tempvar (incf register-next)))
				   `((put-variable-temporary ,tempvar ,A))))
				((variablep arg)
				 (let ((vardata (cdr (assoc arg assign-table))))
				   (list
				    (case (car vardata)
				      (temporary
				       (if (initialized-p arg)
					   `(put-value-temporary ,(cdr vardata) ,A)
					   (prog1 `(put-variable-temporary ,(cdr vardata) ,A)
					     (pushnew arg initialized-vars))))
				      (permanent
				       (if (initialized-p arg)
					   `(put-value-permanent ,(cdr vardata) ,A)
					   (prog1 `(put-variable-permanent ,(cdr vardata) ,A)
					     (pushnew arg initialized-vars))))))))
				((listterm-p arg)
				 `((put-list ,A)
				   ,@(multiple-value-bind
				      (m p) (compile-body-struct-args (cdr arg))
				      (setq prep-code (append prep-code p))
				      m)))
				((termp arg)
				 (if (= (arity arg) 0)
				     `((put-constant ,(car arg) ,A))
				     `((put-structure ,(cons (car arg) (arity arg)) ,A)
				       ,@(multiple-value-bind
					  (m p) (compile-body-struct-args (cdr arg))
					  (setq prep-code (append prep-code p))
					  m))))))
			(cdr term))))
		 (nconc prep-code main-code)))
	     (compile-body-struct-args (struct-args)
	       (let* ((prep-code nil)
		      (main-code
		       (mapcan
			(lambda (arg)
			  (cond ((anonymousvar-p arg) (list (list 'set-void 1)))
				((variablep arg)
				 (let ((vardata (cdr (assoc arg assign-table))))
				   (list
				    (case (car vardata)
				      (temporary
				       (if (initialized-p arg)
					   `(set-value-temporary ,(cdr vardata))
					   (prog1 `(set-variable-temporary ,(cdr vardata))
					     (pushnew arg initialized-vars))))
				      (permanent
				       (if (initialized-p arg)
					   `(set-value-permanent ,(cdr vardata))
					   (prog1 `(set-variable-permanent ,(cdr vardata))
					     (pushnew arg initialized-vars))))))))
				((listterm-p arg)
				 (let ((tempvar (incf register-next)))
				   (prog1
				       `((set-value-temporary ,tempvar))
				     (setq prep-code
					   (append
					    prep-code
					    (multiple-value-bind
						  (m p) (compile-body-struct-args (cdr arg))
					      `(,@p
						(put-list ,tempvar)
						,@m)))))))
				((termp arg)
				 (let ((tempvar (incf register-next)))
				   (if (= (arity arg) 0)
				       `((set-constant ,(car arg)))
				       (prog1 `((set-value-temporary ,tempvar))
					 (setq
					  prep-code
					  (append
					   prep-code
					   (multiple-value-bind
						 (m p) (compile-body-struct-args (cdr arg))
					     `(,@p
					       (put-structure
						,(cons (car arg) (arity arg)) ,tempvar)
					       ,@m))))))))))
			struct-args)))
		 (values main-code prep-code))))
	  (let ((have-permanent (some #'plusp remain-list))
		(have-deep-cut (assoc '|!| assign-table))
		(body-num -1)
		(deallocate-emitted nil))
	    (nconc
	     (cons-when
	      (and (not query-mode) have-permanent)
	      (list 'allocate)
	      (cons-when
	       (and query-mode (> (length assign-table) 0))
	       (list 'allocate-for-query (length assign-table))
	       (cons-when
		have-deep-cut
		(list 'get-level (cddr (assoc '|!| assign-table)))
		(append
		 (compile-head-term head)
		 (aif (mapcan
		       (lambda (b remain)
			 (incf body-num)
			 (append
			  (compile-body-term b)
			  (let ((deallocate-part
				 (when (and (not query-mode) have-permanent
					    (>= 0 remain) (not deallocate-emitted))
				   (setf deallocate-emitted t)
				   (list (list 'deallocate)))))
			    (cond
			      ((cut-operator-p b)
			       (append
				(if (= body-num 0)
				    (list (list 'neck-cut))
				    `((cut ,(cddr (assoc '|!| assign-table)))))
				deallocate-part))
			      ((= -1 remain)
			       (append deallocate-part
				       `((execute ,(cons (car b) (arity b))))))
			      (t
			       (append deallocate-part
				       `((call ,(cons (car b) (arity b)) ,remain))))))))
		       body remain-list)
		      it (list (list 'proceed)))))))
	     (if query-mode `((notify-solution ,assign-table)) nil))))))))

(defmacro append-hash (key val ht)
  `(if (have-key? ,key ,ht)
       (setf (gethash ,key ,ht) (append (gethash ,key ,ht) (list ,val)))
       (setf (gethash ,key ,ht) (list ,val))))

(defun mapcanhash (f ht)
  (let (acc)
    (with-hash-table-iterator (iter ht)
      (loop
	 (multiple-value-bind (has-next? k v) (iter)
	   (if has-next?
	       (setf acc (nconc acc (funcall f k v)))
	       (return)))))
    acc))


(defun compile-switch-constant (const-table)
  (let* ((switch-hashtable (make-hash-table))
	 (dispatch-code
	  (mapcanhash (lambda (k v)
			(if (= (length v) 1)
			    (prog1 nil (setf (gethash k switch-hashtable) (car v)))
			    (let ((candidate-count (length v))
				  (counter 0)
				  (new-label (gensym)))
			      (setf (gethash k switch-hashtable) new-label)
			      (cons
			       `(label ,new-label)
			       (mapcar
				(lambda (lbl)
				  (incf counter)
				  (cond ((= counter 1)
					 `(try ,lbl))
					((= counter candidate-count)
					 `(trust ,lbl))
					(t
					 `(retry ,lbl)))) v))))) const-table)))
    (values dispatch-code switch-hashtable)))

(defun compile-switch-struct (struct-table)
  (let* ((switch-hashtable (make-hash-table :test #'equal))
	 (dispatch-code
	  (mapcanhash (lambda (k v)
			(if (= (length v) 1)
			    (prog1 nil (setf (gethash k switch-hashtable) (car v)))
			    (let ((candidate-count (length v))
				  (counter 0)
				  (new-label (gensym)))
			      (setf (gethash k switch-hashtable) new-label)
			      (cons
			       `(label ,new-label)
			       (mapcar
				(lambda (lbl)
				  (incf counter)
				  (cond ((= counter 1)
					 `(try ,lbl))
					((= counter candidate-count)
					 `(trust ,lbl))
					(t
					 `(retry ,lbl)))) v))))) struct-table)))
    (values dispatch-code switch-hashtable)))

(defun compile-switch-listterm (list-list)
  (let ((candidate-count (length list-list))
	(counter 0)
	(new-label (gensym)))
    (if (= 1 (length list-list))
	(values nil (car list-list))
	(values (cons `(label ,new-label)
		      (mapcar
		       (lambda (lbl)
			 (incf counter)
			 (cond ((= counter 1)
				`(try ,lbl))
			       ((= counter candidate-count)
				`(trust ,lbl))
			       (t
				`(retry ,lbl)))) list-list)) new-label))))		      

(defun compile-indexing-code (entrance const-table list-list struct-table)
  (let* ((const-label (if (= 0 (hash-table-count const-table))
			  'fail
			  (gensym)))
	 (list-label (if (= 0 (length list-list))
			 'fail
			 (gensym)))
	 (struct-label (if (= 0 (hash-table-count struct-table))
			   'fail
			   (gensym)))
	 (const-block (when (not (eq 'fail const-label))
			(multiple-value-bind (dc ht) (compile-switch-constant const-table)
			  (cons `(label ,const-label)
				(cons `(switch-on-constant ,ht) dc)))))
	 (list-block (when (not (eq 'fail list-label))
		       (multiple-value-bind (dc lbl) (compile-switch-listterm list-list)
			 (setq list-label lbl)
			 dc)))
	 (struct-block (when (not (eq 'fail struct-label))
			 (multiple-value-bind (dc ht) (compile-switch-struct struct-table)
			   (cons `(label ,struct-label)
				 (cons `(switch-on-structure ,ht) dc))))))
    (nconc `((switch-on-term ,entrance ,const-label ,list-label ,struct-label))
	   const-block list-block struct-block)))

(defun compile-subsequence (subseq)
  (if (= (length subseq) 1)
      (copy-tree (cdar subseq))
      (let* ((clause-count (length subseq)) (counter 0)
	     (entrance (gensym)) (next-label entrance)
	     (constant-label-table (make-hash-table))
	     (struct-label-table (make-hash-table :test #'equal))
	     (list-label-list nil)
	     (main-code
	      (mapcan
	       (lambda (s)
		 (incf counter)
		 (cons-when next-label
			    `(label ,next-label)
			    (cons
			     (cond ((= counter 1)
				    (setq next-label (gensym)) `(try-me-else ,next-label))
				   ((= counter clause-count)
				    (list 'trust-me))
				   (t
				    (setq next-label (gensym)) `(retry-me-else ,next-label)))
			     (cons
			      (let ((arg-1 (car s)) (new-label (gensym)))
				(cond
				  ((listterm-p arg-1)
				   ;;list
				   (setq list-label-list (append list-label-list (list new-label))))
				  ((and (termp arg-1) (= (arity arg-1) 0))
				   ;;constant
				   (append-hash (car arg-1) new-label constant-label-table))
				  (t
				   ;;structure
				   (append-hash
				    (cons (car arg-1) (arity arg-1))
				    new-label struct-label-table)))
				`(label ,new-label))	    
			      (copy-tree (cdr s)))))) subseq))
	     (indexing-code
	      (compile-indexing-code
	       entrance constant-label-table list-label-list struct-label-table)))
	(nconc indexing-code main-code))))

(defun set-label-pointer! (code)
  (let ((label-ptr-table (make-hash-table :test #'eq)))
    (mapl (lambda (part)
	    (when (eq (caar part) 'label)
	      (setf (gethash (cadar  part) label-ptr-table) (cdr part)))) code)
    (mapc (lambda (inst)
	    (case (car inst)
	      ((try-me-else
		retry-me-else
		try
		retry
		trust)
	       (nconc inst (list (gethash (second inst) label-ptr-table))))
	      (switch-on-term
	       (if (eq 'fail (second inst))
		   (nconc inst (list 'fail))
		   (nconc inst (list (gethash (second inst) label-ptr-table))))
	       (if (eq 'fail (third inst))
		   (nconc inst (list 'fail))
		   (nconc inst (list (gethash (third inst) label-ptr-table))))
	       (if (eq 'fail (fourth inst))
		   (nconc inst (list 'fail))
		   (nconc inst (list (gethash (fourth inst) label-ptr-table))))
	       (if (eq 'fail (fifth inst))
		   (nconc inst (list 'fail))
		   (nconc inst (list (gethash (fifth inst) label-ptr-table)))))
	      (switch-on-constant
	       (let ((new-table (make-hash-table :test #'eq)))
		 (maphash
		  (lambda (k v)
		    (setf (gethash k new-table) (gethash v label-ptr-table))) (second inst))
		 (nconc inst (list new-table)))) 
	      (switch-on-structure
	       (let ((new-table (make-hash-table :test #'equal)))
		 (maphash
		  (lambda (k v)
		    (setf (gethash k new-table) (gethash v label-ptr-table))) (second inst))
		 (nconc inst (list new-table)))))) code)))

(defun call-N-copy-to-next-instraction (code)
  (mapl (lambda (part)
	  (when (eq (caar part) 'call)
	    (nconc (second part) (list (third (car part)))))) code))


(defun compile-dispatching-code (key)
  (call-N-copy-to-next-instraction
   (set-label-pointer!
    (let ((subsequence-list nil)
	  (current-subsequence nil))
      (dolist (pair (gethash key *clause-code-table*))
	(if (or (variablep (car pair)) (null (car pair)))
	    (progn
	      (when current-subsequence
		(push (reverse current-subsequence) subsequence-list)
		(setq current-subsequence nil))
	      (push (list pair) subsequence-list))
	    (push pair current-subsequence)))
      (when current-subsequence
	(push (reverse current-subsequence) subsequence-list))
      (setq subsequence-list (reverse subsequence-list))
      (if (= (length subsequence-list) 1)
	  (compile-subsequence (car subsequence-list))
	  (let ((subseq-count (length subsequence-list))
		(next-label nil) (counter 0))
	    (mapcan (lambda (s)
		      (incf counter)
		      (cons-when next-label
				 `(label ,next-label)
				 (cons
				  (cond ((= counter 1)
					 (setq next-label (gensym)) `(try-me-else ,next-label))
					((= counter subseq-count)
					 (list 'trust-me))
					(t
					 (setq next-label (gensym)) `(retry-me-else ,next-label)))
				  (compile-subsequence s)))) subsequence-list)))))))

(defun compile-and-optimize (clause head body &optional (query-mode nil))
  (let* ((arity-list (make-arity-list head body))
	 (A-start (1+ (apply #'max arity-list)))
	 (newcode (compile-clause clause query-mode)))
    (let ((newcode (optimize-wamcode newcode A-start)))
      (let ((newcode (remove-unnecessary-code newcode)))
	(let ((newcode (set-unsafe-and-local! newcode)))
	  (let ((newcode (reallocate-registers! newcode A-start arity-list)))
	    (let ((newcode (remove-unnecessary-pair newcode)))
	      newcode)))))))

(defun next-solution-prompt ()
  (loop
     (write-char #\? *query-io*)
     (clear-input *query-io*)
     (case (read-char *query-io*)
       ((#\n #\;)
	(clear-input *query-io*)
	(return 'no))
       ((#\y #\Newline)
	(clear-input *query-io*)
	(return 'yes))
       (#\a
	(clear-input *query-io*)
	(return 'all)))
     (clear-input *query-io*)))

(defun join (element list)
  (let (acc)
    (mapl (lambda (a)
	    (push (car a) acc)
	    (unless (null (cdr a))
	      (push element acc))) list)
    (reverse acc)))

(defun concat-string-list (list)
  (with-output-to-string (s)
    (mapc (lambda (e) (princ e s)) list)))

(defun prolog-list->string (list)
  (labels ((flatten (rest acc)
	     (if (and (consp rest) (eq (car rest) '|.|))
		 (flatten (third rest) (cons (second rest) acc))
		 (values (reverse acc) rest))))
    (multiple-value-bind (butlast last) (flatten list nil)
      (format nil "[~A]"
	      (concat-string-list
	       (append (join "," (mapcar #'prolog-expr->string butlast))
		       (if (not (eq last '[])) (list "|" (prolog-expr->string last)))))))))

(defun prolog-expr->string (expr)
  (cond ((eq expr *unbound-variable*)
	 (format nil "<unbound>"))
	((atom expr)
	 (format nil "~A" expr))
	((eq (car expr) '|.|)
	 (prolog-list->string expr))
	(t
	 (format nil "~A(~A)" (car expr)
		 (concat-string-list
		  (join "," (mapcar #'prolog-expr->string (cdr expr)))))))) 

(defun show-solution (vars)
  (dolist (v vars)
    (unless (eq (car v) '|!|)
      (format *query-io* "~A = ~A~%" (car v) (prolog-expr->string (cdr v))))))

(defun update-dispatching-code (key)
  (setf (gethash key *dispatching-code-table*) (compile-dispatching-code key)))

(defun show-wamcode (functor-str arity)
  (print-wamcode (gethash (cons (intern functor-str) arity) *dispatching-code-table*)))

(defun repl ()
  (loop
     (block eval-once
       (write-string  "> " *query-io*)
       (let ((find-all-solution nil))
	 (handler-bind
	     ((prolog-query-failed (lambda (c)
				     (declare (ignore c))
				     (format *query-io* "no.~%")
				     (return-from eval-once)))
	      (prolog-found-solution (lambda (c)
				       (show-solution (vars c))
				       (if (or (null (vars c)) (not (can-backtrack? c)))
					   (progn (format *query-io* "~%yes.~%")
						  (return-from eval-once))
					   (if find-all-solution
					       (progn (format *query-io* "~%")
						      (invoke-restart 'next-solution))
					       (case (next-solution-prompt)
						 (no (invoke-restart 'next-solution))
						 (all (setq find-all-solution t)
						      (invoke-restart 'next-solution))
						 (yes (format *query-io* "~%yes.~%")
						      (return-from eval-once)))))))
	      (prolog-syntax-error (lambda (c)
				     (princ c *query-io*) (format *query-io* "~%")
				     (clear-input *standard-input*) (return-from eval-once))))
	   (let ((clause (parse *standard-input*)))
	     (multiple-value-bind (head body clause-type) (devide-head-body clause)
	       (case clause-type
		 ((fact rule)
		  (let ((wamcode (compile-and-optimize clause head body))) 
		    (let ((key (cons (car head) (arity head))))
		      (progn (if (have-key? key *clause-code-table*)
				 (rplacd (last (gethash key *clause-code-table*))
					 (list (cons (cadr head) wamcode)))
				 (setf (gethash key *clause-code-table*)
				       (list (cons (cadr head) wamcode))))
			     (update-dispatching-code key)))))
		 (query
		  (let ((wamcode (call-n-copy-to-next-instraction
				  (compile-and-optimize clause head body t))))
		    (send-query wamcode)))
		 (call
		  (let ((wamcode (call-n-copy-to-next-instraction
				  (compile-and-optimize clause head body)))) 
		    (send-query wamcode)))))))))))


#|
---address representation---
register: 1,2,3,...
stack: -1,-3,-5,...
heap: -2,-4,-6,...
|#

(defvar *register-area* (make-array 10 :adjustable t))
(defvar *heap-area* (make-array 30 :adjustable t))
(defvar *stack-area* (make-array 50 :adjustable t))
(defvar *trail-area* (make-array 20 :adjustable t))
(defvar *P*)
(defvar *CP*)
(defvar *S*)
(defvar *HB*)
(defvar *H*)
(defvar *B0*)
(defvar *B*)
(defvar *E*)
(defvar *TR*)
(defvar *fail*)
(defvar *num-of-args* nil)
(defvar *mode*)


(defconstant bottom-of-stack -3) ;;-1はBの初期値としてつかう（初期状態でB<Eにしたいため）
(defconstant bottom-of-heap -2)
(defconstant initial-value-of-B -1)

(deftype stack-address ()
  '(and (satisfies minusp) (satisfies oddp)))

(deftype heap-address ()
  '(and (satisfies minusp) (satisfies evenp)))

(deftype register-number ()
  '(and (integer 1 *)))

;;第一引数はアドレス(stack or heap)
;;残りの引数は加算する定数
(defun addr+ (target &rest nums)
  (+ target (* -2 (apply #'+ nums))))


(defun addr< (addr1 addr2)
  (cond
    ((and (typep addr1 'stack-address) (typep addr2 'heap-address)) nil)
    ((and (typep addr1 'heap-address) (typep addr2 'stack-address)) t)
    (t (> addr1 addr2))))

(defun store (addr)
  (typecase addr
    (register-number (register addr))
    (stack-address (stack addr))
    (heap-address (heap addr))))

(defun (setf store) (new-val addr)
  (typecase addr
    (register-number (setf (register addr) new-val))
    (stack-address (setf (stack addr) new-val))
    (heap-address (setf (heap addr) new-val))))

(defun heap (addr)
  (aref *heap-area* (1- (- (/ addr 2)))))

(defun (setf heap) (new-val addr)
  (let ((real-addr (1- (- (/ addr 2)))))
    ;(format t "heap ~A =  ~A~%" addr new-val)
    (unless (array-in-bounds-p *heap-area* real-addr)
      (adjust-array *heap-area* (* 2 real-addr)))
    (setf (aref *heap-area* real-addr) new-val)))

(defun stack (addr)
  (aref *stack-area* (1- (- (floor addr 2)))))

(defun (setf stack) (new-val addr)
  (let ((real-addr (1- (- (floor addr 2)))))
    ;(format t "stack ~A = ~A~%" addr new-val)
    (unless (array-in-bounds-p *stack-area* real-addr)
      (adjust-array *stack-area* (* 2 real-addr)))
    (setf (aref *stack-area* real-addr) new-val)))

(defun register (num)
  (aref *register-area* num))	

(defun (setf register) (new-val num)
  ;(format t "register ~A = ~A~%" num new-val)
  (unless (array-in-bounds-p *register-area* num)
    (adjust-array *register-area* (* 2 num)))
  (setf (aref *register-area* num) new-val))

(defun trail (addr)
  (aref *trail-area* addr))

(defun (setf trail) (new-val addr)
  (unless (array-in-bounds-p *trail-area* addr)
    (adjust-array *trail-area* (* 2 addr)))
  (setf (aref *trail-area* addr) new-val))

(defun stackvar (y)
  (stack (addr+ *E* y 1)))	

(defun (setf stackvar) (new-val y)
  (setf (stack (addr+ *E* y 1)) new-val))

(defun print-memory ()
  (loop for i from 0 to (1- (length *heap-area*)) do
       (if (eq 0  (heap (* -2 (1+ i)))) (return))
       (format t "H ~A : ~A~%" (* -2 (1+ i)) (heap (* -2 (1+ i)))))
  (format t "--------------~%")
  (loop for i from 0 to (1- (length *stack-area*)) do
       (if (and (> i 5) (eq 0  (stack (1+ (* -2 (1+ i)))))) (return))
       (format t "S ~A : ~A~%" (1+ (* -2 (1+ i))) (stack (1+ (* -2 (1+ i))))))
  (format t "--------------~%")
  (loop for i from 0 to (- (length *register-area*) 2) do
       (if (eq 0 (register (1+ i))) (return))
       (format t "R ~A : ~A~%" (1+ i) (register (1+ i)))))

(defmacro while (condition &body body)
  `(loop
      (unless ,condition (return))
      ,@body))

(defun maprange (f b e)
  (let (acc)
    (loop for i from b to (1- e) do
	 (push (funcall f i) acc))
    (reverse acc)))


(defun send-query (query-code)
  (setq *P* query-code)
  (setq *CP* nil)
  (setq *B* initial-value-of-B)
  (setq *E* bottom-of-stack)
  (setq *H* bottom-of-heap)
  (setq *HB* bottom-of-heap)
  (setq *TR* 0)
  (setq *fail* nil)
  (macrolet
      ((backtrack-or-continue ()
	 '(if *fail* (backtrack) (setq *P* (cdr *P*)))))
    (labels
	((dereference (a)
	   (destructuring-bind (tag . value) (store a)
	     (if (and (eq tag 'ref) (/= value a))
		 (dereference value)
		 a)))
	 (backtrack ()
	   (if (eq *B* initial-value-of-B)
	       (signal (make-condition 'prolog-query-failed))
	       (progn (setq *fail* nil)
		      (setf *B0* (stack (addr+ *B* (stack *B*) 7)))
		      (setf *P* (stack (addr+ *B* (stack *B*) 4))))))
	 (bind (a1 a2)
	   (let ((t1 (car (store a1))) (t2 (car (store a2))))
	     (if (and (eq t1 'ref) (or (not (eq t2 'ref)) (addr< a2 a1)))
		 (progn (setf (store a1) (store a2))
			(set-to-trail a1))
		 (progn (setf (store a2) (store a1))
			(set-to-trail a2)))))
	 (set-to-trail (a)
	   ;(format t "trail?... a:~A HB:~A H:~A B:~A~%" a *HB* *H* *B*)
	   (when (or (addr< a *HB*) (and (addr< *H* a) (addr< a *B*)))
	     ;(format t "trail! : ~A~%" a)
	     (setf (trail *TR*) a)
	     (incf *TR*)))
	 (unwind-trail (a1 a2)
	   (loop for i from a1 to (1- a2) do
		;(format t "unwind! : ~A~%" (trail i))
		(setf (store (trail i)) (cons 'ref (trail i)))))
	 (tidy-trail ()
	   (let ((i (stack (addr+ *B* (stack *B*) 5))))
	     (while (< i *TR*)
	       (if (or (addr< (trail i) *HB*) (and (addr< *H* (trail i)) (addr< (trail i) *B*)))
		   (incf i)
		   (progn (setf (trail i) (trail (1- *TR*)))
			  (decf *TR*))))))
	 (defined (pair)
	   (if (gethash pair *dispatching-code-table*)
	       t
	       nil))
	 (unify (a1 a2)
	   (let ((pdl nil))
	     (push a1 pdl) (push a2 pdl) (setq *fail* nil)
	     (loop while (not (or (null pdl) *fail*)) do
		  (let ((d1 (dereference (pop pdl))) (d2 (dereference (pop pdl))))
		    (when (/= d1 d2)
		      (destructuring-bind
			    ((t1 . v1) (t2 . v2)) (list (store d1) (store d2))
			(if (eq t1 'ref)
			    (bind d1 d2)
			    (case t2
			      (ref (bind d1 d2))
			      (con
			       (setq *fail*
				     (or (not (eq t1 'con)) (not (eq v1 v2)))))
			      (lis
			       (if (not (eq t1 'lis))
				   (setq *fail* t)
				   (progn
				     (push v1 pdl) (push v2 pdl)
				     (push (addr+ v1 1) pdl)
				     (push (addr+ v2 1) pdl))))
			      (struct
			       (if (not (eq t1 'struct))
				   (setq *fail* t)
				   (progn
				     (destructuring-bind
					   ((f1 .  n1) (f2 . n2))
					 (list (store v1) (store v2))
				       (if (or (not (eq f1 f2)) (/= n1 n2))
					   (setq *fail* t)
					   (loop for i from 1 to n1 do
						(push (addr+ v1 i) pdl)
						(push (addr+ v2 i) pdl)))))))))))))))
	 (to-lisp-object (addr)
	   (let* ((d (dereference addr))
		  (obj (store d)))
	     (case (car obj)
	       (ref *unbound-variable*)
	       (con (cdr obj))
	       (lis (list '|.| (to-lisp-object (cdr obj)) (to-lisp-object (addr+ (cdr obj) 1))))
	       (struct
		(let* ((struct-info (store (cdr obj)))
		       (functor (car struct-info)) (arity (cdr struct-info)))
		  (cons functor
			(maprange (lambda (i)
				    (to-lisp-object (addr+ (cdr obj) i))) 1 (1+ arity))))))))
	 (make-solution-result (assign-table)
	   (let (alist)
	     (mapc
	      (lambda (ele)
		(let ((k (car ele)) (v (cdr ele)))
		  (push (cons k (to-lisp-object (addr+ *E* (cdr v) 1))) alist))) assign-table)
	     alist)))
      (loop
	 (let ((inst (car *P*)))
	   ;(format t "next: ~A  fail:~A~%" (car inst) *fail*)
	   (if (null inst)
	       (return)
	       (if (eq 'fail inst)
		   (backtrack)
		   (case (car inst)
		     (notify-solution
		      (restart-case
			  (signal (make-condition 'prolog-found-solution
						  :vars (make-solution-result (second inst))
						  :can-backtrack? (/= *B* initial-value-of-B)))
			(next-solution ()
			  (backtrack))))
		     (put-variable-temporary (let ((x (cadr inst)) (a (caddr inst)))
					       (setf (heap *H*) (cons 'ref *H*))
					       (setf (register x) (heap *H*))
					       (setf (register a) (heap *H*))
					       (setf *H* (addr+ *H* 1))
					       (setq *P* (cdr *P*))))
		     (put-variable-permanent (let* ((y (cadr inst)) (a (caddr inst))
						    (addr (addr+ *E* y 1)))
					       (setf (stack addr) (cons 'ref addr))
					       (setf (register a) (stack addr))
					       (setq *P* (cdr *P*))))
		     (put-value-temporary (let ((x (cadr inst)) (a (caddr inst)))
					    (setf (register a) (register x))
					    (setq *P* (cdr *P*))))
		     (put-value-permanent (let ((y (cadr inst)) (a (caddr inst)))
					    (setf (register a) (stackvar y))
					    (setq *P* (cdr *P*))))
		     (put-unsafe-value (let* ((y (cadr inst)) (a (caddr inst))
					      (addr (dereference (addr+ *E* y 1))))
					 (if (addr< addr *E*)
					     (setf (register a) (store addr))
					     (progn
					       (setf (heap *H*) (cons 'ref *H*))
					       (bind addr *H*)
					       (setf (register a) (heap *H*))
					       (setf *H* (addr+ *H* 1))))
					 (setq *P* (cdr *P*))))
		     (put-structure (let ((f (cadr inst)) (a (caddr inst)))
				      (setf (heap *H*) f)
				      (setf (register a) (cons 'struct *H*))
				      (setf *H* (addr+ *H* 1))
				      (setq *P* (cdr *P*))))
		     (put-list (let ((a (cadr inst)))
				 (setf (register a) (cons 'lis *H*))
				 (setq *P* (cdr *P*))))
		     (put-constant (let ((c (cadr inst)) (a (caddr inst)))
				     (setf (register a) (cons 'con c))
				     (setq *P* (cdr *P*))))
		     (set-variable-temporary (let ((x (cadr inst)))
					       (setf (heap *H*) (cons 'ref *H*))
					       (setf (register x) (heap *H*))
					       (setf *H* (addr+ *H* 1))
					       (setq *P* (cdr *P*))))
		     (set-variable-permanent (let ((y (cadr inst)))
					       (setf (heap *H*) (cons 'ref *H*))
					       (setf (stackvar y) (heap *H*))
					       (setf *H* (addr+ *H* 1))
					       (setq *P* (cdr *P*))))
		     (set-value-temporary (let ((x (cadr inst)))
					    (setf (heap *H*) (register x))
					    (setf *H* (addr+ *H* 1))
					    (setq *P* (cdr *P*))))
		     (set-value-permanent (let ((y (cadr inst)))
					    (setf (heap *H*) (stack (addr+ *E* y 1)))
					    (setf *H* (addr+ *H* 1))
					    (setq *P* (cdr *P*))))
		     (set-local-value-temporary
		      (let* ((x (cadr inst))
			     (addr (dereference x)))
			(if (addr< addr *H*)
			    (setf (heap *H*) (heap addr))
			    (progn
			      (setf (heap *H*) (cons 'ref *H*))
			      (bind addr *H*)))
			(setf *H* (addr+ *H* 1))
			(setq *P* (cdr *P*))))
		     (set-local-value-permanent
		      (let* ((y (cadr inst))
			     (addr (dereference (addr+ *E* y 1))))
			(if (addr< addr *H*)
			    (setf (heap *H*) (heap addr))
			    (progn
			      (setf (heap *H*) (cons 'ref *H*))
			      (bind addr *H*)))
			(setf *H* (addr+ *H* 1))
			(setq *P* (cdr *P*))))
		     (set-constant (let ((c (cadr inst)))
				     (setf (heap *H*) (cons 'con c))
				     (setf *H* (addr+ *H* 1))
				     (setq *P* (cdr *P*))))
		     (set-void (let ((n (cadr inst)))
				 (dotimes (i n)
				   (setf (heap (addr+ *H* i)) (cons 'ref (addr+ *H* i))))
				 (setf *H* (addr+ *H* n))
				 (setq *P* (cdr *P*))))
		     (get-variable-temporary
		      (let ((x (cadr inst)) (a (caddr inst)))
			(setf (register x) (register a))
			(setq *P* (cdr *P*))))
		     (get-variable-permanent
		      (let ((y (cadr inst)) (a (caddr inst)))
			(setf (stackvar y) (register a))
			(setq *P* (cdr *P*))))
		     (get-value-temporary
		      (let ((x (cadr inst)) (a (caddr inst)))
			(unify x a)
			(backtrack-or-continue)))
		     (get-value-permanent
		      (let ((y (cadr inst)) (a (caddr inst)))
			(unify (addr+ *E* y 1) a)
			(backtrack-or-continue)))
		     (get-structure
		      (let* ((f (cadr inst)) (a (caddr inst))
			     (addr (dereference a)))
			(case (car (store addr))
			  (ref (setf (heap *H*) (cons 'struct (addr+ *H* 1)))
			       (setf (heap (addr+ *H* 1)) f)
			       (bind addr *H*)
			       (setf *H* (addr+ *H* 2))
			       (setq *mode* 'write))
			  (struct (let ((struct-addr (cdr (store addr))))
				    (if (equal (heap struct-addr) f)
					(progn
					  (setf *S* (addr+ struct-addr 1))
					  (setq *mode* 'read))
					(setq *fail* t))))
			  (t (setq *fail* t)))
			(backtrack-or-continue)))
		     (get-list (let* ((a (cadr inst))
				      (addr (dereference a)))
				 (case (car (store addr))
				   (ref (setf (heap *H*) (cons 'lis (addr+ *H* 1)))
					(bind addr *H*)
					(setf *H* (addr+ *H* 1))
					(setq *mode* 'write))
				   (lis (let ((list-addr (cdr (store addr))))
					  (setf *S* list-addr)
					  (setq *mode* 'read)))
				   (t (setq *fail* t)))
				 (backtrack-or-continue)))
		     (get-constant (let* ((c (cadr inst)) (a (caddr inst))
					  (addr (dereference a)))
				     (case (car (store addr))
				       (ref (setf (store addr) (cons 'con c))
					    (set-to-trail addr))
				       (con (let ((c2 (cdr (store addr))))
					      (if (not (equal c c2))
						  (setq *fail* t))))
				       (t (setq *fail* t)))
				     (backtrack-or-continue)))
		     (unify-variable-temporary
		      (let ((x (cadr inst)))
			(case *mode*
			  (read (setf (register x) (heap *S*))
				(setf *S* (addr+ *S* 1)))
			  (write
			   (setf (heap *H*) (cons 'ref *H*))
			   (setf (register x) (heap *H*))
			   (setf *H* (addr+ *H* 1))))
			(setq *P* (cdr *P*))))
		     (unify-variable-permanent
		      (let ((y (cadr inst)))
			(case *mode*
			  (read (setf (stackvar y) (heap *S*))
				(setf *S* (addr+ *S* 1)))
			  (write
			   (setf (heap *H*) (cons 'ref *H*))
			   (setf (stackvar y) (heap *H*))
			   (setf *H* (addr+ *H* 1))))
			(setq *P* (cdr *P*))))
		     (unify-value-temporary
		      (let ((x (cadr inst)))
			(case *mode*
			  (read (unify x *S*)
				(setf *S* (addr+ *S* 1)))
			  (write (setf (heap *H*) (register x))
				 (setf *H* (addr+ *H* 1))))
			(backtrack-or-continue)))
		     (unify-value-permanent
		      (let ((y (cadr inst)))
			(case *mode*
			  (read (unify (addr+ *E* y 1) *S*)
				(setf *S* (addr+ *S* 1)))
			  (write (setf (heap *H*) (stackvar y))
				 (setf *H* (addr+ *H* 1))))
			(backtrack-or-continue)))
		     (unify-local-value-temporary
		      (let ((x (cadr inst)))
			(case *mode*
			  (read (unify x *S*)
				(setf *S* (addr+ *S* 1)))
			  (write (let ((addr (dereference x)))
				   (if (addr< addr *H*)
				       (setf (heap *H*) (heap addr))
				       (progn
					 (setf (heap *H*) (cons 'ref *H*))
					 (bind addr *H*)))
				   (setf *H* (addr+ *H* 1)))))
			(backtrack-or-continue)))
		     (unify-local-value-permanent
		      (let ((y (cadr inst)))
			(case *mode*
			  (read (unify (addr+ *E* y 1) *S*)
				(setf *S* (addr+ *S* 1)))
			  (write (let ((addr (dereference (addr+ *E* y 1))))
				   (if (addr< addr *H*)
				       (setf (heap *H*) (heap addr))
				       (progn
					 (setf (heap *H*) (cons 'ref *H*))
					 (bind addr *H*)))
				   (setf *H* (addr+ *H* 1)))))
			(backtrack-or-continue)))
		     (unify-constant
		      (let ((c (cadr inst)))
			(case *mode*
			  (read (let ((addr (dereference *S*)))
				  (case (car (store addr))
				    (ref (setf (store addr) (cons 'con c))
					 (set-to-trail addr))
				    (con (if (not (equal (cdr (store addr)) c))
					     (setq *fail* t)))
				    (t (setq *fail* t)))
				  (setf *S* (addr+ *S* 1))))
			  (write (setf (heap *H*) (cons 'con c))
				 (setf *H* (addr+ *H* 1))))
			(backtrack-or-continue)))
		     (unify-void
		      (let ((n (cadr inst)))
			(case *mode*
			  (read (setf *S* (addr+ *S* n)))
			  (write (dotimes (i n)
				   (setf (heap (addr+ *H* i)) (cons 'ref (addr+ *H* i))))
				 (setf *H* (addr+ *H* n))))
			(setq *P* (cdr *P*))))
		     (allocate
		      (let ((new-E
			     (if (addr< *B* *E*)
				 (addr+ *E* (car (last (car (stack (addr+ *E* 1)))))  2)
				 (addr+ *B* (stack *B*) 8))))
			(setf (stack new-E) *E*)
			(setf (stack (addr+ new-E 1)) *CP*)
			(setf *E* new-E)
			(setq *P* (cdr *P*))))
		     (deallocate
		      (setf *CP* (stack (addr+ *E* 1)))
		      (setf *E* (stack *E*))
		      (setq *P* (cdr *P*)))
		     (allocate-for-query
		      (setf (stack *E*) *E*)
		      (setf (stack (addr+ *E* 1)) *P*)
		      (setq *P* (cdr *P*)))
		     (call (let ((pair (cadr inst)))
			     (if (defined pair)
				 (progn (setf *CP* (cdr *P*))
					(setf *num-of-args* (cdr pair))
					(setf *B0* *B*)
					(setf *P* (gethash pair *dispatching-code-table*)))
				 (backtrack))))
		     (execute (let ((pair (cadr inst)))
				(if (defined pair)
				    (progn (setf *num-of-args* (cdr pair))
					   (setf *B0* *B*)
					   (setf *P*
						 (gethash pair *dispatching-code-table*)))
				    (backtrack))))
		     (proceed
		      (setf *P* *CP*))
		     (try-me-else
		      (let* ((l (third inst))
			     ;;(second inst)にはラベルのシンボルが入ってる（表示用）
			     ;;(third inst)に、ラベルの次の命令へのポインタが入ってる
			     (new-B
			      (if (addr< *B* *E*)
				  (addr+ *E* (car (last (car (stack (addr+ *E* 1))))) 2)
				  (addr+ *B* (stack *B*) 8)))
			     (n *num-of-args*))
			(setf (stack new-B) *num-of-args*)
			(dotimes (i n)
			  (setf (stack (addr+ new-B (1+ i))) (register (1+ i))))
			(setf (stack (addr+ new-B n 1)) *E*)
			(setf (stack (addr+ new-B n 2)) *CP*)
			(setf (stack (addr+ new-B n 3)) *B*)
			(setf (stack (addr+ new-B n 4)) l)
			(setf (stack (addr+ new-B n 5)) *TR*)
			(setf (stack (addr+ new-B n 6)) *H*)
			(setf (stack (addr+ new-B n 7)) *B0*)
			(setf *B* new-B)
			(setf *HB* *H*)
			(setq *P* (cdr *P*))))
		     (retry-me-else
		      (let ((l (third inst))
			    (n (stack *B*)))
			(setf *num-of-args* n)
			(dotimes (i n)
			  (setf (register (1+ i)) (stack (addr+ *B* (1+ i)))))
			(setf *E* (stack (addr+ *B* n 1)))
			(setf *CP* (stack (addr+ *B* n 2)))
			(setf (stack (addr+ *B* n 4)) l)
			(unwind-trail (stack (addr+ *B* n 5)) *TR*)
			(setf *TR* (stack (addr+ *B* n 5)))
			(setf *H* (stack (addr+ *B* n 6)))
			(setf *HB* *H*)
			(setq *P* (cdr *P*))))
		     (trust-me (let ((n (stack *B*)))
				 (setf *num-of-args* n)
				 (dotimes (i n)
				   (setf (register (1+ i)) (stack (addr+ *B* (1+ i)))))
				 (setf *E* (stack (addr+ *B* n 1)))
				 (setf *CP* (stack (addr+ *B* n 2)))
				 (unwind-trail (stack (addr+ *B* n 5)) *TR*)
				 (setf *TR* (stack (addr+ *B* n 5)))
				 (setf *H* (stack (addr+ *B* n 6)))
				 (setf *B* (stack (addr+ *B* n 3)))
				 (if (/= *B* initial-value-of-B)
				     (setf *HB* (stack (addr+ *B* n 6)))
				     (setf *HB* bottom-of-heap))
				 (setq *P* (cdr *P*))))
		     (try
		      (let* ((l (third inst))
			     (new-B
			      (if (addr< *B* *E*)
				  (addr+ *E* (car (last (car (stack (addr+ *E* 1))))) 2)
				  (addr+ *B* (stack *B*) 8)))
			     (n *num-of-args*))
			(setf (stack new-B) *num-of-args*)
			(dotimes (i n)
			  (setf (stack (addr+ new-B (1+ i))) (register (1+ i))))
			(setf (stack (addr+ new-B n 1)) *E*)
			(setf (stack (addr+ new-B n 2)) *CP*)
			(setf (stack (addr+ new-B n 3)) *B*)
			(setf (stack (addr+ new-B n 4)) (cdr *P*))
			(setf (stack (addr+ new-B n 5)) *TR*)
			(setf (stack (addr+ new-B n 6)) *H*)
			(setf (stack (addr+ new-B n 7)) *B0*)
			(setf *B* new-B)
			(setf *HB* *H*)
			(setf *P* l)))
		     (retry (let ((l (third inst))
				  (n (stack *B*)))
			      (setf *num-of-args* n)
			      (dotimes (i n)
				(setf (register (1+ i)) (stack (addr+ *B* (1+ i)))))
			      (setf *E* (stack (addr+ *B* n 1)))
			      (setf *CP* (stack (addr+ *B* n 2)))
			      (setf (stack (addr+ *B* n 4)) (cdr *P*))
			      (unwind-trail (stack (addr+ *B* n 5)) *TR*)
			      (setf *TR* (stack (addr+ *B* n 5)))
			      (setf *H* (stack (addr+ *B* n 6)))
			      (setf *HB* *H*)
			      (setf *P* l)))
		     (trust (let ((l (third inst))
				  (n (stack *B*)))
			      (setf *num-of-args* n)
			      (dotimes (i n)
				(setf (register (1+ i)) (stack (addr+ *B* (1+ i)))))
			      (setf *E* (stack (addr+ *B* n 1)))
			      (setf *CP* (stack (addr+ *B* n 2)))
			      (unwind-trail (stack (addr+ *B* n 5)) *TR*)
			      (setf *TR* (stack (addr+ *B* n 5)))
			      (setf *H* (stack (addr+ *B* n 6)))
			      (setf *B* (stack (addr+ *B* n 3)))
			      (if (/= *B* initial-value-of-B)
				  (setf *HB* (stack (addr+ *B* n 6)))
				  (setf *HB* bottom-of-heap))
			      (setf *P* l)))
		     (switch-on-term (let ((v (sixth inst)) (c (seventh inst))
					   (l (eighth inst)) (s (ninth inst)))
				       ;(format t "switch=>~A~%" (store (dereference 1)))
				       (case (car (store (dereference 1)))
					 (ref (setf *P* v))
					 (con (setf *P* c))
					 (lis (setf *P* l))
					 (struct (setf *P* s)))))
		     (switch-on-constant
		      (let* ((ht (third inst))
			     (val (cdr (store (dereference 1))))
			     (result (gethash val ht)))
			(if result
			    (setf *P* result)
			    (backtrack))))
		     (switch-on-structure
		      (let* ((ht (third inst))
			     (val (cdr (store (dereference 1))))
			     (result (gethash val ht)))
			(if result
			    (setf *P* result)
			    (backtrack))))
		     (neck-cut (when (addr< *B0* *B*)
				 (setq *B* *B0*)
				 (tidy-trail))
			       (setq *P* (cdr *P*))) 
		     (get-level (let ((y (cadr inst)))
				  (setf (stack (addr+ *E* 2 y)) *B0*)))
		     (cut (let ((y (cadr inst)))
			    (when (addr< (stack (addr+ *E* 2 y)) *B*)
			      (setf *B* (stack (addr+ *E* 2 y)))
			      (tidy-trail))
			    (setq *P* (cdr *P*))))
		     (label
		      (setq *P* (cdr *P*)))
		     (otherwise (error
				 (make-condition
				  'prolog-bad-instruction-error :inst (car inst))))))))))))

