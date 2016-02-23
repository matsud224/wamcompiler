;;(defun main ()
;;  (format t "hello,world~%"))
;;
;;(sb-ext:save-lisp-and-die "wamcompiler"
;;			  :toplevel #'main
;;			  :executable t)

(defvar *operator-list* nil)


(defmacro op-atom (x) `(car ,x))
(defmacro op-prec (x) `(cadr ,x))
(defmacro op-assoc (x) `(caddr ,x))

(defmacro dostream (bind &body body)
  `(loop (let ((,(car bind) (read-char ,(cadr bind))))
	   (if (null ,(car bind)) (return nil)) ,@body)))


(defvar *non-alphanum-chars* '(#\# #\$ #\& #\* #\+ #\- #\. #\/ #\: #\< #\= #\> #\? #\@ #\^ #\~ #\\))

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
	  ((digit-char-p c) (unread-char c s) (cons 'int (read-int s)))
	  ((member c *non-alphanum-chars*) (unread-char c s) (read-non-alphanum-atom s))
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
      (cond ((or (upper-case-p (char str 0)) (equal str "_")) (cons 'variable (intern str)))
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
		   ((op< (car head) x) (cons (car head) (insert-op-sorted (cdr head) x)))
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
    (labels ((next-token ()
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
		 (otherwise (princ (car tree)) (error "Bad ast."))))
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
					(t (error "Syntax error! (near argument list)"))))))
		       (reverse (get-args nil)))
		     (progn (unget-token next) nil))))
	     (parse-list ()
	       (let ((next (next-token)))
		 (if (eq (car next) 'rbracket)
		     (list 'struct '|[]|)
		     (labels ((get-args (acc)
				(let ((arg (parse-sub *operator-list* t))
				      (n (next-token)))
				  (cond ((eq (car n) 'rbracket) (cons (list 'struct '|[]|) (cons arg acc)))
					((eq (car n) 'vertical-bar)
					 (cons (prog1
						   (parse-sub *operator-list* t)
						 (unless (eq (car (next-token)) 'rbracket)
						   (error "Syntax error! (expected rbracket next to tail part)")))
					       (cons arg acc)))
					((commap n)
					 (get-args (cons arg acc)))
					(t (error "Syntax error! (near argument list)"))))))
		       (unget-token next)
		       `(list ,@(reverse (get-args nil)))))))
	     (parse-prim (ignore-comma)
	       (let ((next (next-token)))
		 (cond ((eq (car next) 'lparen)
			(let ((inside (parse-sub *operator-list* ignore-comma)))
			  (if (eq (car (next-token)) 'rparen)
			      inside
			      (error "Syntax error! (no rparen)"))))
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
		       ((separatorp gottok ignore-comma) (error "Syantax error! (unexpected end of clause)"))
		       ((null head) (unget-token gottok) (return-from exit (parse-prim ignore-comma)))
		       ((operatorp gottok)
			(let* ((gottok-2 (let ((n (next-token))) (unget-token n) n))
			       (have-operand (not (non-operandp gottok-2 ignore-comma))))
			  (do ((op (car head) (progn (setq head (cdr head)) (car head))))
			      ((or (null op) (not (eq current-prec (op-prec op)))
				   (not (member (op-assoc op) '(fx fy))))
			       (unget-token gottok))
			    (if have-operand
				(case (op-assoc op)
				  (fx (return-from exit `(struct ,(op-atom op) ,(parse-sub next-head ignore-comma))))
				  (fy (return-from exit `(struct ,(op-atom op) ,(parse-sub current-head ignore-comma)))))))))
		       (t (unget-token gottok))))
		   (let ((operand-1 (parse-sub next-head ignore-comma))
			 (gottok (next-token)))
		     (cond ((separatorp gottok ignore-comma) (unget-token gottok) (return-from exit operand-1))
			   ((operatorp gottok)
			    (let* ((gottok-2 (let ((n (next-token))) (unget-token n) n))
				   (have-operand (not (non-operandp gottok-2 ignore-comma))))
			      (do ((op (car head) (progn (setq head (cdr head)) (car head))))
				  ((or (null op) (not (eq current-prec (op-prec op))))
				   (progn (unget-token gottok) (return-from exit operand-1)))
				(if (and (eq (car gottok) 'atom) (eq (cdr gottok) (op-atom op)))
				    (if have-operand
					(case (op-assoc op)
					  (xfy	(return-from exit `(struct ,(op-atom op) ,operand-1 ,(parse-sub current-head ignore-comma))))
					  (xfx	(return-from exit `(struct ,(op-atom op) ,operand-1 ,(parse-sub next-head ignore-comma))))
					  (yfx
					   (unget-token `(struct ,(op-atom op) ,operand-1 ,(parse-sub next-head ignore-comma)))
					   (return-from exit (parse-sub current-head ignore-comma))))
					(case (op-assoc op)
					  (xf (return-from exit `(struct ,(op-atom op) ,operand-1)))
					  (yf
					   (unget-token `(struct ,(op-atom op) ,operand-1))
					   (return-from exit (parse-sub current-head ignore-comma)))))))))))))))
      (let ((result (arrange (parse-sub *operator-list*))))
	(if (not (eq (car (next-token)) 'dot)) (error "Syntax error! (arity error)") result)))))

(defmacro variablep (x) `(atom ,x))
(defmacro anonymousvar-p (x) `(eq ,x '|_|))
(defmacro termp (x) `(listp ,x))
(defmacro listterm-p (x) `(and (listp ,x) (eq (car ,x) '|.|)))

(defmacro arity (term) `(length (cdr ,term)))


(defun flatten-comma (tree)
  (cond ((and (consp tree) (eq '|,| (car tree))) (append (flatten-comma (cadr tree)) (flatten-comma (caddr tree))))
	(t (list tree))))

(defun collect-vars (goal)
  (cond ((variablep goal) (list goal))
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
	    (setq tbl (cons (cons v (cons cnt cnt)) tbl))))
      (incf cnt))
    tbl))

(defun make-arity-list (head body)
  (let ((head-arity (arity head))
	(body1-arity (arity (car body))))
    (cons (max head-arity body1-arity) (mapcar (lambda (term) (arity term)) (cdr body)))))		  

(defun head-firstbody-conj (head body)
  (cond ((null head) (car body))
	(t (append head (cdar body)))))

(defun assign-variables (head body)
  (let* ((Y-counter 0) (arity-list (make-arity-list head body))
	 (A-counter (apply #'max arity-list))
	 (postbl (make-varposition-table (cons (head-firstbody-conj head body) (cdr body))))
	 (sorted-tbl (sort postbl (lambda (x y) (> (cddr x) (cddr y)))))
	 (assigned-tbl (mapcar (lambda (v)
				 (let ((var (car v))
				       (first-appear (cadr v)) (last-appear (cddr v)))
				   (if (= first-appear last-appear)
				       (cons var (cons 'temporary (incf A-counter)))
				       (cons var (cons 'permanent (incf Y-counter)))))) sorted-tbl))
	 (remain-list (make-list (length body) :initial-element 0))
	 (head-vars (collect-vars head))
	 (permanent-lastgoal-table
	  (remove-if (lambda (i)
		       (if (and (consp i) (member (car i) head-vars)) i))
		     (mapcar (lambda (v)
			       (let ((var (car v))
				     (first-appear (cadr v)) (last-appear (cddr v)))
				 (when (/= first-appear last-appear)
				   (cons var last-appear)))) postbl))))
    ;;make remain-list
    (dolist (v sorted-tbl)
      (loop for i from 0 to (1- (cddr v)) do
	   (incf (nth i remain-list))))
    (when (consp remain-list)
      (setf (car (last remain-list)) -1))
    (values assigned-tbl arity-list remain-list permanent-lastgoal-table)))
  

(defun assign-test ()
  (let ((expr (parse *standard-input*)))
    (cond ((and (eq (car expr) '|:-|) (= (arity expr) 2))
	   (assign-variables (cadr expr) (flatten-comma (caddr expr))))
	  ((and (eq (car expr) '|:-|) (= (arity expr) 1))
	   (assign-variables nil (flatten-comma (cadr expr))))
	  (t
	   (assign-variables expr nil)))))

(defun compile-test ()
  (let ((expr (parse *standard-input*)))
    (print-wamcode (compile-clause expr))))

(defun print-wamcode (code)
  (dolist (inst code)
    (case (car inst)
      (put-variable-temporary
       (format t "put-variable X~A,A~A~%" (cadr inst) (caddr inst)))
      (put-variable-permanent
       (format t "put-variable Y~A,A~A~%" (cadr inst) (caddr inst)))
      (put-value-temporary
       (format t "put-value X~A,A~A~%" (cadr inst) (caddr inst)))
      (put-value-permanent
       (format t "put-value Y~A,A~A~%" (cadr inst) (caddr inst)))
      (put-unsafe-value
       (format t "put-unsafe-value Y~A,A~A~%" (cadr inst) (caddr inst)))
      (put-structure
       (format t "put-structure ~A/~A,A~A~%" (caadr inst) (cdadr inst) (caddr inst)))
      (put-list
       (format t "put-list A~A~%" (cadr inst)))
      (put-constant
       (format t "put-constant ~A,A~A~%" (cadr inst) (caddr inst)))
      (set-variable-temporary
       (format t "set-variable X~A~%" (cadr inst)))
      (set-variable-permanent
       (format t "set-variable Y~A~%" (cadr inst)))
      (set-value-temporary
       (format t "set-value X~A~%" (cadr inst)))
      (set-value-permanent
       (format t "set-value Y~A~%" (cadr inst)))
      (set-local-value-temporary
       (format t "set-local-value X~A~%" (cadr inst)))
      (set-local-value-permanent
       (format t "set-local-value Y~A~%" (cadr inst)))
      (set-constant
       (format t "set-constant ~A~%" (cadr inst)))
      (set-void
       (format t "set-void ~A~%" (cadr inst)))
      (get-variable-temporary
       (format t "get-variable X~A,A~A~%" (cadr inst) (caddr inst)))
      (get-variable-permanent
       (format t "get-variable Y~A,A~A~%" (cadr inst) (caddr inst)))
      (get-value-temporary
       (format t "get-value X~A,A~A~%" (cadr inst) (caddr inst)))
      (get-value-permanent
       (format t "get-value Y~A,A~A~%" (cadr inst) (caddr inst)))
      (get-structure
       (format t "get-structure ~A/~A,A~A~%" (caadr inst) (cdadr inst) (caddr inst)))
      (get-list
       (format t "get-list A~A~%" (cadr inst)))
      (get-constant
       (format t "get-constant ~A,A~A~%" (cadr inst) (caddr inst)))
      (unify-variable-temporary
       (format t "unify-variable X~A~%" (cadr inst)))
      (unify-variable-permanent
       (format t "unify-variable Y~A~%" (cadr inst)))
      (unify-value-temporary
       (format t "unify-value X~A~%" (cadr inst)))
      (unify-value-permanent
       (format t "unify-value Y~A~%" (cadr inst)))
      (unify-local-value-temporary
       (format t "unify-local-value X~A~%" (cadr inst)))
      (unify-local-value-permanent
       (format t "unify-local-value Y~A~%" (cadr inst)))
      (unify-constant
       (format t "unify-constant ~A~%" (cadr inst)))
      (unify-void
       (format t "unify-void ~A~%" (cadr inst)))
      (allocate
       (format t "allocate~%"))
      (deallocate
       (format t "deallocate~%"))
      (call
       (format t "call ~A/~A,~A~%" (caadr inst) (cdadr inst) (caddr inst)))
      (execute
       (format t "execute ~A/~A~%" (caadr inst) (cdadr inst)))
      (proceed
       (format t "proceed~%"))
      (try-me-else
       (format t "try-me-else ~A~%" (cadr inst)))
      (retry-me-else
       (format t "retry-me-else ~A~%" (cadr inst)))
      (trust-me
       (format t "trust-me ~A~%" (cadr inst)))
      (try
       (format t "try ~A~%" (cadr inst)))
      (retry
       (format t "retry ~A~%" (cadr inst)))
      (trust
       (format t "trust ~A~%" (cadr inst)))
      (switch-on-term
       (format t "switch-on-term ~A,~A,~A,~A~%" (cadr inst) (caddr inst) (cadddr inst) (car (cddddr inst))))
      (switch-on-constant
       (format t "switch-on-constant ~A~%" (cadr inst)))
      (switch-on-structure
       (format t "switch-on-structure ~A~%" (cadr inst)))
      (neck-cut
       (format t "neck-cut~%"))
      (get-level
       (format t "get-level Y~A~%" (cadr inst)))
      (cut
       (format t "cut Y~A~%" (cadr inst)))
      (t (error (format nil "Unknown instruction (~A)" (car inst)))))))


(defun have-key? (key ht)
  (multiple-value-bind (val exist) (gethash key ht)
    exist))

(defmacro awhen (condition &body body)
  `(let ((it ,condition))
     (when it
       ,@body)))

(defun propagate! (code)
  (let ((temporary-table (make-hash-table :test #'eq))
	(deftime-table (make-hash-table :test #'eq))
	(timestamp 0)
	(changed? nil))
    (labels ((def-temporary (var-number val)
	       (unless (have-key? var-number temporary-table)
		 (setf (gethash var-number temporary-table) val)
		 (setf (gethash var-number deftime-table) (incf timestamp))))
	     (def-temporary-force (var-number val)
	       (kill-temporary var-number)
	       (def-temporary var-number val))
	     (kill-temporary (var-number)
	       (remhash var-number temporary-table)
	       (remhash var-number deftime-table))
	     (use-temporary-sub (var-number)
	       (when (have-key? var-number temporary-table)
		 (let* ((def-val (gethash var-number temporary-table))
			(def-time (gethash var-number deftime-table))
			(def2-time (gethash def-val deftime-table)))
		   (if def2-time
		       (if (> def-time def2-time) def-val)
		       def-val)))))
      (macrolet ((use-temporary (pos)
		   `(awhen (use-temporary-sub ,pos)
		      (setf changed? t) (setf ,pos it))))
	(dolist (inst code)
	  (case (car inst)
	    (put-variable-temporary
	     (def-temporary (caddr inst) (cadr inst)))
	    (put-value-temporary
	     (def-temporary (caddr inst) (cadr inst))
	     (use-temporary (cadr inst)))
	    (get-variable-temporary
	     (def-temporary (cadr inst) (caddr inst))
	     (use-temporary (cadr inst)))
	    (get-value-temporary
	     (def-temporary (cadr inst) (caddr inst))
	     (use-temporary (cadr inst)))
	    (set-variable-temporary
	     (use-temporary (cadr inst)))
	    (set-value-temporary
	     (use-temporary (cadr inst)))
	    (set-local-value-temporary
	     (use-temporary (cadr inst)))
	    (unify-variable-temporary
	     (use-temporary (cadr inst)))
	    (unify-value-temporary
	     (use-temporary (cadr inst)))
	    (unify-local-value-temporary
	     (use-temporary (cadr inst)))
	    ((call execute)
	     (clrhash temporary-table)
	     (clrhash deftime-table))))
	(values code changed?)))))

(defun remove-unnecessary-code (code)
  (let ((changed? nil))
    (labels ((not-used (var-number rest-code)
	       (dolist (inst rest-code)
		 (case (car inst)
		   ((put-variable-temporary
		     put-value-temporary
		     set-variable-temporary
		     set-value-temporary
		     set-local-value-temporary
		     get-variable-temporary
		     get-value-temporary
		     unify-variable-temporary
		     unify-value-temporary
		     unify-local-value-temporary)
		    (if (= (cadr inst) var-number) (return nil)))
		   ((call execute)
		    (return t))))))
      (values
       (remove nil
	       (maplist (lambda (current-code)
			  (let ((inst (car current-code)))
			    (case (car inst)
			      ((put-variable-temporary put-value-temporary)
			       (cond ((= (cadr inst) (caddr inst)) (setq changed? t) nil)
				     (t inst)))
			      ((get-variable-temporary get-value-temporary)
			       (cond ((or (= (cadr inst) (caddr inst))
					  (not-used (cadr inst) (cdr current-code)))
				      (setq changed? t) nil)
				     (t inst)))
			      (t inst))))
			code))
       changed?))))
  

;;remove put/set/unify instruction which operands are not initialized.
(defun remove-uninitialized-variable (code)
  (let ((changed? nil)
	(lookedvars nil))
    (values
     (remove nil
	     (maplist (lambda (current-code)
			(let ((inst (car current-code)))
			  (case (car inst)
			    ((get-variable-temporary
			      get-value-temporary)
			     (pushnew (cadr inst) lookedvars)
			     (pushnew (caddr inst) lookedvars)
			     inst)
			    ((unify-variable-temporary
			      unify-value-temporary
			      unify-local-value-temporary)
			     (pushnew (cadr inst) lookedvars)
			     inst)
			    ((put-variable-temporary
			      put-value-temporary
			      set-variable-temporary
			      set-value-temporary
			      set-local-value-temporary)
			     (if (member (cadr inst) lookedvars)
				 inst
				 nil))
			    (t inst))))
		      code))
     changed?)))

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
	  ((call execute)
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
	  ((call execute)
	   (incf body-num)))))
    code))

	

(defun reallocate-registers! (code head body)
  (let* ((arity-list (make-arity-list head body))
	 (A-start (1+ (apply #'max arity-list)))
	 (using-register-list (list nil)))
    ;;collect temporary variables...
    	(dolist (inst code)
	  (case (car inst)
	    ((put-variable-temporary
	      put-value-temporary
	      get-variable-temporary
	      get-value-temporary
	      set-variable-temporary
	      set-value-temporary
	      set-local-value-temporary
	      unify-variable-temporary
	      unify-value-temporary
	      unify-local-value-temporary)
	     (when (>= (cadr inst) A-start)
	       (pushnew (cadr inst) (car using-register-list))))
	    ((call execute)
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
		get-variable-temporary
		get-value-temporary
		set-variable-temporary
		set-value-temporary
		set-local-value-temporary
		unify-variable-temporary
		unify-value-temporary
		unify-local-value-temporary)
	       (awhen (assoc (cadr inst) (car reallocate-list))
		 (setf (cadr inst) (cdr it))))
	      ((call execute)
	       (setq reallocate-list (cdr reallocate-list)))))
	  code)))
	  
(defun propagate-test ()
  (let ((clause (parse *standard-input*)))
    (multiple-value-bind (head body) (devide-head-body clause)
      (let ((compiled (compile-clause clause)))
	(print-wamcode compiled)
	(format t "***************~%")
	(multiple-value-bind (newcode changed?) (propagate! compiled)
	  (print-wamcode newcode)
	  (format t "***************~%")
	  (multiple-value-bind (newcode changed?) (remove-unnecessary-code newcode)
	    (print-wamcode newcode)
	    (format t "***************~%")
	    (multiple-value-bind (newcode changed?) (remove-uninitialized-variable newcode)
	      (print-wamcode newcode)
	      (format t "***************~%")
	      (let ((newcode (set-unsafe-and-local! newcode)))
		(print-wamcode newcode)
		(format t "***************~%")
		(let ((newcode (reallocate-registers!  newcode head body)))
		  (print-wamcode newcode)
		changed?)))))))))
    
  
(defmacro cons-when (condition element list)
  `(if ,condition
       (cons ,element ,list)
       ,list))

(defun devide-head-body (clause)
  (destructuring-bind (head . body) (cond ((and (eq (car clause) '|:-|) (= (arity clause) 2))
					 (cons (cadr clause) (flatten-comma (caddr clause))))
					((and (eq (car clause) '|:-|) (= (arity clause) 1))
					 (cons nil (flatten-comma (cadr clause))))
					((and (eq (car clause) '|?-|) (= (arity clause) 1))
					 (cons nil (flatten-comma (cadr clause))))
					(t
					 (cons clause nil)))
    (values head body)))

(defun compile-clause (clause)
  (multiple-value-bind  (head body) (devide-head-body clause)
    (multiple-value-bind (assign-table register-next remain-list permanent-lastgoal-tbl) (assign-variables head body)
      (let ((varstate-table (mapcar (lambda (v) (cons (car v) 'uninitialized)) assign-table))) ;;uninitialized/initialized/globalized
	(labels ((uninitialized-p (var)
		   (case (cdr (assoc var varstate-table))
		     (uninitialized t)
		     (initialized nil)
		     (globalized nil)))
		 (initialized-p (var)
		   (case (cdr (assoc var varstate-table))
		     (uninitialized nil)
		     (initialized t)
		     (globalized t)))
		 (globalized-p (var)
		   (case (cdr (assoc var varstate-table))
		     (uninitialized nil)
		     (initialized nil)
		     (globalized t)))
		 (varstate (var)
		   (cdr (assoc var varstate-table)))
		 (compile-head-term (term)
		   (let ((A 0))
		     (mapcan (lambda (arg)
			       (incf A)
			       (cond ((anonymousvar-p arg) nil)
				     ((variablep arg)
				      (let ((vardata (cdr (assoc arg assign-table))))
					(list
					 (case (car vardata)
					   (temporary
					    (if (uninitialized-p arg)
						(prog1 `(get-variable-temporary ,(cdr vardata) ,A)
						  (rplacd (assoc arg varstate-table) 'initialized))
						`(get-value-temporary ,(cdr vardata) ,A)))
					   (permanent
					    (if (uninitialized-p arg)
						(prog1 `(get-variable-permanent ,(cdr vardata) ,A)
						  (rplacd (assoc arg varstate-table) 'initialized))   
						`(get-value-permanent ,(cdr vardata) ,A)))))))
				     ((listterm-p arg)
				      `((get-list ,A)
					,@(compile-head-struct-args (cdr arg))))
				     ((termp arg)
				      (if (= (arity arg) 0)
					  `((get-constant ,(car arg) ,A))
					  `((get-structure ,(cons (car arg) (arity arg)) ,A)
					    ,@(compile-head-struct-args (cdr arg)))))))
			     (cdr term))))
		 (compile-head-struct-args (struct-args)
		   (let (remaining-code) ;;code for nested structure which must be put after unification of parent structure
		     (nconc (mapcan (lambda (arg)
				      (cond ((anonymousvar-p arg) (list (list 'unify-void 1)))
					    ((variablep arg)
					     (let ((vardata (cdr (assoc arg assign-table))))
					       (list
						(case (car vardata)
						  (temporary
						   (cond ((globalized-p arg) `(unify-value-temporary ,(cdr vardata)))
						  ((initialized-p arg) (rplacd (assoc arg varstate-table) 'globalized)
						   `(unify-value-temporary ,(cdr vardata)))
						  (t (rplacd (assoc arg varstate-table) 'globalized)
						     `(unify-variable-temporary ,(cdr vardata)))))
						  (permanent
						   (cond ((globalized-p arg) `(unify-value-permanent ,(cdr vardata)))
							 ((initialized-p arg) (rplacd (assoc arg varstate-table) 'globalized)
							  `(unify-value-permanent ,(cdr vardata)))
							 (t (rplacd (assoc arg varstate-table) 'globalized)
							    `(unify-variable-permanent ,(cdr vardata)))))))))
					    ((listterm-p arg)
					     (let ((tempvar (incf (first register-next))))
					       (prog1
						   `((unify-variable-temporary ,tempvar))
						 (setq remaining-code
						       (append remaining-code
							       `((get-list ,tempvar)
								 ,@(compile-head-struct-args (cdr arg))))))))
					    ((termp arg)
					     (let ((tempvar (incf (first register-next))))
					       (if (= (arity arg) 0)
						   `((unify-constant ,(car arg)))
						   (prog1
						       `((unify-variable-temporary ,tempvar))
						     (setq remaining-code
							   (append remaining-code
								   `((get-structure ,(cons (car arg) (arity arg)) ,tempvar)
								     ,@(compile-head-struct-args (cdr arg)))))))))))
				    struct-args) remaining-code)))
		 (compile-body-term (term body-num)
		   (let ((A 0))
		     (mapcan (lambda (arg)
			       (incf A)
			       (cond ((anonymousvar-p arg)
				      (let ((tempvar (incf (nth body-num register-next))))
					`((put-variable-temporary ,tempvar ,A))))
				     ((variablep arg)
				      (let ((vardata (cdr (assoc arg assign-table))))
					(list
					 (case (car vardata)
					   (temporary
					    (if (uninitialized-p arg)
						(prog1 `(put-variable-temporary ,(cdr vardata) ,A)
						  (rplacd (assoc arg varstate-table) 'globalized))
						`(put-value-temporary ,(cdr vardata) ,A)))
					   (permanent
					    (cond ((globalized-p arg) `(put-value-permanent ,(cdr vardata) ,A))
						  ((initialized-p arg)
						   (if (eq body-num (cdr (assoc arg permanent-lastgoal-tbl)))
						       (prog1 `(put-value-permanent ,(cdr vardata) ,A)
							   (rplacd (assoc arg varstate-table) 'globalized))
						       `(put-value-permanent ,(cdr vardata) ,A)))
						  (t (rplacd (assoc arg varstate-table) 'initialized)
						     `(put-variable-permanent ,(cdr vardata) ,A))))))))
				     ((listterm-p arg)
				      `((put-list ,A)
					,@(compile-body-struct-args (cdr arg) body-num)))
				     ((termp arg)
				      (if (= (arity arg) 0)
					  `((put-constant ,(car arg) ,A))
					  `((put-structure ,(cons (car arg) (arity arg)) ,A)
					    ,@(compile-body-struct-args (cdr arg) body-num))))))
			     (cdr term))))
		 (compile-body-struct-args (struct-args body-num)
		   (let (prep-code)
		     (nconc prep-code
			    (mapcan (lambda (arg)
				      (cond ((anonymousvar-p arg) (list (list 'set-void 1)))
					    ((variablep arg)
					     (let ((vardata (cdr (assoc arg assign-table))))
					       (list
						(case (car vardata)
						  (temporary
						   (cond ((globalized-p arg) `(set-value-temporary ,(cdr vardata)))
							 ((initialized-p arg) (rplacd (assoc arg varstate-table) 'globalized)
							  `(set-value-temporary ,(cdr vardata)))
							 (t (rplacd (assoc arg varstate-table) 'globalized)
							    `(set-variable-temporary ,(cdr vardata)))))
						  (permanent
						   (cond ((globalized-p arg) `(set-value-permanent ,(cdr vardata)))
							 ((initialized-p arg) (rplacd (assoc arg varstate-table) 'globalized)
							  `(set-value-permanent ,(cdr vardata)))
							 (t (rplacd (assoc arg varstate-table) 'globalized)
							    `(set-variable-permanent ,(cdr vardata)))))))))
					    ((listterm-p arg)
					     (let ((tempvar (incf (nth body-num register-next))))
					       (prog1
						   `((set-value-temporary ,tempvar))
						 (setq prep-code
						       (append prep-code
							       `((put-list ,tempvar)
								 ,@(compile-head-struct-args (cdr arg))))))))
					    ((termp arg)
					     (let ((tempvar (incf (nth body-num register-next))))
					       (if (= (arity arg) 0)
						   `((set-constant ,(car arg)))
						   (prog1
						       `((set-value-temporary ,tempvar))
						     (setq prep-code
							   (append prep-code
								   `((put-structure ,(cons (car arg) (arity arg)) ,tempvar)
								     ,@(compile-head-struct-args (cdr arg)))))))))))
				    struct-args)))))
	  (let ((have-permanent (some #'plusp remain-list)) (body-num -1)
		(deallocate-emitted nil))
	    (cons-when have-permanent
		       (list 'allocate) (append (compile-head-term head)
						(mapcan (lambda (b remain)
							  (incf body-num)
							  (append (compile-body-term b body-num)
								  (cons-when (and have-permanent (>= 0 remain) (not deallocate-emitted))
									     (prog1 (list 'deallocate) (setf deallocate-emitted t))
									     (list
									      (if (= -1 remain)
										  `(execute ,(cons (car b) (arity b)))
										  `(call ,(cons (car b) (arity b)) ,remain))))))
							       body remain-list)))))))))



#|
---address representation---
register: 1,2,3,...
stack: -1,-3,-5,...
heap: -2,-4,-6,...
|#

(defconstant *bottom-of-stack* -1)
(defconstant *bottom-of-heap* -2)

(deftype stack-address ()
  '(and (satisfies minusp) (satisfies oddp)))

(deftype heap-address ()
  '(and (satisfies minusp) (satisfies evenp)))

(deftype register-number ()
  '(and (integer 1 *)))

(defun store (addr)
  (declare (special stack-area heap-area register-area))
  (typecase addr
    (register-number (aref register-area addr))
    (stack-address (aref stack-area (- addr)))
    (heap-address (aref heap-area (- (floor addr 2))))))

(defun (setf store) (new-val addr)
  (declare (special stack-area heap-area register-area))
  (typecase addr
    (register-number (setf (aref register-area addr) new-val))
    (stack-address (setf (aref stack-area (- addr)) new-val))
    (heap-address (setf (aref heap-area (- (floor addr 2))) new-val))))

(defun heap (addr)
  (declare (special heap-area))
  (aref heap-area (- (floor addr 2))))

(defun (setf heap) (new-val addr)
  (declare (special heap-area))
  (setf (aref heap-area (- addr)) new-val))

(defun stack (addr)
  (declare (special stack-area))
  (aref stack-area (- addr)))	

(defun (setf stack) (new-val addr)
  (declare (special stack-area))
  (setf (aref stack-area (- addr)) new-val))

(defun register (num)
  (declare (special register-area))
  (aref register-area num))	

(defun (setf register) (new-val num)
  (declare (special register-area))
  (setf (aref register-area num) new-val))

(defun trail (addr)
  (declare (special trail-area))
  (aref trail-area addr))	

(defun (setf trail) (new-val addr)
  (declare (special trail-area))
  (setf (aref trail-area addr) new-val))

(defun stackvar (y)
  (declare (special stack-area E))
  (aref stack-area (- (addr+ E y 1))))	

(defun (setf stackvar) (new-val y)
  (declare (special stack-area E))
  (setf (aref stack-area (- (addr+ E y 1))) new-val))


(defun make-machine ()
  (let* ((register-area (make-array 10 :adjustable t)) (heap-area (make-array 30 :adjustable t))
	 (stack-area (make-array 50 :adjustable t)) (trail-area (make-array 20 :adjustable t)) (pdl)
	 (P) (CP) (S) (HB) (H) (B0) (B) (E) (TR) (fail) (num-of-args))
    (lambda (query-code)
      (setq P query-code)
      (macrolet ((backtrack-or-continue ()
		   '(if fail (backtrack) (incf P))))
	(labels ((addr+ (target &rest nums)
		   (+ target (* -2 (apply #'+ nums))))
		 (addr< (addr1 addr2)
		   (cond ((and (typep addr1 'stack-address) (typep addr2 'heap-address)) nil)
			 ((and (typep addr1 'heap-address) (typep addr1 'stack-address)) t)
			 (t (> addr1 addr2))))
		 (dereference (a)
		   (destructuring-bind (tag . value) (store a)
		     (if (and (eq tag 'ref) (/= value a))
			 (dereference value)
			 a)))
		 (backtrack ()
		   (if (eq B *bottom-of-stack*)
		       (print "no.")
		       (progn (setf B0 (stack (addr+ B (stack B) 7)))
			      (setf P (stack (addr+ B (stack B) 4))))))
		 (bind (a1 a2)
		   (let ((t1 (car (store a1))) (t2 (car (store a2))))
		     (if (and (eq t1 'ref) (or (not (eq t2 'ref)) (addr< a2 a1)))
			 (progn (setf (store a1) (store a2))
				(trail a1))
			 (progn (setf (store a2) (store a1))
				(trail a2)))))
		 
		 (trail (a)
		   (if (or (addr< a HB) (and (addr< H a) (addr< a B)))
		       (progn (setf (trail TR) a)
			      (incf TR))))
		 
		 (unwind-trail (a1 a2)
		   (loop for i from a1 to (1- a2) do
			(setf (store (trail i)) (cons 'ref (trail i)))))
		 (unify (a1 a2)
		   (declare (special fail))
		   (let ((pdl nil))
		     (push a1 pdl) (push a2 pdl)
		     (setq fail nil)
		     (loop while (not (null pdl) fail) do
			  (let ((d1 (dereference (pop pdl))) (d2 (dereference (pop pdl))))
			    (if (/= d1 d2)
				(progn
				  (destructuring-bind ((t1 v1) (t2 v2)) (list (store d1) (store d2))
				    (if (eq t1 'ref)
					(bind d1 d2)
					(case t2
					  (ref (bind d1 d2))
					  (con (setq fail (or (not (eq t1 'con)) (not (eq v1 v2)))))
					  (lis (if (not (eq t1 'lis))
						   (setq fail t)
						   (progn
						     (push v1 pdl)
						     (push v2 pdl)
						     (push (addr+ v1 1) pdl)
						     (push (addr+ v2 1) pdl))))
					  (struct (if (not (eq t1 'struct))
						      (setq fail t)
						      (progn
							(destructuring-bind ((f1 n1) (f2 n2)) (list (store v1) (store v2))
							  (if (or (not (eq f1 f2)) (/= n1 n2))
							      (setq fail t)
							      (loop for i from 1 to n1 do
								   (push (addr+ v1 i) pdl)
								   (push (addr+ v2 i) pdl))))))))))))))))
		 (run-code (code)
		   (let ((inst (car code)))
		     (case (car inst)
		       (put-variable-temporary (let ((x (cadr inst)) (a (caddr inst)))
						 (setf (heap H) (cons 'ref H))
						 (setf (register x) (heap H))
						 (setf (register a) (heap H))
						 (setf H (addr+ H 1))
						 (incf P)))
		       (put-variable-permanent (let ((y (cadr inst)) (a (caddr inst)))
						 (setf (stackvar y) (cons 'ref addr))
						 (setf (register a) (stackvar y))
						 (incf P)))
		       (put-value-temporary (let ((x (cadr inst)) (a (caddr inst)))
					      (setf (register a) (register x))
					      (incf P)))
		       (put-value-permanent (let ((y (cadr inst)) (a (caddr inst)))
					      (setf (register a) (stackvar y))
					      (incf P)))
		       (put-unsafe-value (let* ((y (cadr inst)) (a (caddr inst))
						(addr (dereference (addr+ E y 1))))
					   (if (addr< addr E)
					       (setf (register a) (store addr))
					       (progn
						 (setf (heap H) (cons 'ref H))
						 (bind addr H)
						 (setf (register a) (heap H))
						 (setf H (addr+ H 1))))
					   (incf P)))
		       (put-structure (let ((f (cadr inst)) (a (caddr inst)))
					(setf (heap H) (cons 'functor f))
					(setf (register a) (cons 'struct H))
					(setf H (addr+ H 1))
					(incf P)))
		       (put-list (let ((a (cadr inst)))
				   (setf (register a) (cons 'lis H))
				   (incf P)))
		       (put-constant (let ((c (cadr inst)) (a (caddr inst)))
				       (setf (register a) (cons 'con c))
				       (incf P)))
		       (set-variable-temporary (let ((x (cadr inst)))
						 (setf (heap H) (cons 'ref H))
						 (setf (register x) (heap H))
						 (setf H (addr+ H 1))
						 (incf P)))
		       (set-variable-permanent (let ((y (cadr inst)))
						 (setf (heap H) (cons 'ref H))
						 (setf (stackvar y) (heap H))
						 (setf H (addr+ H 1))
						 (incf P)))
		       (set-value-temporary (let ((x (cadr inst)))
					      (setf (heap H) (register x))
					      (setf H (addr+ H 1))
					      (incf P)))
		       (set-value-permanent (let ((y (cadr inst)))
					      (setf (heap H) (stack (addr+ E y 1)))
					      (setf H (addr+ H 1))
					      (incf P)))
		       (set-local-value-temporary (let* ((x (cadr inst))
							 (addr (dereference (register x))))
						    (if (addr< addr H)
							(setf (heap H) (heap addr))
							(progn
							  (setf (heap h) (cons 'ref H))
							  (bind addr H)))
						    (setf H (addr+ H 1))
						    (incf P)))
		       (set-local-value-permanent (let* ((y (cadr inst))
							 (addr (dereference (stackvar y))))
						    (if (addr< addr H)
							(setf (heap H) (heap addr))
							(progn
							  (setf (heap h) (cons 'ref H))
							  (bind addr H)))
						    (setf H (addr+ H 1))
						    (incf P)))
		       (set-constant (let ((c (cadr inst)))
				       (setf (heap H) (cons 'con c))
				       (setf H (addr+ H 1))
				       (incf P)))
		       (set-void (let ((n (cadr inst)))
				   (dotimes (i n)
				     (setf (heap (addr+ H i) (cons 'ref (addr+ H i)))))
				   (setf H (addr+ H n))
				   (incf P)))
		       (get-variable-temporary (let ((x (cadr inst)) (a (caddr inst)))
						 (setf (register x) (register a))
						 (incf P)))
		       (get-variable-permanent (let ((y (cadr inst)) (a (caddr inst)))
						 (setf (stackvar y) (register a))
						 (incf P)))
		       (get-value-temporary (let ((x (cadr inst)) (a (caddr inst)))
					      (unify x a)
					      (backtrack-or-continue)))
		       (get-value-permanent (let ((y (cadr inst)) (a (caddr inst)))
					      (unify (addr+ E y 1) a)
					      (backtrack-or-continue)))
		       (get-structure (let* ((f (cadr inst)) (a (caddr inst))
					     (addr (dereference a)))
					(case (car (car (store addr)))
					  (ref (setf (heap h) (cons 'struct (addr+ H 1)))
					       (setf (heap (addr+ h 1)) f)
					       (bind addr H)
					       (setf H (addr+ H 2))
					       (setq mode 'write))
					  (struct (let ((struct-addr (cdr (store addr))))
						    (if (eq (heap struct-addr) f)
							(progn
							  (setf S (1+ struct-addr))
							  (setq mode 'read))
							(setq fail t)))
						  (t (setq fail t)))
					  (backtrack-or-continue))))
		       (get-list (let* ((a (cadr inst))
					(addr (dereference a)))
				   (case (car (store addr))
				     (ref (setf (heap H) (cons 'lis (addr+ H 1)))
					  (bind addr H)
					  (setq mode 'write))
				     (lis (let ((list-addr (cdr (store addr))))
					    (setf S list-addr)
					    (setq mode 'read)))
				     (t (setq fail t)))
				   (backtrack-or-continue)))
		       (get-constant (let* ((c (cadr inst)) (a (caddr inst))
					    (addr (dereference a)))
				       (case (car (store addr))
					 (ref (setf (store addr) (cons 'con c))
					      (trail addr))
					 (con (let ((c2 (cdr (store addr))))
						(if (not (eq c c2))
						    (setq fail t))))
					 (t (setq fail t)))
				       (backtrack-or-continue)))
		       (unify-variable-temporary (let ((x (cadr inst)))
						   (case mode
						     (read (setf (register x) (heap S)))
						     (write
						      (setf (heap H) (cons 'ref H))
						      (setf (register x) (heap H))))
						   (setf S (addr+ S 1))
						   (incf P)))
		       (unify-variable-permanent (let ((y (cadr inst)))
						   (case mode
						     (read (setf (stackvar y) (heap S)))
						     (write
						      (setf (heap H) (cons 'ref H))
						      (setf (stackvar y) (heap H))))
						   (setf S (addr+ S 1))
						   (incf P)))
		       (unify-value-temporary (let ((x (cadr inst)))
						(case mode
						  (read (unify x S))
						  (write (setf (heap H) (register x))
							 (setf H (addr+ H 1))))
						(setf S (addr+ S 1))
						(backtrack-or-continue)))
		       (unify-value-permanent (let ((y (cadr inst)))
						(case mode
						  (read (unify (addr+ E y 1) S))
						  (write (setf (heap H) (stackvar y))
							 (setf H (addr+ H 1))))
						(setf S (addr+ S 1))
						(backtrack-or-continue)))
		       (unify-local-value-temporary (let ((x (cadr inst)))
						      (case mode
							(read (unify x S))
							(write (let ((addr (dereference x)))
								 (if (addr< addr H)
								     (setf (heap H) (heap addr))
								     (progn
								       (setf (heap H) (cons 'ref H))
								       (bind addr H)
								       (setf (register x) (heap H))))
								 (setf H (addr+ H 1)))))
						      (setf S (addr+ S 1))
						      (backtrack-or-continue)))
		       (unify-local-value-permanent (let ((y (cadr inst)))
						      (case mode
							(read (unify (addr+ E y 1) S))
							(write (let ((addr (dereference (addr+ E y 1))))
								 (if (addr< addr H)
								     (setf (heap H) (heap addr))
								     (progn
								       (setf (heap H) (cons 'ref H))
								       (bind addr H)
								       (setf (stackvar y) (heap H))))
								 (setf H (addr+ H 1)))))
						      (setf S (addr+ S 1))
						      (backtrack-or-continue)))
		       (unify-constant (let ((c (cadr inst)))
					 (case mode
					   (read (let ((addr (dereference S)))
						   (case (car (store addr))
						     (ref (setf (store addr) (cons 'con c)))
						     (con (if (not (eq (cdr (store addr) c)))
							      (setq fail t)))
						     (t (setq fail t)))))
					   (write (setf (heap H) (cons 'con c))
						  (setf H (addr+ H 1))))
					 (backtrack-or-continue)))
		       (unify-void (let ((n (cadr inst)))
				     (case mode
				       (read (setf S (addr+ S n)))
				       (write (dotimes (i n)
						(setf (heap (addr+ H i) (cons 'ref (addr+ H i)))))
					      (setf H (addr+ H n))))
				     (incf P)))
		       (allocate
			(let ((new-E (if (addr< B E)
					 (addr+ E (code (addr+ (stack (addr+ E 1)) -1)  2))
					 (addr+ B (stack B) 8))))
			  (setf (stack new-E) E)
			  (setf (stack (addr+ new-E 1)) CP)
			  (setf E new-E)
			  (incf P)))
		       (deallocate
			(setf CP (stack (addr+ E 1)))
			(setf E (stack E))
			(setf incf P))
		       (call (let ((p (cadr inst)) (n (caddr inst)))
			       (if (defined p)
				   (progn (setf CP (1+ P))
					  (setf num-of-args (arity p))
					  (setf B0 B)
					  (setf P (code-address p)))
				   (backtrack))))
		       (execute (let ((p (cadr inst)))
				  (if (defined p)
				      (progn (setf num-of-args (arity p))
					     (setf B0 B)
					     (setf P (code p)))
				      (backtrack))))
		       (proceed
			(setf P CP))
		       (try-me-else (let* ((l (cadr inst))
					   (new-B (if (addr< B E)
						      (addr+ E (code (addr+ (stack (addr+ E 1)) -1)  2))
						      (addr+ B (stack B) 8)))
					   (n (stack new-B)))
				      (setf (stack new-B) num-of-args)
				      (dotimes (i n)
					(setf (stack (addr+ new-B (1+ i)) (register (1+ i)))))
				      (setf (stack (addr+ new-B n 1)) E)
				      (setf (stack (addr+ new-B n 2)) CP)
				      (setf (stack (addr+ new-B n 3)) B)
				      (setf (stack (addr+ new-B n 4)) l)
				      (setf (stack (addr+ new-B n 5)) TR)
				      (setf (stack (addr+ new-B n 6)) H)
				      (setf (stack (addr+ new-B n 7)) B0)
				      (setf B new-B)
				      (setf HB H)
				      (incf P)))
		       (retry-me-else (let ((l (cadr inst))
					    (n (stack B)))
					(dotimes (i n)
					  (setf (register (1+ i)) (stack (addr+ B (1+ i)))))
					(setf E (stack (addr+ B n 1)))
					(setf CP (stack (addr+ B n 2)))
					(setf (stack (addr+ B n 4)) l)
					(unwind-trail (stack (addr+ B n 5)) TR)
					(setf TR (stack (addr+ B n 5)))
					(setf H (stack (addr+ B n 6)))
					(setf HB H)
					(incf P)))
		       (trust-me (let ((n (stack B)))
				   (dotimes (i n)
				     (setf (register (1+ i)) (stack (addr+ B (1+ i)))))
				   (setf E (stack (addr+ B n 1)))
				   (setf CP (addr+ B n 2))
				   (unwind-trail (stack (addr+ B n 5)) TR)
				   (setf TR (stack (addr+ B n 5)))
				   (setf H (stack (addr+ B n 6)))
				   (setf B (stack (addr+ B n 3)))
				   (setf HB (stack (addr+ B n 6)))
				   (incf P)))
		       (try (let* ((l (cadr inst))
				  (new-B (if (addr< B E)
					     (addr+ E (code (addr+ (stack (addr+ E 1)) -1)) 2)
					     (addr+ B (stack B) 8)))
				  (n (stack new-B)))
			      (setf (stack new-B) num-of-args)
			      (dotimes (i n)
				(setf (stack (addr+ new-B (1+ i))) (register (1+ i))))
			      (setf (stack (addr+ new-B n 1)) E)
			      (setf (stack (addr+ new-B n 2)) CP)
			      (setf (stack (addr+ new-B n 3)) B)
			      (setf (stack (addr+ new-B n 4)) (1+ P))
			      (setf (stack (addr+ new-B n 5)) TR)
			      (setf (stack (addr+ new-B n 6)) H)
			      (setf (stack (addr+ new-B n 7)) B0)
			      (setf B new-B)
			      (setf HB H)
			      (setf P l)))
		       (retry (let ((l (cadr inst))
				    (n (stack B)))
				(dotimes (i n)
				  (setf (register (1+ i)) (stack (addr+ B (1+ i)))))
				(setf E (stack (addr+ B n 1)))
				(setf CP (stack (addr+ B n 2)))
				(setf (stack (addr+ B n 4)) (1+ P))
				(unwind-trail (stack (addr+ B n 5)) TR)
				(setf TR (stack (addr+ B n 5)))
				(setf H (stack (addr+ B n 6)))
				(setf HB H)
				(setf P l)))
		       (trust (let ((l (cadr inst))
				    (n (stack B)))
				(dotimes (i n)
				  (setf (register (1+ i)) (stack (addr+ B (1+ i)))))
				(setf E (stack (addr+ B n 1)))
				(setf CP (stack (addr+ B n 2)))
				(unwind-trail (stack (addr+ B n 5)) TR)
				(setf TR (stack (addr+ B n 5)))
				(setf H (stack (addr+ B n 6)))
				(setf B (stack (addr+ B n 3)))
				(setf HB (stack (addr+ B n 6)))
				(setf P l)))
		       (switch-on-term (destructuring-bind (v c l s) (cdr inst)
					 (case (car (store (dereference (register 1))))
					   (ref (setf P v))
					   (con (setf P c))
					   (lis (setf P l))
					   (struct (setf P s)))))
		       (switch-on-constant (let* ((alist (cadr inst))
						  (val (cdr (store (dereference (register 1)))))
						  (result (assoc val alist)))
					     (if result
						 (setf P (cdr result))
						 (backtrack))))
		       (switch-on-structure (let ((alist (cadr inst))
						  (val (cdr (store (dereference (register 1)))))
						  (result (assoc val alist)))
					      (if result
						  (setf P (cdr result))
						  (backtrack))))
		       (neck-cut (error "not implemented"))
		       (get-level (let ((y (cadr inst)))
				    (error "not implemented")))
		       (cut (let ((y (cadr inst)))
			      (error "not implemented")))
		       (otherwise (error "unknown instruction!")))))))))))
		       
