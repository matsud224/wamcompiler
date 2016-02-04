;(defun main ()
;  (format t "hello,world~%"))
;
;(sb-ext:save-lisp-and-die "plcompiler"
;			  :toplevel #'main
					;			  :executable t)

(defvar *operator-list* nil)
(defvar *ignore-comma* nil)

(defmacro dostream (bind &body body)
  `(loop (let ((,(car bind) (read-char ,(cadr bind))))
	   (if (null ,(car bind)) (return nil)) ,@body)))

(defvar *non-alphanum-chars* '(#\# #\$ #\& #\* #\+ #\- #\. #\/ #\: #\< #\= #\> #\? #\@ #\^ #\~ #\\))

(defun get-token (s)
  (skip-whitespace s)
  (let ((c (read-char s)))
    (unread-char c s)
    (cond ((null c) nil)
	  ((eq c #\%) (skip-comment s) (get-token s))
	  ((eq c #\)) (read-char s) '(rparen))
	  ((eq c #\() (read-char s) '(lparen))
	  ((eq c #\,) (read-char s) (cons 'atom '|,|))
	  ((digit-char-p c) (cons 'int (read-int s)))
	  ((eq c #\-) (read-char s)
	   (cons 'int (- (read-int s))))
	  ((upper-case-p c) (cons 'variable (read-alphanum-atom s)))
	  ((or (alpha-char-p c) (eq #\_ c)) (cons 'atom (read-alphanum-atom s)))
	  ((member c *non-alphanum-chars*) (read-non-alphanum-atom s))
	  (t (cons 'atom (read-alphanum-atom s))))))

(defun skip-whitespace (s)
  (dostream (c s)
    (if (not
	 (or (not (graphic-char-p c)) (eq c #\Space)))
	(progn (unread-char c s) (return nil)))))

(defun skip-comment (s)
  (dostream (c s)
	    (if (not (eq c #\Newline)) (return nil))
					;pass
	    ))

(defun read-int (s)
  (let ((acc))
    (dostream (c s)
	      (if (not (digit-char-p c)) (progn (unread-char c s) (return nil)))
	      (setq acc (cons c acc)))
    (parse-integer (concatenate 'string (reverse acc)))))
  
(defun read-alphanum-atom (s)
  (let ((acc))
    (dostream (c s)
	      (if (not (or (alphanumericp c) (eq #\_ c)))
		  (progn (unread-char c s) (return nil)))
	      (setq acc (cons c acc)))
    (intern (concatenate 'string (reverse acc)))))

(defun read-non-alphanum-atom (s)
  (let ((acc))
    (dostream (c s)
	      (if (not (member c *non-alphanum-chars*))
		  (progn (unread-char c s) (return nil)))
	      (setq acc (cons c acc)))
    (let ((str (concatenate 'string (reverse acc))))
      (cond ((equal str ".") '(dot))
	    (t (cons 'atom (intern str)))))))

(defmacro register-operators (oplist)
  (labels ((op< (x y)
	     (if (eq (op-prec x) (op-prec y))
		 (string< (symbol-name (op-assoc x)) (symbol-name (op-assoc y)))
		 (> (op-prec x) (op-prec y))))
	   (insert-op-sorted (head x)
	     (cond ((null head) (list x))
		   ((op< (car head) x) (cons (car head) (insert-op-sorted (cdr head) x)))
		   (t (cons x head)))))
    (setq *operator-list* nil)
    (dolist (op oplist)
      (let ((a (op-atom op)))
	(setf (get a 'op) t)
	(setq *operator-list* (insert-op-sorted *operator-list* op))))))
    

(register-operators
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

(defmacro op-atom (x) `(car ,x))
(defmacro op-prec (x) `(cadr ,x))
(defmacro op-assoc (x) `(caddr ,x))

(defun commap (x)
  (and (eq (car x) 'atom) (eq (cdr x) '|,|)))

(defun make-clause (tag pairs)
  (let ((result))
    (dolist (p pairs)
      (if (eq (car p) t)
	  (push `(t ,@(cdr p)) result)
	  (push `((eq ,tag ,(car p)) ,@(cdr p)) result))
    result)))

(defmacro cond-tag (op cons &rest pairs)
  (let ((tag (gensym)))
    `(let ((,tag (,op ,cons)))
       (cond ,@(make-clause tag pairs)))))

(defun parse (stream)
  (let ((tokenstack)) ;;tokenstackにはungetしたトークンを入れる
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
	     (parse-arguments ()
	       (let ((next (next-token)) (*ignore-comma* t))
		 (if (eq (car next) 'lparen)
		     (labels ((get-args (acc)
				(let ((arg (parse-sub *operator-list*))
				      (n (next-token)))
				  (cond ((eq (car n) 'rparen) (cons arg acc))
					((commap n)
					 (get-args (cons arg acc)))
					(t (error "Syntax error! (near argument)"))))))
		       (reverse (get-args nil)))
		     (progn (unget-token next) nil))))
	     (parse-prim ()
	       (let ((next (next-token)))
		 (cond ((eq (car next) 'lparen)
			(let ((inside (parse-sub *operator-list*)))
			  (if (eq (car (next-token)) 'rparen)
			      inside
			      (error "Syntax error! (閉じ括弧がありません)"))))
		       ((eq (car next) 'atom)
			(let ((cdr-part (cdr next)))
			  (if nil;(get cdr-part 'op)
			      (error "Syntax error! (演算子の使い方が正しくありません)")
			      `(struct ,cdr-part ,(parse-arguments)))))
		       (t  next))))
	     (parse-sub (head)
	       (block exit
		 (if (null head) (return-from exit (parse-prim)))
		 (let ((current-prec (op-prec (car head)))
		       (current-head head)
		       (next-head (next-prec-head head)))
		   (do ((op (car head) (progn (setq head (cdr head)) (car head))))
		       ((or (null op) (not (eq current-prec (op-prec op)))
			    (not (member (op-assoc op) '(fx fy)))))
		     (let ((gottok (next-token)))
		       (cond ((eq (car gottok) 'dot) (unget-token gottok) (return))
			     ((and (eq (car gottok) 'atom) (eq (cdr gottok) (op-atom op))
				   (let ((n (next-token)))
				     (if (and (not (commap n)) (not (member (car n) '(rparen dot))))
					 (progn (unget-token n) t) (progn (unget-token n) nil))))
			      (cond ((eq (op-assoc op) 'fx)
				     (return-from exit `(struct ,(op-atom op) ,(parse-sub next-head))))
				    ((eq (op-assoc op) 'fy)
				     (return-from exit `(struct ,(op-atom op) ,(parse-sub current-head))))))
			     (t (unget-token gottok)))))
		   (let ((operand-1 (parse-sub next-head)))
		     (do ((op (car head) (progn (setq head (cdr head)) (car head))))
			 ((or (null op) (not (eq current-prec (op-prec op)))))
		       (let ((gottok (next-token)))
			 (cond ((eq (car gottok) 'dot) (unget-token gottok) (return))
			       ((and *ignore-comma* (commap gottok)) (unget-token gottok) (return))
			       ((and (eq (car gottok) 'atom) (eq (cdr gottok) (op-atom op)))
				(cond ((eq (op-assoc op) 'xf)
				       (return-from exit `(struct ,(op-atom op) ,operand-1)))
				      ((eq (op-assoc op) 'yf)
				       (unget-token `(struct ,(op-atom op) ,operand-1))
				       (return-from exit (parse-sub current-head)))
				      ((eq (op-assoc op) 'xfy)
				       (return-from exit `(struct ,(op-atom op) ,(list operand-1 (parse-sub current-head)))))
				      ((eq (op-assoc op) 'xfx)
				       (return-from exit `(struct ,(op-atom op) ,(list operand-1 (parse-sub next-head)))))
				      ((eq (op-assoc op) 'yfx)
				       (unget-token `(struct ,(op-atom op) ,(list operand-1 (parse-sub next-head))))
				       (return-from exit (parse-sub current-head)))))
				(t (unget-token gottok)))))
		     (return-from exit operand-1))))))
      (let ((result (parse-sub *operator-list*)))
	(if (not (eq (car (next-token)) 'dot)) (error "Syntax error!") result)))))
     
