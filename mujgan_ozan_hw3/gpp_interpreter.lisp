(load "gpp_lexer.lisp")

;; Global parser state
(defvar *current-token-index* 0)
(defvar *tokens* '())
(defvar *values* '())
(defvar *identifiers* '())

;; Ortam (environment) ve fonksiyon tabloları:
(defvar *variables* (make-hash-table :test 'equal))
(defvar *functions* (make-hash-table :test 'equal))

(define-condition syntax-error (error)
  ((message :initarg :message :initform "Syntax error occurred." :accessor syntax-error-message)))

(defun check-function-syntax (tokens)
  "Check if tokens represent either a function call or a function definition."
  (cond
    ;; Function Definition
    ((and (not (null tokens))
          (string= (car tokens) "OP_OP")
          (string= (cadr tokens) "KW_DEFFUN"))
     (if (and (> (length tokens) 5)
              (string= (car (last tokens)) "OP_CP"))
         "Function definition syntax is valid."
       "Syntax Error: Invalid function definition format. (deffun IDENTIFIER IDLIST EXPLISTI) bekleniyor."))
    
    ;; Function Call
    ((and (not (null tokens))
          (string= (car tokens) "OP_OP")
          (string= (cadr tokens) "IDENTIFIER")
          (string= (car (last tokens)) "OP_CP"))
     "Function call syntax is valid.")
    
    (t "Not a function call or definition.")))


(defun gppinterpreter ()
  "READ-EVAL-PRINT loop"
  (loop
   (format t "~%>>> ")
   (let ((line (read-line *standard-input* nil)))
     (if (or (null line) (string= "exit" line))
         (return)
       (progn
         (getTokenList line)
         (format t "Tokens: ~a~%" tokenList)
         (format t "Values: ~a~%" valueList)
         (format t "Identifiers: ~a~%" identifierList)

         (let ((syntax-check (check-function-syntax tokenList)))
           (format t "~a~%" syntax-check))

         (setq *tokens* tokenList)
         (setq *values* valueList)
         (setq *identifiers* identifierList)
         (setq *current-token-index* 0)

         (handler-case
             (let ((ast (parse-start)))
               (if ast
                   (progn
                     (format t "Grammar is correct.~%")
                     (let ((result (evaluate-explist ast)))
                       (format t "RESULT: ~a~%" result)))
                 (format t "No expressions found.~%")))
           (syntax-error (e)
             (format t "Syntax Error: ~a~%" (syntax-error-message e))
             (format t "RESULT: NIL~%"))))))))

;;-------------------------------------
;; Parser Fonksiyonları
;;-------------------------------------

(defun next-token ()
  (if (< *current-token-index* (length *tokens*))
      (elt *tokens* *current-token-index*)
      nil))

(defun advance-token ()
  (incf *current-token-index*))

(defun match (token-type)
  "Eğer sıradaki token token-type ise tüketir ve t döner."
  (let ((tk (next-token)))
    (if (string= tk token-type)
        (progn (advance-token) t)
      nil)))

(defun match-keyword (kw)
  (let ((nt (next-token)))
    (if (string= nt kw)
        (progn (advance-token) t)
      nil)))

(defun parse-start ()
  "Start rule: Parse an EXPLIST"
  (parse-explist))

(defun parse-explist ()
  "EXPLIST -> $EXP $EXPLIST | $EXP"
  (let ((first (parse-exp)))
    (if first
        (let ((rest (parse-explist)))
          (if rest
              (cons first rest)
            (list first)))
      nil))) 

(defun parse-exp ()
  "EXP -> (OPERATOR EXP EXP) | (if EXPB EXPLIST) | (if EXPB EXPLIST EXPLIST)
       | (while (EXPB) EXPLIST) | (for (Id EXP EXP) EXPLIST)
       | (set IDENTIFIER EXPLIST)
       | IDENTIFIER | VALUEF | FCALL"
  (let ((tk (next-token)))
    (cond
     ((string= tk "OP_OP")
      (advance-token) ;; '('
      (parse-paren-expression))

     ((string= tk "IDENTIFIER")
      (advance-token)
      (let ((id (car *identifiers*)))
        (setq *identifiers* (cdr *identifiers*))
        id))

     ((string= tk "VALUEF")
      (advance-token)
      (let ((val (car *values*)))
        (setq *values* (cdr *values*))
        val))

     (t nil))))

(defun parse-paren-expression ()
  "Parantezli ifadeleri parse eder."
  (let ((nt (next-token)))
    (cond
     ;; Aritmetik Operatörler
     ((or (string= nt "OP_PLUS") (string= nt "OP_MINUS")
          (string= nt "OP_MULT") (string= nt "OP_DIV"))
      (advance-token)
      (let ((expr1 (parse-exp))
            (expr2 (parse-exp)))
        (if (and expr1 expr2 (match "OP_CP"))
            (list (intern nt) expr1 expr2)
          (signal 'syntax-error :message "Aritmetik ifade hatası"))))

     ;; set ifadesi: (set IDENTIFIER EXPLIST)
     ((match-keyword "KW_SET")
      (let ((id (parse-identifier))
            (exps (parse-list-of-exps))) 
        (if (match "OP_CP")
            (list 'SET id exps)
          (signal 'syntax-error :message "Eksik ) set ifadesinde"))))

     ;; defvar
     ((match-keyword "KW_DEFVAR")
      (let ((id (parse-identifier))
            (val (parse-exp)))
        (if (match "OP_CP")
            (list 'DEFVAR id val)
          (signal 'syntax-error :message "Eksik ) defvar ifadesinde"))))

     ;; deffun
     ((match-keyword "KW_DEFFUN")
      (let ((fn-name (parse-identifier))
            (params (parse-idlist))
            (body (parse-explisti)))
        (if (match "OP_CP")
            (list 'DEFFUN fn-name params body)
          (signal 'syntax-error :message "Eksik ) deffun ifadesinde"))))

     ;; if ifadesi
     ((match-keyword "KW_IF")
      (let ((cond-exp (parse-expb))
            (then-part (parse-list-of-exps)))
        (let ((tk (next-token)))
          (if (string= tk "OP_CP")
              (progn (advance-token)
                     (list 'IF cond-exp then-part))
            (let ((else-part (parse-list-of-exps)))
              (if (match "OP_CP")
                  (list 'IF cond-exp then-part else-part)
                (signal 'syntax-error :message "Eksik ) if ifadesinde")))))))

     ;; while ifadesi: (while (EXPB) EXPLIST)
     ((match-keyword "KW_WHILE")
      (if (match "OP_OP")
          (let ((cond-exp (parse-expb)))
            (if (match "OP_CP")
                (let ((body (parse-list-of-exps)))
                  (if (match "OP_CP")
                      (list 'WHILE cond-exp body)
                    (signal 'syntax-error :message "Eksik ) while ifadesinde")))
              (signal 'syntax-error :message "Eksik ) while condition")))
        (signal 'syntax-error :message "While ifadesinde '(' bekleniyor")))

     ;; for ifadesi: (for (Id EXP EXP) EXPLIST)
     ((match-keyword "KW_FOR")
      (if (match "OP_OP")
          (let ((id (parse-identifier))
                (start-val (parse-exp))
                (end-val (parse-exp)))
            (if (match "OP_CP")
                (let ((body (parse-list-of-exps)))
                  (if (match "OP_CP")
                      (list 'FOR id start-val end-val body)
                    (signal 'syntax-error :message "Eksik ) for ifadesinde")))
              (signal 'syntax-error :message "Eksik ) for döngüsünde")))
        (signal 'syntax-error :message "For ifadesinde '(' bekleniyor")))

     ;; Fonksiyon Çağrısı: (IDENTIFIER EXPLIST)
     ((string= nt "IDENTIFIER")
      (advance-token)
      (let ((fn-name (parse-identifier))
            (args (parse-list-of-exps)))
        (if (match "OP_CP")
            (list 'FCALL fn-name args)
          (signal 'syntax-error :message "Fonksiyon çağrısında ) bekleniyor"))))

     (t (signal 'syntax-error :message "Bilinmeyen parantezli ifade")))))

(defun parse-list-of-exps ()
  "EXPLIST -> '(' {EXP} ')'"
  (if (match "OP_OP")
      (let ((exps '()))
        (loop for expr = (parse-exp) then (parse-exp)
              while expr do (push expr exps))
        (if (match "OP_CP")
            (reverse exps)
          (signal 'syntax-error :message "Eksik ) EXPLIST içinde")))
    (signal 'syntax-error :message "EXPLIST için '(' bekleniyor")))

(defun parse-identifier ()
  (let ((tk (next-token)))
    (if (string= tk "IDENTIFIER")
        (progn
          (advance-token)
          (let ((id (car *identifiers*)))
            (setq *identifiers* (cdr *identifiers*))
            id))
      (signal 'syntax-error :message "Identifier bekleniyor"))))

(defun parse-idlist ()
  "IDLIST -> '(' {IDENTIFIER} ')'"
  (if (match "OP_OP")
      (let ((ids '()))
        (loop while (string= (next-token) "IDENTIFIER")
              do (push (parse-identifier) ids))
        (if (match "OP_CP")
            (reverse ids)
          (signal 'syntax-error :message "IDLIST için ')' eksik")))
    (signal 'syntax-error :message "IDLIST için '(' bekleniyor")))

(defun parse-explisti ()
  "EXPLISTI -> $EXP $EXPLISTI | $EXP | ε
   Burada EXPLISTI parantezsiz bir şekilde birden fazla EXP parse etmeye yarar.
   Örneğin DEFFUN gövdesinde birden çok EXP olabilir."
  (let ((expr (parse-exp)))
    (if expr
        (cons expr (parse-explisti))
      nil)))

(defun parse-expb ()
  "EXPB -> (equal EXP EXP) | (less EXP EXP) | true | false | (true) | (false)"
  (let ((tk (next-token)))
    (cond
     ((match "OP_OP")
      (let ((op (next-token)))
        (cond
         ((string= op "KW_EQUAL")
          (advance-token)
          (let ((e1 (parse-exp))
                (e2 (parse-exp)))
            (if (match "OP_CP")
                (list 'EQUAL e1 e2)
              (signal 'syntax-error :message "Eksik ) equal ifadesinde"))))
         ((string= op "KW_LESS")
          (advance-token)
          (let ((e1 (parse-exp))
                (e2 (parse-exp)))
            (if (match "OP_CP")
                (list 'LESS e1 e2)
              (signal 'syntax-error :message "Eksik ) less ifadesinde"))))

         ((string= op "KW_TRUE")
          (advance-token)
          (if (match "OP_CP")
              'TRUE
            (signal 'syntax-error :message "Eksik ) true ifadesinde")))

         ((string= op "KW_FALSE")
          (advance-token)
          (if (match "OP_CP")
              'FALSE
            (signal 'syntax-error :message "Eksik ) false ifadesinde")))

         (t (signal 'syntax-error :message "Bilinmeyen boolean operatör")))))

     ((match-keyword "KW_TRUE") 'TRUE)
     ((match-keyword "KW_FALSE") 'FALSE)
     (t (signal 'syntax-error :message "Boolean ifade bekleniyor")))))


;; Değerlendirme Fonksiyonları
(defun evaluate-ast (ast)
  (cond
   ((stringp ast)
    (if (find #\: ast)
        ast
      (lookup-variable ast)))

   ((consp ast)
    (case (car ast)
      (DEFFUN
       (define-function (cadr ast) (caddr ast) (cadddr ast))
       "0:1")

      (FCALL
       (apply-function (cadr ast) (caddr ast)))

      (SET
       (let* ((exprs (caddr ast))
              (vals (mapcar #'evaluate-ast exprs))
              (last-val (car (last vals))))
         (set-variable (cadr ast) last-val)
         last-val))

      (DEFVAR
       (defvar-variable (cadr ast) (evaluate-ast (caddr ast)))
       "0:1")

      (IF
       (let ((cond-val (evaluate-boolean (cadr ast))))
         (if cond-val
             (evaluate-explist (caddr ast))
           (if (cadddr ast)
               (evaluate-explist (cadddr ast))
             "0:1"))))

      (WHILE
       (while-loop (cadr ast) (caddr ast))
       "0:1")

      (FOR
       (for-loop (cadr ast) (caddr ast) (cadddr ast) (cadddr (cdr ast)))
       "0:1")

      (EQUAL
       (evaluate-boolean-op '= (cadr ast) (caddr ast)))

      (LESS
       (evaluate-boolean-op '< (cadr ast) (caddr ast)))

      (OP_PLUS
       (op-plus-fn (evaluate-ast (cadr ast)) (evaluate-ast (caddr ast))))

      (OP_MINUS
       (op-minus-fn (evaluate-ast (cadr ast)) (evaluate-ast (caddr ast))))

      (OP_MULT
       (op-mult-fn (evaluate-ast (cadr ast)) (evaluate-ast (caddr ast))))

      (OP_DIV
       (op-div-fn (evaluate-ast (cadr ast)) (evaluate-ast (caddr ast))))

      (t (error "Bilinmeyen AST düğümü: ~a" (car ast)))))

   (t (error "Geçersiz AST düğümü: ~a" ast))))

(defun evaluate-explist (explist)
  (let ((result "0:1"))
    (dolist (e explist)
      (setq result (evaluate-ast e)))
    result))

(defun evaluate-boolean (bexpr)
  (cond
   ((eq bexpr 'TRUE) t)
   ((eq bexpr 'FALSE) nil)
   ((consp bexpr)
    (case (car bexpr)
      (EQUAL (equal (evaluate-ast (cadr bexpr)) (evaluate-ast (caddr bexpr))))
      (LESS (less-than (evaluate-ast (cadr bexpr)) (evaluate-ast (caddr bexpr))))
      (t (error "Bilinmeyen boolean ifade"))))
   (t (error "Boolean ifade çözümlenemedi"))))

(defun less-than (f1 f2)
  (let ((val1 (fraction-to-float f1))
        (val2 (fraction-to-float f2)))
    (< val1 val2)))

(defun evaluate-boolean-op (op e1 e2)
  (cond
   ((eq op '=) (if (equal (evaluate-ast e1) (evaluate-ast e2)) 'TRUE 'FALSE))
   ((eq op '<) (if (less-than (evaluate-ast e1) (evaluate-ast e2)) 'TRUE 'FALSE))
   (t (error "Bilinmeyen boolean op"))))

;; Yardımcı Fonksiyonlar
(defun fraction-to-float (frac)
  (multiple-value-bind (n d) (split-fraction frac)
    (/ (parse-integer n) (float (parse-integer d)))))

(defun split-fraction (input-string)
  (let ((split-position (position #\: input-string)))
    (if split-position
        (values (subseq input-string 0 split-position)
                (subseq input-string (1+ split-position)))
      (error "Geçersiz VALUEF formatı: ~a" input-string))))

(defun op-plus-fn (frac1 frac2)
  (multiple-value-bind (n1 d1) (split-fraction frac1)
    (multiple-value-bind (n2 d2) (split-fraction frac2)
      (let* ((d1-int (parse-integer d1))
             (d2-int (parse-integer d2))
             (n1-int (parse-integer n1))
             (n2-int (parse-integer n2))
             (common-denominator (* d1-int d2-int))
             (new-n1 (* n1-int d2-int))
             (new-n2 (* n2-int d1-int))
             (result-n (+ new-n1 new-n2)))
        (format nil "~a:~a" result-n common-denominator)))))

(defun op-minus-fn (frac1 frac2)
  (multiple-value-bind (n1 d1) (split-fraction frac1)
    (multiple-value-bind (n2 d2) (split-fraction frac2)
      (let* ((d1-int (parse-integer d1))
             (d2-int (parse-integer d2))
             (n1-int (parse-integer n1))
             (n2-int (parse-integer n2))
             (common-denominator (* d1-int d2-int))
             (new-n1 (* n1-int d2-int))
             (new-n2 (* n2-int d1-int))
             (result-n (- new-n1 new-n2)))
        (format nil "~a:~a" result-n common-denominator)))))

(defun op-mult-fn (frac1 frac2)
  (multiple-value-bind (n1 d1) (split-fraction frac1)
    (multiple-value-bind (n2 d2) (split-fraction frac2)
      (let* ((n1-int (parse-integer n1))
             (d1-int (parse-integer d1))
             (n2-int (parse-integer n2))
             (d2-int (parse-integer d2))
             (result-n (* n1-int n2-int))
             (result-d (* d1-int d2-int)))
        (format nil "~a:~a" result-n result-d)))))

(defun op-div-fn (frac1 frac2)
  (multiple-value-bind (n1 d1) (split-fraction frac1)
    (multiple-value-bind (n2 d2) (split-fraction frac2)
      (let* ((n1-int (parse-integer n1))
             (d1-int (parse-integer d1))
             (n2-int (parse-integer n2))
             (d2-int (parse-integer d2)))
        (if (= n2-int 0)
            (error "Sıfıra bölme hatası!")
          (format nil "~a:~a" (* n1-int d2-int) (* d1-int n2-int)))))))

(defun lookup-variable (id)
  (or (gethash id *variables*)
      (error "Tanımsız değişken: ~a" id)))

(defun set-variable (id val)
  (setf (gethash id *variables*) val)
  val)

(defun defvar-variable (id val)
  (setf (gethash id *variables*) val)
  val)

(defun define-function (name params body)
  (setf (gethash name *functions*) (list params body))
  "0:1")

(defun apply-function (name args)
  (let ((fn (gethash name *functions*)))
    (if fn
        (let ((params (car fn))
              (body (cadr fn)))
          (when (/= (length params) (length args))
            (error "Parametre sayısı ile argüman sayısı uyuşmuyor."))

          (let ((old-vars (copy-hash-table *variables*)))
            (unwind-protect
                 (progn
                   (dotimes (i (length params))
                     (setf (gethash (nth i params) *variables*)
                           (evaluate-ast (nth i args))))
                   (evaluate-explist body))
              (clrhash *variables*)
              (maphash (lambda (k v) (setf (gethash k *variables*) v)) old-vars))))
      (error "Tanımsız fonksiyon: ~a" name))))

(defun evaluate-ast-list (lst)
  (let ((res "0:1"))
    (dolist (x lst)
      (setq res (evaluate-ast x)))
    res))

(defun while-loop (cond-expr body)
  (let ((res "0:1"))
    (loop while (evaluate-boolean cond-expr)
          do (setq res (evaluate-explist body)))
    res))

(defun for-loop (id start end body)
  (let ((start-val (evaluate-ast start))
        (end-val (evaluate-ast end)))
    (multiple-value-bind (sn sd) (split-fraction start-val)
      (multiple-value-bind (en ed) (split-fraction end-val)
        (let ((s (parse-integer sn))
              (e (parse-integer en))
              (res "0:1"))
          (dotimes (i (1+ (- e s)))
            (set-variable id (format nil "~a:1" (+ s i)))
            (setq res (evaluate-explist body)))
          res)))))

;; Çalıştırma
(gppinterpreter) 
