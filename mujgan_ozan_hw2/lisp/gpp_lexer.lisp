
(defun interpret-shell()
    (loop (format t "~%>>> ") (interpreter (read-line))))

(defun interpret-file (filename)
    (let ((input-stream (open filename :if-does-not-exist nil)))        
      (when input-stream (loop for line = (read-line input-stream nil)
                while line do (execute-interpreter line)) (close input-stream))
      (unless input-stream (format t "ERROR: File not found: '~a'" filename))))

(defun interpreter(seq)
    "Tokenizes the seq:str, input sequence could only be line.
     Prints the tokens. line ex. : (deffun sumup (x))"

    (let ((lst (input-to-lst seq)))
        (if (string= (car lst) ";;") (print "COMMENT")  
        (map nil #'(lambda (token) (print (tokenize token (get-token-lst)))) lst))
    ))


(defun get-token-lst ()
    "The G++ token classes are stored in an association list."

    (let ((token_key '("and" "or" "not" "equal" "less" "nil" "list"
                     "append" "concat" "set" "deffun" "for" "if" 
                     "exit" "load" "disp" "true" "false" "+" "-" 
                     "/" "*" "(" ")"  ","))
         (token_value '("KW_AND" "KW_OR" "KW_NOT" "KW_EQUAL" "KW_LESS" 
                       "KW_NIL" "KW_LIST" "KW_APPEND" "KW_CONCAT" "KW_SET" 
                       "KW_DEFFUN" "KW_FOR" "KW_IF" "KW_EXIT" "KW_LOAD" 
                       "KW_DISP" "KW_TRUE" "KW_FALSE" "OP_PLUS" "OP_MINUS"
                       "OP_DIV" "OP_MULT" "OP_OP" "OP_CP"  "OP_COMMA")))
        (pairlis token_key token_value)))

(defun tokenize (token lst)
    "Classifies the unnamed token string using DFA.
     lst is hard-coded token list."

     ;; Start state of DFA.
     ;; Scan the first char and direct it to proper state.
     (let ((c (string (char token 0)))) 
     (cond ((is-alphap c) (tokenize-identifier token lst))  ;; [a-zA-z] identifier and kw
           ((is-numericp c) (tokenize-value token))         ;; [0-9] value
           ((is-quomarkp c) (tokenize-string token))
           (t (if (tokenize-op token lst)                   ;; operator
           (tokenize-op token lst) (errout token c))))))    ;; else, syntax error.
                                             

(defun tokenize-identifier (token lst)
    "Identifier State of DFA.  
     Start State ---- [a-zA-z] ---> Identifier State <- [a-zA-z0-9]*
     Starting state can reach this state with having first letter alpha."

    (assert (is-alphap (string (char token 0))))

    ;; Scanning one by one.
    (loop for c across token 
        do (if (not (or (is-alphap c) (is-numericp c))) 
        (return-from tokenize-identifier (errout token c))))

    (let ((kw (tokenize-kw token lst)))
        (if (null kw) (format nil "IDENTIFIER") kw)))
    


(defun tokenize-value (token)
  "Determines if the token is an integer (VALUEI), fractional (VALUEF), 
   or triggers a syntax error based on having one 'f' or ':' in the correct format."

  (cond
   ;; Case for fractional number with exactly one 'f'
   ((and (search "f" token)
         (= (count #\f token) 1)
         (every #'digit-char-p (subseq token 0 (position #\f token)))
         (every #'digit-char-p (subseq token (1+ (position #\f token)))))
    (format nil "VALUEF"))

   ;; Case for fractional number with exactly one ':'
   ((and (search ":" token)
         (= (count #\: token) 1)
         (every #'digit-char-p (subseq token 0 (position #\: token)))
         (every #'digit-char-p (subseq token (1+ (position #\: token)))))
    (format nil "VALUEF"))

   ;; Case for an integer
   ((every #'digit-char-p token)
    (format nil "VALUEI"))

   ;; Case for identifier with 'f' but no valid number format
   ((and (not (search ":" token)) (string= token "f"))
    (format nil "IDENTIFIER"))

   ;; Otherwise, syntax error
   (t
    (format nil "SYNTAX ERROR: Invalid format for token '~a'" token))))




(defun tokenize-kw (token lst)
    "Keywords state of DFA.
     lst is hard-coded reserved keys. "
     (let ((value (assoc token lst :test #'string=)))
        (if value (format nil "~a" (cdr value)) nil)))

(defun tokenize-op (token lst)
    "Operator state of DFA."

     (let ((value (assoc token lst :test #'string=)))
        (if value (format nil "~a" (cdr value)) nil)))

(defun tokenize-string (token)
    "String State of DFA.  
     Start State ---- [\"] ---> String State <- [*] -> [\"]"

    (assert (is-quomarkp (string (char token 0))))
    (format nil "STRING"))

(defun errout (token c)
  (format t "SYNTAX ERROR: '~a' : '~a'~%" token c)
  (exit))


(defun list-to-string (lst)
    (format nil "~{~A~}" lst))

(defun is-bracketsp (chr)
    "If chr := ( or ) returns true, nil otherwise."
    (let ((c (char-int (coerce chr 'character))))
        (or (= c 40) (= c 41)))) ;; 40: ( 41: )

(defun is-zerop (chr)
    (eq (char-code (coerce chr 'character)) 48));; ->0

(defun is-semicolonp (chr)
    (eq (char-code (coerce chr 'character)) 59));; ->;

(defun is-quomarkp (chr)
    (eq (char-code (coerce chr 'character)) 34));; ->"

(defun is-numericp (chr)
    "Determines if chr is numeric."

    (let ((c (char-int (coerce chr 'character))))
        (and (>= c (char-int #\0)) (<= c (char-int #\9)))))
    
(defun is-alphap (chr)
    "Determines if chr is letter: returns true or nil.
     ex. : A -> t, '(' -> nil"

    (let ((c (char-int (coerce chr 'character))))
        (and (>= c (char-int #\A)) (<= c (char-int #\z)))))

(defun cleanup (str)
    "Cleans the 'str' from predefined -leading and trailing- stuff."

    ; trim-lst: leading and trailing garbage predefined.
    (let ((trim-lst '(#\Space #\Newline #\Backspace #\Tab #\Return )))
    (string-trim trim-lst str)))


(defun split-seq-with-space (string &key (is-spacep #'is-spacep))
    "Splits the given key sequence to list, due to space delimeter delimeter. 
     i.e -> 'hello world' becomes ('hello' 'world')."

  (loop :for start = (position-if-not is-spacep string)
    :then (position-if-not is-spacep string :start (1+ end))
    :for end = (and start (position-if is-spacep string :start start))
    :when start :collect (subseq string start end)
    :while end))

(defun is-spacep (c) (char= c #\Space))

(defun input-to-lst (str)
    "Turns the sequence into list with unnamed tokens, 
     which will be tokenized with DFA later on."

    ;; cleanup str and span around of brackets with spaces.
    (setq str (list-to-string (map 'list #'(lambda (c) 
        (if (is-bracketsp c) (concatenate 'string " " (string c) " ") 
        (string c))) (cleanup str))))

    ;; in sequence, find the indices of spaces in between quo-mark
    (let ((lst (loop for idx from 0 to (- (length str) 1)
                when (char= (aref str idx) #\") collect idx))
          (idx1 '()) (idx2 '()) (space-idx '()))
    (loop while lst do (setq idx1 (car lst)) 
                    do (setq idx2 (car (cdr lst)))
                    do (setq lst (cdr (cdr lst)))
                    do (setq space-idx
            (loop for idx from 0 to (- (length str) 1)
            when (and (> idx idx1) (< idx idx2) (char= (aref str idx) #\Space)) 
            collect idx)))

    ;; connect the string in between quo-marks with dots.
    ;; turn the string to list with #Space delimeter.
    (split-seq-with-space (list-to-string
    (loop for idx from 0 to (- (length str) 1)
        if (member idx space-idx) collect #\. else collect (aref str idx))))))

(defun gppinterpreter (&optional filename)
    (if filename (interpret-file filename) (interpret-shell))
    )

;; Either tokenizes the given file.
;; Or opens the command prompt and tokenizes immediately.
(if *args* (gppinterpreter (car *args*)) (gppinterpreter))


