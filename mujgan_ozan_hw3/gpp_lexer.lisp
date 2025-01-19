(setq keyword-list
      (list "and" "or" "not" "equal" "less" "nil" "list" "append" "concat" "set"
            "deffun" "for" "if" "exit" "load" "print" "true" "false" "defvar" "while"))

(defvar tokenList (list)) 
(defvar valueList (list)) 
(defvar identifierList (list)) 
(defvar input (list))

(defun getTokenList (line)
  (setq tokenList '())
  (setq valueList '())
  (setq identifierList '())
  (setq input '())
  (split-line line)
  ;; Liste sıralarını düzelt
  (setq tokenList (reverse tokenList))
  (setq valueList (reverse valueList))
  (setq identifierList (reverse identifierList))
  (setq input (reverse input))
)

(defun split-line (string)
  "Split given string from spaces into words"
  (setq string (custom-left-trim string)) ; Remove leading spaces
  (let ((words '()) (start nil))
    (loop for i from 0 to (1- (length string))
          do (if (char= (char string i) #\Space)
                 (if start
                     (progn
                       (push (subseq string start i) words)
                       (setf start nil)))
                 (unless start (setf start i)))
          finally (if start (push (subseq string start) words)))

    (setq words (reverse words))

    (dolist (word words)
      (if (equal (check-tokens word) 1)
          (return)))
    'done))

(defun check-tokens (word)
  (if (string= word "")
      nil
    (let ((char (char word 0)))
      (cond
       ((char= char #\;)
        (push "COMMENT" tokenList)
        1)
       ((char= char #\()
        (push "OP_OP" tokenList)
        (push "(" input)
        (check-tokens (subseq word 1)))
       ((char= char #\))
        (push "OP_CP" tokenList)
        (push ")" input)
        (check-tokens (subseq word 1)))
       ((char= char #\+)
        (push "OP_PLUS" tokenList)
        (push "+" input)
        (check-tokens (subseq word 1)))
       ((char= char #\-)
        (push "OP_MINUS" tokenList)
        (push "-" input)
        (check-tokens (subseq word 1)))
       ((char= char #\/)
        (push "OP_DIV" tokenList)
        (push "/" input)
        (check-tokens (subseq word 1)))
       ((char= char #\*)
        (push "OP_MULT" tokenList)
        (push "*" input)
        (check-tokens (subseq word 1)))
       ((char= char #\,)
        (push "OP_COMMA" tokenList)
        (push "," input)
        (check-tokens (subseq word 1)))
       (t
        (let ((cp-position (position #\) word))
              (comment-position (position #\; word)))
          (cond
           (comment-position
            (check-tokens (subseq word 0 comment-position))
            (push "COMMENT" tokenList)
            1)
           (cp-position
            (if (not (is-keyword (subseq word 0 cp-position)))
                (if (digit-char-p (char (subseq word 0 cp-position) 0))
                    (is-value (subseq word 0 cp-position))
                  (is-identifier (subseq word 0 cp-position))))
            (check-tokens (subseq word cp-position)))
           (t
            (if (not (is-keyword word))
                (if (digit-char-p (char word 0))
                    (is-value word)
                  (is-identifier word)))))))))))

(defun is-keyword (string)
  (let ((is-keyword (member string keyword-list :test #'string=)))
    (when is-keyword 
      (push (concatenate 'string "KW_" (string-upcase string)) tokenList)
      (if (string= string "deffun") 
          (push "deffun" input)))
    is-keyword))

(defun is-value (string)
  "Check if given string is a valid value fraction N:D"
  (let ((len (length string))
        (colon-count 0)
        (digit-counter 0))
    (if (not (digit-char-p (char string (1- len))))
        nil
      (loop for i from 0 below len
            do (let ((c (char string i)))
                 (cond
                  ((char= c #\:) (incf colon-count))
                  ((digit-char-p c) (incf digit-counter))
                  (t (return nil))))
            finally
            (let ((is-value (and (= colon-count 1) (= digit-counter (- len 1)))))
              (if is-value 
                  (progn
                    (push string valueList)
                    (push string input)
                    (push "VALUEF" tokenList))
                (push "&%+" tokenList))
              is-value)))))

(defun is-identifier (str)
  "Check if given string is a valid identifier"
  (let ((char-count 0)
        (digit-count 0)
        (underscore-count 0))
    (dotimes (i (length str))
      (let ((c (char str i)))
        (cond
         ((alpha-char-p c) (incf char-count))
         ((digit-char-p c) (incf digit-count))
         ((char= c #\_) (incf underscore-count)))))
    (let ((is-identifier (and (> char-count 0)
                              (= (+ char-count digit-count underscore-count) (length str))
                              (alpha-char-p (char str 0)))))
      (if is-identifier
          (progn
            (push str identifierList)
            (push "IDENTIFIER" tokenList))
        (push "UNKNOWN" tokenList))
      is-identifier)))


(defun custom-left-trim (string)
  "Remove leading spaces from string"
  (let ((start 0)
        (end (length string)))
    (loop while (and (< start end) (char<= (char string start) #\Space))
          do (incf start))
    (subseq string start)))

