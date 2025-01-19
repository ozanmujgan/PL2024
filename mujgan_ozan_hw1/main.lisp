
;; 1. Dosyayı Okuma Fonksiyonu
(defun read-file (file)
  "C dosyasını satır satır okur ve bir liste döndürür."
  (with-open-file (stream file)
    (loop for line = (read-line stream nil)
          while line
          do (let* ((line-typed (line-type line))
                     (conversion-func (conversion-foo line-typed)))  ; conversion-func'u burada alıyoruz
                ;; Burada line-typed değişkeni kullanılabilir
                (print (convert line conversion-func))  ; convert fonksiyonunu çağırıyoruz
          )
          collect line)))

;; 2. Dosyaya Yazma Fonksiyonu
(defun write-file (file content)
  "Dönüştürülmüş Lisp kodunu dosyaya yazar."
  (with-open-file (stream file :direction :output :if-exists :supersede)
    (dolist (line content)
      (format stream "~a~%" line))))

(defun line-type (line)
  (cond
    ;; Satırda sadece '}' varsa veya '}' içeriyorsa
    ((search "}" line) 'closing-brace)
    ;; If statement
    ((and (search "if" line :test #'equalp)
          (search "(" line)
          (search ")" line)) 'if-statement)
    ;; For loop
    ((and (search "for" line :test #'equalp)
          (search "(" line)
          (search ")" line)) 'for-loop)
    ;; While loop
    ((and (search "while" line :test #'equalp)
          (search "(" line)
          (search ")" line)) 'while-loop)
    ;; Logical operation (&&, ||, !)
    ((or (search "&&" line) 
         (search "||" line)
         (search "!" line)) 'logical-operation)
         ;; Return statement
    ((search "return" line :test #'equalp) 'return-statement)
        ;; Arithmetic operation (contains +, -, *, /)
    ((or (search "+" line) 
         (search "-" line) 
         (search "*" line) 
         (search "/" line)) 'arithmetic-operation)
    ;; Variable  definitions
    ((and (or (search "int" line :test #'equalp)
              (search "float" line :test #'equalp)
              (search "double" line :test #'equalp)
              (search "char" line :test #'equalp))
          (search "=" line)) 'variable-definition)
    ;; Variable assignment (e.g., x = 5)
    ((and (search "=" line) (not (search "int" line)) (not (search "float" line))
          (not (search "double" line)) (not (search "char" line))) 
     'variable-assignment)
    ;; Function definition
    ((and (or (search "int" line :test #'equalp)
              (search "void" line :test #'equalp)
              (search "float" line :test #'equalp)
              (search "double" line :test #'equalp)
              (search "char" line :test #'equalp))
          (search "(" line)
          (search ")" line)
          (search "{" line)) 'function-definition)
       ;; printf with format specifier and variables
    ((and (search "printf" line :test #'equalp)
          (search "," line)) 'printf-comma-call)
           ;; Function call (specific case for printf)
    ((search "printf" line :test #'equalp) 'printf-call)
          ;; Increment or decrement (i++ or i--)
    ((or (search "++" line) (search "--" line)) 'increment-statement)
    ;; Function call
    ((and (search "(" line)
          (search ")" line)
          (search ";" line)) 'function-call)
         
  
    ;; Default case
    (t 'unknown)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dönüşüm Fonksiyonları
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun conversion-foo (line-type)
  (case line-type
    ('if-statement #'convert-if-statement)
    ('for-loop #'convert-for-loop)
    ('while-loop #'convert-while-loop)
    ('logical-operation #'convert-logical-operation)
    ('return-statement #'convert-return-statement) 
    ('variable-definition #'convert-variable-definition)
    ('variable-assignment #'convert-variable-assignment) ;
    ('function-definition #'convert-function-definition)
    ('function-call #'convert-function-call)
    ('printf-call #'convert-printf-call) 
    ('printf-comma-call #'convert-printf-comma) 
    ('increment-statement #'convert-increment-statement)
    ('arithmetic-operation #'convert-arithmetic-operation)
    ('closing-brace #'convert-closing-brace)
    (t #'convert-unknown)))






(defun convert (line conversion-function)
  (funcall conversion-function line))
  
 
 
 (defun split-string (string delimiter)
  "Verilen metni belirtilen ayraca göre böler."
  (let ((start 0) (result '()))
    (loop for end = (search delimiter string :start2 start)
          while end do
          (push (subseq string start end) result)
          (setf start (+ end (length delimiter))))
    (push (subseq string start) result)
    (nreverse result)))
 
(defun replace-all (string old new)
  "Verilen metindeki tüm `old` ifadelerini `new` ile değiştirir."
  (with-output-to-string (out)
    (loop for part on (split-string string " ")  ; split-sequence yerine split-string kullanıyoruz
          do (princ (if (equal (car part) old) new (car part)) out)
          (princ " " out))))
;; 5. Dönüşüm Fonksiyonları



(defun convert-variable-assignment (line)
  "Değişken atama ifadesini dönüştür."
  (let* ((parts (split-string line "="))
         (var (string-trim '(#\Space #\;) (first parts)))
         (value (string-trim '(#\Space #\;) (second parts))))
    (format nil "(setf ~a ~a)" var value))) ; Lisp formatında atama

(defun convert-closing-brace (line)
  "Satırdaki '}' karakterini ')' olarak değiştirir."
  (replace-all line "}" ")"))




(defun split-string3 (string delimiters)
  "Metni belirtilen ayıraclara göre böler ve gereksiz boşlukları temizler."
  (let ((start 0) (result '()))
    (loop for end = (position-if (lambda (char) (member char delimiters))
                                 string :start start)
          while end do
          (when (/= start end)
            (push (string-trim '(#\Space #\;) (subseq string start end)) result))  ;; Operand ekle
          (push (string (char string end)) result)  ;; Operatör ekle
          (setf start (1+ end)))
    (when (< start (length string))
      (push (string-trim '(#\Space #\;) (subseq string start)) result))  ;; Kalan kısmı ekle
    (nreverse result)))  ;; Sonuç ters çevir

(defun convert-if-statement (line)
  "C stilindeki bir if koşulunu ayırır ve Lisp formatında döndürür."
  (let* ((start (search "(" line))
         (end (search ")" line)))
    (if (and start end)
        (let* ((condition (subseq line (+ start 1) end)) ;; Koşulu ayıkla
               (tokens (split-string3 condition '(#\> #\< #\= #\!)))  ;; Operatörlere göre ayır
               (operand1 (first tokens))   ;; İlk operand
               (operator (second tokens))   ;; Operatör
               (operand2 (third tokens)))   ;; İkinci operand
          ;; Lisp formatında döndür
          (format nil "(if (~a ~a ~a)" operator operand1 operand2)) 
        (format nil "Geçersiz if ifadesi.")))) ;; Geçersizse mesaj döndür





(defun convert-increment-statement (line)
  "Artırma veya azaltma ifadelerini Lisp formatına dönüştürür."
  (let ((end-pos (search "++" line)))
    (if end-pos
        (let ((variable (string-trim '(#\Space #\Tab) (subseq line 0 end-pos))))
          (format nil "(incf ~a)" variable))
        (let ((end-pos (search "--" line)))
          (if end-pos
              (let ((variable (string-trim '(#\Space #\Tab) (subseq line 0 end-pos))))
                (format nil "(decf ~a)" variable))
              line)))))  ;; Eğer başka bir şeyse orijinal satırı döndür





(defun split-string4 (string delimiter)
  "Metni belirli bir ayıraca göre böler ve boşlukları temizler."
  (let ((result '()) (start 0))
    (loop for pos = (position delimiter string :start start)
          while pos do
          (push (string-trim '(#\Space) (subseq string start pos)) result)
          (setf start (1+ pos)))
    (push (string-trim '(#\Space) (subseq string start)) result)
    (nreverse result)))

(defun convert-for-loop (line)
  "C tarzı bir for döngüsünü Lisp 'loop' ifadesine dönüştürür."
  (let* ((start (search "(" line))
         (end (search ")" line)))
    (if (and start end)
        (let* ((loop-content (subseq line (+ start 1) end)) ;; Parantez içeriği
               (tokens (split-string4 loop-content #\;))     ;; Başlatma, koşul, artış
               (init (first tokens))                        ;; int i = 0 kısmı
               (condition (second tokens))                  ;; i < 10 kısmı
               (increment (third tokens)))                  ;; i++ kısmı
          (let* ((init-tokens (split-string4 init #\Space)) ;; int i = 0 kısmını parçala
                 (var (second init-tokens))                ;; Değişken: i
                 (start-value (remove #\= (third init-tokens) :test #'char=)) ;; Başlangıç değeri: 0
                 (condition-tokens (split-string4 condition #\Space)) ;; i < 10 kısmı
                 (end-value (third condition-tokens)))      ;; Bitiş değeri: 10
            ;; Dönüştürülmüş loop ifadesini döndür
            (format nil "(loop for ~a from ~a below ~a do ~%" var start-value end-value)))
      (format nil "Geçersiz for döngüsü ifadesi.")))) ;; Geçersizse mesaj döndür






(defun convert-while-loop (line)
 (let* ((start (search "(" line))
         (end (search ")" line)))
    (if (and start end)
        (let* ((condition (subseq line (+ start 1) end)) ;; Koşulu ayıkla
               (tokens (split-string3 condition '(#\> #\< #\= #\!)))  ;; Operatörlere göre ayır
               (operand1 (first tokens))   ;; İlk operand
               (operator (second tokens))   ;; Operatör
               (operand2 (third tokens)))   ;; İkinci operand
          ;; Lisp formatında döndür
          (format nil "(loop while (~a ~a ~a ) do" operator operand1 operand2)) 
        (format nil "Geçersiz if ifadesi.")))
        )  ;; Geçersiz durumda hata mesajı döndür

 


;;printf başarılı bir şelilde çevrildi
(defun replace-substring-in-string (string old-substring new-substring)
  "STRING içinde OLD-SUBSTRING'i NEW-SUBSTRING ile değiştirir."
  (let ((start 0))
    (loop
      (let ((pos (search old-substring string :start2 start)))
        (if pos
            (progn
              (setf string (concatenate 'string
                                        (subseq string 0 pos)    ;; Önceki kısmı ekle
                                        new-substring             ;; Yeni alt dizeyi ekle
                                        (subseq string (+ pos (length old-substring))))) ;; Sonraki kısmı ekle
              (setf start (+ pos (length new-substring))))  ;; Yeni başlangıç pozisyonu ayarla
            (return string))))))  ;; Dizeyi döndür

(defun extract-printf-content (line)
  "printf içeriğini çıkarır, \" ve \\n gibi karakterleri doğru işler."
  (let ((start (search "printf(" line)))
    (when start
      (let ((content-start (+ start 7)))  ;; "printf(" sonrası
        (loop for i from content-start below (length line)
              for char = (char line i)
              when (and (char= char #\")  ;; Dize sonunu bul
                        (> i content-start))  ;; İlk tırnağı atla
              return (subseq line content-start (+ i 1)))))))  ;; Alt dizeyi al

(defun convert-printf-call (line)
  "printf çağrısını Lisp formatına dönüştürür."
  (let ((printf-content (extract-printf-content line)))
    (if printf-content
        (let ((formatted-string (replace-substring-in-string 
                                  printf-content "\\n" "~%"))) ;; \n'leri değiştir
          (format nil "(format t ~a)" formatted-string))  ;; Dönüştürülmüş format
        line)))  ;; Orijinal satırı döndür






(defun extract-variable (line)
  "Virgül ve parantez arasındaki değişkeni çıkarır."
  (let ((comma-pos (position #\, line))
        (close-paren-pos (position #\) line :start (position #\, line))))
    (when (and comma-pos close-paren-pos)
      (string-trim '(#\Space) 
                   (subseq line (1+ comma-pos) close-paren-pos)))))

(defun convert-printf-comma (line)
  "Virgüllü printf çağrısını Lisp formatına dönüştürür."
  (let ((printf-content (extract-printf-content line))
        (variable (extract-variable line)))
    (if printf-content
        (let ((formatted-string (replace-substring-in-string 
                                  printf-content "\\n" "~%"))) ;; \n'leri değiştir
          ;; Eğer bir değişken varsa onu ekle
          (if variable
              (format nil "(format t ~a ~a)" formatted-string variable)
              (format nil "(format t ~a)" formatted-string)))  ;; Değişkensiz format
        line)))  ;; Orijinal satırı döndür





  
(defun convert-logical-operation (line)
  "Convert a C-style logical operation in LINE to Lisp syntax."
  (let ((equal-pos (position #\= line)))  ; Find the position of '='
    (if equal-pos
        (let* ((var (string-trim '(#\Space) (subseq line 0 equal-pos)))
               (expr (string-trim '(#\Space #\;) (subseq line (+ equal-pos 1)))))
          (format nil "(setq ~a ~a)" var (convert-expression expr)))
        line)))  ; If no '=', return the line as is.

(defun convert-expression (expr)
  "Convert logical operators in EXPR to Lisp equivalents."
  (cond
    ((search "&&" expr) (replace-logical expr "&&" "and"))
    ((search "||" expr) (replace-logical expr "||" "or"))
    ((search "! " expr) (replace-logical expr "! " "not"))
    (t expr)))  ; If no operator, return the expression unchanged.

(defun replace-logical (expr old new)
  "Replace OLD operator with NEW in EXPR and convert to Lisp syntax."
  (let* ((op-pos (search old expr))
         (left (string-trim '(#\Space) (subseq expr 0 op-pos)))
         (right (string-trim '(#\Space #\;) (subseq expr (+ op-pos (length old))))))
    (format nil "(~a ~a ~a)" new left right)))
  
  
  
  
  
(defparameter *let-inserted* nil) ;; Let'in eklenip eklenmediğini takip etmek için global değişken
(defun convert-variable-definition (line)
  "C değişken tanımını Lisp formatına çevirir."
  (let* ((parts (split-string line "=")) ;; Eşit işaretine göre ayır
         (var-def (string-trim '(#\Space #\;) (first parts))) ;; Tür ve ad
         (value (string-trim '(#\Space #\;) (second parts))) ;; Değer
         (var-parts (split-string var-def " ")) ;; Tür ve değişken adını ayır
         (var-name (second var-parts)) ;; Değişken adını al
         (converted (format nil "(~a ~a)" var-name value))) ;; (değişken-değeri)
    (if (not *let-inserted*) ;; Eğer let henüz eklenmediyse
        (progn
          (setf *let-inserted* t) ;; Let'in eklendiğini işaretle
          (format nil "(let(~a" converted)) ;; Let ekleyip yaz
      converted))) ;; Sadece dönüştürüp döndür.




  




;;Fonsiyon çağırma işlemi başarılı

(defun convert-function-call (line)
  "C fonksiyon tanımını Lisp formatına dönüştürür."
  (multiple-value-bind (return-type name param-list) (parse-function-call line)
    (let* ((param-types (mapcar #'extract-type param-list)) ; Parametrelerin türlerini al
           (param-string (join-strings param-types " ")))    ; Türleri birleştir
      (format nil "(declaim (ftype (function (~a) ~a) ~a))"
              param-string
              (c-type-to-lisp-type return-type)
              name))))

(defun parse-function-call (line)
  "Fonksiyon satırını ayrıştırır: dönüş türü, fonksiyon adı ve parametreler."
  (let* ((paren-pos (position #\( line))              ; İlk '(' pozisyonu
         (return-and-name (subseq line 0 paren-pos))  ; Fonksiyon adı ve dönüş tipi
         (params (subseq line (1+ paren-pos) (position #\) line))) ; Parametreler
         (return-type (first (split-string return-and-name " "))) ; Dönüş tipi
         (name (second (split-string return-and-name " ")))       ; Fonksiyon adı
         (param-list (split-string params ", ")))                 ; Parametre listesi
    (values return-type name param-list)))

(defun extract-type (param)
  "Parametre türünü çıkarır."
  (first (split-string param " ")))  ; 'int a' -> 'int'

(defun c-type-to-lisp-type (c-type)
  "C tiplerini Lisp eşdeğerlerine dönüştürür."
  (cond
    ((string= c-type "int") "integer")
    ((string= c-type "float") "float")
    ((string= c-type "double") "double-float")
    ((string= c-type "char") "character")
    (t "unknown-type")))

(defun join-strings (strings separator)
  "Bir dizi stringi belirtilen bir ayırıcıyla birleştirir."
  (reduce (lambda (x y) (concatenate 'string x separator y)) strings))


              
              
              
              
              
   



;;fonksiyon tanmalama başarılı

(defun convert-function-definition (line)
  "C fonksiyon tanımını Lisp formatına dönüştürür."
  (multiple-value-bind (name params) (parse-function-definition line)
    (let ((param-names (mapcar #'extract-param-name params)))
      (format nil "(defun ~a (~a)" 
              name 
              (join-strings param-names " ")))))  ; Parametreleri birleştir

(defun parse-function-definition (line)
  "Fonksiyon tanımını ayrıştırır: ad ve parametreler."
  (let* ((paren-pos (position #\( line))                 ; İlk '(' pozisyonu
         (name (second (split-string (subseq line 0 paren-pos) " "))) ; Fonksiyon adı
         (params (subseq line (1+ paren-pos) (position #\) line))) ; Parametreler
         (param-list (split-string params ", ")))        ; Parametreleri diziye çevir
    (values name param-list)))

(defun extract-param-name (param)
  "Parametre isminden yalnızca ismi çıkarır. Örneğin, 'int a' -> 'a'."
  (second (split-string param " ")))  ; İkinci elemanı al






;;return işlemi başarılı

(defun convert-return-statement (line)
  "C'deki return ifadesini Lisp formatına dönüştürür."
  (let* ((return-value (string-trim '(#\Space #\;) 
                                    (subseq line (+ (search "return" line :test #'equalp) 6))))
         (arithmetic-operators '(#\+ #\- #\* #\/)))
    (if (some (lambda (op) (search (string op) return-value)) arithmetic-operators)
        ;; Aritmetik operatör varsa, aritmetik işlemi dönüştür
        (convert-arithmetic-operation return-value)
        ;; Operatör yoksa doğrudan değeri döndür
        return-value)))



;; aritmetik işlemler bşarılı

(defun split-string2 (string delimiters)
  "Metni belirtilen ayıraclara göre böler ve gereksiz boşlukları temizler."
  (let ((start 0) (result '()))
    (loop for end = (position-if (lambda (char) (member char delimiters))
                                 string :start start)
          while end do
          (when (/= start end)
            (push (string-trim '(#\Space #\;) (subseq string start end)) result))  ;; Operand ekle
          (push (string (char string end)) result)  ;; Operatör ekle
          (setf start (1+ end)))
    (when (< start (length string))
      (push (string-trim '(#\Space #\;) (subseq string start)) result))  ;; Kalan kısmı ekle
    (nreverse result)))  ;; Sonuç ters çevir

(defun convert-arithmetic-operation (line)
  "Aritmetik işlemi Lisp formatına dönüştürür."
  (let* ((tokens (split-string2 line '(#\+ #\- #\* #\/)))  ;; Operatörlere göre ayır
         (operator (second tokens))  ;; Operatör
         (operand1 (first tokens))   ;; İlk operand
         (operand2 (third tokens)))  ;; İkinci operand
    (format nil "(~a ~a ~a)" operator operand1 operand2)))  ;; (+ a b) formatında dö










(defun convert-unknown (line)
  ;; Handle unknown lines
  (format nil "Unknown line type: ~a" line))

;; 9. Satırları İşleme Fonksiyonu
(defun process-lines (lines)
  "Satırları okur ve uygun dönüşüm fonksiyonunu çağırır."
  (let ((converted-lines (mapcar (lambda (line)
                                   (let ((type (line-type line)))
                                     (funcall (conversion-foo type) line)))
                                 lines)))
    ;; Eğer `let` kullanıldıysa, kapanış parantezini ekle
    (if *let-inserted*
        (append converted-lines '(")"))  ;; `let` için kapanış parantezi ekle
        converted-lines)))  ;; Aksi takdirde olduğu gibi döndür

;; 10. Ana Fonksiyon
(defun main (input-file output-file)
  "Dosyayı okuyup satırları dönüştürür ve yeni dosyaya yazar."
  (let ((lines (read-file input-file)))
    (write-file output-file (process-lines lines))))

;; 11. Çalıştırma
(main "main.c" "main.txt")
