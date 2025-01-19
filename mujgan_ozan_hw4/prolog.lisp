;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1) Temel Türetme (father, mother => parent => ancestor)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prolog-prove (axioms)
  "Tüm aile ilişkilerini türetir. father/mother => parent => ancestor;
   ancestor(X, Y) & ancestor(Y, Z) => ancestor(X, Z).
   Ardından derive-extra-relations ile sibling => uncle vb. ek ilişkileri bulur."
  (let ((results '()))
    ;; 1) father / mother aksiyomlarını ekleyip parent, ancestor çıkar
    (dolist (axiom axioms)
      (let ((relation (car axiom))
            (x (cadr axiom))
            (y (caddr axiom)))
        (cond
         ;; father
         ((equal relation "father")
          (push (list "father" x y) results)
          (push (list "parent" x y) results)
          (push (list "ancestor" x y) results))
         ;; mother
         ((equal relation "mother")
          (push (list "mother" x y) results)
          (push (list "parent" x y) results)
          (push (list "ancestor" x y) results))

     
         ((or (equal relation "sibling")
              (equal relation "uncle"))
          ;; Sibling, Uncle vb. aksiyomları da doğrudan ekleyelim
          (push (list relation x y) results)))))

    ;; 2) parent => ancestor
    (dolist (rel (copy-list results))
      (when (equal (car rel) "parent")
        (let ((px (cadr rel))
              (py (caddr rel)))
          (unless (member (list "ancestor" px py) results :test #'equal)
            (push (list "ancestor" px py) results)))))

    ;; 3) ancestor transitive closure
    (let ((new-relations t))
      (loop 
        while new-relations
        do (progn
             (setf new-relations nil)
             (dolist (ra (copy-list results))
               (when (equal (car ra) "ancestor")
                 (let ((xa (cadr ra))
                       (ya (caddr ra)))
                   (dolist (rb (copy-list results))
                     (when (equal (car rb) "ancestor")
                       (let ((xb (cadr rb))
                             (yb (caddr rb)))
                         (when (equal ya xb)
                           (let ((new (list "ancestor" xa yb)))
                             (unless (member new results :test #'equal)
                               (push new results)
                               (setf new-relations t)))))))))))))

    ;; 4) Ek ilişkileri (sibling => uncle vb.) türet:
    (derive-extra-relations results)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2) Ek Kuralları Türeten Fonksiyon
;;;    sibling(X, Z), father(Z, Y) => uncle(X, Y) vb.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun derive-extra-relations (rels)
  "rels listesinde (father, mother, parent, ancestor, sibling, vb.) var.
   Bu fonksiyon sibling => uncle kuralını (ve isterseniz başkalarını) uygular."
  (let ((results (copy-list rels))
        (changed t))
    (loop
      while changed
      do (progn
           (setf changed nil)
           (dolist (r1 (copy-list results))
             (dolist (r2 (copy-list results))
               (let ((rel1 (car r1))
                     (x1   (cadr r1))
                     (z1   (caddr r1))
                     (rel2 (car r2))
                     (x2   (cadr r2))
                     (y2   (caddr r2)))
                 ;; UNCLE KURALI:
                 ;;   sibling(X, Z) ve father(Z, Y) => uncle(X, Y).
                 (when (and (equal rel1 "sibling")
                            (equal rel2 "father")
                            (equal z1 x2)) ;; Z = x2 => (sibling X Z), (father Z Y)
                   (let ((new (list "uncle" x1 y2)))
                     (unless (member new results :test #'equal)
                       (push new results)
                       (setf changed t)))))))))
    (remove-duplicates results :test #'equal)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 3) Tek Koşullu ve Çok Koşullu Sorgu Fonksiyonları
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun unify-query (relations query)
  "Örnek: (unify-query relations '((\"uncle\" \"X\" \"alice\"))) => X ler."
  (let ((solutions '()))
    (destructuring-bind (rel var val) (car query)
      (dolist (r relations)
        (destructuring-bind (rrel rx ry) r
          (when (and (equal rrel rel)
                     (equal ry val))
            (push rx solutions)))))
    (remove-duplicates solutions :test #'equal)))

(defun unify-query-multiple (relations queries)
  "queries: birden fazla koşulu test etmek.
   Her alt-sorgu unify-query ile çözülür; sonuçların kesişimi döndürülür."
  (let* ((solutions-of-subqueries
          (mapcar (lambda (q)
                    (unify-query relations (list q)))
                  queries)))
    (reduce (lambda (acc s)
              (intersection acc s :test #'equal))
            solutions-of-subqueries
            :initial-value (first solutions-of-subqueries))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 4) Test / Ana Program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* ((axioms '(
                 ;; Bazı temel ilişkiler
                 ("father" "bob"  "alice")
                 ("father" "alex"  "john")
                 ("mother" "mary" "jill")
                 ("father" "samm" "jim")
                 ("father" "jim"  "jill")
                 ("father" "samm"  "bob")
                 ;; sibling aksiyom
                 ("sibling" "samm" "bob")

                 ;; Sembolik kurallar (kullanılmıyor ama tutabiliriz)
                 ("ancestor" "X" "Y" "<" "parent"   "X" "Y")
                 ("uncle"    "X" "Y" "<" "sibling" "X" "Z" "father" "Z" "Y")
                 ("ancestor" "X" "Y" "<" "ancestor" "X" "Z" "ancestor" "Z" "Y")
                 ("parent"   "X" "Y" "<" "mother"   "X" "Y")
                 ("parent"   "X" "Y" "<" "father"   "X" "Y")
                 ))
       ;; Türetme
       (all-relations (prolog-prove axioms)))
  
  ;; 1) Tüm ilişkileri basalım
  (format t "~%All Family Relationships:~%")
  (dolist (relation all-relations)
    (format t "~S~%" relation))

  ;; 2) Tek sorgu => uncle(X, alice) => X?
  (format t "~%Single query: => X = ? ~%")
  (let ((sol (unify-query all-relations '(("ancestor" "X" "john")))))
    (cond (sol
           (dolist (s sol)
             (format t "  X = ~a~%" s)))
          (t (format t "  No solutions found.~%"))))

  ;; 3) Çoklu sorgu => hem ancestor(X, jill) hem father(X, bob) => X?
  (format t "~%Multiple queries: => X = ? ~%")
  (let ((sol2 (unify-query-multiple all-relations
                                    '(("ancestor" "X" "jill")
                                      ("father"   "X" "bob")))))
    (cond
      (sol2
       (dolist (s sol2)
         (format t "  X = ~a~%" s)))
      (t
       (format t "  No solutions found.~%")))))

