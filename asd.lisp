(defparameter h1 (make-hash-table))
(defparameter h2 (make-hash-table))
(defparameter h3 (make-hash-table))
(defparameter operaPrecedente nil)
(defparameter defaultAns '("L'opera" opera.id "e'" opera.nome) )

;riempimento per prova
;


;(setf (gethash 'dove h1) '("L'opera" opera.nome "e' stata scritta a" opera.luogo))
(setf (gethash 'quando h1) '("L'opera" opera.nome "e' stata scritta nel" opera.data))
;(setf (gethash 'argomento h1) '("L'opera" opera.nome "tratta di" opera.tema))
;(setf (gethash 'tema h1) '("L'opera" opera.nome "tratta di" opera.tema))
(setf (gethash 'chiave h1) '("L'opera" opera.nome "e' scritta in" opera.chiave))
(setf (gethash 'raccolta h1) '("L'opera" opera.id "fa parte" opera.raccolta))
(setf (gethash '1 h2) '("Opera1" "2001" "Chiave1" "Raccolta1" "id1"))
(setf (gethash '2 h2) '("Opera2" "12-10-2010" "Maropati"))
(setf (gethash '3 h2) '("Opera3" "13-10-2001" "Cosenza"))
(setf (gethash '4 h2) '("Opera4" "14-10-2001" "Rende"))

;(setf (gethash 'mazurche h3) '("Le Mazurche sono 10 opere scritte per i polacchi bastardi"))
;(setf (gethash 'racc2 h3) '(raccolta n two))
;(setf (gethash 'racc3 h3) '(raccolta n three))
;(setf (gethash 'piace h3) '("Mi piace tutto il repertorio di Chopin"))
;
;fine prova
;
;stampa di prova
;
;(write (gethash 'dove h1))
;(terpri)
;(write (gethash '1 h2))
;(terpri)


(defun string-to-list (str)
        (if (not (streamp str))
           (string-to-list (make-string-input-stream str))
           (if (listen str)
               (cons (read str) (string-to-list str))
               nil)))

(defun loadCsv (csv hashmap)
  (dolist (row csv)
    (setf (gethash (car (string-to-list (car row))) hashmap) (cdr row))
  )
)

(defun f2 (input)

    ;booleani che usiamo anche come risultati, tanto qualsiasi cosa != da NIL è true.
    (setq b1 nil)
    (setq b2 nil)
    (setq b3 nil)
    (setq bOp nil)

    (dolist (parola input)
        (cond 
          (
            (and (not(numberp parola)) (or (string-equal "op" (string parola)) (string-equal "opera" (string parola))) ) 
            (setq bOp T)
          )
        )
        (cond ((null b1)
        ;se b1 è null, facciamo il lookup in h1
            (setq tmp1 (gethash parola h1))
            (cond ((not (null tmp1)) (setq b1 tmp1))   )
            )

        )

        (cond ((null b2)
        ;se b2 è null, facciamo il lookup in h2
            (setq tmp2 (gethash parola h2))
            (cond ((not (null tmp2)) (setq b2 tmp2))   )
            )

        )

        (cond ((null b3)
        ;se b3 è null, facciamo il lookup in h3
            (setq tmp3 (gethash parola h3))
            (cond ((not (null tmp3)) (setq b3 tmp3))  )
            )

        )
    )
    (list b1 b2 b3)


)



;ve l'abbiamo iniziata noi perché l'avevamo ormai fatto, ma ci siamo resi conto
;dopo che serve farlo in f3

(defun f3 (input l)
    ;(print (list b1 b2 b3))
    ;a questo punto abbiamo b1, b2, b3
    (cond
      (b2 (setq operaPrecedente b2)) ;TODO vedere se è possibile eliminare il confronto

      ;bool1 && length(input)<2 && !bool2 && operaPrecedente!=null && !bool3 -> gli rispondo con l'opera precedente
      ;TODO non riconosce se uno scrive dove o cose del genere che abbiamo messo nella risposta di default
      ( (and  b1  (null b2) operaPrecedente (null b3))
        (setq b2 operaPrecedente)
      )

      (T (setq operaPrecedente NIL))
    )

    (cond

        ;bool1 && bool2 -> rispondiamo con info su opera indicata
        ;TODO SE CAMBIA COL CSV CAMBIARE QUI
        ( (and b1 b2 bOp)
          (subst (car b2) 'opera.nome (subst (caddr b2) 'opera.chiave (subst (cadddr b2) 'opera.raccolta (subst (cadr (cdddr b2)) 'opera.id (subst (cadr b2) 'opera.data b1)))))
        )

        ;!bool2 && bool3 -> rispondiamo con info su raccolta indicata
      ( (and (null b2) b3)
        b3
       )

        ;!bool1 && bool2 -> (L'opera opera.nome è stata scritta a opera.luogo nel opera.data)
        ( (and (null b1)  b2 bOp)
          (subst (car b2) 'opera.nome (subst (cadr (cdddr b2)) 'opera.id defaultAns))
        )


        ;bool1 && (length(input)>1 ||operaPrecedente==null) && !bool2 && !bool3 -> "Mi dispiace, non conosco quest'opera"
        ;non si capisce l'or
        ( (and (or b1 bOp) (null operaPrecedente) (null b2) (null b3) )
          (list "Mi dispiace, non conosco quest'opera")
          )

        ;!bool1 && !bool2 && !bool3 -> "Ciao, puoi chiedermi informazioni su tutte le opere di Chopin"
        ( T 
          (list "Ciao, puoi chiedermi informazioni su tutte le opere di Chopin")
        )
    )
)

(defun bot ()
    (write-line "Ciao, per abbandonare la chat scrivi \"q\"")
    (loop
        (princ ">>  ")
        (let* ((line (read-line))

            (lineP (deleteP line))
            ;input è la lista che contiene le parole che compongono la stringa di input
            (input (read-from-string (concatenate 'string "(" lineP ")"))))
            (when (string-equal lineP "q") (return))
            ; qui chiamata f1
            (format t "~{~a~^ ~}" (f3 input (f2 input)))
            (terpri)

            ; qui chiamata f3 che sostituisce la porcheria che sta sotto (commentata)
            ;(setq *bindings* nil)
            ;(format t "~{~(~a ~)~}~%"
            ;(dolist (r *rules*)
            ;   (when (match (car r) input)
            ;     (return
            ;     (subs (random-elt (cdr r)))))))
        )
    )
)

(defun deleteP (string)
  (setq result "")
  (
    dotimes (i (length string))
      (
        cond
          ( (or (string= (char string i) #\') (string= (char string i) #\") (string= (char string i) #\,) (string= (char string i) #\.) (string= (char string i) #\!) (string= (char string i) #\?) (string= (char string i) #\:) (string= (char string i) #\;))
            (
              cond
                ( (and (/= i (- (length string) 1)) (string/= (char string (+ i 1)) " ")) (setq result (concatenate 'string result " ")) )
            )
          )
          ((= i (- (length string) 1)) (cond ((string/= (char string i) " ") (setq result (concatenate 'string result (string (char string i)))))))
          (t (setq result (concatenate 'string result (string (char string i)))))
      )
  )
  result
)


;load libreria
(load "C:/Users/mirie/quicklisp/setup.lisp")
(ql:quickload "cl-csv" :silent t)

;read csv
(setq csvRaccolte (cdr (cl-csv:read-csv #P"./raccolte2")))
(setq csvOpere (cdr (cl-csv:read-csv #P"./opere.csv")))

;load hashmap
(loadCsv csvRaccolte h3)
(loadCsv csvOpere h2)
;(write h2)

(bot)