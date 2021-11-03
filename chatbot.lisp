(defparameter h1 (make-hash-table))
(defparameter h2 (make-hash-table))
(defparameter h3 (make-hash-table))
(defparameter operaPrecedente nil)

(defparameter defaultAns '("L'opera" opera.id "di Chopin e'" opera.nome) )


(setf (gethash 'quando h1) '("L'opera" opera.nome "e' stata scritta nel" opera.data))
(setf (gethash 'anno h1) '("L'opera" opera.nome "e' stata scritta nel" opera.data))
(setf (gethash 'risale h1) '("L'opera" opera.nome "e' stata scritta nel" opera.data))
(setf (gethash 'risalente h1) '("L'opera" opera.nome "e' stata scritta nel" opera.data))
(setf (gethash 'data h1) '("L'opera" opera.nome "e' stata scritta nel" opera.data))
(setf (gethash 'momento h1) '("L'opera" opera.nome "e' stata scritta nel" opera.data))
(setf (gethash 'movimento h1) '("Il tempo dell'opera" opera.nome "e'" opera.movimento))
(setf (gethash 'tempo h1) '("Il tempo dell'opera" opera.nome "e'" opera.movimento))
(setf (gethash 'agogica h1) '("Il tempo dell'opera" opera.nome "e'" opera.movimento))
(setf (gethash 'ritmo h1) '("Il tempo dell'opera" opera.nome "e'" opera.movimento))
;(setf (gethash 'argomento h1) '("L'opera" opera.nome "tratta di" opera.tema))
(setf (gethash 'chiave h1) '("L'opera" opera.nome "e' scritta in" opera.chiave))
(setf (gethash 'tonalità h1) '("L'opera" opera.nome "e' scritta in" opera.chiave))
(setf (gethash 'tonalita h1) '("L'opera" opera.nome "e' scritta in" opera.chiave))
(setf (gethash 'chiave h1) '("L'opera" opera.nome "e' scritta in" opera.chiave))
(setf (gethash 'raccolta h1) '("L'opera" opera.id "di Chopin fa parte" opera.raccolta))
(setf (gethash 'parte h1) '("L'opera" opera.id "di Chopin fa parte" opera.raccolta))
(setf (gethash 'categoria h1) '("L'opera" opera.id "di Chopin fa parte" opera.raccolta))
(setf (gethash 'piace h1) '("Mi piace l'opera" opera.id "di Chopin"))
(setf (gethash 'pianoforte h1) '("Chopin ha composto quasi esclusivamente per pianoforte"))
(setf (gethash 'strumento h1) '("Chopin ha composto quasi esclusivamente per pianoforte"))


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
    (print b2)
    (terpri)
    (print operaPrecedente)
    (cond

        ;bool1 && bool2 -> rispondiamo con info su opera indicata
        ;TODO SE CAMBIA COL CSV CAMBIARE QUI
        ( (and b1 b2 (or bOp operaPrecedente))
          (subst (car b2) 'opera.nome (subst (caddr b2) 'opera.chiave (subst (cadddr b2) 'opera.raccolta (subst (cadr (cdddr b2)) 'opera.id (subst (cadr b2) 'opera.data (subst (caddr (cdddr b2)) 'opera.movimento b1))))))
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
        ( T (list "Ciao, puoi chiedermi informazioni su tutte le opere di Chopin. Ricordati che Chopin non da un titolo alle proprie opere, quindi dimmi il numero dell'opera")
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
            (format t "~{~a~^ ~}" (f3 input (f2 input)))
            (terpri)
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
(setq csvOpere (cdr (cl-csv:read-csv #P"./opere1")))

;load hashmap
(loadCsv csvRaccolte h3)
(loadCsv csvOpere h2)
;(write h2)

(bot)