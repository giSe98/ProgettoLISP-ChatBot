
(defparameter h1 (make-hash-table))
(defparameter h2 (make-hash-table))
(defparameter h3 (make-hash-table))
(defparameter operaPrecedente nil)
(defparameter defaultAns '("L'opera" opera.nome "e' stata scritta a" opera.luogo "nel" opera.data) )

;riempimento per prova
;
;(setf (gethash 'dove h1) '("L'opera" opera.nome "e' stata scritta a" opera.luogo "nel" opera.data))
;(setf (gethash 'quando h1) '("L'opera" opera.nome "e' stata scritta a" opera.luogo "nel" opera.data))
(setf (gethash 'argomento h1) '("L'opera" opera.nome "tratta di" opera.tema))
(setf (gethash 'tema h1) '("L'opera" opera.nome "tratta di" opera.tema))
(setf (gethash 'chiave h1) '("L'opera" opera.nome "e' scritta in" opera.chiave))
(setf (gethash 'raccolta h1) '("L'opera" opera.nome "fa parte delle" opera.raccolta))
(setf (gethash '1 h2) '("Opera1" "2001" "Rende" "Chiave1" "Tema1" "Raccolta1"))
(setf (gethash '2 h2) '("Opera2" "12-10-2010" "Maropati"))
(setf (gethash '3 h2) '("Opera3" "13-10-2001" "Cosenza"))
(setf (gethash '4 h2) '("Opera4" "14-10-2001" "Rende"))
(setf (gethash 'racc1 h3) '(raccolta n one))
(setf (gethash 'racc2 h3) '(raccolta n two))
(setf (gethash 'racc3 h3) '(raccolta n three))
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

(defun f2 (input)

    ;booleani che usiamo anche come risultati, tanto qualsiasi cosa != da NIL è true.
    (setq b1 nil)
    (setq b2 nil)
    (setq b3 nil)


    (dolist (parola input)

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
      ((and b2 T) (setq operaPrecedente b2)) ;TODO vedere se è possibile eliminare il confronto

      ;bool1 && length(input)<2 && !bool2 && operaPrecedente!=null && !bool3 -> gli rispondo con l'opera precedente
      ;TODO non riconosce se uno scrive dove o cose del genere che abbiamo messo nella risposta di default
      ( (and  b1 (< (length input) 2) (null b2) operaPrecedente (null b3))
        (setq b2 operaPrecedente)
      )

      (T (setq operaPrecedente NIL))
    )

    (cond

        ;bool1 && bool2 -> rispondiamo con info su opera indicata
        ( (and b1 b2)
          (subst (car b2) 'opera.nome (subst (cadddr b2) 'opera.chiave (subst (cadr (cdddr b2)) 'opera.tema (subst (caddr (cdddr b2)) 'opera.raccolta b1))))
        )

        ;!bool2 && bool3 -> rispondiamo con info su raccolta indicata
        ( (and (null b2) b3)

        )

        ;!bool1 && bool2 -> (L'opera opera.nome è stata scritta a opera.luogo nel opera.data)
        ( (and (null b1)  b2 )
          (subst (car b2) 'opera.nome (subst (cadr b2) 'opera.data (subst (caddr b2) 'opera.luogo defaultAns)))
        )


        ;bool1 && (length(input)>1 ||operaPrecedente==null) && !bool2 && !bool3 -> "Mi dispiace, non conosco quest'opera"
        ;non si capisce l'or
        ( (and b1 (or (> (length input) 1) (null operaPrecedente)) (null b2) (null b3) )
          (list "Mi dispiace, non conosco quest'opera")
          )

        ;!bool1 && !bool2 && !bool3 -> "Ciao, puoi chiedermi informazioni su tutte le opere di Chopin"
        ( (and (null b1) (null b2 ) (null b3))
          (list "Ciao, puoi chiedermi informazioni su tutte le opere di Chopin")
        )
    )
)

(defun bot ()
    (write-line "Ciao, per abbandonare la chat scrivi \"q\"")
    (loop
        (princ ">>  ")
        (let* ((line (read-line))
            ;input è la lista che contiene le parole che compongono la stringa di input
            (input (read-from-string (concatenate 'string "(" line ")"))))
            (when (string-equal line "q") (return))
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

(bot)
