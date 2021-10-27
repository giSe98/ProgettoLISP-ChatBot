
(defparameter h1 (make-hash-table))
(defparameter h2 (make-hash-table))
(defparameter h3 (make-hash-table))
(defparameter operaPrecedente nil)
(defparameter defaultAns '(L'opera opera.nome è stata scritta a opera.luogo nel opera.data) )

;riempimento per prova
;
(setf (gethash 'dove h1) '(L'opera opera.nome è stata scritta a opera.luogo nel opera.data))
(setf (gethash 'quando h1) '(L'opera opera.nome è stata scritta a opera.luogo nel opera.data))
(setf (gethash 'argomento h1) '(L'opera opera.nome tratta di opera.tema))
(setf (gethash 'tema h1) '(L'opera opera.nome tratta di opera.tema))
(setf (gethash '1 h2) '(Opera1 "10-10-2001" "Rende" ))
(setf (gethash '2 h2) '(Opera2 "12-10-2010" "Maropati" ))
(setf (gethash '3 h2) '(Opera3 "13-10-2001" "Cosenza" ))
(setf (gethash '4 h2) '(Opera4 "14-10-2001" "Rende" ))
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

    ;a questo punto abbiamo b1, b2, b3
    (cond ( (not (null b2)) (setq operaPrecedente b2) )  )

    
    (cond 
        ;bool1 && bool2 -> rispondiamo con info su opera indicata
        ( (and (not (null b1)) (not (null b2)) )  (<COSTRUIRE RISPOSTA CON B1 E B2>) )

        ;!bool2 && bool3 -> rispondiamo con info su raccolta indicata
        ( (and (null b2) (not (null b3)) ) (<COSTRUIRE RISPOTA CON B3>) ) 

        ;!bool1 && bool2 -> (L'opera opera.nome è stata scritta a opera.luogo nel opera.data) 
        ( (and (null b1) (not (null b2)) ) (<COSTRUIRE RISPOSTA CON defaultAns e B2>) )
        
        ;bool1 && length(input)=1 && !bool2 && operaPrecedente!=null && !bool3 -> gli rispondo con l'opera precedente
        ( (and (not (null b1)) (= 1 (length input)) (null b2) (not (null operaPrecedente)) (null b3)) (<COSTRUIRE RISPOSTA CON operaPrecedente>) )
    
        ;bool1 && (length(input)>1 ||operaPrecedente==null) && !bool2 && !bool3 -> "Mi dispiace, non conosco quest'opera"
        ( (and (not (null b1)) (or (> 1 (length input)) (null operaPrecedente)) (null b2) (null b3) ) (<COSTRUIRE RISP MI DISPIACE NON CONOSCO QUEST'OPERA>))
        
        ;!bool1 && !bool2 && !bool3 -> "Ciao, puoi chiedermi informazioni su tutte le opere di Chopin"
        ( (and (null b1) (null b2 ) (null b3)) (COSTRUIRE RISPOSTA "Ciao, puoi chiedermi informazioni su tutte le opere di Chopin") )
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
            (write (f3 input (f2 input)))
            

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