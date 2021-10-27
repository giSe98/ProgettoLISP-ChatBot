

(defun bot ()
    (write-line "Ciao, per abbandonare la chat scrivi \"q\"")
    (loop
        (princ ">>  ")
        (let* ((line (read-line))
            ;input è la lista che contiene le parole che compongono la stringa di input
            (input (read-from-string (concatenate 'string "(" line ")")))) 
            (when (string-equal line "q") (return))
            (setq *bindings* nil)
            (format t "~{~(~a ~)~}~%"
             (dolist (r *rules*)
               (when (match (car r) input)
                 (return 
                  (subs (random-elt (cdr r))))))))))

(bot)