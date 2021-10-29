(defun deleteP (string)
  (setq result "")
  (
    dotimes (i (length string))
      (
        cond
          ( (or (string= (char string i) #\,) (string= (char string i) #\.) (string= (char string i) #\!) (string= (char string i) #\?) (string= (char string i) #\:) (string= (char string i) #\;)) 
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