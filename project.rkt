#lang racket
(require data/either)

;sends a list of lines to be tokenized
(define (parse file-name)
  ;turns file in to a list of lists with each line being a list
  (tokenize-file (file->lines file-name)))

;maps each line to the tokenize function and then puts the output into the first parse production
(define (tokenize-file file-lines)
  ;NOTE: the "##" is appended to the end as an eof marker for the parsing process
  ;this is used as a precaution in the event that "$$" is not put at the end of the file - the absense of "$$" results in contract violations within the code 
  (let ([output (append (map tokenize-line file-lines) '(("##")))])
    (program output)))

;tokenizes each line based off regular expression
(define (tokenize-line file-line)
  (regexp-match* #rx"[A-Za-z][A-Za-z0-9]*|[0-9]+|([<>]=*)|\\$\\$|[^ ]" file-line))

;program -> linelist $$ 
(define (program input)
  (let ([output (linelist input)])
    ;the $$ symbol will actually be returned up from failing a check in the stmt production
    (if (equal? (first (first (from-either output))) "$$")
        (printf "Accept")
        ;error line calculated by subtracting the original line list size from the returned line list size
        (let ([error-line (+(-(length input) (length (from-either output))) 1)][error-type (first (first (from-either output)))])
          (if (not (equal? (first (first (from-either output))) "##"))
              (printf "Error line ~a: ~a" error-line error-type)
              (printf "Error line ~a: ~a" error-line "expected: $$"))))))

;linelist -> line linelist | epsilon
(define (linelist input)
  (let ([output (line input)])
    (if(success? output)
       (linelist (from-either output));recursively calls itself until a call to the line function fails
       output)))

;line ->  label stmt linetail 
(define (line input)
  (let ([output (label input)])
    (if(success? output)
       (let ([output2 (stmt (from-either output))]);"label"
         (if(success? output2)
            (linetail (from-either output2));"label stmt linetail"
            output2));"label" but not "stmt"
       (failure input))));not "label"

;label -> id: | epsilon 
(define (label input)
  (if (success? (id input))
      (let ([output (id input)]);"id"
        (if (success? output)
            (if (equal? (first (first (from-either output))) ":")
                (success (trim-list (from-either output)));"id:"
                (success input));epsilon
            (success input)));epsilon
      (success input)));epsilon

;linetail -> ;stmt+ | epsilon 
(define (linetail input)
  (if (equal? (first (first input)) ";")
      (let ([output (trim-list input)]);";"
        (let ([output2 (stmt output)])
          (if (success? output2)
              (success (multiple-stmt (from-either output2)));"; stmt+"
              output2)));";" but not "stmt"
      (success input)));epsilon

;used by the linetail function to recursively remove stmt if there are more than one
(define (multiple-stmt input)
  (let ([output (stmt input)])
    (if (success? output)
        (multiple-stmt (from-either output));recursive case
        input)));base case

;stmt
(define (stmt input)
  (cond
    [(success? (id input))
     ;stmt -> id = expr 
     (let ([output (id input)]);"id"
       (if (equal? (first (first (from-either output))) "=")
           (expr (trim-list (from-either output)));"id = expr"
           (format-error input "expected: = given: " #t)))];"id" but not "="
    ;stmt -> if (boolean) stmt
    [(equal? (first (first input)) "if")
     (let ([output (trim-list input)]);"if"
       (if (regexp-match-exact? #rx"[/(]" (first (first output)))
           (let ([output2 (trim-list output)]);"if ("
             (let ([output3 (boolean output2)])
               (if (success? output3)
                   (if (regexp-match-exact? #rx"[/)]" (first (first  (from-either output3))));"if (boolean"
                       (stmt (trim-list (from-either output3)));"if (boolean) stmt"
                       (format-error (from-either output3) "expected: ) given: " #t));"if (boolean" but not ")"
                   output3)));"if (" but not boolean
           (format-error output "expected:( given: " #t)))]
    ;stmt -> while (boolean) linelist endwhile
    [(equal? (first (first input)) "while")
     (let ([output (trim-list input)]);"while"
       (if (regexp-match-exact? #rx"[/(]" (first (first output)))
           (let ([output2 (trim-list output)]);"while ("
             (let ([output3 (boolean output2)])
               (if (success? output3)
                   (if (regexp-match-exact? #rx"[/)]" (first (first (from-either output3))));"while (boolean"
                       (let ([output4 (trim-list (from-either output3))]);"while (boolean)"
                         (let ([output5 (success (from-either (linelist output4)))]);"while (boolean) linelist"
                           (if(equal? (first (first (from-either output5))) "endwhile")
                              (success (trim-list (from-either output5)));"while (boolean) linelist endwhile"
                              (format-error (from-either output5) "expected: endwhile given: " #t))));"while (boolean) linelist endwhile"
                       (format-error (from-either output3) "expected: ) given: " #t))
                   output3)));"while (" but not "boolean"        
           (format-error output "expected:( given: " #t)))]
    ;stmt -> write expr
    [(equal? (first (first input)) "write")
     (expr (trim-list input))]
    ;stmt -> read id | goto id | gosub id
    [(member (first (first input))`("read" "goto" "gosub"))
     (id (trim-list input))]
    ;stmt -> | return | break | end
    [(member (first (first input))`("return" "break" "end"))
     (success (trim-list input))]
    ;the tokens "endwhile" and "$$" can fail here but it is not a true error so the error message is not added
    [else (format-error input "expected: stmt given: " #f)]))

;boolean -> true | false | expr bool-op expr 
(define (boolean input)
  (cond
    [(equal? (first (first input)) "true")
     (success (trim-list input))]
    [(equal? (first (first input)) "false")
     (success (trim-list input))]
    [(success? (expr input))
     (let ([output (expr input)]);"expr"
       (let ([output2 (bool-op (from-either output))])
         (if(success? output2);"expr bool-op"
            (expr (from-either output2));"expr bool-op expr"
            output2)))];"expr" but not "bool-op"
    [else  (format-error input "expected: true | false | expr bool-op expr given: " #t)]));not boolean

;bool-op -> < | > | >= | <= | <> | =
(define (bool-op input)
  (if (member (first (first input)) `("<" ">" "=" "<=" ">=" "<>"))
      (success (trim-list input))
      (format-error input "expected: < | > | >= | <= | <> | = given: " #t)))

;I can reduce this as well

;expr -> id etail | num etail | (expr) 
(define (expr input)
  (cond
    [(success? (id input))
     (let ([output (id input)]);"id"
       (etail (from-either output)))];"id etail"
    [(success? (num input));"num"
     (let ([output (num input)]);"num etail"
       (etail (from-either output)))]
    [(regexp-match-exact? #rx"[/(]" (first (first input)))
     (let ([output (trim-list input)]);"("
       (let ([output2 (expr output)])
         (if (success? output2)
             (if (regexp-match-exact? #rx"[/)]" (first (first (from-either output2))));"(expr"
                 (success (trim-list (from-either output2)));"(expr)"
                 (format-error (failure (from-either output2)) "expected: ) given: " #t));"(expr" but not ")"
             output2)))];"(" but not "expr"
    [else (format-error input "expected: expr given: " #t)]));not an expr

;etail -> + expr | - expr | * expr | / expr | epsilon
(define (etail input)
  (if (regexp-match-exact? #rx"[+-/*//]" (first (first input)))
      (let ([output (trim-list input)]);"+-/*"
        (let ([output2 (expr output)])
          (if (success? output2)
              output2;"+-/* expr"
              (success input))));"+-/*" but not "expr"
      (success input)));epsilon

;id -> [a-zA-Z][a-zA-Z0-9]*
(define (id input)
  (if (and (regexp-match-exact? #rx"[A-Za-z][A-Za-z0-9]*" (first (first input))) (find-keyword input))
      (success (trim-list input));"id"
      (format-error input "invalid id expected: [a-zA-Z][a-zA-Z0-9]* given: " #t)));not "id"

;num -> numsign digit digit*
(define (num input)
  (let ([output (numsign input)])
    (if (success? output)
        (let ([output2 (digit (from-either output))]);"numsign"
          (if (success? output2)
              (success (multiple-digits (from-either output2)));"numsign digit"
              (failure (from-either output))));"numsign" but not "digit"
        (failure input))));not "numsign"

;numsign -> + | - | epsilon 
(define (numsign input)
  (if (regexp-match-exact? #rx"[+-]" (first (first input)))
      (success (trim-list input));"+-"
      (success input)));epsilon

;digit -> [0-9]
(define (digit input)
  (if (regexp-match-exact? #rx"[0-9]+" (first (first input)))
      (success (trim-list input));"digit"
      (format-error input "invalid digit expected: [0-9] given: " #t)));not "digit"

;removes the first token of the list in the first position of the list of lists
;the list in the first position of the list of lists is removed if the last token in it is removed
(define (trim-list input)
  (if(= (length (first input)) 1)
     (rest input)
     (list-set input 0 (rest (first input)))))

;used by the num functiion to recursively remove digits if there are more than one
(define (multiple-digits input)
  (let ([output (digit input)])
    (if (success? output)
        (multiple-digits (from-either output))
        input)))

;alters the first token of the list in the first position of the list of lists to include an error message along with the token
;overide is a boolean put in place to prevent "$$" and "endwhile" from producing a true error in certain cases
;"$$" and "endwhile"
(define (format-error input errormsg override)
  (if(or (not (member (first (first input)) `("$$" "endwhile" "##"))) override)
     (failure (list-set input 0 (list-set (first input) 0 (string-append errormsg (first(first input))))))
     (failure input)))

;exception rule which makes a token invalid as an id if it is a keyword
(define (find-keyword input)
  (not (member (first (first input)) `("if" "while" "goto" "gosub" "true" "false" "read" "write" "return" "break" "end" "endwhile"))))