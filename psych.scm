;; This is the code for ``Computer Psychiatrist'' (Doctor)
#lang scheme
; for Racket users...
(#%require (only racket/base random))
; *******************
;Problem 4
;terminate with special name 'suppertime'
(define (visit-doctor)
  (let ((name (ask-patient-name))); map name with user input
    (
     cond ((equal? name 'suppertime) '(TIME TO GO HOME)) ;if the name is suppertime then terminate the program
     (else (write-line (list 'hello name))
     (write-line '(what seems to be the trouble?))
     (doctor-driver-loop name nil)
     (visit-doctor) ;start another round of visting
    )))
  )
  
;Problem 3 
(define (doctor-driver-loop name memory)
  (newline)
  (write '**)
  (let ((user-response (read)))
    ;(set! memory (append memory (list user-response)))
    (cond ((equal? user-response '(goodbye))
             (write-line (list 'goodbye name))
             (write-line '(see you next week)))
          (else (write-line (reply user-response (append memory (list user-response))))
                (doctor-driver-loop name (append memory (list user-response)))))))
;pick one sentence from memory and change the person words
(define (pick-memory memory)
  (change-person (list-ref memory (random (length memory))))
  )

;wrap the picked sentence with prefix
(define (memory-response memory)
  (append '(earlier you said that) (pick-memory memory))
  )

(define (reply user-response memory)
  (cond ((prob 10 100) (memory-response memory)) ; have 10% chance to use memory-response
        (else (cond ((fifty-fifty) (append (qualifier) (change-person user-response)))
                    (else (hedge)))))); qualifier and hedge will share the remain 50% chance equally

(define (fifty-fifty)
  (= (random 2) 0))

;Problem 1
(define (qualifier)
  (pick-random '((what's the reason makes you believe) ; my extension
                 (why do you think)                    ; my extension
                 (what makes you feel that)            ; my extension
                 (Do you have any reason to believe)   ; my extension
                 (It souds like you believe)           ; my extension 
                 (you seem to think)
                 (you feel that)
                 (why do you believe)
                 (why do you say))))

(define (hedge)
  (pick-random '((please go on)
                 (hmm, many people feel like you did)     ; my extension
                 (I can understand that kind of feeling ) ; my extension
                 (I am listening, please go on)           ; my extension
                 (Several patients met the same situation, and they feel just what you did) ;my extension
                 (many people have the same sorts of feelings)
                 (many of my patients have told me the same thing)
                 (please continue))))

(define (replace pattern replacement lst)
  (cond ((null? lst) '())
        ((equal? (car lst) pattern)
           (cons replacement
                 (replace pattern replacement (cdr lst))))
        (else (cons (car lst)
              (replace pattern replacement (cdr lst))))))

(define (many-replace replacement-pairs lst)
  (cond ((null? replacement-pairs) lst)
         (else (let ((pat-rep (car replacement-pairs)))
            (replace (car pat-rep)
                     (cadr pat-rep)
                     (many-replace (cdr replacement-pairs)
                     lst))))))
;problem 2 

;3 kinds of rules
(define you-to-i '((you i) (are am) (your my)))
(define i-to-you '((i you) (am are) (my your) (me you) (I you)))
(define mix-rules (append you-to-i i-to-you))

; The old one, never used anymore 
;(define (change-person phrase)
;  (many-replace i-to-you
;                phrase))

(define (pick-random lst)
  (nth (+ 1 (random (length lst))) lst))

;rule should be a pair, like (i you)
(define (match rule word)
  (cond ((equal? (car rule) word) #t)
        (else #f)
        )
  )
; give one word and use every rule in rules,which is a list to test if it match.
; The fucntion will return word itself if no rules matches, or return transferred word if it matches any rule.
; value returned will be wrapped as a list, makes it easier to append to list (I just don't like cons)
(define (transfer-word rules word)
  (cond ((null? rules) (list word))
        ((match (car rules) word) (cdr (car rules))) ;if word match return the transferred word
        (else (transfer-word (cdr rules) word))
   )
  )

; translate the sentence word by word
(define (transfer-sentence rules sentence)
  (cond ((null? sentence) sentence) ;if sentence is empty return null
         (else (append (transfer-word rules (car sentence)) (transfer-sentence rules (cdr sentence)))); transfer the current word and glue it with the rest of sentence which is transferred already
        )
  )
; wrap my function to the interface of doctor system, hide the details of rules
(define (change-person phrase)
  (transfer-sentence mix-rules phrase)
  )

;This is a test for transfer-word
;(transfer-word i-to-you 'iw)

;this is a test for transfer-sentence
;(change-person '(you are not being very helpful to me))
;(change-person '(you say i am not you))
;;******

(define (prob n1 n2)
  (< (random n2) n1))

(define (ask-patient-name)
  (write-line '(next!))
  (write-line '(who are you?))
  (car (read)))

(define (nth n lst)
  (if (= n 1) 
      (car lst)
      (nth (- n 1) (cdr lst))))
;;******

(define (atom? a) (not (pair? a)))

(define nil '())

(define (write-line x) (begin (write x) (newline)))

;;******
