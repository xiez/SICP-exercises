;;; EBNF grammar
;;; sentence = <noun-phrase>, <verb-phrase>;
;;; noun-phrase = <simple-noun-phrase> | <simple-noun-phrase>, {<prep-phrase>};
;;; verb-phrase = verb | verb, {<prep-phrase>};
;;; prep-phrase = preposistion, <noun-phrase>;
;;; simple-noun-phrase = article, noun;

;;; noun = student | professor | cat | class;
;;; verb = studies | lectures | eats | sleeps;
;;; article = the | a;
;;; preposistion = for | to | in | by | with;

(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))
(define preposistions '(prep for to in by with))

(define (parse-sentence)
  (list 'sentence (parse-noun-phrase) (parse-verb-phrase)))
(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend
          (list 'noun-phrase noun-phrase (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))
(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend
          (list 'verb-phrase verb-phrase (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))
(define (parse-prepositional-phrase)
  (list 'prep-phrase (parse-word preposistions) (parse-noun-phrase)))
(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase (parse-word articles) (parse-word nouns)))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

(define *unparsed* '())
(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*))
    sent))

;;; exer 4.45
;;; the professor lectures to the student in the class with the cat.
;;; 1. the professor, in the class with the cat, lectures to the student.
;;; 2. the professor, with the cat, lectures to the student in the class.
;;; 3. the professor lectures to the student in the class with the cat.
;;; 4. the professor, with the cat, lectures to the student, in the class.
;;; 5. the professor, in the class, lectures to the student, with the cat.

;;; exer 4.46
;; (parse-sentence) has to start with (parse-noun-phrase) first, since (parse-word) is a left-to-right parser.

;;; exer 4.47
;; TODO

;;; exer 4.48
;; TODO
