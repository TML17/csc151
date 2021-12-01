#lang racket

(require csc151)
(require csc151/rex)
(require csc151/verbose-bindings)
(require rackunit)
(require csc151www)

;; CSC 151.02 (Fall 2021)
;; Mini-Project 7: Transforming Web Pages
;; Authors: Tommy Liu
;; Date: 11/16/2021
;; Acknowledgements:

;; (compute-dale-chall-score num-difficult-words total-words num-sentences)-->num
;; num-difficult-words : integer?
;; total-words : integer?
;; num-sentences : integer?
;; calculate the readability of prose based on the Dale-Chall readability formula
(define compute-dale-chall-score
  (lambda (num-difficult-words total-words num-sentences)
    (let* ([ASL (/ total-words num-sentences)]
           [PDW (/ num-difficult-words total-words)]
           [score (+ (* 0.1579 (* PDW 100)) (* 0.0496 ASL))])
      (if (> PDW 0.05)
          (+ 3.6365 score)
          score))))

;; (score->grade score)-->string?
;; score : integer?
;; takes in a grade and export its grade according to the gradebook
(define score->grade
  (lambda (score)
    (cond
      [(< score 5)
       "4th grade or lower"]
      [(< score 6)
       "5th-6th grade"]
      [(< score 7)
       "7th-8th grade"]
      [(< score 8)
       "9th-10th grade"]
      [(< score 9)
       "11th-12th grade"]
      [else
       "13th-15th grade"]
      )))

;;; (extract-words str)
;;;   str : string?
;;; Extract all of the words from str.  A word is a sequence of
;;; letters and apostrophes.
(define extract-words
  (let ([rex-word
         (rex-repeat (rex-any-of (rex-char-range #\a #\z)
                                 (rex-char-range #\A #\Z)
                                 (rex-string "'")))])
    (lambda (str)
      (rex-find-matches rex-word str))))

;; (read-easy-word)-->string?
;; read in the esay word document and save it as a string
(define read-easy-word
  (extract-words (file->string "easy-words.txt")))

;; (difficult-word? str)-->boolean?
;; str : string?
;; decide whether a single string is a difficult word
(define difficult-word?
  (lambda (str)
    (equal? 0 (tally-value read-easy-word (string-downcase str)))))

;; (num-difficult-word?)-->integer?
;; str : string?
;; count the number of difficult words in an input string
(define num-difficult-word?
  (lambda (str)
    (tally-value (map (section difficult-word? <>) (string-split str)) #t)))

;; (num-sentence?)-->integer?
;; str : string?
;; count the number of sentences in an input string
(define num-sentence?
  (lambda (str)
    (length (rex-split-string (rex-any-of (rex-string "!")
                                          (rex-string ".")
                                          (rex-string "?")) str))))

;; (num-word?)-->integer?
;; str : string?
;; count the number of total words in an input string
(define num-word?
  (lambda (str)
    (length (string-split str))))

;;(dale-chall-score str)-->string?
;; str : string?
;; it takes in an non-empty string and exports string description by score->grade
(define dale-chall-score
  (lambda (str)
    (compute-dale-chall-score (num-difficult-word? str) (num-word? str) (num-sentence? str))))

;;; (add-dc-info-string infile outfile)
;;; infile : file?
;;; outfile : file?
;;; takes two file names as parameters, reads an HTML file from the infile and gets the content as a string
(define add-dc-info-string
  (lambda (infile outfile)
    (let ([add-dc-info-helper (let ([input (xml->string (file->xml infile))])
                                (rex-split-string (rex-concat (rex-string "<")
                                                              (rex-concat (rex-repeat-0 (rex-string "/"))
                                                                          (rex-repeat(rex-char-range #\a #\z)))
                                                              (rex-string ">")) input))])
      (apply string-append add-dc-info-helper))))

;;; (add-dc-info infile outfile)
;;; infile : file?
;;; outfile : file?
;;; takes two file names as parameters, reads an HTML file from the infile, adds some preliminary information to the start of the body, and writes the result to outfile
(define add-dc-info
  (lambda (infile outfile)
    (let ([result (add-dc-info-string infile outfile)])
      (let ([str1 (string-append "Number of words: "
                                 (number->string (num-word? result))
                                 " ("
                                 (number->string (num-difficult-word? result))
                                 " easy, "
                                 (number->string (- (num-word? result) (num-difficult-word? result)))
                                 " difficult) \n")]
            [str2 (string-append "Number of sentences: "
                                 (number->string (num-sentence? result))
                                 "\n")]
            [str3 (string-append "Given a score of "
                                 (number->string (dale-chall-score result))
                                 ", this document is appropriate for a "
                                 (score->grade (dale-chall-score result))
                                 "audience. \n")])
        (string->file (string-append str1 str2 str3 result) outfile)
        ))))

(define find-format-without-id
  (lambda (str)
    (rex-find-matches (rex-concat (rex-string "<h")
                                  (rex-char-set "123")
                                  (rex-string ">")
                                  (rex-repeat (rex-any-of (rex-char-range #\a #\z)
                                                          (rex-char-range #\A #\Z)
                                                          (rex-char-set ",./';[]!@#$%^&*()_+= ?><")))
                                  (rex-string "</h")
                                  (rex-char-set "123")
                                  (rex-string ">")) str)))

(define find-format-with-id
  (lambda (str)
    (rex-find-matches (rex-concat (rex-string "<h")
                                  (rex-char-set "123")
                                  (rex-string " id=")
                                  (rex-repeat (rex-any-char))
                                  (rex-string ">")
                                  (rex-repeat (rex-any-of (rex-char-range #\a #\z)
                                                          (rex-char-range #\A #\Z)
                                                          (rex-char-set ",./';[]!@#$%^&*()_+= ?")))
                                  (rex-string "</h")
                                  (rex-char-set "123")
                                  (rex-string ">")) str)))

(define find-format-all
  (lambda (str)
    (rex-find-matches (rex-any-of (rex-concat (rex-string "<h")
                                              (rex-char-set "123")
                                              (rex-string " id=")
                                              (rex-repeat (rex-any-char))
                                              (rex-string ">")
                                              (rex-repeat (rex-any-of (rex-char-range #\a #\z)
                                                                      (rex-char-range #\A #\Z)
                                                                      (rex-char-set ",./';[]!@#$%^&*()_+= ?")))
                                              (rex-string "</h")
                                              (rex-char-set "123")
                                              (rex-string ">"))
                                  (rex-concat (rex-string "<h")
                                              (rex-char-set "123")
                                              (rex-string ">")
                                              (rex-repeat (rex-any-of (rex-char-range #\a #\z)
                                                                      (rex-char-range #\A #\Z)
                                                                      (rex-char-set ",./';[]!@#$%^&*()_+= ?><")))
                                              (rex-string "</h")
                                              (rex-char-set "123")
                                              (rex-string ">"))) str)))

(define document-id (make-hash))

(define add-hash-without
  (lambda (hash-table lst)
    (if (null? lst)
        hash-table
        (when #t
          (hash-set! hash-table (substring (car lst) 4 (- (string-length (car lst)) 5)) (random 100000 1000000))
          (add-hash-without hash-table (cdr lst))))))

(define add-hash-with
  (lambda (hash-table lst)
    (let ([splitter (list-ref (string-split "<h2 id=\"conclusion\">Conclusion</h2>" ">") 1)])
      (if (null? lst)
          hash-table
          (when #t
            (hash-set! hash-table (list-ref (string-split (car lst) "\"") 1) (substring splitter 0 (- (string-length splitter) 4)))
            (add-hash-with hash-table (cdr lst)))))))

(define example-list
  (find-format-all (xml->string (file->xml "toc-test-input.txt"))))

(define make-tabs
  (lambda (num)
    (apply string-append (make-list num "\t"))))

;;; (add-hash-with document-id (find-format-with-id (xml->string (file->xml "toc-test-input.txt"))))
;;; (add-hash-without document-id (find-format-without-id (xml->string (file->xml "toc-test-input.txt"))))
;;; (generate-body (find-format-all (xml->string (file->xml "toc-test-input.txt"))) 1 document-id)
;;; default value for count is 1
(define generate-body
  (lambda (lst count hash-table)
    (if (null? lst)
        (string)
        (if (> (- (char->integer (string-ref (car lst) 2)) 47) count)
            (if (null? (rex-find-matches (rex-string "id") (car lst)))
                (string-append (make-tabs count) "<li><a href=" "#element"
                               (number->string (hash-ref hash-table (substring (car lst) 4 (- (string-length (car lst)) 5))))
                               ">" (substring (car lst) 4 (- (string-length (car lst)) 5)) "</a></li>\n" (make-tabs count) "<ul>\n" (generate-body (cdr lst) (+ 1 count) hash-table))
                (string-append (make-tabs count) "<li><a href=" "#"
                               (hash-ref hash-table (list-ref (string-split (car lst) "\"") 1))
                               ">" (list-ref (string-split (car lst) "\"") 1) "</a></li>\n" (make-tabs count) "<ul>\n" (generate-body (cdr lst) (+ 1 count) hash-table)))    
            (string-append (make-tabs (- count 1)) "<li><a href=" "#element"
                           (if (null? (rex-find-matches (rex-string "id") (car lst)))
                               (number->string (hash-ref hash-table (substring (car lst) 4 (- (string-length (car lst)) 5))))
                               (hash-ref hash-table (list-ref (string-split (car lst) "\"") 1) ))
                           ">" (substring (car lst) 4 (- (string-length (car lst)) 5)) "</a></li>\n" (generate-body (cdr lst) 1 hash-table))))))

;;; add-toc infile outfile)
;;; infile : file?
;;; outfile : file?
;;; takes two file names as parameters, reads an HTML file from the infile, adds a table of contents at the start of the body, and writes the result to outfile
(define add-toc
  (lambda (infile outfile)
    (let ([lst (find-format-all (xml->string (file->xml infile)))])
      (string->file (when #t
                    (add-hash-with document-id (find-format-with-id (xml->string (file->xml infile))))
                    (add-hash-without document-id (find-format-without-id (xml->string (file->xml infile))))
                    (string-append "<div class=/toc/>\n" "ul\n"
                                   (generate-body lst 1 document-id)
                                   "<div>\n" (apply string-append (map (section string-append <> "\n") lst)))) outfile))))

; (xml->string (file->xml "toc-test-input.txt"))
;;(define example-list
;;(find-format-all (xml->string (file->xml "toc-test-input.txt"))))