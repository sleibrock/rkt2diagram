#lang racket/base

#|
A Graphviz UML free code generator

Generate Graphviz UML models from your code
Heavily Work-In-Progress


CONCEPT
---
Convert a code expression into a UML diagram that
can be followed as if it were laid out manually,
removing the need to design UML first, code later.
You can now code first, then have UML documentation
later.

---
Convert the expressions into graph form

(if (= x 5)
    (displayln "is true")
    (displayln "isn't true"))

digraph {
         "if (= x 5)" -> "(displayln "is true")"
         "if (= x 5)" -> "(displayln "isn't true")"
}

---

(cond
  ([= x 5] 3)
  (else    4))

digraph {
         "cond" -> "(= x 5)"
         "cond" -> "else"
         "(= x 5)" -> "3"
         "else" -> "4"
}

---

(case x
  ((a b c) 4)
  (else    0))

digraph {
         "case" -> "(a b c)"
         "case" -> "else"
         "(a b c)" -> "4"
         "else" -> 0
}


The program should be able to mimic these basic flows, as well
as conditionals like `when` and `unless`, which are easy
to implement from the same flow as `if`


Multiple sections of code (aka a sequence of code) like a `begin`
sequence should be connected properly.
If a `when` happens before an `if`, then the result of the `when`
should connect to the following `if` body, and so on and so forth,
exactly as it would with Racket code.


There will be at least 5 rules for code here to render.
* if
* cond
* case
* when
* unless

Since code can look similar in many places, we need to bind
IDs to each code datum using `gensym` to generate IDs on the fly.
This will be used to build our virtual graph traversal structure
|#


(require racket/match)

(provide Define Render-to)

(define *things-to-graph*
  (make-parameter '()))


(define *output-items*
  (make-parameter '()))



(struct Node  (id contents))
(struct Graph (nodes edges))


(define (Graph-init)
  (Graph (make-immutable-hash '())))


(define (Graph-add-connection N1 N2)



(define-syntax-rule (Define code ...)
  (begin
    (*things-to-graph* (cons `(code ...)
                             (*things-to-graph*)))
    (define code ...)))


(define (build-graph code-datums)
  (define (inner code gcc)
    (if (empty? code)
        gcc
        (let ([head (car code)]
              [tail (cdr code)])
          (if (empty? tail)
              (inner tail (graph-update ))


(define (Render-to fname)
  (for ([x (*things-to-graph*)])
    0))


(Define g 3)

;; testing section
(Define (f x)
  (if (= x 3) 3 4))


(Render-to "Test.dot")

; end
