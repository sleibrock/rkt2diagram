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


(require racket/match
         (only-in racket/list empty? remove-duplicates)
         (only-in racket/string string-join)
         )

(provide define/uml Render-to)

(define *things-to-graph*
  (make-parameter '()))


(define *output-items*
  (make-parameter '()))



(struct Node  (id contents) #:transparent)
(struct Graph (entrynode nodes edges) #:transparent)


(define (Graph-init firstid)
  (Graph firstid
         (make-immutable-hash '())
         '()))

(define (Graph-add-edge G node-id other-id)
  (Graph (Graph-entrynode G)
         (Graph-nodes G)
         (cons `(,node-id . ,other-id)
               (Graph-edges G))))

(define (Graph-add-edges G node-id [other-ids '()])
  (cond
    ([empty?   other-ids] G)
    ([symbol?  other-ids] G)
    ([boolean? other-ids] G)
    (else
     (foldl (λ (pair acc)
              (Graph-add-edge acc (car pair) (cdr pair)))
            G
            (map (λ (gid) (cons node-id gid)) other-ids)))))
  

(define (Graph-add-node G node #:connects [uids #f])
  (let ([nodes (Graph-nodes G)])
    (if (hash-has-key? nodes (Node-id node))
        (if (not (eqv? #f uids))
            G
            (Graph-add-edges G (Node-id node) uids))
        (Graph-add-edges
         (Graph (Graph-entrynode G)
                (hash-set nodes (Node-id node) node)
                (Graph-edges G))
         (Node-id node)
         uids))))



(define-syntax-rule (define/uml code ...)
  (begin
    (*things-to-graph* (cons `(define code ...)
                             (*things-to-graph*)))
    (define code ...)))



;; Convert a singular piece of code into a list of graph items
;; each item will be then later added to a graph with a foldl
(define (block->graph-items code)
  (define (inner code uid gcc #:next-section [ns #f])
    (printf "Code: ~a\n" code)
    (printf "UID: ~a\n" uid)
    (printf "Next section: ~a\n" ns)
    (if (empty? code)
        gcc
        (match code
          ; Add the define header, then recurse over the code
          ([list 'define (list f args ...) datum ...]
           (let ([next-id (gensym)])
             (inner datum next-id
                    (Graph-add-node gcc (Node uid (format "~a ~a" f args))
                                    #:connects (list next-id)))))
          ([list 'if C T F]
           (let ([leftid (gensym)]
                 [rightid (gensym)])
             (let ([rightc (inner F rightid gcc #:next-section ns)])
               (let ([leftc (inner T leftid rightc #:next-section ns)])
                 (Graph-add-node leftc
                                 (Node uid (format "if ~a" C))
                                 #:connects (list leftid rightid))))))

          ([list x ...]
           (let ([next-id (gensym)])
             (inner (cdr x) next-id 
                    (inner (car x) uid gcc
                           #:next-section (if (empty? (cdr x)) #f next-id)))))
          
          (dat (Graph-add-node gcc
                               (Node uid (format "~a" dat))
                               #:connects
                               (if (not (eqv? #f ns))
                                   (list ns)
                                   '())))
          )))
  (let ([first-sym (gensym)])
    (inner code first-sym (Graph-init first-sym))))



(define (find-edges k edges)
  (remove-duplicates
   (map cdr
        (filter (λ (edge) (eqv? k (car edge)))
                edges))))


(define (write-graph-dot G)
  (displayln "digraph {")

  ; write all the nodes out (somehow...)
  (for ([key-val (hash->list (Graph-nodes G))])
    (define k (car key-val))
    (define node (cdr key-val))
    (printf "~a [label=\"~a\"]\n"
            (Node-id node)
            (Node-contents node)))

  ; then connect all the edges
  (for ([edge-p (remove-duplicates (Graph-edges G))])
    (printf "~a -> ~a\n"
            (car edge-p)
            (cdr edge-p)))
  
  (displayln "}"))
  



(define (Render-to fname)

  (define graphs (map block->graph-items (*things-to-graph*)))
  (for ([g graphs])
    (displayln g))
  
  (call-with-output-file fname
    #:exists 'replace
    (λ (out)
      (parameterize ([current-output-port out])
        (for ([g graphs])
          (write-graph-dot g)))))) 


;; testing section
(define/uml (pet animal time)
  (if (string=? animal "dog")
      (if (< time 12)
          "Walk time"
          "Lunch time")
      "You don't know what to do with this animal")
  (if (= time 12)
      "Morbin time"
      "Not morbin time")
  (displayln "get morbed")
  (void))

(Render-to "Test.dot")

; end
