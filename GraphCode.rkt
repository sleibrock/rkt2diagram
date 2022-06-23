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


(define *verbosity*
  (make-parameter #f))


(define (puts . args)
  (when (*verbosity*)
    (apply printf args)))


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



;; steps to converting to a graph
;; 1. differentiate between a (define x y) and a (define (f ...) c ...)
;; 2. separate the two into different categories (binding vs function)
;; 3. create a generalized code parser that parses all types of conditionals
;; 4. if we reach a `begin` block, we start over and treat it like a sequence

;; This is the main entrypoint to defer to the correct branches
(define (code->graph code)
  (match code
    ([list 'define (list f args ...) datum ...]
     (let ([id       (gensym)]
           [first-id (gensym)])
       (parse-sequence datum first-id 
                       (Graph-add-node
                        (Graph-init id)
                        (Node id (format "~a ~a" f args))
                        #:connects (list first-id)))))
    ([list 'define varname result]
     (let ([id (gensym)])
       (parse-code result id (Graph-init id))))
    (else (error "wtf is this?"))))



;; A code block is a singular piece of code that can
;; be translated into a graph-like structure by nesting
;; it's components. An If has two branches, cond and
;; case have many, and when/unless/begin are considered
;; "sequences" because they contain many pieces of code
(define (parse-code code uid gcc #:next-section [ns #f])
  (puts "--Code--\n")
  (puts "--Code--\n")
  (puts "uid: ~a\n" uid)
  (puts "C: ~a\n" code)
  (match code
    ([list 'let (list binds ...) seq ...]
     (displayln "Got a Let"))
    ([list 'if C T F]
     (let ([leftid  (gensym)]
           [rightid (gensym)])
       (let ([rightcc (parse-code F rightid gcc #:next-section ns)])
         (let ([leftcc (parse-code T leftid rightcc #:next-section ns)])
           (Graph-add-node leftcc (Node uid (format "if ~a" C))
                           #:connects (list leftid rightid))))))
    ([list 'cond conds ...]
     (displayln "Got a cond"))
    ([list 'case v seq ...]
     (displayln "Got a case"))
    ([list 'when C seq ...]
     (displayln "Got a when"))
    ([list 'unless C seq ...]
     (displayln "Got an unless"))
    ([list 'begin seq ...]
     (displayln "Got a begin"))
  (datum
   (Graph-add-node gcc
                   (Node uid (format "~a" datum))
                   #:connects
                   (if (not (eqv? #f ns))
                            (list ns)
                            '())))))


;; A sequence of code is different.
;; A sequence is a linear list of actions to take, which may not have
;; any impact on other functions in the sequence, but are still executed
;; in a linear first-in-first-out fashion.
;; A sequence is like `begin`, where results are stored, but discarded
;; per each execution, if not bound locally via let.
;;
;; Example:
;; ```
;; (begin
;;  (displayln hello)
;;  (+ 1 2 3))
;; ```
;; While displayln returns void, it's result is not connected to
;; the + call, but displayln occurs before +, so in that sense,
;; the "flow" is displayln first, then +, meaning displayln
;; connects to +
;;
;; To parse this, we must iterate over all expressions in a sequence
;; and link the final outputs of each code piece to the next sequence
;; by using the #:next-section hidden variable in parse-code.
;; parse-code will use #:next-section when it's at the bottom of an
;; expression tree to connect to the next sequence.
(define (parse-sequence codes sid gcc)
  (printf "----Section----\n")
  (printf "sid: ~a\n" sid)
  (printf "c: ~a\n" codes)
  (match codes
    ([list lastcode]
     (displayln "GOT ONE ITEM")
     (parse-code lastcode sid gcc))
    ([list code1 code2]
     (displayln "GOT TWO ITEMS")
     (let ([id1 (gensym)]
           [id2 (gensym)])
       (parse-code code2 id2
                   (parse-code code1 sid gcc
                               #:next-section id2))))
    ([list codes ...]
     (displayln "GOT N ITEMS")
     (let ([id1 (gensym)]
           [id2 (gensym)])
       (parse-sequence (cdr codes)
                       id2
                       (parse-code (car codes) sid gcc
                                   #:next-section id2))))
    (_ gcc)))
     



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
  (define graphs (map code->graph (*things-to-graph*)))
  (for ([g graphs])
    (displayln g))
  
  (call-with-output-file fname
    #:exists 'replace
    (λ (out)
      (parameterize ([current-output-port out])
        (for ([g graphs])
          (write-graph-dot g)))))) 


(define/uml (code->uml code)
  (displayln "You want to make a graph")
  (if (list? code)
      (code->graph code)
      "Not a code list")
  (displayln "Your code was UML'd"))


;(for ([code (*things-to-graph*)])
;  (displayln code)
;  (displayln (code->graph code)))

(Render-to "Test.dot")


; end
